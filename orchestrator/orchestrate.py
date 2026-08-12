#!/usr/bin/env python3
"""OpenSpec PDCA orchestrator.

Drives the full OpenSpec artifact workflow (proposal -> specs -> design ->
tasks -> plan -> do -> check -> act -> archive) using opencode backed by a
local Qwen 3.6 model served via llama.cpp.

Loop semantics:
  - Read `openspec status --change <name> --json` to find the first `ready`
    artifact, run `openspec instructions <id> --change <name>` to get the
    template/context, dispatch artifact generation to opencode, write the
    output, then advance to the next artifact. Repeat until all artifacts
    are `complete`.
  - Pause whenever opencode surfaces a question (the `question` tool or an
    explicit `<QUESTION>...</QUESTION>` block in the output). Write the
    question to `questions/<change>.md`, wait for a human answer on stdin,
    then resume.
  - After the last artifact: run `openspec validate --change <name>` and
    `openspec instructions apply --change <name>` (implementation check).
    If validation passes and no remediation items remain, auto-archive
    with `openspec archive --change <name> -y`.
  - Remediation is bounded: re-feed each flagged issue as a fix-up pass
    for up to `--max-remediation` (default 3) rounds before giving up.

CLI: see `python3 orchestrator/orchestrate.py --help`.

Requires `openspec`, `opencode`, and a reachable llama.cpp server (Qwen 3.6)
on PATH / network. Pure stdlib except PyYAML (already in the Graphos dev shell).
"""

from __future__ import annotations

import argparse
import dataclasses
import datetime as _dt
import json
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import time
import traceback
from pathlib import Path
from typing import Any, Iterable, Optional

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - dev shell provides pyyaml
    yaml = None


# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

REPO_ROOT = Path(os.environ.get("ORCHESTRATOR_REPO_ROOT") or os.getcwd())
_ORCHESTRATOR_DIR = REPO_ROOT / "orchestrator"
LOG_DIR = REPO_ROOT / "graphos-out" / "orchestrator"
METRICS_FILE = LOG_DIR / "metrics.jsonl"

DEFAULT_MODEL = "llama/qwen3.6-35b-a3b"
DEFAULT_OPENCODE_AGENT = "build"
DEFAULT_MAX_REMEDIATION = 3
DEFAULT_OPENSPEC_TIMEOUT = 120
DEFAULT_OPENCODE_TIMEOUT = 1800  # 30 min per artifact
DEFAULT_LLAMA_BASEURL = "http://100.120.26.64:8080/v1"

QUESTION_BLOCK_RE = re.compile(
    r"<QUESTION>(?P<body>.*?)</QUESTION>", re.IGNORECASE | re.DOTALL
)


# --------------------------------------------------------------------------- #
# Data classes
# --------------------------------------------------------------------------- #


@dataclasses.dataclass
class ArtifactStatus:
    id: str
    status: str
    output_path: str
    resolved_output_path: str
    missing_deps: list[str] = dataclasses.field(default_factory=list)

    @property
    def is_ready(self) -> bool:
        return self.status == "ready"

    @property
    def is_complete(self) -> bool:
        return self.status in ("complete", "done")


@dataclasses.dataclass
class ChangeStatus:
    name: str
    schema: str
    change_root: Path
    artifacts: list[ArtifactStatus]
    is_complete: bool

    @classmethod
    def from_json(cls, payload: dict[str, Any]) -> "ChangeStatus":
        arts = [
            ArtifactStatus(
                id=a["id"],
                status=a["status"],
                output_path=a.get("outputPath", ""),
                resolved_output_path=a.get("resolvedOutputPath", "")
                or a.get("outputPath", ""),
                missing_deps=a.get("missingDeps", []),
            )
            for a in payload.get("artifacts", [])
        ]
        return cls(
            name=payload["changeName"],
            schema=payload.get("schemaName", ""),
            change_root=Path(payload.get("changeRoot", "")),
            artifacts=arts,
            is_complete=bool(payload.get("isComplete", False)),
        )

    def next_ready(self) -> Optional[ArtifactStatus]:
        for a in self.artifacts:
            if a.is_ready:
                return a
        return None


@dataclasses.dataclass
class StepResult:
    artifact_id: str
    ok: bool
    output_path: Optional[Path]
    questions: list[str]
    raw_output: str
    elapsed_s: float
    error: Optional[str] = None


# --------------------------------------------------------------------------- #
# Logging
# --------------------------------------------------------------------------- #


class Logger:
    def __init__(self, change: str, log_dir: Path = LOG_DIR) -> None:
        self.change = change
        self.log_dir = log_dir
        self.log_dir.mkdir(parents=True, exist_ok=True)
        stamp = _dt.datetime.now().strftime("%Y%m%d-%H%M%S")
        self.run_log = log_dir / f"{change}-{stamp}.log"
        self._fh = open(self.run_log, "a", encoding="utf-8")

    def _ts(self) -> str:
        return _dt.datetime.now().isoformat(timespec="seconds")

    def log(self, msg: str, level: str = "INFO") -> None:
        line = f"[{self._ts()}] [{level}] {msg}"
        self._fh.write(line + "\n")
        self._fh.flush()
        print(line, file=sys.stderr)

    def info(self, msg: str) -> None:
        self.log(msg, "INFO")

    def warn(self, msg: str) -> None:
        self.log(msg, "WARN")

    def error(self, msg: str) -> None:
        self.log(msg, "ERROR")

    def metric(self, **fields: Any) -> None:
        record = {
            "ts": _dt.datetime.now().isoformat(timespec="milliseconds"),
            "change": self.change,
            **fields,
        }
        with open(METRICS_FILE, "a", encoding="utf-8") as f:
            f.write(json.dumps(record, ensure_ascii=False) + "\n")


# --------------------------------------------------------------------------- #
# Shell helpers
# --------------------------------------------------------------------------- #


def run(
    cmd: list[str],
    *,
    cwd: Path = REPO_ROOT,
    timeout: int = DEFAULT_OPENSPEC_TIMEOUT,
    env: Optional[dict[str, str]] = None,
    capture: bool = True,
) -> subprocess.CompletedProcess[str]:
    pretty = " ".join(shlex.quote(c) for c in cmd)
    if not capture:
        return subprocess.run(
            cmd, cwd=str(cwd), env=env, check=False, timeout=timeout
        )
    proc = subprocess.run(
        cmd,
        cwd=str(cwd),
        env=env,
        check=False,
        timeout=timeout,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if proc.returncode != 0 and proc.stderr:
        # surface stderr for debugging but do not raise
        sys.stderr.write(proc.stderr)
    return proc


def require_tool(name: str) -> str:
    path = shutil.which(name)
    if not path:
        raise RuntimeError(
            f"required tool '{name}' not found on PATH. "
            "Run inside the Graphos dev shell (nix-shell shell.nix)."
        )
    return path


# --------------------------------------------------------------------------- #
# OpenSpec CLI wrappers
# --------------------------------------------------------------------------- #


def openspec_status(change: str) -> ChangeStatus:
    proc = run(
        ["openspec", "status", "--change", change, "--json"],
        timeout=DEFAULT_OPENSPEC_TIMEOUT,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"openspec status failed for '{change}': {proc.stderr.strip()}"
        )
    try:
        payload = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"openspec status returned non-JSON: {exc}") from exc
    return ChangeStatus.from_json(payload)


def openspec_instructions(change: str, artifact: str) -> dict[str, Any]:
    proc = run(
        [
            "openspec",
            "instructions",
            artifact,
            "--change",
            change,
            "--json",
        ],
        timeout=DEFAULT_OPENSPEC_TIMEOUT,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"openspec instructions {artifact} failed: {proc.stderr.strip()}"
        )
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"openspec instructions returned non-JSON: {exc}"
        ) from exc


def openspec_validate(change: str) -> dict[str, Any]:
    proc = run(
        [
            "openspec",
            "validate",
            change,
            "--json",
            "--no-interactive",
        ],
        timeout=DEFAULT_OPENSPEC_TIMEOUT,
    )
    if proc.returncode != 0 and not proc.stdout:
        raise RuntimeError(
            f"openspec validate failed: {proc.stderr.strip()}"
        )
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"openspec validate returned non-JSON: {exc}"
        ) from exc


def openspec_archive(change: str, skip_specs: bool = False) -> bool:
    cmd = ["openspec", "archive", change, "-y", "--json"]
    if skip_specs:
        cmd.append("--skip-specs")
    proc = run(cmd, timeout=DEFAULT_OPENSPEC_TIMEOUT)
    if proc.returncode != 0:
        sys.stderr.write(proc.stderr)
        return False
    return True


def list_active_changes() -> list[str]:
    proc = run(
        ["openspec", "list", "--json"], timeout=DEFAULT_OPENSPEC_TIMEOUT
    )
    if proc.returncode != 0:
        return []
    try:
        payload = json.loads(proc.stdout)
    except json.JSONDecodeError:
        return []
    items = (
        payload.get("changes", [])
        if isinstance(payload, dict)
        else (payload if isinstance(payload, list) else [])
    )
    changes: list[str] = []
    for item in items:
        if not isinstance(item, dict):
            continue
        # Archived changes live under changes/archive/; skip them.
        path = item.get("path", "") or item.get("dir", "")
        if "/archive/" in path or path.endswith("/archive"):
            continue
        name = item.get("name") or item.get("id")
        if name:
            changes.append(name)
    return changes


# --------------------------------------------------------------------------- #
# opencode driver
# --------------------------------------------------------------------------- #


def build_opencode_prompt(
    change: str, artifact: str, instr: dict[str, Any]
) -> str:
    """Compose the prompt sent to opencode for one artifact generation."""
    template = instr.get("template", "")
    instruction = instr.get("instruction", "")
    context = instr.get("context", "")
    rules = instr.get("rules", [])
    output_path = instr.get("resolvedOutputPath", "") or instr.get(
        "outputPath", ""
    )

    rules_text = (
        "\n".join(f"- {r}" for r in rules) if isinstance(rules, list) else str(rules)
    )

    return (
        f"You are driving the OpenSpec PDCA workflow for change '{change}'.\n"
        f"Generate the '{artifact}' artifact now.\n\n"
        f"## Instruction\n{instruction}\n\n"
        f"## Template\n{template}\n\n"
        f"## Project context\n{context}\n\n"
        f"## Rules\n{rules_text}\n\n"
        f"## Output\nWrite the completed artifact to:\n{output_path}\n\n"
        f"Write the full file content directly. Do not ask for permission. "
        f"If you MUST ask a human a blocking question, wrap it in "
        f"<QUESTION>...</QUESTION> tags and stop. Otherwise produce the "
        f"complete artifact file.\n"
    )


@dataclasses.dataclass
class OpenCodeResult:
    """Parsed result of one `opencode run --format json` invocation."""
    text: str  # concatenated assistant text output
    questions: list[str]  # questions surfaced via the `question` tool
    raw: str  # the raw JSONL stream


def parse_opencode_stream(raw: str) -> OpenCodeResult:
    """Parse the opencode `--format json` JSONL event stream.

    Event types of interest:
      - ``text``: assistant text chunk -> append ``part.text``.
      - ``tool_use`` with ``part.tool == "question"``: a blocking question;
        extract the question text from the tool input.
      - ``error``: surface as an exception-like message in the text.
    Anything else (step_start/step_finish/tool_result) is ignored.
    """
    text_parts: list[str] = []
    questions: list[str] = []
    for line in raw.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            evt = json.loads(line)
        except json.JSONDecodeError:
            continue
        etype = evt.get("type")
        part = evt.get("part", {}) if isinstance(evt.get("part"), dict) else {}
        if etype == "text":
            chunk = part.get("text", "")
            if chunk:
                text_parts.append(chunk)
        elif etype == "tool_use" and part.get("tool") == "question":
            state = part.get("state", {})
            inp = (
                state.get("input", {})
                if isinstance(state, dict)
                else {}
            )
            q = inp.get("question") or inp.get("text") or json.dumps(inp)
            if isinstance(q, str) and q.strip():
                questions.append(q.strip())
        elif etype == "error":
            err = evt.get("error", {})
            msg = err.get("message", "") if isinstance(err, dict) else str(err)
            text_parts.append(f"[opencode error] {msg}")
    return OpenCodeResult(
        text="".join(text_parts),
        questions=questions,
        raw=raw,
    )


def extract_questions_from_text(text: str) -> list[str]:
    """Fallback: detect <QUESTION>...</QUESTION> blocks in plain text."""
    questions: list[str] = []
    for m in QUESTION_BLOCK_RE.finditer(text):
        body = m.group("body").strip()
        if body:
            questions.append(body)
    return questions


def run_opencode(
    prompt: str,
    *,
    model: str,
    agent: str,
    cwd: Path = REPO_ROOT,
    timeout: int = DEFAULT_OPENCODE_TIMEOUT,
) -> OpenCodeResult:
    """Run opencode headless with --format json and return parsed result."""
    cmd = [
        "opencode",
        "run",
        "--format",
        "json",
        "--model",
        model,
        "--auto",
        "--dir",
        str(cwd),
        prompt,
    ]
    if agent:
        cmd.extend(["--agent", agent])
    proc = run(cmd, cwd=cwd, timeout=timeout)
    raw = proc.stdout or ""
    if not raw and proc.stderr:
        raw = proc.stderr
    result = parse_opencode_stream(raw)
    # Merge fallback regex matches from assistant text.
    result.questions.extend(extract_questions_from_text(result.text))
    return result


# --------------------------------------------------------------------------- #
# Question handling
# --------------------------------------------------------------------------- #


def write_question_file(change: str, questions: list[str], log: Logger) -> Path:
    qdir = _ORCHESTRATOR_DIR / "questions"
    qdir.mkdir(parents=True, exist_ok=True)
    qpath = qdir / f"{change}.md"
    stamp = _dt.datetime.now().isoformat(timespec="seconds")
    body = [
        f"# Open questions for change `{change}`",
        "",
        f"_Generated: {stamp}_",
        "",
    ]
    for i, q in enumerate(questions, 1):
        body.append(f"## Question {i}")
        body.append("")
        body.append(q.strip())
        body.append("")
        body.append("<!-- answer below -->")
        body.append("")
    qpath.write_text("\n".join(body), encoding="utf-8")
    log.warn(f"PAUSED — {len(questions)} question(s) written to {qpath}")
    return qpath


def wait_for_answers(change: str, qpath: Path, log: Logger) -> list[str]:
    """Block on stdin until the user signals answers are ready.

    Returns the answers parsed from the question file. If stdin is not a TTY
    (e.g. when run via `devenv tasks run` with no attached terminal), exits with
    a distinct code (10) so the caller knows to answer and re-run.
    """
    if not sys.stdin.isatty():
        log.error(
            f"questions written to {qpath}. "
            "Edit the file to add answers below each '<!-- answer below -->' "
            "marker, then re-run the orchestrator. Exiting with code 10 "
            "(PAUSED_QUESTIONS)."
        )
        sys.exit(10)
    log.info(
        "Answer the questions in the file, then press <Enter> to resume "
        f"(or type 'skip' to abort this change)."
    )
    try:
        resp = input(f"[orchestrator:{change}] resume? <Enter>/skip > ")
    except EOFError:
        resp = "skip"
    if resp.strip().lower() == "skip":
        return []
    text = qpath.read_text(encoding="utf-8")
    answers: list[str] = []
    cur: list[str] = []
    in_answer = False
    for line in text.splitlines():
        if "<!-- answer below -->" in line:
            in_answer = True
            cur = []
            continue
        if in_answer and line.startswith("## Question"):
            answers.append("\n".join(cur).strip())
            in_answer = False
            cur = []
            continue
        if in_answer:
            cur.append(line)
    if in_answer:
        answers.append("\n".join(cur).strip())
    return [a for a in answers if a]


# --------------------------------------------------------------------------- #
# Core step: generate one artifact
# --------------------------------------------------------------------------- #


def generate_artifact(
    change: str,
    artifact: ArtifactStatus,
    *,
    model: str,
    agent: str,
    log: Logger,
    extra_context: str = "",
    timeout: int = DEFAULT_OPENCODE_TIMEOUT,
) -> StepResult:
    start = time.monotonic()
    log.info(f"artifact '{artifact.id}' — fetching instructions")
    instr = openspec_instructions(change, artifact.id)
    prompt = build_opencode_prompt(change, artifact.id, instr)
    if extra_context:
        prompt += (
            "\n\n## Additional context (remediation / prior answers)\n"
            + extra_context
            + "\n"
        )

    log.info(f"artifact '{artifact.id}' — dispatching to opencode ({model})")
    oc_result = run_opencode(
        prompt, model=model, agent=agent, timeout=timeout
    )
    elapsed = time.monotonic() - start

    questions = oc_result.questions
    change_root = change_status_root(change)
    out_path = Path(artifact.resolved_output_path) if artifact.resolved_output_path else None
    if out_path and out_path.is_absolute():
        # resolvedOutputPath is absolute; make it relative to the change root
        # so Path.glob() gets a relative pattern (Python's glob rejects absolute).
        try:
            out_path = out_path.relative_to(change_root)
        except ValueError:
            # Falls outside the change root — keep absolute for the exists() check.
            pass
    elif out_path:
        out_path = Path(change_root) / out_path

    # Glob-style output paths (e.g. "specs/**/*.md") match multiple files;
    # treat any non-empty glob match as success.
    ok = bool(out_path and out_path.exists())
    if not ok and out_path and ("*" in str(out_path) or "?" in str(out_path)):
        glob_pattern = str(out_path)
        if Path(glob_pattern).is_absolute():
            # Shouldn't happen after the relative_to() above, but guard anyway:
            # glob against the change root using the relative tail.
            try:
                glob_pattern = str(Path(glob_pattern).relative_to(change_root))
            except ValueError:
                pass
        matches = list(change_root.glob(glob_pattern))
        ok = bool(matches)
        if ok:
            out_path = matches[0]
    return StepResult(
        artifact_id=artifact.id,
        ok=ok,
        output_path=out_path,
        questions=questions,
        raw_output=oc_result.raw,
        elapsed_s=elapsed,
    )


def change_status_root(change: str) -> Path:
    return REPO_ROOT / "openspec" / "changes" / change


# --------------------------------------------------------------------------- #
# Remediation
# --------------------------------------------------------------------------- #


def collect_validation_issues(payload: dict[str, Any]) -> list[str]:
    issues: list[str] = []
    for item in payload.get("items", []):
        if not isinstance(item, dict):
            continue
        for iss in item.get("issues", []):
            if isinstance(iss, dict):
                lvl = iss.get("level") or iss.get("severity") or "ERROR"
                if str(lvl).upper() in ("ERROR", "FATAL"):
                    issues.append(iss.get("message", str(iss)))
            else:
                issues.append(str(iss))
    return issues


def remediate(
    change: str,
    issues: list[str],
    *,
    model: str,
    agent: str,
    log: Logger,
    max_rounds: int,
    timeout: int,
) -> bool:
    for rnd in range(1, max_rounds + 1):
        log.info(f"remediation round {rnd}/{max_rounds} — {len(issues)} issue(s)")
        joined = "\n".join(f"- {i}" for i in issues)
        extra = (
            "The previous artifact failed validation with these issues. "
            "Fix the affected files directly and re-emit the corrected "
            "content:\n" + joined
        )
        # Re-run the last 'ready' artifact (which will be the failed one).
        status = openspec_status(change)
        art = status.next_ready()
        if not art:
            log.warn("no ready artifact to remediate; validating again")
            break
        result = generate_artifact(
            change,
            art,
            model=model,
            agent=agent,
            log=log,
            extra_context=extra,
            timeout=timeout,
        )
        log.metric(
            event="remediation",
            round=rnd,
            artifact=result.artifact_id,
            ok=result.ok,
            elapsed_s=result.elapsed_s,
        )
        # Re-validate.
        payload = openspec_validate(change)
        issues = collect_validation_issues(payload)
        if not issues:
            log.info(f"remediation round {rnd}: clean")
            return True
    log.error(f"remediation exhausted after {max_rounds} rounds")
    return False


# --------------------------------------------------------------------------- #
# Per-change driver
# --------------------------------------------------------------------------- #


def drive_change(
    change: str,
    *,
    model: str,
    agent: str,
    max_remediation: int,
    timeout: int,
    log: Logger,
) -> bool:
    log.info(f"=== driving change '{change}' (schema from status) ===")
    # Main artifact loop.
    while True:
        status = openspec_status(change)
        if status.is_complete:
            log.info("all artifacts complete — proceeding to verification")
            break
        art = status.next_ready()
        if not art:
            log.warn("no ready artifact and change not complete — stuck")
            return False
        result = generate_artifact(
            change, art, model=model, agent=agent, log=log, timeout=timeout
        )
        log.metric(
            event="artifact",
            artifact=result.artifact_id,
            ok=result.ok,
            questions=len(result.questions),
            elapsed_s=result.elapsed_s,
        )
        if result.questions:
            qpath = write_question_file(change, result.questions, log)
            answers = wait_for_answers(change, qpath, log)
            if not answers:
                log.warn("no answers provided — aborting change")
                return False
            extra = "\n".join(f"A: {a}" for a in answers)
            # Re-run the same artifact with the answers as context.
            log.info(f"resuming artifact '{art.id}' with user answers")
            result = generate_artifact(
                change,
                art,
                model=model,
                agent=agent,
                log=log,
                extra_context=extra,
                timeout=timeout,
            )
            log.metric(
                event="artifact_after_answer",
                artifact=result.artifact_id,
                ok=result.ok,
                elapsed_s=result.elapsed_s,
            )
        if not result.ok:
            log.error(f"artifact '{art.id}' did not produce output")
            return False
        log.info(f"artifact '{art.id}' written to {result.output_path}")

    # Verification gate.
    log.info("running openspec validate")
    payload = openspec_validate(change)
    issues = collect_validation_issues(payload)
    log.metric(
        event="validate",
        issues=len(issues),
    )
    if issues:
        ok = remediate(
            change,
            issues,
            model=model,
            agent=agent,
            log=log,
            max_rounds=max_remediation,
            timeout=timeout,
        )
        if not ok:
            log.error("validation failed after remediation — not archiving")
            return False
    else:
        log.info("validation clean")

    # Implementation check (apply instructions) — informational only.
    try:
        apply_proc = run(
            [
                "openspec",
                "instructions",
                "apply",
                "--change",
                change,
                "--json",
            ],
            timeout=DEFAULT_OPENSPEC_TIMEOUT,
        )
        if apply_proc.stdout:
            apply_payload = json.loads(apply_proc.stdout)
            ctx_files = apply_payload.get("contextFiles", {})
            log.metric(event="apply_check", context_files=len(ctx_files))
    except Exception as exc:  # noqa: BLE001
        log.warn(f"apply check skipped: {exc}")

    # Archive.
    log.info(f"archiving change '{change}'")
    # Tooling/infra changes with no spec deltas use --skip-specs when needed.
    skip = _needs_skip_specs(change)
    archived = openspec_archive(change, skip_specs=skip)
    log.metric(event="archive", ok=archived, skip_specs=skip)
    if archived:
        log.info(f"archived '{change}' successfully")
        return True
    log.error(f"archive failed for '{change}'")
    return False


def _needs_skip_specs(change: str) -> bool:
    """Heuristic: if the change has no specs/ delta directory, pass --skip-specs."""
    specs_dir = change_status_root(change) / "specs"
    if not specs_dir.exists():
        return True
    return not any(specs_dir.rglob("*.md"))


# --------------------------------------------------------------------------- #
# llama.cpp health check
# --------------------------------------------------------------------------- #


def check_llama_server(baseurl: str, log: Logger, timeout: int = 10) -> bool:
    """Lightweight reachability check for the llama.cpp /v1/models endpoint."""
    import urllib.request
    import urllib.error

    url = baseurl.rstrip("/") + "/models"
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            if resp.status == 200:
                log.info(f"llama.cpp server reachable at {baseurl}")
                return True
    except Exception as exc:  # noqa: BLE001
        log.warn(f"llama.cpp server not reachable at {baseurl}: {exc}")
        return False
    return False


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        prog="orchestrate",
        description="Drive OpenSpec PDCA artifacts via opencode + Qwen 3.6/llama.cpp.",
    )
    p.add_argument(
        "change",
        nargs="?",
        help="Change name to drive. Use '--all' to drive every active change.",
    )
    p.add_argument(
        "--all",
        action="store_true",
        help="Drive every active (non-archived) change serially.",
    )
    p.add_argument(
        "--model",
        default=DEFAULT_MODEL,
        help=f"opencode model id (default: {DEFAULT_MODEL}).",
    )
    p.add_argument(
        "--agent",
        default=DEFAULT_OPENCODE_AGENT,
        help=f"opencode agent (default: {DEFAULT_OPENCODE_AGENT}).",
    )
    p.add_argument(
        "--max-remediation",
        type=int,
        default=DEFAULT_MAX_REMEDIATION,
        help="Max remediation rounds before giving up (default: 3).",
    )
    p.add_argument(
        "--timeout",
        type=int,
        default=DEFAULT_OPENCODE_TIMEOUT,
        help="Per-artifact opencode timeout in seconds (default: 1800).",
    )
    p.add_argument(
        "--llama-baseurl",
        default=os.environ.get("LLAMA_BASEURL", DEFAULT_LLAMA_BASEURL),
        help="llama.cpp OpenAI-compatible base URL.",
    )
    p.add_argument(
        "--no-health-check",
        action="store_true",
        help="Skip the llama.cpp reachability check.",
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the plan but do not invoke opencode.",
    )
    return p.parse_args(argv)


def select_changes(args: argparse.Namespace, log: Logger) -> list[str]:
    if args.all:
        changes = list_active_changes()
        if not changes:
            log.warn("--all set but no active changes found")
        return changes
    if not args.change:
        log.error("provide a change name or use --all")
        sys.exit(2)
    return [args.change]


def main(argv: Optional[list[str]] = None) -> int:
    args = parse_args(argv)
    log = Logger(args.change or "all")
    log.info(f"orchestrator start | model={args.model} agent={args.agent}")
    log.metric(event="start", model=args.model, agent=args.agent)

    # Tool checks.
    for tool in ("openspec", "opencode"):
        try:
            require_tool(tool)
        except RuntimeError as exc:
            log.error(str(exc))
            return 3

    # llama.cpp health.
    if not args.no_health_check:
        if not check_llama_server(args.llama_baseurl, log):
            log.warn(
                "llama.cpp not reachable — continuing anyway. "
                "Set --no-health-check to silence."
            )

    if args.dry_run:
        log.info("dry-run — would drive: " + ", ".join(select_changes(args, log)))
        return 0

    changes = select_changes(args, log)
    if not changes:
        return 0

    overall_ok = True
    for change in changes:
        try:
            ok = drive_change(
                change,
                model=args.model,
                agent=args.agent,
                max_remediation=args.max_remediation,
                timeout=args.timeout,
                log=log,
            )
            overall_ok = overall_ok and ok
            log.metric(event="change_done", change=change, ok=ok)
        except KeyboardInterrupt:
            log.warn(f"interrupted by user on change '{change}'")
            return 130
        except Exception as exc:  # noqa: BLE001
            log.error(f"change '{change}' crashed: {exc}")
            log.error(traceback.format_exc())
            log.metric(event="change_crash", change=change, error=str(exc))
            overall_ok = False

    log.metric(event="orchestrator_done", ok=overall_ok)
    return 0 if overall_ok else 1


def _handle_signal(signum: int, frame: Any) -> None:  # noqa: ANN401
    sys.stderr.write(
        f"\n[orchestrator] received signal {signum}, exiting.\n"
    )
    sys.exit(128 + signum)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, _handle_signal)
    signal.signal(signal.SIGTERM, _handle_signal)
    sys.exit(main())