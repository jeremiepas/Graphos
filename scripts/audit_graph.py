#!/usr/bin/env python3
"""Audit graph.json against GRAPH_REPORT.md and baseline quality targets."""

import json, os, re, sys
from collections import Counter, defaultdict

OUT = os.environ.get("GRAPHO_OUT", "graphos-out")
GRAPH = os.path.join(OUT, "graph.json")
REPORT = os.path.join(OUT, "GRAPH_REPORT.md")


def connected_components(nodes, edges):
    adj = defaultdict(set)
    node_set = {n["id"] for n in nodes}
    for e in edges:
        adj[e["source"]].add(e["target"])
        adj[e["target"]].add(e["source"])
    seen = set()
    comps = 0
    isolated = 0
    sizes = []
    for n in node_set:
        if n in seen:
            continue
        comps += 1
        stack = [n]
        seen.add(n)
        sz = 0
        while stack:
            u = stack.pop()
            sz += 1
            for v in adj[u]:
                if v not in seen:
                    seen.add(v)
                    stack.append(v)
        if sz == 1:
            isolated += 1
        sizes.append(sz)
    sizes.sort(reverse=True)
    return comps, isolated, sizes


def main():
    errors = []
    warnings = []
    g = json.load(open(GRAPH))
    nodes = g["nodes"]
    edges = g["edges"]
    comms = g.get("communities", {})
    if isinstance(comms, dict):
        ncomm = len(comms)
    else:
        ncomm = len(comms)
    comps, isolated, sizes = connected_components(nodes, edges)
    print(
        f"Nodes: {len(nodes)}  Edges: {len(edges)}  Communities: {ncomm}  Components: {comps}  Isolated: {isolated}"
    )

    # Report parity
    if os.path.exists(REPORT):
        text = open(REPORT).read()
        m = re.search(r"Nodes:\s*(\d+)", text)
        r_nodes = int(m.group(1)) if m else None
        m = re.search(r"Edges:\s*(\d+)", text)
        r_edges = int(m.group(1)) if m else None
        m = re.search(r"Communities:\s*(\d+)", text)
        r_comms = int(m.group(1)) if m else None
        if r_nodes != len(nodes):
            errors.append(f"Report node count {r_nodes} != graph.json {len(nodes)}")
        if r_edges != len(edges):
            errors.append(f"Report edge count {r_edges} != graph.json {len(edges)}")
        if r_comms != ncomm:
            errors.append(f"Report community count {r_comms} != graph.json {ncomm}")
        dups = len(re.findall(r"- \*\*[^*]+ → [^*]+\*\*", text)) != len(
            set(re.findall(r"- \*\*([^*]+ → [^*]+)\*\*", text))
        )
        if dups:
            errors.append("Report has duplicate Surprising Connections")
        print(f"Report totals: nodes={r_nodes} edges={r_edges} comms={r_comms}")
    else:
        warnings.append("GRAPH_REPORT.md not found")

    # Quality
    kind_counts = Counter(n.get("kind") for n in nodes)
    none_count = kind_counts[None]
    truncated = [
        n
        for n in nodes
        if len(n.get("label", "")) == 20 and n.get("kind") in (None, "")
    ]
    cross_file = sum(
        1
        for e in edges
        if e.get("relation") == "imports"
        and next((n.get("source_file") for n in nodes if n["id"] == e["source"]), None)
        != next((n.get("source_file") for n in nodes if n["id"] == e["target"]), None)
    )
    print(
        f"kind=None: {none_count}  truncated(20): {len(truncated)}  cross-file imports: {cross_file}"
    )

    # Islanding check: components approaching the file count means cross-file
    # linking failed (e.g. import resolution broken). Honest doc/content islands
    # are expected, so the bound is per-file, not per-node.
    n_files = len({n.get("source_file") for n in nodes if n.get("source_file")})
    if n_files > 0 and comps >= n_files:
        errors.append(
            f"Too many components: {comps} (>= {n_files} source files — per-file islanding)"
        )

    if cross_file == 0:
        errors.append("No cross-file imports edges")
    if len(truncated) > 0:
        errors.append(f"{len(truncated)} truncated 20-char labels remain")

    if errors:
        print("\nERRORS:")
        for e in errors:
            print(f"  - {e}")
        sys.exit(1)
    if warnings:
        print("\nWARNINGS:")
        for w in warnings:
            print(f"  - {w}")
    print("\nAudit passed.")


if __name__ == "__main__":
    main()
