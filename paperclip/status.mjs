#!/usr/bin/env node
/**
 * `status` — summarize the Paperclip agent's current assigned tasks.
 *
 * Queries the Paperclip API using the injected runtime environment
 * (PAPERCLIP_API_URL, PAPERCLIP_API_KEY, PAPERCLIP_AGENT_ID,
 * PAPERCLIP_COMPANY_ID) and prints a human-readable summary grouped by
 * task status, highlighting the "current focus" task.
 *
 * Run inside a Paperclip heartbeat run so the PAPERCLIP_* env vars are
 * present. Flags override the environment where supplied.
 *
 *   node paperclip/status.mjs
 *   node paperclip/status.mjs --json
 *   node paperclip/status.mjs --agent-id <id> --api-key <token>
 */
const PRIORITY = { critical: 0, high: 1, medium: 2, low: 3 };
const STATUS_ORDER = ['blocked', 'in_progress', 'in_review', 'todo', 'backlog', 'done', 'cancelled'];
const STATUS_ICON = {
  blocked: '🚫',
  in_progress: '🔧',
  in_review: '👀',
  todo: '📝',
  backlog: '🗒️',
  done: '✅',
  cancelled: '❌',
};

function parseArgs(argv) {
  const opts = { json: false };
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    switch (a) {
      case '--json': opts.json = true; break;
      case '--agent-id': opts.agentId = argv[++i]; break;
      case '--company-id': opts.companyId = argv[++i]; break;
      case '--api-url': opts.apiUrl = argv[++i]; break;
      case '--api-key': opts.apiKey = argv[++i]; break;
      case '-h': case '--help': opts.help = true; break;
      default:
        if (a.startsWith('--agent-id=')) opts.agentId = a.slice(13);
        else if (a.startsWith('--company-id=')) opts.companyId = a.slice(15);
        else if (a.startsWith('--api-url=')) opts.apiUrl = a.slice(10);
        else if (a.startsWith('--api-key=')) opts.apiKey = a.slice(10);
        else throw new Error(`Unknown argument: ${a}`);
    }
  }
  return opts;
}

async function readJson(url, token, runId) {
  const headers = { Authorization: `Bearer ${token}` };
  if (runId) headers['X-Paperclip-Run-Id'] = runId;
  const res = await fetch(url, { headers });
  if (!res.ok) {
    const body = await res.text().catch(() => '');
    throw new Error(`API ${res.status} ${res.statusText} for ${url}${body ? `: ${body.slice(0, 200)}` : ''}`);
  }
  return res.json();
}

function sortByPriority(tasks) {
  return [...tasks].sort((a, b) => {
    const pa = PRIORITY[(a.priority || 'low').toLowerCase()] ?? 9;
    const pb = PRIORITY[(b.priority || 'low').toLowerCase()] ?? 9;
    if (pa !== pb) return pa - pb;
    return new Date(b.updatedAt || 0) - new Date(a.updatedAt || 0);
  });
}

function pickCurrentFocus(tasks) {
  const withActive = tasks.find((t) => t.activeRun);
  if (withActive) return withActive;
  const inProgress = sortByPriority(tasks.filter((t) => t.status === 'in_progress'));
  if (inProgress.length) return inProgress[0];
  return sortByPriority(tasks)[0] ?? null;
}

function fmtId(id) {
  // Short identifier for display (e.g. AVI-1348).
  const m = String(id).match(/[A-Z]+-\d+/);
  return m ? m[0] : id;
}

function renderText(data) {
  const { agent, tasks } = data;
  const lines = [];
  const sep = '='.repeat(60);
  const sub = '-'.repeat(60);
  lines.push(sep);
  lines.push(`📋 Paperclip Status — ${agent.name || 'agent'}`);
  lines.push(`   Role     : ${agent.role || '—'}${agent.title ? ` — ${agent.title}` : ''}`);
  lines.push(`   Agent ID : ${agent.id}`);
  lines.push(`   Company  : ${agent.companyId}  (${agent.apiUrl})`);
  lines.push(`   Generated: ${new Date().toISOString()}`);
  lines.push(sep);

  const focus = pickCurrentFocus(tasks);
  if (focus) {
    const ident = focus.identifier || fmtId(focus.id);
    lines.push(`🎯 Current focus : ${ident}  [${focus.priority || 'low'}] ${focus.status}`);
    if (focus.title) lines.push(`                ${focus.title}`);
    else lines.push(`                —`);
  } else {
    lines.push('🎯 Current focus : (no assigned tasks)');
  }
  lines.push(sub);

  const byStatus = {};
  for (const t of tasks) {
    (byStatus[t.status] ||= []).push(t);
  }

  for (const status of STATUS_ORDER.filter((s) => byStatus[s]?.length)) {
    const group = sortByPriority(byStatus[status]);
    const count = group.length;
    lines.push(`${STATUS_ICON[status] || '• '} ${status.padEnd(12)} (${String(count).padStart(2)})`);
    for (const t of group) {
      const ident = t.identifier || fmtId(t.id);
      const tag = `  ${STATUS_ICON[status]} ${ident}`.padEnd(20);
      const focusMark = t.id === focus.id ? '  ← current' : '';
      const title = t.title ? ` ${t.title}` : '';
      lines.push(`${tag} [${t.priority || 'low'}]${title}${focusMark}`);
    }
    lines.push(sub);
  }

  const counts = {};
  for (const t of tasks) counts[t.status] = (counts[t.status] || 0) + 1;
  const total = tasks.length;
  const blocked = counts.blocked || 0;
  const inProgress = counts.in_progress || 0;
  lines.push(`Totals: ${total} assigned · ${inProgress} in progress · ${blocked} blocked · ${counts.todo || 0} todo`);
  lines.push(sep);
  return lines.join('\n');
}

function usage() {
  return [
    'status — summarize the Paperclip agent\'s current assigned tasks',
    '',
    'Usage: node status.mjs [--json] [--agent-id <id>] [--company-id <id>] [--api-url <url>] [--api-key <token>]',
    '',
    'Reads PAPERCLIP_* env vars by default (available inside a Paperclip run).',
  ].join('\n');
}

async function main() {
  let opts;
  try {
    opts = parseArgs(process.argv);
  } catch (e) {
    process.stderr.write(`${e.message}\n\n${usage()}\n`);
    process.exit(2);
  }
  if (opts.help) {
    process.stdout.write(usage() + '\n');
    return;
  }

  const apiUrl = (opts.apiUrl || process.env.PAPERCLIP_API_URL || '').replace(/\/$/, '');
  const apiKey = opts.apiKey || process.env.PAPERCLIP_API_KEY || '';
  const agentId = opts.agentId || process.env.PAPERCLIP_AGENT_ID || '';
  const companyId = opts.companyId || process.env.PAPERCLIP_COMPANY_ID || '';
  const runId = process.env.PAPERCLIP_RUN_ID || '';

  if (!apiUrl || !apiKey) {
    process.stderr.write(
      'Error: missing PAPERCLIP_API_URL / PAPERCLIP_API_KEY.\n' +
      'Run this inside a Paperclip run (heartbeat), or pass --api-url and --api-key.\n'
    );
    process.exit(1);
  }

  try {
    const [me, inbox] = await Promise.all([
      readJson(`${apiUrl}/api/agents/me`, apiKey, runId),
      readJson(`${apiUrl}/api/agents/me/inbox-lite`, apiKey, runId),
    ]);
    const agent = {
      id: agentId || me.id,
      name: me.name || me.title || me.role || 'agent',
      role: me.role || '—',
      title: me.title || null,
      companyId: companyId || me.companyId || '—',
      apiUrl,
    };
    const tasks = Array.isArray(inbox) ? inbox : [];

    if (opts.json) {
      process.stdout.write(
        JSON.stringify({ agent, currentFocus: pickCurrentFocus(tasks), tasks, generatedAt: new Date().toISOString() }, null, 2) + '\n'
      );
    } else {
      process.stdout.write(renderText({ agent, tasks }) + '\n');
    }
  } catch (e) {
    process.stderr.write(`status: ${e.message}\n`);
    process.exit(1);
  }
}

// Resolve module dir for the help text (optional nicety).
main();
