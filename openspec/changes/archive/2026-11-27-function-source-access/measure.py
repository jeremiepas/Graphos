"""Measure Haskell declaration nodes of a graph.json (function-source-access).

usage: measure.py GRAPH [CORPUS_ROOT] [--against OTHER_GRAPH]
"""
import json, sys, collections

def load(p):
    g = json.load(open(p))['graph']
    return g['nodes'], g['edges']

def fields(md):
    return all(k in md for k in ('line_start', 'line_end', 'signature'))

def main():
    args = sys.argv[1:]
    other = None
    if '--against' in args:
        i = args.index('--against'); other = args[i + 1]; del args[i:i + 2]
    path = args[0]; root = args[1] if len(args) > 1 else None
    nodes, edges = load(path)
    fn = {k: v for k, v in nodes.items() if v['metadata'].get('kind') in ('Function', 'Method')}
    hs = {k: v for k, v in fn.items() if v['metadata'].get('source_file', '').endswith('.hs')}
    hsdecl = {k: v for k, v in nodes.items()
              if v['metadata'].get('source_file', '').endswith('.hs')
              and v['metadata'].get('kind') in ('Function', 'Type', 'Class', 'Instance')}
    print(f'nodes={len(nodes)} edges={len(edges)}')
    print(f'function+method nodes={len(fn)} with three fields={sum(fields(v["metadata"]) for v in fn.values())}')
    by_ext = collections.Counter(v['metadata']['source_file'].rsplit('.', 1)[-1]
                                 for v in fn.values() if fields(v['metadata']))
    print(f'  with fields by extension: {dict(by_ext)}')
    print(f'haskell function nodes={len(hs)} with three fields={sum(fields(v["metadata"]) for v in hs.values())}')
    print(f'haskell declaration nodes (Function/Type/Class/Instance)={len(hsdecl)} '
          f'with three fields={sum(fields(v["metadata"]) for v in hsdecl.values())}')
    if root:
        # Sanity check of task 1.3: start <= end, no overlap in a file, first line names the node.
        byfile = collections.defaultdict(list)
        for k, v in hsdecl.items():
            md = v['metadata']
            if fields(md):
                byfile[md['source_file']].append((md['line_start'], md['line_end'], v['label'], k))
        ok = bad = 0; bad_examples = []
        for f, ds in byfile.items():
            try:
                lines = open(f'{root}/{f}', encoding='utf-8', errors='replace').read().split('\n')
            except OSError:
                bad += len(ds); continue
            ds.sort()
            for i, (s, e, lab, k) in enumerate(ds):
                good = s <= e and lab in lines[s - 1] and (i == 0 or ds[i - 1][1] < s)
                if good: ok += 1
                else:
                    bad += 1
                    if len(bad_examples) < 8: bad_examples.append((f, s, e, lab))
        tot = ok + bad
        print(f'sanity check: {ok}/{tot} pass ({100 * ok / tot:.1f}%)' if tot else 'sanity check: no ranged nodes')
        for x in bad_examples: print('  fails:', x)
    if other:
        on, oe = load(other)
        es = lambda E: {(x['source'], x['target'], x['relation']) for x in E}
        print(f'node ids equal to {other}: {set(nodes) == set(on)} '
              f'(only here {len(set(nodes) - set(on))}, only there {len(set(on) - set(nodes))})')
        print(f'edges equal: {es(edges) == es(oe)} '
              f'(only here {len(es(edges) - es(oe))}, only there {len(es(oe) - es(edges))})')

main()
