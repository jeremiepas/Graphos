#!/usr/bin/env python3
"""Tiny example extractor: Perl subroutines and `use` imports.

Prints graph.json-shaped {"nodes": [...], "edges": [...]} on stdout. Graphos
fills in source_file, prefixes ids with a per-file bucket, and records a
per-file error (then stub-extracts) if this process fails or emits bad JSON.
"""

import json
import os
import re
import sys

SUB = re.compile(r"^\s*sub\s+([A-Za-z_]\w*)")
USE = re.compile(r"^\s*use\s+([A-Za-z_]\w*(?:::\w+)*)")


def main(path):
    module = os.path.basename(path)
    nodes = [
        {
            "id": "module:" + module,
            "label": module,
            "kind": "Module",
            "line_start": 1,
        }
    ]
    edges = []

    with open(path, encoding="utf-8", errors="replace") as handle:
        for lineno, line in enumerate(handle, start=1):
            sub = SUB.match(line)
            if sub:
                nid = "sub:" + sub.group(1)
                nodes.append(
                    {
                        "id": nid,
                        "label": sub.group(1),
                        "kind": "Function",
                        "line_start": lineno,
                    }
                )
                edges.append(
                    {
                        "source": "module:" + module,
                        "target": nid,
                        "relation": "contains",
                    }
                )
            use = USE.match(line)
            if use:
                target = "external:" + use.group(1)
                nodes.append(
                    {
                        "id": target,
                        "label": use.group(1),
                        "kind": "External",
                    }
                )
                edges.append(
                    {
                        "source": "module:" + module,
                        "target": target,
                        "relation": "imports",
                    }
                )

    json.dump({"nodes": nodes, "edges": edges}, sys.stdout)
    sys.stdout.write("\n")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.stderr.write("usage: extract.py FILE\n")
        sys.exit(2)
    main(sys.argv[1])
