import json
import re
import subprocess
import sys
import os
import argparse


def extract_payload(html_content):
    start_marker = "const _payloadData = "
    start_idx = html_content.find(start_marker)
    if start_idx == -1:
        raise ValueError("_payloadData not found")
    json_start = start_idx + len(start_marker)
    brace_count = 0
    in_string = False
    escape = False
    json_end = json_start
    for i, ch in enumerate(html_content[json_start:], start=json_start):
        if escape:
            escape = False
            continue
        if in_string:
            if ch == "\\":
                escape = True
            elif ch == '"':
                in_string = False
            continue
        if ch == '"':
            in_string = True
            continue
        if ch == "{":
            brace_count += 1
        elif ch == "}":
            brace_count -= 1
            if brace_count == 0:
                json_end = i + 1
                break
    return json.loads(html_content[json_start:json_end])


def measure_payload(graph_json_path, html_path, skip_pipeline=False):
    # 1. Run the pipeline to generate the HTML (if not skipping)
    if not skip_pipeline:
        print(f"[*] Running pipeline on {graph_json_path}...")
        try:
            subprocess.run(
                ["cabal", "run", "graphos", "--", graph_json_path], check=True
            )
        except subprocess.CalledProcessError as e:
            print(f"[!] Pipeline failed: {e}")
            sys.exit(1)
    else:
        print("[*] Skipping pipeline, using existing files.")

    # 2. Read the generated HTML
    if not os.path.exists(html_path):
        print(f"[!] HTML file not found at {html_path}")
        sys.exit(1)

    with open(html_path, "r", encoding="utf-8") as f:
        html_content = f.read()

    # 3. Read the graph.json for counts
    if not os.path.exists(graph_json_path):
        print(f"[!] Graph JSON not found at {graph_json_path}")
        sys.exit(1)

    with open(graph_json_path, "r", encoding="utf-8") as f:
        graph_data = json.load(f)

    node_count = len(graph_data.get("nodes", []))
    edge_count = len(graph_data.get("edges", []))

    # 4. Extract the inline payload
    payload = extract_payload(html_content)

    # Measure section sizes by re-serializing compactly
    sections = {
        "nodes": json.dumps(payload.get("nodes", []), separators=(",", ":")),
        "edges": json.dumps(payload.get("edges", []), separators=(",", ":")),
        "aggregates": json.dumps(payload.get("aggregates", []), separators=(",", ":")),
        "strings": json.dumps(payload.get("strings", []), separators=(",", ":")),
        "files": json.dumps(payload.get("files", []), separators=(",", ":")),
        "kinds": json.dumps(payload.get("kinds", []), separators=(",", ":")),
        "relations": json.dumps(payload.get("relations", []), separators=(",", ":")),
    }
    section_sizes = {k: len(v.encode("utf-8")) for k, v in sections.items()}

    # 5. Calculate document size
    doc_size = os.path.getsize(html_path)

    # 6. Print results
    print("\n" + "=" * 40)
    print("MEASUREMENT RESULTS")
    print("=" * 40)
    print(f"Graph JSON: {graph_json_path}")
    print(f"HTML File:  {html_path}")
    print(f"Nodes:      {node_count}")
    print(f"Edges:      {edge_count}")
    print("-" * 40)
    print(
        f"Nodes payload:        {section_sizes['nodes']:,} B ({section_sizes['nodes'] / node_count if node_count > 0 else 0:.1f} B/node)"
    )
    print(
        f"Edges payload:        {section_sizes['edges']:,} B ({section_sizes['edges'] / edge_count if edge_count > 0 else 0:.1f} B/edge)"
    )
    print(f"Aggregates payload:   {section_sizes['aggregates']:,} B")
    print(f"Strings payload:      {section_sizes['strings']:,} B")
    print(f"Files payload:        {section_sizes['files']:,} B")
    print(f"Kinds payload:        {section_sizes['kinds']:,} B")
    print(f"Relations payload:    {section_sizes['relations']:,} B")
    print(f"Document total:       {doc_size:,} B")
    print("=" * 40 + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("graph_json", help="Path to graph.json")
    parser.add_argument(
        "--html", help="Path to graph.html (default: same dir as graph.json)"
    )
    parser.add_argument(
        "--skip-pipeline", action="store_true", help="Skip running the pipeline"
    )
    args = parser.parse_args()

    graph_path = args.graph_json
    if args.html:
        html_path = args.html
    else:
        html_path = os.path.join(os.path.dirname(graph_path), "graph.html")

    measure_payload(graph_path, html_path, args.skip_pipeline)
