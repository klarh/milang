#!/usr/bin/env python3
"""mdBook preprocessor that runs milang code blocks and appends output.

Code blocks tagged with `milang,run` are executed via `./milang run` and
the output is appended in a subsequent ``` block.

Usage in markdown:
    ```milang,run
    x = 2 + 3
    y = x * 2
    ```

Produces:
    ```milang
    x = 2 + 3
    y = x * 2
    ```
    ```
    x = 5
    y = 10
    ```
"""

import json
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed

MILANG_BIN = os.environ.get("MILANG_BIN", os.path.join(os.path.dirname(__file__), "..", "milang"))
TIMEOUT = 10

FENCE_RE = re.compile(
    r"```milang,run\s*\n(.*?)```",
    re.DOTALL,
)


def run_snippet(code: str) -> str:
    with tempfile.NamedTemporaryFile(suffix=".mi", mode="w", delete=False) as f:
        f.write(code)
        f.flush()
        try:
            result = subprocess.run(
                [MILANG_BIN, "run", f.name],
                capture_output=True, text=True, timeout=TIMEOUT,
            )
            output = result.stdout.strip()
            if result.returncode != 0 and result.stderr.strip():
                output = result.stderr.strip() if not output else output
            return output
        except subprocess.TimeoutExpired:
            return "-- (timed out)"
        finally:
            os.unlink(f.name)


def process_chapter(content: str) -> str:
    def replace_block(m):
        code = m.group(1)
        output = run_snippet(code)
        result = f"```milang\n{code}```\n```\n{output}\n```"
        return result

    return FENCE_RE.sub(replace_block, content)


def process_book(book: dict) -> dict:
    # Collect all (chapter, match) pairs for parallel execution
    tasks = []

    def collect(items):
        for item in items:
            if "Chapter" in item:
                ch = item["Chapter"]
                for m in FENCE_RE.finditer(ch["content"]):
                    tasks.append((ch, m.group(1)))
                if ch.get("sub_items"):
                    collect(ch["sub_items"])

    collect(book["items"])

    if not tasks:
        return book

    # Run all snippets in parallel
    results = {}
    with ThreadPoolExecutor() as pool:
        futures = {pool.submit(run_snippet, code): code for _, code in tasks}
        for future in as_completed(futures):
            results[futures[future]] = future.result()

    # Substitute results back into chapter content
    def walk(items):
        for item in items:
            if "Chapter" in item:
                ch = item["Chapter"]
                def replace_block(m, _results=results):
                    code = m.group(1)
                    output = _results.get(code, "-- (no output)")
                    return f"```milang\n{code}```\n```\n{output}\n```"
                ch["content"] = FENCE_RE.sub(replace_block, ch["content"])
                if ch.get("sub_items"):
                    walk(ch["sub_items"])

    walk(book["items"])
    return book


def main():
    # mdBook calls with "supports <renderer>" to check compatibility
    if len(sys.argv) > 1 and sys.argv[1] == "supports":
        sys.exit(0)  # we support all renderers

    # Read context + book from stdin
    context, book = json.load(sys.stdin)
    book = process_book(book)
    json.dump(book, sys.stdout)


if __name__ == "__main__":
    main()
