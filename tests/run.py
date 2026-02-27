import argparse
import collections
import multiprocessing
import os
import subprocess
import sys

DummyResult = collections.namedtuple("DummyResult", ["stdout", "stderr", "returncode"])

parser = argparse.ArgumentParser(description="Run tests")
parser.add_argument(
    "-n", "--n-workers", type=int, help="Number of tests to run in parallel"
)
parser.add_argument("-m", "--milang", default="milang", help="Milang executable to use")
parser.add_argument(
    "-t", "--timeout", type=int, default=30, help="Timeout for all tests"
)
parser.add_argument(
    "-d", "--test-dir", default=os.path.dirname(__file__), help="Directory for tests"
)


def run(filename, milang, timeout):
    error_file = "{}.errors".format(filename)
    cmd = [milang, "run", filename]

    try:
        result = subprocess.run(cmd, timeout=timeout, capture_output=True, text=True)
    except subprocess.TimeoutExpired:
        result = DummyResult("", "", -1)
        return dict(
            success=False, summary="FAIL: {} (timeout)".format(filename), output=result
        )

    if os.path.exists(error_file):
        with open(error_file, "r") as f:
            expected = [
                line.strip() for line in f if line.strip() and not line.startswith("#")
            ]

        for exp in expected:
            if exp not in result.stderr:
                return dict(
                    success=False,
                    summary="FAIL: {} (missing expected error)".format(filename),
                    output=result,
                )

        return dict(success=True, output=result)

    if result.returncode == 0:
        return dict(success=True, output=result)
    return dict(
        success=False,
        summary="FAIL: {} (return code {})".format(filename, result.returncode),
        output=result,
    )


def main(n_workers, milang, timeout, test_dir):
    milang = os.path.abspath(milang)

    tests = []
    for item in os.listdir(test_dir):
        if not item.endswith(".mi"):
            continue
        tests.append(os.path.join(test_dir, item))

    with multiprocessing.Pool(n_workers) as p:
        thunks = []
        for t in tests:
            thunks.append(p.apply_async(run, (t, milang, timeout)))

        failures = []
        counts = collections.Counter()

        for t in thunks:
            result = t.get()
            counts[result["success"]] += 1
            if not result["success"]:
                failures.append(result)

    for result in failures:
        print(result["summary"].strip())
        for name in ("stdout", "stderr"):
            value = getattr(result["output"], name).strip()
            if not value:
                continue
            print("---- {}:".format(name))
            print(value)
        print(8 * "-")

    sys.exit(counts[0])


if __name__ == "__main__":
    main(**vars(parser.parse_args()))
