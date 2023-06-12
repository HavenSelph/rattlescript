#!/usr/bin/env python3
import os.path
from subprocess import run, PIPE
import argparse
from ast import literal_eval
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Union, Optional, Tuple
import multiprocessing
import colorama
colorama.init()


class Result(Enum):
    EXIT_WITH_CODE = 1
    EXIT_WITH_OUTPUT = 2
    FAIL = 3
    SKIP_SILENTLY = 4
    SKIP_REPORT = 5


@dataclass(frozen=True)
class Expected:
    type: Result
    value: Union[int, str, None]


def get_expected(filename) -> Optional[Expected]:
    with open(filename) as file:
        for line in file:
            if not line.startswith("///"):
                break

            line = line[3:].strip()

            # Commands with no arguments
            if line == "skip":
                return Expected(Result.SKIP_SILENTLY, None)
            if line == "":
                continue

            if ":" not in line:
                print(f'[-] Invalid parameters in {filename}: "{line}"')
                break

            # Commands with arguments
            name, value = map(str.strip, line.split(":", 1))

            if name == "exit":
                return Expected(Result.EXIT_WITH_CODE, int(value))
            if name == "out":
                return Expected(Result.EXIT_WITH_OUTPUT, value)
            if name == "fail":
                return Expected(Result.FAIL, value)

            print(f'[-] Invalid parameter in {filename}: {line}')
            break

    return Expected(Result.SKIP_REPORT, None)


def handle_test(interpreter: str, path: Path, expected: Expected) -> Tuple[bool, str, Path]:
    # if expected.type == Result.COMPILE_FAIL:
    #     if process.returncode == 0:
    #         return False, "Expected compilation failure, but succeeded", path
    #
    #     error = process.stdout.decode("utf-8").strip()
    #     expected_error = expected.value
    #
    #     if expected_error in error:
    #         return True, "(Success)", path
    #     else:
    #         try:
    #             remaining = error.split("Error: ")[1]
    #         except IndexError:
    #             remaining = error
    #         return False, f"Did not find expected error message\n  expected: {expected_error}\n  got: '{remaining}'", path
    #
    # elif process.returncode != 0:
    #     stdout = textwrap.indent(process.stdout.decode("utf-8"), " "*10).strip()
    #     stderr = textwrap.indent(process.stderr.decode("utf-8"), " "*10).strip()
    #     return False, f"Compilation failed:\n  code: {process.returncode}\n  stdout: {stdout}\n  stderr: {stderr}", path
    #
    # elif expected.type == Result.COMPILE_SUCCESS:
    #     return True, "(Success)", path
    #
    # process = run([], stdout=PIPE, stderr=PIPE)
    process = run(
        [interpreter, "--disable-error-context", str(path)],
        stdout=PIPE,
        stderr=PIPE
    )

    error = process.stderr.decode("utf-8").strip()

    if expected.type == Result.FAIL:
        if process.returncode == 0:
            return False, "Expected failure, but succeeded", path

        expected_error = expected.value

        if expected_error in error:
            return True, "(Success)", path
        else:
            try:
                error_line = error.splitlines()[0]
                remaining = error_line.split("Error: ")[1]
            except IndexError:
                remaining = error
            return False, f"Did not find expected error message\nexpected: {expected_error}\ngot: '{remaining}'", path

    if process.returncode != 0 and expected.type != Result.EXIT_WITH_CODE:
        return False, f"Expected exit code 0, but got {process.returncode}\n{error}", path

    if expected.type == Result.EXIT_WITH_CODE:
        if process.returncode != expected.value:
            return False, f"Expected exit code {expected.value}, but got {process.returncode}\n{error}", path

    if expected.type == Result.EXIT_WITH_OUTPUT:
        output = process.stdout.decode('utf-8').strip()
        try:
            expected_out = literal_eval(expected.value).strip()
        except ValueError:
            expected_out = expected.value.strip()
        except SyntaxError:
            raise SyntaxError(f'Invalid options in file {path}')
        if output != expected_out:
            return False, f'Incorrect output produced\nexpected: {repr(expected_out)}\ngot: {repr(output)}', path

    return True, "(Success)", path


def pool_helper(args):
    return handle_test(*args)


def main():
    parser = argparse.ArgumentParser(description="Runs RattleScript test suite")
    parser.add_argument(
        "-i",
        "--interpreter",
        default="debug",
        help="Which interpreter to use for testing. (default: debug)"
    )
    parser.add_argument(
        "files",
        nargs="?",
        default=["tests"],
        help="Files / folders to run"
    )
    parser.add_argument(
        "-c",
        "--cpus",
        type=int,
        default=multiprocessing.cpu_count(),
    )
    args = parser.parse_args()
    if args.interpreter != 'debug':
        run(["cargo", "build", f"--{args.interpreter}"])
    else:
        run(["cargo", "build"])
    interpreter_path = Path().cwd() / 'target' / args.interpreter / 'rattlescript'
    if not os.path.isfile(interpreter_path):
        if not os.path.isfile(interpreter_path.with_suffix('.exe')):
            raise FileNotFoundError(f'Could not find interpreter {args.interpreter} at {interpreter_path}')
        else:
            interpreter_path = interpreter_path.with_suffix('.exe')
    arg_files = args.files if isinstance(args.files, list) else [args.files]
    test_paths = [Path(pth) for pth in arg_files]

    tests_to_run = []
    for path in test_paths:
        files = []

        if path.is_dir():
            for path_ in path.glob('**/*.rat'):
                if path_.is_file():
                    files.append(path_)
        else:
            files.append(path)

        for file in files:
            expected = get_expected(file)
            if expected.type == Result.SKIP_SILENTLY:
                continue
            if expected.type == Result.SKIP_REPORT:
                print(f'[-] Skipping {file}')
                continue
            tests_to_run.append((file, expected))

    num_passed = 0
    num_failed = 0
    num_total = len(tests_to_run)

    arguments = [
        (interpreter_path, test_path, expected)
        for num, (test_path, expected) in enumerate(tests_to_run)
    ]

    with multiprocessing.Pool(args.cpus) as pool:
        for passed, message, path in pool.imap_unordered(pool_helper, arguments):
            print(f" \33[2K[\033[92m{num_passed:3d}\033[0m", end="")
            print(f"/\033[91m{num_failed:3d}\033[0m]", end="")
            print(f" Running tests, finished {num_passed+num_failed} / {num_total}\r", end="", flush=True)
            if passed:
                num_passed += 1
            else:
                num_failed += 1
                print(f"\r\33[2K\033[91m[-] Failed {path.absolute()}\033[0m")
                message = message.replace('\n', '\n      ')
                print(f"    {message}", flush=True)

    print("\33[2K")
    print(f"Tests passed: \033[92m{num_passed}\033[0m")
    print(f"Tests failed: \033[91m{num_failed}\033[0m")

    if num_failed > 0:
        exit(1)


if __name__ == "__main__":
    main()
