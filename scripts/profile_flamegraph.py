#!/usr/bin/env python3
"""Generate inferno flame graphs for the example models.

Builds a dedicated profiling binary and runs each selected example model under
`perf`, rendering the result with `flamegraph` (cargo-flamegraph, which uses
inferno to collapse the stacks and draw the SVG).

The plain Release build carries no debug information, so a separate build tree
`build/profile` is used: Release optimisation plus `-g -fno-omit-frame-pointer`,
which is what makes the sampled stacks resolve to Fortran procedure names.
Stacks are unwound from DWARF rather than frame pointers, because the bundled
external libraries (HDF5, stdlib) are not rebuilt with frame pointers.

Each model is profiled in its own directory under `examples/_flamegraphs/`
rather than in `examples/*/compute/`, so profiling runs do not disturb the
result comparison workflow.

Usage:
    python scripts/profile_flamegraph.py
    python scripts/profile_flamegraph.py -m Cobres -m Slapton
    python scripts/profile_flamegraph.py --no-build --freq 499
    python scripts/profile_flamegraph.py --list

(C) 2026, Sven Berendsen
"""

import argparse
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DIR_EXAMPLES = ROOT / "examples"
DIR_OUT = DIR_EXAMPLES / "_flamegraphs"
DIR_BUILD = ROOT / "build" / "profile"

# Mirrors examples/_methods/settings.py, which this script deliberately does not
# import: that package expects to be used from within examples/.
DIR_INPUTS = "model"
DIR_COMPUTE = "compute"
SHETRAN_EXE = "shetran.exe" if os.name == "nt" else "shetran"

DEFAULT_FREQ = 999

# Above 1, perf refuses to collect the userspace call graphs this script needs.
MAX_PERF_EVENT_PARANOID = 1
FN_PERF_EVENT_PARANOID = Path("/proc/sys/kernel/perf_event_paranoid")

TOOL_PACKAGES = {
    "perf": "perf",
    "flamegraph": "cargo-flamegraph",
    "cmake": "cmake",
    "gfortran": "gcc-fortran",
    "ifx": "Intel oneAPI HPC Toolkit (see COMPILING.md)",
}

RE_SAMPLE_EVENTS = re.compile(r"^\s*SAMPLE events:\s*(\d+)", re.MULTILINE)


def check_tools(tools):
    """Exit unless every required executable is on PATH."""

    missing = [tool for tool in tools if shutil.which(tool) is None]
    if not missing:
        return

    for tool in missing:
        print(f"ERROR: required tool not found on PATH: {tool}", file=sys.stderr)
        print(f"ERROR:   install it, for example: sudo pacman -S {TOOL_PACKAGES[tool]}",
              file=sys.stderr)
    sys.exit(1)


def check_perf_permissions():
    """Exit unless the kernel allows perf to sample call graphs."""

    if not FN_PERF_EVENT_PARANOID.is_file():
        print("WARNING: cannot read kernel.perf_event_paranoid; "
              "continuing and hoping perf is permitted.", file=sys.stderr)
        return

    value = int(FN_PERF_EVENT_PARANOID.read_text().strip())
    if value <= MAX_PERF_EVENT_PARANOID:
        return

    print(f"ERROR: kernel.perf_event_paranoid is {value}, which stops perf from "
          "sampling this process.", file=sys.stderr)
    print("ERROR: relax it for this session with:", file=sys.stderr)
    print(f"ERROR:   sudo sysctl kernel.perf_event_paranoid={MAX_PERF_EVENT_PARANOID}",
          file=sys.stderr)
    print("ERROR: or persist it by writing that setting to "
          "/etc/sysctl.d/99-perf.conf", file=sys.stderr)
    sys.exit(1)


def find_rundata(directory):
    """Return the model's rundata file, which SHETRAN requires to be rundata_*.txt."""

    candidates = sorted(directory.glob("rundata_*.txt"))
    return candidates[0] if candidates else None


def resolve_input_dir(dir_model):
    """Return the directory holding the model inputs, preferring the committed ones."""

    for name in (DIR_INPUTS, DIR_COMPUTE):
        directory = dir_model / name
        if directory.is_dir() and find_rundata(directory) is not None:
            return directory
    return None


def discover_models():
    """Return {model name: input directory} for every runnable example."""

    models = {}
    for directory in sorted(DIR_EXAMPLES.iterdir()):
        if not directory.is_dir() or directory.name.startswith("_"):
            continue
        dir_input = resolve_input_dir(directory)
        if dir_input is not None:
            models[directory.name] = dir_input
    return models


def select_models(models, wanted):
    """Return the requested subset, or everything when nothing was requested."""

    if not wanted:
        return dict(models)

    unknown = [name for name in wanted if name not in models]
    if unknown:
        print(f"ERROR: unknown model(s): {', '.join(unknown)}", file=sys.stderr)
        print(f"ERROR: available: {', '.join(models)}", file=sys.stderr)
        sys.exit(1)

    return {name: models[name] for name in wanted}


def build_profiling_binary(compiler, jobs):
    """Configure and build the symbolised profiling binary."""

    print(f"INFO: configuring profiling build in {DIR_BUILD} ...")
    subprocess.run(
        [
            "cmake",
            "-S", str(ROOT),
            "-B", str(DIR_BUILD),
            "-DCMAKE_BUILD_TYPE=Release",
            f"-DCMAKE_Fortran_COMPILER={compiler}",
            "-DCMAKE_Fortran_FLAGS=-g -fno-omit-frame-pointer",
            "-DSHETRAN_BUILD_TESTS=OFF",
        ],
        cwd=ROOT,
        check=True,
    )

    print("INFO: building SHETRAN ...")
    subprocess.run(
        ["cmake", "--build", str(DIR_BUILD), "--target", "SHETRAN",
         "--parallel", str(jobs)],
        cwd=ROOT,
        check=True,
    )


def prepare_run_dir(model, dir_input):
    """Give the model a clean directory of its own and return it with its rundata file."""

    dir_run = DIR_OUT / model / "run"
    if dir_run.exists():
        shutil.rmtree(dir_run)
    dir_run.mkdir(parents=True)

    for entry in sorted(dir_input.iterdir()):
        if entry.is_file():
            shutil.copy2(entry, dir_run / entry.name)

    return dir_run, find_rundata(dir_run)


def count_samples(fn_perf_data):
    """Return the number of recorded samples, or None when perf will not say."""

    result = subprocess.run(
        ["perf", "report", "--stats", "-i", str(fn_perf_data)],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
        errors="replace",
    )
    match = RE_SAMPLE_EVENTS.search(result.stdout)
    return int(match.group(1)) if match else None


def profile_model(model, dir_input, exe, args):
    """Profile one model and return a result record for the summary."""

    print()
    print(f"INFO: profiling {model} (inputs: {dir_input.relative_to(ROOT)}) ...")
    if dir_input.name == DIR_COMPUTE:
        print(f"WARNING: {model} has no {DIR_INPUTS}/ directory; using the "
              f"existing {DIR_COMPUTE}/ inputs instead.", file=sys.stderr)

    dir_run, fn_rundata = prepare_run_dir(model, dir_input)
    fn_svg = DIR_OUT / f"{model}.svg"

    # The frequency has to travel inside --cmd: cargo-flamegraph rejects
    # --freq and --cmd together.
    cmd = [
        "flamegraph",
        "-o", str(fn_svg),
        "-c", f"record --call-graph dwarf -F {args.freq} -e cycles:u",
        "--title", f"SHETRAN {model}",
        # Keep colours tied to procedure names so graphs stay comparable
        # between runs and compilers.
        "--deterministic",
    ]
    if args.flamechart:
        cmd.append("--flamechart")
    cmd += ["--", str(exe), "-f", fn_rundata.name]

    t_start = time.monotonic()
    result = subprocess.run(cmd, cwd=dir_run)
    duration = time.monotonic() - t_start

    fn_perf_data = dir_run / "perf.data"
    samples = count_samples(fn_perf_data) if fn_perf_data.is_file() else None

    # DWARF call graphs are bulky enough to matter across every example.
    if fn_perf_data.is_file() and not args.keep_perf_data:
        fn_perf_data.unlink()

    if result.returncode != 0 or not fn_svg.is_file():
        print(f"ERROR: profiling {model} failed (exit code {result.returncode}).",
              file=sys.stderr)
        return {"model": model, "ok": False, "duration": duration,
                "samples": samples, "svg": None}

    print(f"INFO: wrote {fn_svg.relative_to(ROOT)}")
    return {"model": model, "ok": True, "duration": duration,
            "samples": samples, "svg": fn_svg}


def print_summary(results):
    """Print one line per model, longest run first."""

    print()
    print("Flame graph summary")
    print("===================")
    print(f"{'model':<46} {'status':<8} {'runtime':>10} {'samples':>10}")
    for record in sorted(results, key=lambda r: r["duration"], reverse=True):
        samples = "n/a" if record["samples"] is None else f"{record['samples']:,}"
        print(f"{record['model']:<46} "
              f"{'ok' if record['ok'] else 'FAILED':<8} "
              f"{record['duration']:>9.1f}s "
              f"{samples:>10}")
    print()
    print(f"Output directory: {DIR_OUT.relative_to(ROOT)}")


def parse_args():

    parser = argparse.ArgumentParser(
        description="Generate inferno flame graphs for the example models.")
    parser.add_argument("-m", "--model", action="append", default=[],
                        metavar="NAME",
                        help="model to profile; repeat for several "
                             "(default: all models)")
    parser.add_argument("-l", "--list", action="store_true",
                        help="list the available models and exit")
    parser.add_argument("--no-build", action="store_true",
                        help="use the existing build/profile binary instead of "
                             "rebuilding it")
    parser.add_argument("-c", "--compiler", default="gfortran",
                        choices=["gfortran", "ifx"],
                        help="Fortran compiler for the profiling build "
                             "(default: gfortran)")
    parser.add_argument("-j", "--jobs", type=int, default=os.cpu_count() or 1,
                        metavar="N",
                        help="parallel build jobs (default: %(default)s)")
    parser.add_argument("-F", "--freq", type=int, default=DEFAULT_FREQ,
                        metavar="HZ",
                        help="perf sampling frequency in Hz "
                             "(default: %(default)s)")
    parser.add_argument("--flamechart", action="store_true",
                        help="produce a flame chart (samples in time order, "
                             "stacks not merged) instead of a flame graph")
    parser.add_argument("--keep-perf-data", action="store_true",
                        help="keep each run's perf.data file; these are large "
                             "with DWARF call graphs")
    return parser.parse_args()


def main():

    args = parse_args()

    models = discover_models()
    if not models:
        sys.exit(f"ERROR: no runnable models found in {DIR_EXAMPLES}.")

    if args.list:
        for model, dir_input in models.items():
            print(f"{model:<46} {dir_input.relative_to(DIR_EXAMPLES)}")
        return 0

    selected = select_models(models, args.model)

    tools = ["perf", "flamegraph"]
    if not args.no_build:
        tools += ["cmake", args.compiler]
    check_tools(tools)
    check_perf_permissions()

    exe = DIR_BUILD / "bin" / SHETRAN_EXE
    if args.no_build:
        if not exe.is_file():
            sys.exit(f"ERROR: {exe} not found; run without --no-build first.")
        print(f"INFO: using existing binary {exe.relative_to(ROOT)}")
    else:
        build_profiling_binary(args.compiler, args.jobs)
        if not exe.is_file():
            sys.exit(f"ERROR: the build did not produce {exe}.")

    DIR_OUT.mkdir(parents=True, exist_ok=True)
    print(f"INFO: profiling {len(selected)} model(s) at {args.freq} Hz")

    results = [profile_model(model, dir_input, exe, args)
               for model, dir_input in selected.items()]

    print_summary(results)
    return 0 if all(record["ok"] for record in results) else 1


if __name__ == "__main__":
    sys.exit(main())
