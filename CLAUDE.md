# AI Agent Instructions

## Communication Style

Precise, to the point, engineering-doctorate audience: no basic explanations,
but keep any caveats, assumptions, or limitations material to correctness.

## Consistency Testing

Use `examples/check_results_consistency.py` to compare SHETRAN outputs across
runs (e.g. different compilers/builds/code changes). Details:
`examples/README.md`.

Run it in the `shetran` conda env, if present. Activation, in order:

1. Fish wired up (`~/.config/fish/config.fish`): `fish` + `conda activate shetran`.
2. Else bash wired up (`~/.bashrc`): `bash` + `conda activate shetran`.
3. Else: run with the system/default Python environment.
