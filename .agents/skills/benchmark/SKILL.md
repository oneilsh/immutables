---
name: benchmark
description: Use the benchmark skill when the user requests to run a benchmark, or asks about historical benchmarks.
---


# immutables-bench

Use this skill to run lightweight, repeatable local benchmarks for this repo.

## When to use
- You want quick/full benchmark snapshots outside `testthat`.
- You want run notes (for example: "testing bulk merge") attached to results.
- You want a local comparison of the latest runs.

## Runner file
- `meta/bench_runner.R`

## Commands
Run from repo root.

1. Quick run (C++ enabled):
```bash
R -q -e "source('meta/bench_runner.R'); run_quick(note='testing bulk merge', use_cpp=TRUE)"
```

2. Quick run (R backend):
```bash
R -q -e "source('meta/bench_runner.R'); run_quick(note='control (no cpp)', use_cpp=FALSE)"
```

3. Full run:
```bash
R -q -e "source('meta/bench_runner.R'); run_full(note='pre-optimization baseline', use_cpp=TRUE)"
```

4. Compare latest runs:
```bash
R -q -e "source('meta/bench_runner.R'); compare_last(n=2)"
```

5. List available scenarios (and default params by base profile):
```bash
R -q -e "source('meta/bench_runner.R'); list_scenarios(base='quick')"
```

6. Run a custom scenario mix:
```bash
R -q -e "source('meta/bench_runner.R'); run_scenarios(c('as_flexseq_only','ordered_sequence_insert'), note='targeted mix', use_cpp=TRUE)"
```

7. Run custom mix with parameter overrides:
```bash
R -q -e \"source('meta/bench_runner.R'); run_scenarios(c('as_flexseq_only','ordered_sequence_insert'), note='bigger as_flexseq + ordered inserts', use_cpp=TRUE, params=list(as_flexseq_only=list(n=80000), ordered_sequence_insert=list(n=10000,inserts=1200,key_space=4000)))\"
```

## Outputs
- CSV timings: `meta/bench-results/<run_id>.csv`
- Sidecar metadata: `meta/bench-results/<run_id>.meta.json`

Metadata includes:
- `note`
- `git_sha`
- `branch`
- `datetime_utc`
- `machine`
- `use_cpp`
- `suite`
- `env`

`meta/bench-results/` is gitignored by default.

Report the results to the user formatted as a markdown table with information appropriate to their request.
