---
name: immutables-review
description: Use this skill when reviewing, auditing, or walking through the immutables implementation with a curriculum, progress tracking, and detour triage.
---

# immutables-review

Use this skill to run a durable, session-safe implementation review program for this repository, centered on helping the user understand the code deeply.

## When to use
- The user asks for a code review, implementation walkthrough, audit, curriculum, or progress-tracked deep read.
- The user asks to continue a prior review thread and recover state from repo artifacts.
- The user asks for structured detour handling during review work.

## Required startup checks
Run these checks before starting or resuming review:
1. Read `AGENTS.md`.
2. Read `NAMESPACE`.
3. Read `.review/curriculum.md`.
4. Read `.review/progress-ledger.md`.
5. Read `.review/issue-queue.md`.

If any review file is missing, create it using the templates in `references/`.

## Canonical workflow
1. Pick the next module from `.review/curriculum.md` and restate its exit criteria.
2. Choose the best next topic inside that module and explain why it is the right next step for understanding.
3. Drive a guided walkthrough with concrete code anchors:
   - link to specific files and line numbers,
   - explain what each block does,
   - explain why it matters to package behavior,
   - call out invariants and likely failure modes.
4. Review in this order:
   - tests first (intended behavior),
   - implementation second (mechanics and invariants),
   - backend/edge/error paths third.
5. Record outcomes in `.review/progress-ledger.md`:
   - files reviewed,
   - code anchors covered,
   - findings,
   - decisions,
   - confidence,
   - next step.
6. Add or update issues in `.review/issue-queue.md` with severity, status, and test obligations.
7. Update module status in `.review/curriculum.md`.

## Detour policy
- Fix immediately only when all are true:
  - critical or blocking for current module understanding,
  - estimated fix plus tests is 30 minutes or less,
  - small blast radius (about 3 files or fewer, no broad API redesign).
- Otherwise queue the issue and continue the planned review path.

## Test-run approval policy (required)
- Never run tests automatically.
- Before any test command:
  - propose the exact command(s),
  - state the purpose and expected signal,
  - wait for explicit user approval.
- Only run tests without per-command approval if the user explicitly grants a session override such as "run tests as needed."
- Track confidence gates in `.review/progress-ledger.md` as:
  - `approved+run`
  - `deferred`
  - `waived by user`

## Session closeout template
Use this template at the end of every review session:

```markdown
### Session YYYY-MM-DD HH:MM TZ
- Module: Mx - <name>
- Goal for session:
- Reviewed files:
- Code anchors covered:
- Key findings:
- Decisions made:
- Detours triggered:
- Confidence gates:
- Next step:
```

## Guardrails
- Treat tests and current source as truth when docs differ.
- Preserve persistence semantics; do not assume mutable shortcuts are acceptable.
- Keep ordered semantics deterministic (`lower_bound >=`, `upper_bound >`, FIFO within equal keys).
- Keep priority queue queue-first and preserve intentional API blockers.
- Ordered merge is intentionally removed; do not assume it exists.
