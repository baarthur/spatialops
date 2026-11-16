# Workflow quick reference

Start with `AGENTS.md`, `git status --short`, then relevant memory and backlog. For substantial work: scope -> plan -> implement -> verify -> document -> hand off. Use `quality_reports/plans/` and `quality_reports/session_logs/` when useful.

Shared skills:
- `review-r`: read-only package and API review.
- `diagnose`: trace tests, check, documentation, or runtime failures.
- `package-feature`: implement API, docs, tests, and checks.
- `checkpoint`: record a session handoff.
- `spatial-ops`: choose and verify sf or DuckDB Spatial operations.

Claude adapters in `.claude/skills/` point to the same procedures. Research and `targets` rules from the source project do not apply here.

Inspiration: https://psantanna.com/claude-code-my-workflow/
Package reference: https://github.com/ipea/gtfstools
Guidance sources: https://r-pkgs.org/ ; https://devguide.ropensci.org/ ; https://design.tidyverse.org/ .
