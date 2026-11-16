---
name: diagnose
description: Diagnose failing R tests, R CMD check, documentation generation, or package runtime behavior.
---

# Diagnose package failure
1. Capture command, error, expected behavior, environment, and recent diff.
2. Trace failing function through tests, examples, docs, imports, and relevant `sf` behavior.
3. Reproduce with the smallest case. Separate environment problems from package defects.
4. Report root cause, affected API, evidence, and verification command. Fix when requested.