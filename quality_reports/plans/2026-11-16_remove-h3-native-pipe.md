# Remove H3 wrapper and migrate to native pipe

- Remove `get_h3_grid`, its example, generated help, export, and the `h3jsr` dependency.
- Replace `%>%` with `|>` in package code, examples, vignette, and README; rewrite magrittr placeholder cases explicitly.
- Remove `%>%` reexport and `magrittr` dependency. Set minimum R version to 4.1.0.
- Preserve `summarise_set`, `trim_set`, `fortaleza`, and `metrofor` as requested.
- Regenerate documentation, exercise affected APIs, and run package checks.