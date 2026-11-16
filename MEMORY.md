# Project memory

Durable verified decisions and corrections belong here. Pending work belongs in `BACKLOG.md`.

## Baseline
- `spatialops` is an experimental R package, version 0.1.4 at workflow import.
- Functions live in `R/`; generated documentation is in `man/` and `NAMESPACE`.
- The package uses `sf` and tidyverse packages, with CI for R CMD check and coverage.
- `inst/examples/` and `vignettes/spatialops.Rmd` are part of the usage surface.

Record reusable corrections as `[LEARN:category] previous assumption -> verified practice`, with evidence.
## API e uso confirmado pelo autor

- `geom_bbox` é usado em vários pipelines. Preservar a função pública ao revisar o escopo do pacote.
- A adaptação de `dist_nearest` com DuckDB Spatial no projeto de transporte foi feita pelo próprio autor, que pretende migrá-la para `spatialops`. Não tratar essa ocorrência como evidência de um usuário externo independente.

## Remoções de API

- `get_rais()` e `geom_sf_rais()` foram retiradas de `dev` por serem específicas da RAIS e sem chamadas locais fora de seus próprios exemplos. O dataset `rais_fortaleza` também foi removido por decisão posterior do autor.
