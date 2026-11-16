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

## Escopo definido pelo autor

- O foco de spatialops é operações espaciais. count_features, dist_nearest e as operações de conjuntos (ring_sets, binary_sets e afins) formam o núcleo do projeto.
- Após a limpeza básica da API, avaliar redundâncias entre funções do núcleo.
- `theme_abnq()` e `theme_abnq_map()` permanecem por ora no `spatialops`, com nomes e API preservados. O autor quer wrappers de formatação para tarefas rotineiras; a revisão do repositório existente `abnquarto` foi adiada. Só reconsiderar um pacote R separado se surgirem mais funções ou uso independente recorrente.
- `maphub_to_sf()`, `maphub_tidy_lines()` e `maphub_tidy_stations()` foram restauradas na API pública por decisão do autor enquanto seu destino não estiver definido. O depósito Mendeley Data *Rapid Transit, Labor Market, and Business Growth* usa `maphub_to_sf()`; as outras duas chamadas aparecem comentadas no ZIP.
- `get_osm_roads()`, `get_osm_rail()` e `get_osm_postcodes()` foram retiradas da API atual por decisão do autor: são consultas OSM com filtros fixos, fora do núcleo de operações espaciais. A busca do autor encontrou usos apenas em `spatialops` e `mapchallenge-2023`; a varredura das branches principais públicas encontrou usos de `roads` em `mapchallenge-2023` e nenhuma chamada externa de `rail` ou `postcodes`. Uma pasta local `shenanigans` também usa `roads`.
- O gerador histórico de `metrofor` (`data-raw/metrofor.R`, removido do checkout em `faa82e2`) já consultava `osmdata` diretamente, sem `get_osm_rail()`. Foi recuperado com `usethis::use_data()` no final; não reexecutar a consulta sem querer atualizar o dataset publicado, pois OSM é mutável.
- `geom_sf_toscale()` permanece na API pública, assim como `geom_bbox()`, por decisão do autor.
- `shp_extract_read()` foi retirada por redundância com `sf::st_read()` via GDAL `/vsizip/`. A vignette foi alterada para ler diretamente dois ZIPs do Portal de Mapas de Fortaleza, a serem colocados em `inst/extdata/` para teste.

- Por decisão do autor, `get_h3_grid()` foi removida por redundância com `h3jsr`; `summarise_set()` e `trim_set()` ficam para revisão posterior. Todo o código ativo e os exemplos usam o pipe nativo `|>`, com R mínimo 4.1.0 e sem dependência de `magrittr`.
- Os datasets `fortaleza` e `metrofor` permanecem enquanto úteis, especialmente enquanto o pacote ainda tem pouco conteúdo.
