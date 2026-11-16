# Package backlog

Confirmed unfinished work only. Close items after verification.

## Package quality
- [ ] Quando o autor retomar `abnquarto`, testar o template Quarto e reavaliar onde distribuir `theme_abnq()` e `theme_abnq_map()`. Até lá, preservar as funções no `spatialops`.
- [ ] Definir destino das funções MapHub antes de retirá-las novamente; por ora, preservar as três funções na API. Revisar consultas de dados espaciais e cartografia conforme o escopo de operações espaciais.
- [ ] Verificar reprodução do ZIP Mendeley com a revisão anterior de `spatialops` e preparar v2 do depósito se houver correção necessária.
- [ ] Após a limpeza básica, investigar redundâncias entre count_features, dist_nearest e operações de conjuntos.
- [ ] Verificar licença de redistribuição dos ZIPs de Fortaleza já colocados em `inst/extdata/`; testar leitura via `/vsizip/` e renderizar a vignette quando `sf` e `knitr` estiverem disponíveis.
- [ ] Establish focused `testthat` coverage for exported spatial helpers, especially CRS, units, empty input, and missing geometry behavior.
- [ ] Run and triage a local `R CMD check` baseline; record reproducible findings before code changes.
- [ ] Review documentation and examples for exported functions; align `man/` with roxygen.
- [ ] Review dependency declarations and minimum R version against actual code requirements.

## Release
- [ ] Decide whether a CRAN release is intended before adding CRAN-specific automation.

- [ ] Revisar `summarise_set()` e `trim_set()` após a limpeza básica, junto com a análise de redundâncias do núcleo.
- [ ] Reavaliar `fortaleza` e `metrofor` quando o pacote tiver mais conteúdo ou quando deixarem de ser úteis aos exemplos.
