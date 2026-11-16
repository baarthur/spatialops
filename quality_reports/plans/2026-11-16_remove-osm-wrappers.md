# Retirada dos wrappers OSM

- Remover `get_osm_roads`, `get_osm_rail` e `get_osm_postcodes` após decisão do autor baseada em busca nos próprios repositórios.
- Atualizar código, exports, ajuda e `DESCRIPTION`; preservar referência histórica via git.
- Substituir o trecho da vignette que chama `get_osm_rail` pelo dataset `metrofor` incluído no pacote.
- Validar parse, referências residuais e pacote com as ferramentas disponíveis; registrar limitações.