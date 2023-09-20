devtools::load_all()

library(basedosdados)
set_billing_id(Sys.getenv("bq_billing_id"))
bigrquery::bq_auth(email = Sys.getenv("bq_email"))


# neighborhoods shapefile ---------------------------------------------------------------------

fortaleza <- shp_extract_read("data-raw/Bairros_de_Fortaleza.zip", dsn = "vw_Fortaleza_Bairros.shp",
                              options = "ENCODING=latin1")

fortaleza <- fortaleza %>%
  transmute(
    nome_bairro = stringi::stri_trans_general(stringr::str_to_lower(Nome), "Latin-ASCII"),
    cod_bairro = stringr::str_pad(as.character(id), 4, "left", "0"),
    area_ha = `Área..ha.`
  ) %>%
  st_transform(crs = 4674)



# basedosdados --------------------------------------------------------------------------------

query <- paste0(
  "SELECT ", "ano, quantidade_vinculos_ativos, cep, bairros_fortaleza",
  " FROM ", "`basedosdados.br_me_rais.microdados_estabelecimentos`",
  " WHERE ", "id_municipio='2304400' AND quantidade_vinculos_ativos>0",
  " AND ano=", 2019
)

download(query, path = "data-raw/rais_fortaleza.csv")

rais_dic <- read_sql(
  "SELECT * FROM basedosdados.br_me_rais.dicionario WHERE nome_coluna IN ('bairros_fortaleza', 'cep') AND id_tabela='microdados_estabelecimentos'"
)



# match hoods ---------------------------------------------------------------------------------

match <- rais_dic %>%
  filter(nome_coluna == "bairros_fortaleza") %>%
  transmute(nome_bairro = valor, cod_bairro_rais = stringr::str_pad(chave, 4, "left", "0")) %>%
  mutate(cod_bairro_rais = na_if(cod_bairro_rais, "00-1")) %>%
  mutate(
    nome_bairro = stringi::stri_trans_general(stringr::str_to_lower(nome_bairro), "Latin-ASCII")
  ) %>%
  mutate(nome_bairro = case_match(
    nome_bairro,
    c("alagadico", "alagadico novo") ~ "jose de alencar",
    "cidade dos funcionari" ~ "cidade dos funcionarios",
    "manuel satiro" ~ "vila manuel satiro",
    "parque presidente var" ~ "presidente vargas",
    "prefeito jose walter" ~ "jose walter",
    "presidente tancredo n" ~ "jardim das oliveiras",
    "tauape" ~ "sao joao do tauape",
    "vila ellery" ~ "bairro ellery",
    .default = nome_bairro
  )) %>%
  full_join(fortaleza) %>%
  sf::st_drop_geometry() %>%
  select(nome_bairro, cod_bairro_rais, cod_bairro)



# csv -----------------------------------------------------------------------------------------

rais_fortaleza <- readr::read_csv("data-raw/rais_fortaleza.csv") %>%
  filter(!is.na(bairros_fortaleza)) %>%
  rename(cod_bairro_rais = bairros_fortaleza)

rais_fortaleza <- rais_fortaleza %>%
  left_join(match) %>%
  filter(!is.na(cod_bairro))



# assembly ------------------------------------------------------------------------------------

usethis::use_data(fortaleza, overwrite = TRUE)
usethis::use_data(rais_fortaleza, overwrite = TRUE)
