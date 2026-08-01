##### Criar o Json ####
# Files_row <- uma linha do df files gerado no dts_files_wb
# temp <- o arquivo temporario baixado do datasus
# pasta <- a pasta onde o dbc esta salvo
dts_salvar_metadados <- function(files_row, temp, pasta.dbc) {

  meta <- list(
    nome_arquivo     = files_row$nome_arquivo,
    fonte            = files_row$fonte,
    tipo             = files_row$tipo,
    uf               = files_row$uf,
    ano_mes          = as.character(files_row$sequencia_datas),
    url_origem       = files_row$lnk_final,
    tamanho_bytes    = file.size(temp),
    hash_md5         = digest::digest(file = temp, algo = "md5"),
    data_download    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    versao_dtsus     = as.character(utils::packageVersion("dtsus")),
    path_dbc         = file.path(pasta.dbc, basename(files_row$arquivos)),
    path_parquet     = NA_character_,
    parquet_completo = FALSE,
    status_download  = files_row$status_download
  )


  pasta_meta <- file.path(pasta.dbc, "dtsus_cache") # cria um arquivo ocultoo para salvar o json
  dir.create(pasta_meta, showWarnings = FALSE)

  path_json <- file.path(
    pasta_meta,
    paste0(tools::file_path_sans_ext(basename(files_row$arquivos)), ".json")
  )

  jsonlite::write_json(meta, path_json, auto_unbox = TRUE, pretty = TRUE)
}

##### Carregar o Json ####
# Files_row <- uma linha do df files gerado no dts_files_wb
# pasta <- a pasta onde o dbc esta salvo
dts_carregar_json <- function(files_row, pasta.dbc){
  pasta_meta <- file.path(pasta.dbc, "dtsus_cache")

  if (!dir.exists(pasta_meta)) {
    warning("Pasta cache nao encontrada: ", pasta_meta)
    return(NULL)
  }

  arquivo <- tools::file_path_sans_ext(files_row$arquivos)


  arquivo_json <- file.path(pasta_meta, paste0(arquivo, ".json"))

  if (!file.exists(arquivo_json)) {
    message("json nao encontrado: ", basename(arquivo_json))
    return(NULL)
  }

  jsonlite::read_json(arquivo_json)
}





