dts_tamanho_remoto <- function(url_arquivo) {
  h <- RCurl::basicTextGatherer()
  ok <- tryCatch({
    RCurl::curlPerform(
      url = url_arquivo,
      nobody = TRUE,
      ftp.use.epsv = TRUE,
      headerfunction = h$update,
      connecttimeout = 2,
      timeout = 2,
      forbid.reuse = TRUE,
      fresh.connect = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NA_real_)
  resp <- h$value()
  linhas <- strsplit(resp, "\r\n")[[1]]
  linha_tamanho <- linhas[grepl("^Content-Length:", linhas)]
  if(length(linha_tamanho) == 0) return(NA_real_)
  as.numeric(sub("Content-Length:\\s*", "", linha_tamanho[1]))
}

dts_verificar_atualizacao <- function(files_row, pasta.dbc, tentativas = 5, espera_seg = 1) {
  json <- dts_carregar_json(files_row, pasta.dbc)
  if (is.null(json)) {
    return(list(status = "sem_metadado", precisa_atualizar = TRUE))
  }


  tamanho_remoto <- NA
  i <- 0
  while (is.na(tamanho_remoto) && i < tentativas) {
    tamanho_remoto <- dts_tamanho_remoto(files_row$lnk_final)
    i <- i + 1
    if (is.na(tamanho_remoto) && i < tentativas) Sys.sleep(espera_seg)
  }

  if (is.na(tamanho_remoto)) {
    warning(sprintf(
      "[AVISO] Nao foi possivel consultar o tamanho remoto de: %s (apos %d tentativas)",
      files_row$nome_arquivo, tentativas
    ), call. = FALSE)
    return(list(status = "erro_conexao", precisa_atualizar = NA))
  }

  if (tamanho_remoto == json$tamanho_bytes) {
    resultado <- list(status = "atualizado", precisa_atualizar = FALSE)
  } else {
    resultado <- list(status = "desatualizado", precisa_atualizar = TRUE)
  }
  return(resultado)
}






