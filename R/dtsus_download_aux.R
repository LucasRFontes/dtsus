# gerencia o download
dtsus_download_aux <- function(
    files,
    save.dbc = FALSE,
    pasta.dbc = NULL,
    open = TRUE,
    filtro = NULL,
    colunas = NULL) {

  # inicializacoes seguras
  files$status_download <- 'Nao Realizado'
  files$status_load <- 'Nao Carregado'
  files$dt_hr <- as.POSIXct(NA, tz = "America/Sao_Paulo")

  data_list <- list()
  idx <- which(!is.na(files$arquivos))

  for (i in idx) {

    l <- files$lnk_final[i]
    data_hora <- Sys.time()
    temp <- NULL

    # DOWNLOAD
    tryCatch({

      temp <- tempfile(fileext = ".dbc")

      withCallingHandlers(
        utils::download.file(l, temp, mode = "wb", method = "libcurl"),
        warning = function(w) stop(w)
      )

      files$status_download[i] <- "Download realizado"
      message("[INFO] Download realizado: ", l)

    }, error = function(e) {

      files$status_download[i] <- "Erro no download"
      message("[ERRO] Falha no download: ", l)
      message("[ERRO] Motivo: ", conditionMessage(e))

      temp <- NULL
    })


    files$dt_hr[i] <- data_hora

    # SALVAR DBC
    if (isTRUE(save.dbc) &&
        !is.null(temp) &&
        file.exists(temp)) {

      tryCatch({
        file.copy(
          temp,
          file.path(pasta.dbc, basename(files$arquivos[i])),
          overwrite = TRUE
        )
        message("[INFO] Arquivo DBC salvo em: ", pasta.dbc)
      }, error = function(e) {
        warning(sprintf(
          "[AVISO] Nao foi possivel salvar o DBC em: %s",
          pasta.dbc
        ), call. = FALSE)
      })
    }

    # ABRIR E PROCESSAR
    if (isTRUE(open) &&
        files$status_download[i] == "Download realizado") {

      tryCatch({

        data_temp <- read.dbc::read.dbc(temp, as.is = TRUE)

        if (!is.null(filtro)) {
          data_temp <- dts_filter_Df(filtro, data_temp)
        }

        if (!is.null(colunas)) {
          data_temp <- dts_select_col(colunas, data_temp)
        }

        data_list[[length(data_list) + 1]] <- data_temp
        files$status_load[i] <- "Carregado"

        message("Arquivo lido: ", l)

      }, error = function(e) {
        files$status_load[i] <- "Erro na leitura"
        warning(sprintf(
          "[AVISO] Arquivo baixado mas nao carregado: %s",
          l
        ), call. = FALSE)
      })
    }

    # LIMPEZA
    if (!is.null(temp) && file.exists(temp)) unlink(temp)
    rm(temp)
    gc()
  }

  # RETORNO FINAL
  return(list(
    files = files,
    data  = data_list
  ))
}
