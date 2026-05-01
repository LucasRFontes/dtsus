#' Loads and Processes DATASUS Microdata
#'
#' Reads previously downloaded DATASUS DBC files from a local directory,
#' applies optional filtering and column selection, and returns the data
#' as a data.frame.
#'
#' @param fonte Character. The abbreviation of the health information system
#'   to be accessed, e.g. "CNES", "SIH", "SIA".
#' @param tipo Character. The abbreviation of the file type to be accessed,
#'   e.g. "LT", "RD".
#' @param uf Character. A UF code or a vector of UF codes (e.g. "MG", "SP", "BR").
#' @param Data_inicio Numeric or character. Start date in the format yyyymm
#'   (monthly) or yyyy (annual), depending on the selected dataset.
#' @param Data_fim Numeric or character. End date in the format yyyymm
#'   (monthly) or yyyy (annual), depending on the selected dataset.
#' @param filtro List. Optional filter specification with two fields:
#'   \code{list(coluna = "COL", valor = c("X","Y"))}.
#' @param colunas Character. Optional vector of columns to keep.
#' @param pasta.dbc Character. Path to the directory where the DBC files
#'   are stored. Defaults to the current working directory if not provided.
#' @param return_files Logical. If TRUE, returns a list with file metadata
#'   and the loaded data. If FALSE, returns only the data.frame.
#' @param verbose Logical. If TRUE, prints progress messages during file reading.
#'
#' @return If \code{return_files = TRUE}, returns a list with:
#' \describe{
#'   \item{files}{A data.frame containing file names, paths, and load status.}
#'   \item{data}{A data.frame with the loaded microdata (or NULL if none loaded).}
#' }
#' If \code{return_files = FALSE}, returns only the data.frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- dtsus_load(
#'   fonte = "CNES",
#'   tipo = "LT",
#'   uf = "MG",
#'   Data_inicio = 201801,
#'   Data_fim = 201803,
#'   verbose = TRUE
#' )
#'
#' head(res$data)
#' }
dtsus_load <- function(
    fonte = NA,
    tipo = NA,
    uf = NA,
    Data_inicio = NA,
    Data_fim = NULL,
    pasta.dbc = NULL,
    filtro = NULL,
    colunas = NULL,
    return_files = TRUE,
    verbose = FALSE
){

  # Validações
  font_valid <- dts_validate_fonte_tipo(fonte, tipo)
  fonte <- font_valid$fonte
  tipo <- font_valid$tipo
  prdcd <- font_valid$periodicidade

  uf <- dts_validate_uf(uf)

  Data_inicio <- dts_validate_data(Data_inicio, periodicidade = prdcd)

  if (is.null(Data_fim)) {
    Data_fim <- Data_inicio
  } else {
    Data_fim <- dts_validate_data(Data_fim, periodicidade = prdcd)
  }

  sequencia_datas <- dts_seq_data(Data_inicio = Data_inicio, Data_fim = Data_fim)

  pasta.dbc <- dts_validate_path(pasta.dbc)

  files_path <- list.files(pasta.dbc, pattern = "\\.dbc$", full.names = TRUE)

  files <- dts_files_wb(fonte, tipo, uf, sequencia_datas)

  # Filtrar arquivos locais por prefixo (sem regex pesada)
  base_names <- basename(files_path)

  filtrados <- files_path[
    vapply(base_names, function(f) {
      any(startsWith(f, files$nome_arquivo))
    }, logical(1))
  ]

  if (length(filtrados) == 0) {
    warning("[AVISO] Nenhum arquivo correspondente encontrado na pasta.", call. = FALSE)
  }

  base_filtrados <- basename(filtrados)

  # Mapear caminhos locais
  files$caminho <- vapply(files$nome_arquivo, function(nome) {

    match_idx <- which(startsWith(base_filtrados, nome))

    if (length(match_idx) > 0) {
      filtrados[match_idx[1]]
    } else {
      NA_character_
    }

  }, character(1))

  files$existe_local <- !is.na(files$caminho)
  files$status_load <- NA_character_

  # Leitura
  data_list <- list()
  idx <- which(files$existe_local)

  if (length(idx) == 0) {
    warning("[AVISO] Nenhum arquivo disponível para leitura.", call. = FALSE)
  }

  for (i in idx) {

    l <- files$caminho[i]

    tryCatch({

      data_temp <- read.dbc::read.dbc(l, as.is = TRUE)

      data_temp <- dts_filter_Df(filtro, data_temp)
      data_temp <- dts_select_col(colunas, data_temp)

      data_list[[length(data_list) + 1]] <- data_temp
      files$status_load[i] <- "Carregado"

      if (verbose) message("Arquivo lido: ", l)

    }, error = function(e) {

      files$status_load[i] <- "Erro na leitura"

      warning(sprintf(
        "[AVISO] Arquivo nao carregado: %s | Erro: %s",
        l, e$message
      ), call. = FALSE)

    })
  }

  # Consolidar
  data <- if (length(data_list) > 0) {
    data.table::rbindlist(data_list, fill = TRUE)
  } else {
    NULL
  }

  if (return_files) {
    return(list(files = files, data = data))
  } else {
    return(data)
  }
}
