
#' Accessing and Processing DATASUS Microdata
#'
#' Downloads public health datasets from the DATASUS FTP server (ftp.datasus.gov.br),
#' supports data preprocessing and filtering, optionally saves the DBC files,
#' and returns the downloaded data as a data.frame.
#'
#' @param fonte Character. The abbreviation of the health information system to be accessed,
#'   e.g. "CNES", "SIH", "SIA".
#' @param tipo Character. The abbreviation of the file type to be accessed, e.g. "LT", "RD".
#' @param uf Character. A UF code or a vector of UF codes (e.g. "MG", "SP", "BR").
#' @param Data_inicio Numeric or character. Start date in the format yyyymm (monthly)
#'   or yyyy (annual), depending on the selected dataset.
#' @param Data_fim Numeric or character. End date in the format yyyymm (monthly)
#'   or yyyy (annual), depending on the selected dataset.
#' @param open Logical. If TRUE, the downloaded files are read and returned as data.
#' @param filtro List. Optional filter specification with two fields:
#'   \code{list(coluna = "COL", valor = c("X","Y"))}.
#' @param colunas Character. Optional vector of columns to keep.
#' @param save.dbc Logical. If TRUE, saves the downloaded DBC files locally.
#' @param pasta.dbc Character. Path to the output directory where the DBC files will be saved.
#'   Defaults to the current working directory if not provided.
#' @param return_files Logical. If TRUE, returns a list with both the file index and the downloaded data.
#'   If FALSE, returns only the downloaded data.
#'
#' @return If \code{return_files = TRUE}, returns a list with:
#' \describe{
#'   \item{files}{A data.frame containing the indexed files, download links and status information.}
#'   \item{data}{A data.frame with the downloaded microdata (only if \code{open = TRUE}).}
#' }
#' If \code{return_files = FALSE}, returns only the \code{data} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- dtsus_download(
#'   fonte = "CNES",
#'   tipo = "LT",
#'   uf = "MG",
#'   Data_inicio = 201801,
#'   Data_fim = 201803,
#'   open = TRUE,
#'   save.dbc = FALSE,
#'   return_files = TRUE
#' )
#'
#' head(res$data)
#' }

dtsus_download <- function(
    fonte = NA,
    tipo = NA,
    uf = NA,
    Data_inicio = NA,
    Data_fim = NULL,
    open = TRUE,
    filtro =NULL,
    colunas = NULL,
    save.dbc = FALSE,
    pasta.dbc = NULL,
    return_files = T
){

  # Ajustando possiveis erros de digitacao na fonte e tipo


  font_valid <- dts_validate_fonte_tipo(fonte,tipo) # Valida fonte e tipo
  fonte <- font_valid$fonte
  tipo <- font_valid$tipo
  prdcd <- font_valid$periodicidade
  uf <- dts_validate_uf(uf) # valida a UF

  Data_inicio <- dts_validate_data(Data_inicio,periodicidade = prdcd) # Validando se a data foi preenchida corretamente

  if(!is.null(Data_fim)){
    Data_fim <- dts_validate_data(Data_fim,periodicidade = prdcd)
  } # Validando se a data FINAL foi preenchida corretamente

  #criando a sequencia das datas para realizar o downlad
  sequencia_datas <- dts_seq_data(Data_inicio = Data_inicio,Data_fim = Data_fim)

  # Testa a conexao com a internet
  if(curl::has_internet() == T){
    message("[INFO] Conexao com a internet: OK")
  }else{
    stop("[ERRO] Sem conexao com a internet. Verifique sua rede e tente novamente.", call. = FALSE)
  }

  # gera o df com os arquivos a serem baixados
  files <- dts_files_wb(fonte,tipo,uf,sequencia_datas)
  files <- dts_files_lnk(files)

  # valida a pasta DBC
  pasta.dbc <- dts_validate_dbc(save.dbc,pasta.dbc)

  res <-dtsus_download_aux(files,save.dbc,pasta.dbc,open,filtro,colunas)
  files <- res$files

  data <- if (length(res$data) > 0) {
    data.table::rbindlist(res$data, fill = TRUE)
  } else {
    NULL
  }

  if(return_files == T){
    return(list(files = files,data = data))
  }else{
    return(data)
  }

}


