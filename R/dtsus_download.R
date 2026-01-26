

# Funcoes de validacao
# Fonte
dts_validate_fonte_tipo <- function(fonte = NA, tipo = NA){

  if (length(fonte) != 1 || is.na(fonte)) {
    stop('ERRO - Selecione uma Fonte de dados do DATASUS')
  }

  fonte <- toupper(trimws(fonte))

  # Fontes de dados disponiveis para download
  base_mappimg <- list(
    CIH = 'CR',
    CIHA = 'CIHA',
    CNES = c("DC","EE","EF","EP","EQ","GM","HB","IN","LT","PF","RC","SR","ST"),
    ESUS = 'DCCR',
    PCE = 'PCE',
    PNI = c("CPNI","DPNI"),
    PO = 'PO',
    RESP = 'RESP',
    SIA = c("AB","ABO","ACF","AD","AM","AN","AQ","AR","ATD","PA","PS","SAD"),
    SIH = c("CH","CM","ER","RD","RJ","SP","MT"),
    SIM = c("DO","DOEXT","DOFET","DOINF","DOMAT","DOR","DOREXT"),
    SINAN = c("ACBI","ACGR","AIDA","AIDC","ANIM","ANTR","BOTU","CANC","CHAG",
              "CHIK","COLE","COQU","DCRJ","DENG","DERM","DIFT","ESPO","ESQU",
              "EXAN","FMAC","FTIF","HANS","HANT","HEPA","HIVA","HIVC","HIVE",
              "HIVG","IEXO","INFL","LEIV","LEPT","LER", "LERD","LTAN","MALA",
              "MENI","MENT","NTRA","PAIR","PEST","PFAN","PNEU","RAIV","ROTA",
              "SDTA","SIFA","SIFC","SIFG","SRC", "TETA","TETN","TOXC","TOXG",
              "TRAC","TUBE","VARC","VIOL","ZIKA"),
    SINASC = c("DN","DNEX"),
    SISCOLO = c("CC","HC"),
    SISMAMA = c("CM","HM","MM"),
    SISPRENATAL = 'PN'
  )

  # Validacao da fonte
  if (!fonte %in% names(base_mappimg)) {
    stop('ERRO - Foi selecionada uma FONTE inválida.')
  }

  tp <- base_mappimg[[fonte]]

  # Se so existir um tipo possível
  if (length(tp) == 1) {
    tipo <- tp

  } else {
    tipo <- toupper(trimws(tipo))

    if(all(is.na(tipo)) || length(tipo) != 1) {
      stop('ERRO - Insira um TIPO válido para esta fonte.')
    }


    if (!tipo %in% tp) {
      stop('ERRO - Um TIPO desconhecido foi selecionado.')
    }
  }
  return(list(fonte = fonte,
              tipo = tipo))
}

# uf
dts_validate_uf <- function(uf = NA){

  # lista com os estados
  ufs <- c(
    "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
    "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
    "RS","RO","RR","SC","SP","SE","TO"
  )

  if(all(is.na(uf))){
    warning("UF não informada. Todas as UFs serão utilizadas.")

    return(ufs)}

  uf <- toupper(trimws(uf))

  #filtrando casos de erro
  erro <- uf[!uf %in% ufs]

  if(length(erro)>0){
    stop(
      sprintf(
        "UF inválida(s): %s. Use siglas válidas (ex: MG, SP, RJ).",
        paste(erro, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  return(uf)
}

# Data
dts_validate_data <- function(x) {

  # Remover espacos antes/depois
  x <- as.character(x)
  x <- trimws(x)
  # Verificar se tem exatamente 6 digitos
  if (!grepl("^[0-9]{6}$", x)) {
    stop("A data deve estar no formato AAAAMM (ex: 202412).")
  }

  # Separar ano e mes
  ano <- substr(x, 1, 4)
  mes <- substr(x, 5, 6)

  # Validar ano
  if (as.integer(ano) <= 1988 |
      as.integer(ano) > as.integer(format(Sys.Date(), "%Y"))) {
    stop("O ano não pode ser menor que 1988 e não pode ser maior que a data atual.")
  }


  # Validar mes
  if (!(mes %in% sprintf("%02d", 1:12))) {
    stop("O mês deve ser entre 01 e 12.")
  }

  # Retornar como lista ou tibble
  return(list(
    ano = as.integer(ano),
    mes = as.integer(mes)
  ))
}

# Valida a pasta que armazena o dbc
dts_validate_dbc <- function(save.dbc, pasta.dbc) {

   # Validando a pasta para receber o dbc
  if (!isTRUE(save.dbc)){return(NULL)}
  if (is.null(pasta.dbc) || !dir.exists(pasta.dbc)) {
    warning(
      paste("Pasta DBC não encontrada. Salvando arquivo DBC em:", getwd()),
      call. = FALSE
    )
    return(getwd())
  }else{
    return(pasta.dbc)
  }

}

# cria Sequencia das datas
dts_seq_data <- function(Data_inicio,Data_fim){

  # mes inicial
  inicio <- as.Date(paste0(Data_inicio$ano, "-", Data_inicio$mes, "-01"))


  # mes final
  if(!all(is.na(Data_fim))){
    fim <- as.Date(paste0(Data_fim$ano, "-", Data_fim$mes, "-01"))

    # valida ordem
    if(inicio > fim){
      stop("Data inicial maior que a final.")}

    # gera sequencia mensal
    seq_datas <- seq(inicio, fim, by = "month")

  }else{
    seq_datas <- inicio
  }

  # voltar para AAAAMM
  seq_datas <- format(seq_datas, "%Y%m")
  return(seq_datas)
}

# Cria o DF que gerencia os arquivos
dts_files_wb <- function(fonte,tipo,uf,sequencia_datas){

  # Fontes de dados disponiveis para download
  base_mappimg <- list(
    CIH = 'CIH/200801_201012/Dados/',
    CIHA ='CIHA/201101_/Dados/',
    CNES ='CNES/200508_/Dados/', #DEPENDE DO TIPO
    ESUS='ESUSNOTIFICA/DADOS/PRELIM/',
    PCE ='PCE/Dados/',
    PNI ='PNI/DADOS/',
    PO ='painel_oncologia/Dados/',
    RESP ='RESP/DADOS/',
    SIA ='SIASUS/', #DEPENDE DA DATA
    SIH ='SIHSUS/', #DEPENDE DA DATA
    SIM = 'SIM/', #DEPENDE DO TIPO E ANO
    SINAN = 'SINAN/DADOS/', # ECOLHER FINAIS OU PRELIMINARES
    SINASC = 'SINASC/', # DEPENDE DO ANO e ESCOLHER FINAIS OU PRELIMINARES
    SISCOLO = 'siscan/SISCOLO4/DADOS/',
    SISMAMA = 'siscan/SISMAMA/DADOS/',
    SISPRENATAL = 'SISPRENATAL/201201_/Dados/'
  )

  lnk <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/",
                base_mappimg[[fonte]]) # criando o link de acesso ao MS

  ## Fontes cuja a URL varia de acordo com o tipo
  if(fonte == 'CNES'){
    lnk <- paste0(lnk,tipo,'/')
    }

    # Preparando lista de arquivos para download
  files <- data.frame(fonte,tipo,uf,sequencia_datas,lnk)

  files$lnk_compl <- mapply(function(fonte, ano) {

    # Apenas SIH e SIA possuem regra
    if (!fonte %in% c("SIH", "SIA")) return(NA_character_)

    if (ano < 2008) {
      if (fonte == "SIH") return("199201_200712/Dados/")
      if (fonte == "SIA") return("199407_200712/Dados")
    }

    return("200801_/Dados/")

  }, fonte = files$fonte, ano = substr(files$sequencia_datas, 1, 4))


  files$lnk_final <- ifelse(is.na(files$lnk_compl),files$lnk,paste0(files$lnk,files$lnk_compl))# Criando o link final
  files$nome_arquivo <- paste0(files$tipo,files$uf,substr(files$sequencia_datas,3,6)) # Criando o nome do arquivo


  ### Verifica quais arquivos estao disponiveris para download ###
  lista_arquivos <- data.frame() # lista dos arquivos a ser baixados

  for(l in unique(files$lnk_final)){
    arquivos <- tryCatch({
      unlist(strsplit(RCurl::getURL(url = l, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")) # gera lista de arquivos disponiveis
    }, error=function(e){
      stop(
        paste("Problema ao acessar o FTP:", l),
        call. = FALSE
      )

    })
      arquivos <- data.frame(arquivos = gsub("\r","",arquivos),
                           nome_arquivo = substr(arquivos,1,8)) # transforma em data frame


    df_temp <- files[files$lnk_final == l,] # dataframe temporario correspondente ao arquivo a ser baixado

    arquivos <- arquivos[arquivos$nome_arquivo %in% df_temp$nome_arquivo,]

    lista_arquivos <- rbind(lista_arquivos,arquivos)
  }

  # Levando os arquivos a serem baixados para o data frame final
  if(all(is.na(lista_arquivos))){
    stop('Erro - TIPO / UF / PERIODO NAO DISPONIVEL  NO MOMENTO')
  }else{
    files <- merge(files,lista_arquivos,by ='nome_arquivo',all.x = T)
    files$arquivos[is.na(files$arquivos)] <- NA
  }

  # Realizando o download das bases
  files$lnk_final <- paste0(files$lnk_final,files$arquivos)


  return(files)

}

# Valida o filtro e aplica se estiver ok
dts_filter_Df <- function(filtro, df) {
  if (!is.null(filtro)) {
    if (!is.list(filtro) ||
        !all(c("coluna", "valor") %in% names(filtro))){
      warning("Filtro inválido. Esperado uma lista com 'coluna' e 'valor'. Filtro não aplicado.",call. = FALSE)

      } else if (all(is.null(filtro$coluna))) {
        warning("Filtro inválido. Campo Coluna não preenchido. Filtro não aplicado.",call. = FALSE)

        } else if (!filtro$coluna %in% names(df)) {
          warning("Filtro inválido. Coluna selecionada não encontrada na base. Filtro não aplicado.",call. = FALSE)

          } else if (is.null(filtro$valor)) {
            warning("Filtro inválido. Nenhum valor informado. Filtro não aplicado.",call. = FALSE)

            } else {
              df <- df[as.character(df[[filtro$coluna]]) %in%
                         as.character(filtro$valor),,drop = FALSE]}
  }
  return(df)
}

# selecionando apenas as colunas necessarias
dts_select_col <- function(colunas, df) {

  if(is.null(colunas)){
    return(df)}
  faltantes <- setdiff(colunas, names(df))
  if (length(faltantes) > 0) {
    warning(sprintf("Coluna(s) ignorada(s): %s",paste(faltantes, collapse = ", ")),call. = FALSE)
  }

  cols_ok <- intersect(colunas, names(df))
  df[, cols_ok, drop = FALSE]
}


# gerencia o download
dtsus_download_aux <- function(
    files,
    save.dbc = FALSE,
    pasta.dbc = NULL,
    open = TRUE,
    filtro = NULL,
    colunas = NULL) {

  # inicializacoes seguras
  files$status_download <- NA_character_
  files$status_load <- NA_character_
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
      download.file(l, temp, mode = "wb", method = "libcurl")
      files$status_download[i] <- "Download realizado"
      cat("✔ Download realizado:", l, "\n")
    }, error = function(e) {
      files$status_download[i] <- "Erro no download"
      cat("✖ Erro em:", l, "->", conditionMessage(e), "\n")
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
        cat("✔ Arquivo DBC salvo em", pasta.dbc, "\n")
      }, error = function(e) {
        cat("✖ Erro ao salvar DBC\n")
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

        cat("✔ Arquivo lido:", l, "\n")

      }, error = function(e) {
        files$status_load[i] <- "Erro na leitura"
        cat("✖ Arquivo não carregado:", l, "\n")
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


#' Accessing and Processing DATASUS Microdata
#'
#' Downloads public health datasets from the DATASUS FTP server (ftp.datasus.gov.br),
#' supports data preprocessing and filtering, exports results to DBC or RData formats,
#' and generates detailed processing logs.
#'
#' @param fonte The abbreviation of the health information system to be accessed, e.g. CNES.
#' @param tipo The abbreviation of the file to be accessed, e.g. LT.
#' @param uf A specific UF or a vector of UFs specified ny their abbreviations.
#' @param Data_inicio Start year and month in the format yyyymm.
#' @param Data_fim End year and month in the format yyyymm.
#' @param origem This argument is internally fixed and cannot be modified by the user.
#' @param open Logical. If TRUE, the generated file is opened automatically.
#' @param filtro A filter specification indicating the column and the values to be used for filtering.
#' @param colunas A specific column or a vector of columns of interest.
#' @param save.dbc Logical. If TRUE, the output is saved as a DBC file.
#' @param pasta.dbc Path to the output directory where the generated files will be saved. Defaults to the current working directory.
#'
#' @returns TROCAR a \code{data.frame} with the contents of the DBC files.
#' @export
#'
#' @examples
#' # Inserir exemplo aqui
dtsus_download <- function(
    fonte = NA,
    tipo = NA,
    uf = NA,
    Data_inicio = NA,
    Data_fim = NULL,
    origem = 'datasus',
    open = TRUE,
    filtro =list(coluna = NULL,valor = NULL),
    colunas = NULL,
    save.dbc = FALSE,
    pasta.dbc = NULL
    ){

  # Ajustando possiveis erros de digitação na fonte e tipo


  font_valid <- dts_validate_fonte_tipo(fonte,tipo) # Valida fonte e tipo
  fonte <- font_valid$fonte
  tipo <- font_valid$tipo
  uf <- dts_validate_uf(uf) # valida a UF

  Data_inicio <- dts_validate_data(Data_inicio) # Validando se a data foi preenchida corretamente

  if(!is.null(Data_fim)){
    Data_fim <- dts_validate_data(Data_fim)
  } # Validando se a data FINAL foi preenchida corretamente

  #criando a sequencia das datas para realizar o downlad
  sequencia_datas <- dts_seq_data(Data_inicio = Data_inicio,Data_fim = Data_fim)

  ## Caso a origem seja o proprio datasus - online
  if(origem == 'datasus'){

    # Testa a conexao com a internet
    if(curl::has_internet() == T){
      message('Internet: Ok')
    }else{
      stop('ERRO -  Verifique sua conexão com a internet')
    }

    # gera o df com os arquivos a serem baixados
    files <- dts_files_wb(fonte,tipo,uf,sequencia_datas)

    # valida a pasta DBC
    pasta.dbc <- dts_validate_dbc(save.dbc,pasta.dbc)

    res <-dtsus_download_aux(files,save.dbc,pasta.dbc,open,filtro,colunas)
    files <- res$files

    data <- if (length(res$data) > 0) {
      do.call(rbind, res$data)
    } else {
      NULL
    }



    return(list(files = files,dados = data))
  }
}





