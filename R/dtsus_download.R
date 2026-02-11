
# Funcoes de validacao
# Fonte
dts_validate_fonte_tipo <- function(fonte = NA, tipo = NA){

  if (length(fonte) != 1 || is.na(fonte)) {
    stop("[ERRO] Informe uma fonte válida do DATASUS (ex: CNES, SIH, SIA, SIM).", call. = FALSE)
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
    SIH = c("ER","RD","RJ","SP"),
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

  if (!fonte %in% names(base_mappimg)) {
    stop(sprintf(
      "[ERRO] Fonte inválida: '%s'. Fontes disponíveis: %s",
      fonte, paste(names(base_mappimg), collapse = ", ")
    ), call. = FALSE)
  }

  tp <- base_mappimg[[fonte]]

  # Se so existir um tipo possível
  if (length(tp) == 1) {
    if (!is.na(tipo) && toupper(trimws(tipo)) != tp) {
      warning(sprintf(
        "[AVISO] Fonte '%s' só possui tipo '%s'. Ignorando tipo informado: '%s'",
        fonte, tp, tipo
      ), call. = FALSE)
    }
    tipo <- tp

  } else {
    tipo <- toupper(trimws(tipo))

    if(all(is.na(tipo)) || length(tipo) != 1) {
      stop(sprintf(
        "[ERRO] Para a fonte '%s', informe um tipo válido. Tipos disponíveis: %s",
        fonte,
        paste(tp, collapse = ", ")
      ), call. = FALSE)
    }


    if (!tipo %in% tp) {
      stop(sprintf(
        "[ERRO] Tipo inválido para '%s': '%s'. Tipos disponíveis: %s",
        fonte, tipo, paste(tp, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # verificando se a fonte e anual ou mensal
  font_anual <- c('ESUS','PCE','PNI','PO','RESP','SIM','SINAN','SINASC')

  if(fonte %in% font_anual){
    periodicidade <- 'anual'
  }else{
    periodicidade <- 'mensal'
  }

  return(list(fonte = fonte,
              tipo = tipo,
              periodicidade = periodicidade))
}

# uf
dts_validate_uf <- function(uf = NA){

  # lista com os estados
  ufs <- c("BR",
           "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
           "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
           "RS","RO","RR","SC","SP","SE","TO",'IG'
  )

  if(all(is.na(uf))){
    warning("[AVISO] UF não informada. Serão utilizadas todas as UFs (incluindo BR).", call. = FALSE)

    return(ufs)}

  uf <- toupper(trimws(uf))

  #filtrando casos de erro
  erro <- uf[!uf %in% ufs]

  if(length(erro)>0){
    stop(
      sprintf(
        "[ERRO] UF inválida(s): %s. Use siglas válidas (ex: MG, SP, RJ).",
        paste(erro, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  return(uf)
}

# Data
dts_validate_data <- function(x,periodicidade) {

  # Remover espacos antes/depois
  x <- as.character(x)
  x <- trimws(x)
  # Verificar se tem exatamente 6 digitos
  if(periodicidade =='mensal'){
    if (!grepl("^[0-9]{6}$", x)) {
      stop("[ERRO] Data inválida. Para bases mensais use AAAAMM (ex: 202412).", call. = FALSE)
    }

    # Separar ano e mes
    ano <- substr(x, 1, 4)
    mes <- substr(x, 5, 6)

    # Validar ano
    if (as.integer(ano) <= 1988 |
        as.integer(ano) > as.integer(format(Sys.Date(), "%Y"))) {
      stop("[ERRO] O ano não pode ser menor que 1988 e não pode ser maior que a data atual.",call. = FALSE)
    }


    # Validar mes
    if (!(mes %in% sprintf("%02d", 1:12))) {
      stop("[ERRO] Data inválida. Para bases mensais use AAAAMM (ex: 202412).", call. = FALSE)
    }

    # Retornar como lista ou tibble
    return(list(
      ano = as.integer(ano),
      mes = as.integer(mes)
    ))
  }

  if(periodicidade =='anual'){
    if (!grepl("^[0-9]{4}$", x)) {
      stop("[ERRO] Data inválida. Para bases anuais use AAAA (ex: 2024).", call. = FALSE)
    }

    # Separar ano e mes
    ano <- x

    # Validar ano
    if (as.integer(ano) <= 1988 |
        as.integer(ano) > as.integer(format(Sys.Date(), "%Y"))) {
      stop("[ERRO] O ano não pode ser menor que 1988 e não pode ser maior que a data atual.",call. = FALSE)
    }

    # Retornar como lista ou tibble
    return(list(
      ano = as.integer(ano)
    ))
  }

}

# Valida a pasta que armazena o dbc
dts_validate_dbc <- function(save.dbc, pasta.dbc) {

  # Validando a pasta para receber o dbc
  if (!isTRUE(save.dbc)){return(NULL)}
  if (is.null(pasta.dbc) || !dir.exists(pasta.dbc)) {
    warning(sprintf(
      "[AVISO] Pasta de saída não encontrada. Salvando DBC em: %s",
      normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    ), call. = FALSE)

    return(getwd())
  }else{
    return(pasta.dbc)
  }

}

# cria Sequencia das datas
dts_seq_data <- function(Data_inicio, Data_fim){

  # inicio
  if(!is.null(Data_inicio$mes)){
    inicio <- as.Date(sprintf("%04d-%02d-01", Data_inicio$ano, Data_inicio$mes))
  } else {
    inicio <- as.Date(sprintf("%04d-01-01", Data_inicio$ano))
  }

  # se não tiver fim, retorna só inicio
  if (is.null(Data_fim) || is.null(Data_fim$ano) || is.na(Data_fim$ano)) {
    seq_datas <- inicio
  } else {

    # fim
    if(!is.null(Data_fim$mes)){
      fim <- as.Date(sprintf("%04d-%02d-01", Data_fim$ano, Data_fim$mes))
    } else {
      fim <- as.Date(sprintf("%04d-01-01", Data_fim$ano))
    }

    # valida ordem
    if(inicio > fim){
      stop("[ERRO] Data_inicio não pode ser maior que Data_fim.", call. = FALSE)
    }

    # sequencia
    if(!is.null(Data_inicio$mes)){
      seq_datas <- seq(inicio, fim, by = "month")
    } else {
      seq_datas <- seq(inicio, fim, by = "year")
    }
  }

  # formatar retorno
  if(!is.null(Data_inicio$mes)){
    seq_datas <- format(seq_datas, "%Y%m")
  } else {
    seq_datas <- format(seq_datas, "%Y")
  }

  return(seq_datas)
}

# Cria o DF que gerencia os arquivos

dts_files_wb <- function(fonte,tipo,uf,sequencia_datas){

  # Preparando lista de arquivos para download
  files <- data.frame(fonte,tipo,uf,sequencia_datas,stringsAsFactors = F)

  # Cria o nome do arquivo
  files$nome_arquivo <- paste0(files$tipo,
                               ifelse(files$uf !='BR'|
                                        ( files$uf == 'BR' & files$tipo %in% c('DN','DCCR','PO')) |
                                        ( files$uf == 'BR' & files$fonte %in% c('SINAN')),files$uf,''), #nao aparece no nome do arquivo
                               ifelse(files$fonte %in% c('SINASC','PO')| files$tipo == 'DO',files$sequencia_datas,substr(files$sequencia_datas,3,6))
  ) # Criando o nome do arquivo
  return(files)

}

# gera os links
dts_files_lnk <- function(files){

  # Fontes de dados disponiveis para download
  base_mappimg <- list(
    CIH = 'CIH/200801_201012/Dados/',
    CIHA ='CIHA/201101_/Dados/',
    CNES ='CNES/200508_/Dados/', #DEPENDE DO TIPO
    ESUS='ESUSNOTIFICA/DADOS/',
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


  files$lnk <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/",
    unlist(base_mappimg[files$fonte])
  )


  # MAPEAMENTO
  files$lnk_compl <- mapply(function(fonte, ano,tipo) {
    ano <- as.integer(ano)

    # fontes com regras diferenciadas para pastas
    if(!fonte %in% c('CNES',"SIH", "SIA",'SINASC','SIM')) return(NA_character_)

    # CNES
    if(fonte =='CNES') return(paste0(tipo,'/'))


    # Regra SINASC
    if( fonte == 'SINASC'){
      if(ano <1996){
        return("1994_1995/Dados/DNRES/")
      }else{
        return('1996_/Dados/DNRES/')
      }
    }

    if(fonte == 'SIM'){
      if(ano <1996){
        if(tipo == 'DO'){
          return('CID9/DORE/')
        }else{
          return('CID9/DOFET/')
        }
      }else{
        if(tipo == 'DO'){
          return('CID10/DORES/')
        }else{
          return('CID10/DOFET/')
        }
      }

    }

    if (fonte %in% c("SIH", "SIA")) {
      if (ano < 2008) {
        if (fonte == "SIH") return("199201_200712/Dados/")
        if (fonte == "SIA") return("199407_200712/Dados/")
      }
      return("200801_/Dados/")
    }

  }, fonte = files$fonte, tipo = files$tipo ,ano = substr(files$sequencia_datas, 1, 4))


  files$lnk_final <- ifelse(is.na(files$lnk_compl),files$lnk,paste0(files$lnk,files$lnk_compl))# Criando o link final

  ### Verifica quais arquivos estao disponiveris para download ###
  lista_arquivos <- list() # lista dos arquivos a ser baixados
  ncaract <- nchar(files$nome_arquivo[1]) # numeros de caracteres q o nome do arquivo tem (fonte varia entre 2 e 3 caract)


  for(l in unique(files$lnk_final)){
    arquivos <- tryCatch({
      unlist(strsplit(RCurl::getURL(url = l, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")) # gera lista de arquivos disponiveis
    }, error=function(e){
      stop(sprintf(
        "[ERRO] Não foi possível acessar o FTP do DATASUS: %s",
        l
      ), call. = FALSE)


    })

    # Verificando se existe a pasta FINAIS e PRELIM
    if('FINAIS\r' %in% arquivos | 'PRELIM\r' %in% arquivos){
      FINAIS <- data.frame(arquivos = NA,nome_arquivo = NA, obs = NA,stringsAsFactors = F)
      PRELIM <- data.frame(arquivos = NA,nome_arquivo = NA, obs = NA,stringsAsFactors = F)


      if('FINAIS\r' %in% arquivos){

        FINAIS <- unlist(strsplit(RCurl::getURL(url = paste0(l,"FINAIS/"), ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))
        FINAIS <- data.frame(arquivos = gsub("\r","",FINAIS),
                             nome_arquivo = substr(FINAIS,1,ncaract),
                             obs = 'FINAIS',
                             stringsAsFactors = F)

      }
      if('PRELIM\r' %in% arquivos){
        PRELIM <- unlist(strsplit(RCurl::getURL(url = paste0(l,"PRELIM/"), ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))
        PRELIM <- data.frame(arquivos = gsub("\r","",PRELIM),
                             nome_arquivo = substr(PRELIM,1,ncaract),
                             obs = 'PRELIM',
                             stringsAsFactors = F)
      }
      arquivos <- rbind(FINAIS,PRELIM)
      arquivos <- arquivos[!is.na(arquivos$nome_arquivo), ]  # Remove NAs

    }else{
      arquivos <- data.frame(arquivos = gsub("\r","",arquivos),
                             nome_arquivo = substr(arquivos,1,ncaract),
                             obs = NA,stringsAsFactors = F) # transforma em data frame

    }


    df_temp <- files[files$lnk_final == l,] # dataframe temporario correspondente ao arquivo a ser baixado

    arquivos <- arquivos[arquivos$nome_arquivo %in% df_temp$nome_arquivo,]

    lista_arquivos[[length(lista_arquivos) + 1]] <- arquivos

  }

  if (length(lista_arquivos) > 0) {
    lista_arquivos <- do.call(rbind, lista_arquivos)
  } else {
    lista_arquivos <- data.frame()  # data.frame vazio explícito
  }

  # Levando os arquivos a serem baixados para o data frame final
  if(nrow(lista_arquivos) == 0){
    stop("[ERRO] Nenhum arquivo encontrado para os parâmetros informados (fonte/tipo/UF/período).", call. = FALSE)
  }else{
    files <- merge(files,lista_arquivos,by ='nome_arquivo',all.x = T)
    files$arquivos[is.na(files$arquivos)] <- NA
  }

  # Realizando o download das bases

  files$lnk_final<- mapply(function(lnk_final, arquivos,obs) {
    out <- ifelse(is.na(obs),
                        paste0(lnk_final,arquivos),
                        paste0(lnk_final,obs,'/',arquivos))

    return(as.character(out))


  },lnk_final = files$lnk_final, arquivos = files$arquivos,obs = files$obs)




  return(files)

}



dts_filter_Df <- function(filtro, df) {

  # Sem filtro = retorna sem warning
  if (is.null(filtro)) return(df)

  # Estrutura mínima
  if (!is.list(filtro) || !all(c("coluna", "valor") %in% names(filtro))) {
    warning("[AVISO] Filtro inválido. Esperado uma lista com 'coluna' e 'valor'. Filtro não aplicado.", call. = FALSE)
    return(df)
  }

  # Normaliza
  filtro$coluna <- trimws(as.character(filtro$coluna))

  # Coluna vazia
  if (is.null(filtro$coluna) || is.na(filtro$coluna) || filtro$coluna == "") {
    warning("[AVISO] Filtro inválido. Campo 'coluna' não preenchido. Filtro não aplicado.", call. = FALSE)
    return(df)
  }

  # Coluna não existe
  if (!filtro$coluna %in% names(df)) {
    warning("[AVISO] Filtro inválido. Coluna selecionada não encontrada na base. Filtro não aplicado.", call. = FALSE)
    return(df)
  }

  # Valor vazio
  if (is.null(filtro$valor) || length(filtro$valor) == 0) {
    warning("[AVISO] Filtro inválido. Nenhum valor informado. Filtro não aplicado.", call. = FALSE)
    return(df)
  }

  # Aplicando filtro
  df_out <- df[as.character(df[[filtro$coluna]]) %in% as.character(filtro$valor), , drop = FALSE]

  # Se filtrou e zerou
  if (nrow(df_out) == 0) {
    warning("[AVISO] O filtro foi aplicado, mas nenhum registro foi encontrado.", call. = FALSE)
  }

  return(df_out)
}



# selecionando apenas as colunas necessarias
dts_select_col <- function(colunas, df) {

  if(is.null(colunas)){
    return(df)}
  faltantes <- setdiff(colunas, names(df))
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[AVISO] Coluna(s) não encontrada(s) e ignorada(s): %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  cols_ok <- intersect(colunas, names(df))

  if (length(cols_ok) == 0) {
    warning("[AVISO] Nenhuma coluna válida encontrada.", call. = FALSE)
    return(df)
  }

  if(length(cols_ok) > 0){
    df[, cols_ok, drop = FALSE]
  }else{df}
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
        download.file(l, temp, mode = "wb", method = "libcurl"),
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
          "[AVISO] Não foi possível salvar o DBC em: %s",
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
          "[AVISO] Arquivo baixado mas não carregado: %s",
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
    open = TRUE,
    filtro =NULL,
    colunas = NULL,
    save.dbc = FALSE,
    pasta.dbc = NULL
){

  # Ajustando possiveis erros de digitação na fonte e tipo


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
    message("[INFO] Conexão com a internet: OK")
  }else{
    stop("[ERRO] Sem conexão com a internet. Verifique sua rede e tente novamente.", call. = FALSE)
  }

  # gera o df com os arquivos a serem baixados
  files <- dts_files_wb(fonte,tipo,uf,sequencia_datas)
  files <- dts_files_lnk(files)

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


