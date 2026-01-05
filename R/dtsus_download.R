

# Funcoes de validacao
# Fonte
dts_validate_fonte_tipo <- function(fonte = NULL, tipo = NULL){

  if (is.null(fonte)) {
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
    PO = 'PO',
    RESP = 'RESP',
    SIA = c("PA","AB","ABO","ACF","AD","AM","AN","AQ","AR","ATD","PS","SAD"),
    SIH = c("RD","RJ","SP","CH","CM","ER","MT"),
    SIM = c("DOEXT","DOFET","DOINF","DOMAT","DOREXT","DO","DOR"),
    SINAN = c("ACBI","ACGR","ANIM","ANTR","BOTU","CANC","CHAG","CHIK","COLE","COQU","DCRJ","DENG","DERM","DIFT","ESPO","ESQU","FMAC","FTIF","HANS","HANT","IEXO",
              "LEIV","LEPT","LER","LERD","LTAN","MALA","MENI","MENT","NTRA","PAIR","PEST","PFAN","PNEU","RAIV","ROTA","SDTA","TETA","TETN","TOXC","TOXG","TRAC",
              "TUBE","VIOL","ZIKA","AIDA","AIDC","EXAN","HEPA","HIVA","HIVC","HIVE","HIVG","SIFA","SIFC","SIFG","SRC","VARC"),
    SINASC = c("DN","DNEX"),
    SISCOLO = c("CC","HC"),
    SISMAMA = c("CM","HM","MM"),
    SISPRENATAL = 'PN',
    PNI = c("CPNI","DPNI")
  )

  # Validação da fonte
  if (!fonte %in% names(base_mappimg)) {
    stop('ERRO - Foi selecionada uma FONTE inválida.')
  }

  tp <- base_mappimg[[fonte]]

  # Se só existir um tipo possível
  if (length(tp) == 1) {
    tipo <- tp

  } else {
    if (is.null(tipo) || length(tipo) != 1) {
      stop('ERRO - Insira um TIPO válido para esta fonte.')
    }

    tipo <- toupper(trimws(tipo))

    if (!tipo %in% tp) {
      stop('ERRO - Um TIPO desconhecido foi selecionado.')
    }
  }

  # Retorno organizado
  return(list(
    fonte = fonte,
    tipo  = tipo
  ))
}

# uf
dts_validate_uf <- function(uf =NULL){

  # lista com os estados
  ufs <- c(
    "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
    "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
    "RS","RO","RR","SC","SP","SE","TO"
  )

  if(is.null(uf)){
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

  # Remover espa?os antes/depois
  x <- trimws(x)
  # Verificar se tem exatamente 6 d?gitos
  if (!grepl("^[0-9]{6}$", x)) {
    stop("A data deve estar no formato AAAAMM (ex: 202412).")
  }

  # Separar ano e m?s
  ano <- substr(x, 1, 4)
  mes <- substr(x, 5, 6)

  # Validar m?s
  if (!(mes %in% sprintf("%02d", 1:12))) {
    stop("O m?s deve ser entre 01 e 12.")
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
  if (isTRUE(save.dbc)) {
    if (is.null(pasta.dbc) || !dir.exists(pasta.dbc)) {
      message("Pasta DBC não encontrada. Salvando arquivo DBC em: ", getwd())
      return(getwd())
    }else{
      return(pasta.dbc)
    }
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

    # gera sequ?ncia mensal
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
    PO ='painel_oncologia/Dados/',
    RESP ='RESP/DADOS/',
    SIA ='SIASUS/', #DEPENDE DA DATA
    SIH ='SIHSUS/', #DEPENDE DA DATA
    SIM = 'SIM/', #DEPENDE DO TIPO E ANO
    SINAN = 'SINAN/', # ECOLHER FINAIS OU PRELIMINARES
    SINASC = 'SINASC',# DEPENDE DO ANO e ESCOLHER FINAIS OU PRELIMINARES
    SISCOLO = 'siscan/SISCOLO4/DADOS/',
    SISMAMA = 'siscan/SISMAMA/DADOS/',
    SISPRENATAL = 'SISPRENATAL/201201_/Dados/',
    PNI ='PNI/DADOS/'
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
    arquivos <- unlist(strsplit(RCurl::getURL(url = l, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")) # gera lista de arquivos disponiveis
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


dts_filter_Df <- function(filtro,df){
  # aplicando os filtros
  if(is.list(filtro) & all(c('coluna','valor') %in% names(filtro))){
    if(!all(is.na(filtro$coluna) & is.na(filtro$valor))){
      if(filtro$coluna %in% names(df) & length(filtro$coluna) == 1){


        df <- df[
          as.character(df[[filtro$coluna]]) %in% as.character(filtro$valor),]
        print('Filtro aplicado corretamente')
        return(df)
      }else{return(df)}
    }else{return(df)}
  }

}

# selecionando apenas as colunas necessarias
dts_select_col <- function(colunas,df){
  if(any(!is.na(colunas) & is.vector(colunas))){
    if(all(colunas %in% names(df))){
      df <- df[,colunas]
      return(df)
    gc()
    }else{return(df)}
  }else{return(df)}
}

dtsus_download <- function(
    files,
    save.dbc = FALSE,
    pasta.dbc = NULL,
    open = TRUE,
    filtro = NULL,
    colunas = NULL) {

  # inicializações seguras
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


dtsus_download_2 <- function(
    fonte = NULL,
    tipo = NULL,
    uf = NA,
    Data_inicio = NA,
    Data_fim = NA,
    origem = 'datasus',
    open = T,
    filtro =list(coluna = NA,valor = NA),
    colunas = NA,
    save.dbc = T,
    pasta.dbc = NULL
    ){



  dts_validate_fonte_tipo(fonte,tipo) # Valida fonte e tipo
  dts_validate_uf(uf) # valida a UF

  Data_inicio <- dts_validate_data(Data_inicio) # Validando se a data foi preenchida corretamente

  if(!all(is.na(Data_fim))){
    Data_fim <- dts_validate_data(Data_fim)
  } # Validando se a data FINAL foi preenchida corretamente

  #criando a sequencia das datas para realizar o downlad
  sequencia_datas <- dts_seq_data(Data_inicio = Data_inicio,Data_fim = Data_fim)

  ## Caso a origem seja o proprio datasus - online
  if(origem == 'datasus'){

    # Testa a conexao com a internet
    if(curl::has_internet() == T){
      print('Internet: Ok')
    }else{
      stop('ERRO -  Verifique sua conexão com a internet')
    }

    # gera o df com os arquivos a serem baixados
    files <- dts_files_wb(fonte,tipo,uf,sequencia_datas)

    # valida a pasta DBC
    pasta.dbc <- dts_validate_dbc(save.dbc,pasta.dbc)

    res <-dtsus_download(files,save.dbc,pasta.dbc,open,filtro,colunas)
    files <- res$files
    data <- do.call(rbind, res$data)
    return(list(files = files,dados = data))
  }
}










TEMP <-dtsus_download(fonte = 'SIH',tipo = 'RD',uf = 'MG',Data_inicio = 202308,colunas = c('CNES','PROC_REA','MUNIC_MOV'),filtro = c(coluna ='MUNIC_MOV',valor = c(310620,310710)))

