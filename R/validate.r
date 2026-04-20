
# Funcoes de validacao
# Fonte
dts_validate_fonte_tipo <- function(fonte = NA, tipo = NA){

  if (length(fonte) != 1 || is.na(fonte)) {
    stop("[ERRO] Informe uma fonte valida do DATASUS (ex: CNES, SIH, SIA, SIM).", call. = FALSE)
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
    SINASC = c("DN","DNEX",'DNR'),
    SISCOLO = c("CC","HC"),
    SISMAMA = c("CM","HM","MM"),
    SISPRENATAL = 'PN'
  )

  if (!fonte %in% names(base_mappimg)) {
    stop(sprintf(
      "[ERRO] Fonte invalida: '%s'. Fontes disponiveis: %s",
      fonte, paste(names(base_mappimg), collapse = ", ")
    ), call. = FALSE)
  }

  tp <- base_mappimg[[fonte]]

  # Se so existir um tipo possivel
  if (length(tp) == 1) {
    if (!is.na(tipo) && toupper(trimws(tipo)) != tp) {
      warning(sprintf(
        "[AVISO] Fonte '%s' so possui tipo '%s'. Ignorando tipo informado: '%s'",
        fonte, tp, tipo
      ), call. = FALSE)
    }
    tipo <- tp

  } else {
    tipo <- toupper(trimws(tipo))

    if(all(is.na(tipo)) || length(tipo) != 1) {
      stop(sprintf(
        "[ERRO] Para a fonte '%s', informe um tipo valido. Tipos disponiveis: %s",
        fonte,
        paste(tp, collapse = ", ")
      ), call. = FALSE)
    }


    if (!tipo %in% tp) {
      stop(sprintf(
        "[ERRO] Tipo invalido para '%s': '%s'. Tipos disponiveis: %s",
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
    warning("[AVISO] UF nao informada. Serao utilizadas todas as UFs (incluindo BR).", call. = FALSE)

    return(ufs)}

  uf <- toupper(trimws(uf))

  #filtrando casos de erro
  erro <- uf[!uf %in% ufs]

  if(length(erro)>0){
    stop(
      sprintf(
        "[ERRO] UF invalida(s): %s. Use siglas validas (ex: MG, SP, RJ).",
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
      stop("[ERRO] Data invalida. Para bases mensais use AAAAMM (ex: 202412).", call. = FALSE)
    }

    # Separar ano e mes
    ano <- substr(x, 1, 4)
    mes <- substr(x, 5, 6)

    # Validar ano
    if (as.integer(ano) <= 1965 |
        as.integer(ano) > as.integer(format(Sys.Date(), "%Y"))) {
      stop("[ERRO] O ano nao pode ser menor que 1965 e nao pode ser maior que a data atual.",call. = FALSE)
    }


    # Validar mes
    if (!(mes %in% sprintf("%02d", 1:12))) {
      stop("[ERRO] Data invalida. Para bases mensais use AAAAMM (ex: 202412).", call. = FALSE)
    }

    # Retornar como lista ou tibble
    return(list(
      ano = as.integer(ano),
      mes = as.integer(mes)
    ))
  }

  if(periodicidade =='anual'){
    if (!grepl("^[0-9]{4}$", x)) {
      stop("[ERRO] Data invalida. Para bases anuais use AAAA (ex: 2024).", call. = FALSE)
    }

    # Separar ano e mes
    ano <- x

    # Validar ano
    if (as.integer(ano) <= 1965 |
        as.integer(ano) > as.integer(format(Sys.Date(), "%Y"))) {
      stop("[ERRO] O ano nao pode ser menor que 1965 e nao pode ser maior que a data atual.",call. = FALSE)
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
      "[AVISO] Pasta de saida nao encontrada. Salvando DBC em: %s",
      normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    ), call. = FALSE)

    return(getwd())
  }else{
    return(pasta.dbc)
  }

}
