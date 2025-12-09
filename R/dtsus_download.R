# INSERIR VALIDADORES DE FONTE, TIPO, DATA
validar_data_aaaamm <- function(x) {

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

# Sequencia das datas
sequencia_data <- function(Data_inicio,Data_fim){

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



dtsus_download <- function(
    fonte = NA,
    tipo = NA,
    uf = NA,
    Data_inicio = 202308,
    Data_fim = 202512,
    origem = 'data.sus'){


  # Validando se a data foi preenchida corretamente
  Data_inicio <- validar_data_aaaamm(Data_inicio)

  if(!all(is.na(Data_fim))){
    Data_fim <- validar_data_aaaamm(Data_fim)
  }

  #criando a sequencia das datas para realizar o downlad
  sequencia_datas <- sequencia_data(Data_inicio = Data_inicio,Data_fim = Data_fim)

  if(origem == 'data.sus'){
    # URL Principal DATASUS
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/"
    lnk <- NA # link que sera formado para download

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

    # Valida se a Fonte foi inserida corretamente e se ela esta disponivel para download
    if(is.na(fonte)){
      return('ERRO - Selecione uma Fonte de dados do DATASUS')
    }
    if(!fonte %in% names(base_mappimg)){
      return('ERRO - Foi selecionada uma FONTE invalida.')
    }else{lnk <- paste0(url,base_mappimg[[fonte]])}

    ## Fontes cuja a URL varia de acordo com o tipo
    if(fonte == 'CNES'){
      if(is.na(tipo)){
        return('ERRO - Necessario informar o TIPO dO arquivo para a fonte')
      }
      if(tipo %in% c( "DC","EE","EF","EP","EQ","GM","HB","IN","LT","PF","RC","SR","ST")){
        lnk <- paste0(lnk,tipo,'/')
      }
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

    files$lnk_final <- paste0(files$lnk,files$lnk_compl) # Criando o link final
    files$nome_arquivo <- paste0(files$tipo,files$uf,substr(files$sequencia_datas,3,6)) # Criando o nome do arquivo

    lista_arquivos <- data.frame() # lista dos arquivos a ser baixados
    # Verificando quais arquivos estao disponiveis
    for(l in unique(files$lnk_final)){
      arquivos <- unlist(strsplit(RCurl::getURL(url = l, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))
      arquivos <- data.frame(arquivos = gsub("\r","",arquivos),
                             nome_arquivo = substr(arquivos,1,8))


      df_temp <- files[files$lnk_final == l,] # dataframe temporario correspondente ao arquivo a ser baixado

      arquivos <- arquivos[arquivos$nome_arquivo %in% df_temp$nome_arquivo,]

      lista_arquivos <- rbind(lista_arquivos,arquivos)
    }

    # Levando os arquivos a serem baixados para o data frame final
    if(all(is.na(lista_arquivos))){
     stop('Erro - TIPO / UF / PERIODO NAO ENCONTRADO')
    }else{
      files <- merge(files,lista_arquivos,by ='nome_arquivo',all.x = T)
      files$arquivos[is.na(files$arquivos)] <- NA
    }

    # Realizando o download das bases
    files$lnk_final <- paste0(files$lnk_final,files$arquivos)


    for (l in files$lnk_final) {

      temp <- tempfile(fileext = '.dbc') # Arquivo temporario que recebe o download
      utils::download.file(l, destfile = temp, mode = 'wb', method = 'libcurl')
      data <- read.dbc::read.dbc(temp,as.is = T)
      unlink(temp)
      gc()
      return(data)


    }





