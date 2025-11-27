dtsus_download <- function(fonte = NA,tipo = NA,Data_inicio = 1901,Data_fim = NA){

  # INSERIR VALIDADORES DE FONTE, TIPO, DATA



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

  # Fontes cuja a URL varia de acordo com a data
  if(fonte %in% c('SIA','SIH')){
    if(is.na(Data_inicio)){
      return('ERRO - Necessario informar o PERIODO INICIAL dO arquivo')
    }

    if(fonte == 'SIH' & Data_inicio){
      lnk <- paste0(lnk,tipo,'/')
    }
  }




  pasta <- unlist(strsplit(RCurl::getURL(url = lnk, ftp.use.epsv = TRUE, dirlistonly = TRUE),'\n'))

  if(Dados %in% pasta)


    vl <- tryCatch({
    unlist(strsplit(RCurl::getURL(url = lnk, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))
  }, error = function(e) {
    return("ERRO - Falha ao acessar o servidor FTP. Verifique sua conexão ou a disponibilidade da base.")
  })








  temp <- tempfile(fileext = '.dbc')
  utils::download.file(link, destfile = temp, mode = 'wb', method = 'libcurl')
  data <- read.dbc::read.dbc(temp,as.is = T)
  unlink(temp)
  gc()
  return(data)


}















