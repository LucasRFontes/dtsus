
# cria Sequencia das datas
dts_seq_data <- function(Data_inicio, Data_fim){

  # inicio
  if(!is.null(Data_inicio$mes)){
    inicio <- as.Date(sprintf("%04d-%02d-01", Data_inicio$ano, Data_inicio$mes))
  } else {
    inicio <- as.Date(sprintf("%04d-01-01", Data_inicio$ano))
  }

  # se nao tiver fim, retorna so inicio
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
      stop("[ERRO] Data_inicio nao pode ser maior que Data_fim.", call. = FALSE)
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
  files <- expand.grid(
    fonte = fonte,
    tipo = tipo,
    uf = uf,
    sequencia_datas = sequencia_datas,
    stringsAsFactors = FALSE
  )

  # Corrigindo o sinaSC se necessario
  files$tipo <- ifelse(files$fonte =='SINASC' & files$tipo == 'DN' & files$sequencia_datas <= 1995,'DNR',files$tipo)

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
        if(tipo %in% c('DO','DOR')){
          return('CID9/DORES/')
        }else{
          return('CID9/DOFET/')
        }
      }else{
        if(tipo %in% c('DO','DOR')){
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

  for(l in unique(files$lnk_final)){
    arquivos <- tryCatch({
      unlist(strsplit(RCurl::getURL(url = l, ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n")) # gera lista de arquivos disponiveis
    }, error=function(e){
      stop(sprintf(
        "[ERRO] Nao foi possivel acessar o FTP do DATASUS: %s",
        l
      ), call. = FALSE)


    })

    # Verificando se existe a pasta FINAIS e PRELIM
    if('FINAIS\r' %in% arquivos | 'PRELIM\r' %in% arquivos){
      FINAIS <- data.frame(arquivos = NA, obs = NA,stringsAsFactors = F)
      PRELIM <- data.frame(arquivos = NA, obs = NA,stringsAsFactors = F)


      if('FINAIS\r' %in% arquivos){

        FINAIS_tp <- unlist(strsplit(RCurl::getURL(url = paste0(l,"FINAIS/"), ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))

        if(length(FINAIS_tp) >0){
          FINAIS <- data.frame(arquivos = gsub("\r","",FINAIS_tp),
                               obs = 'FINAIS',
                               stringsAsFactors = F)

        }
      }
      if('PRELIM\r' %in% arquivos){
        PRELIM_tp <- unlist(strsplit(RCurl::getURL(url = paste0(l,"PRELIM/"), ftp.use.epsv = TRUE, dirlistonly = TRUE), "\n"))

        if(length(PRELIM_tp)>0){
          PRELIM <- data.frame(arquivos = gsub("\r","",PRELIM_tp),
                               obs = 'PRELIM',
                               stringsAsFactors = F)
        }

      }
      arquivos <- rbind(FINAIS,PRELIM)

    }else{
      arquivos <- data.frame(arquivos = gsub("\r","",arquivos),
                             obs = NA,stringsAsFactors = F) # transforma em data frame

    }


    df_temp <- files[files$lnk_final == l,] # dataframe temporario correspondente ao arquivo a ser baixado

    arquivos$nome_arquivo <- basename(gsub("\r", "", arquivos$arquivos))
    arquivos <- arquivos[!is.na(arquivos$arquivos),]

    # acha qual prefixo esperado casa com o comeco do arquivo
    arquivos$nome_arquivo <- vapply(
      arquivos$nome_arquivo,
      function(arq) {
        hit <- df_temp$nome_arquivo[startsWith(arq, df_temp$nome_arquivo)]
        if (length(hit) == 0) return(NA_character_)
        hit[which.max(nchar(hit))]  # pega o mais especifico
      },
      character(1)
    )

    arquivos <- arquivos[arquivos$nome_arquivo %in% df_temp$nome_arquivo,]

    lista_arquivos[[length(lista_arquivos) + 1]] <- arquivos

  }

  if (length(lista_arquivos) > 0) {
    lista_arquivos <- do.call(rbind, lista_arquivos)
  } else {
    lista_arquivos <- data.frame()  # data.frame vazio explicito
  }

  # Levando os arquivos a serem baixados para o data frame final
  if(nrow(lista_arquivos) == 0){
    stop("[ERRO] Nenhum arquivo encontrado para os parametros informados (fonte/tipo/UF/periodo).", call. = FALSE)
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

