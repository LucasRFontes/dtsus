library('testthat')

## Fonte tipo
test_that("Fonte e tipos validados", {

  res <- dts_validate_fonte_tipo(fonte = 'Sih ', tipo = 'rd')
  expect_equal(res$fonte, "SIH")
  expect_equal(res$tipo, "RD")
  expect_equal(res$periodicidade,'mensal')
})

test_that("Erro Fonte invalida", {
  expect_error(
    dts_validate_fonte_tipo("Gol", "LT")
  )
})

test_that("Erro Multiplos tipos", {
  expect_error(dts_validate_fonte_tipo("SIA", c("PA",'APAC')))
})

test_that("Erro tipo nao informado", {
  expect_error(dts_validate_fonte_tipo("SIA"))
})

test_that("Erro fonte invalida ", {
  expect_error(dts_validate_fonte_tipo("SIA", "HAHA"))
})

# UF
test_that('UF VALIDA',{
  res <- dts_validate_uf('Mg ')
  expect_equal(res,'MG')
})

test_that('NENHUMA UF INFORAMDA = TODOS ESTADOS',{
  expect_warning(dts_validate_uf())
})

test_that('UF DESCONHECIDA',{
  expect_error(dts_validate_uf('ZZ'))
})

# Data
test_that('DATA CORRETA',{
  res<- dts_validate_data(202305,periodicidade = 'mensal')
  expect_equal(res$ano,2023)
  expect_equal(res$mes,05)
})

test_that('DATA CORRETA',{
  res<- dts_validate_data(2023,periodicidade = 'anual')
  expect_equal(res$ano,2023)
})

test_that('DATA NAO INFORMADA',{
  expect_error(dts_validate_data())
})

test_that('Formato da data errado',{
  expect_error(dts_validate_data('2023/05',periodicidade = 'mensal'))
})

test_that('Formato da data errado',{
  expect_error(dts_validate_data('2023',periodicidade = 'mensal'))
})


test_that('Formato da data errado',{
  expect_error(dts_validate_data('2023/05',periodicidade = 'anual'))
})

test_that('Ano inforamdo errado',{
  expect_error(dts_validate_data('197505'))
})

test_that('Mes inforamdo errado',{
  expect_error(dts_validate_data('199915'))
})

# save DBC
test_that("pasta dbc nao informada gera warning", {
  expect_warning(
    dts_validate_dbc(save.dbc = TRUE, pasta.dbc = NULL)
  )
})

test_that("save dbc = F", {
  TESTE <- dts_validate_dbc(save.dbc = FALSE, pasta.dbc = NULL)
  expect_equal(is.null(TESTE),TRUE)

})


test_that("PASTA DBC INFORMADA", {
  TESTE <- dts_validate_dbc(save.dbc = T, pasta.dbc = '../')
  expect_equal(TESTE,'../')

})

test_that("PASTA NAO EXISTE", {
  expect_warning(dts_validate_dbc(save.dbc = T, pasta.dbc = 'NOMEPOUCOPROVAVELDEUMAPASTATER')
  )
})

# SEQUENCIA DE DATAS
test_that('Sequencia correta',{
  TESTE <- dts_seq_data(list(ano = 2023,mes= 09),list(ano = 2024,mes= 02))
  expect_equal(TESTE,c('202309','202310','202311','202312','202401','202402'))
})

test_that('Sequencia correta',{
  TESTE <- dts_seq_data(list(ano = 2019),list(ano = 2024))
  expect_equal(TESTE,c('2019','2020','2021','2022','2023','2024'))
})

test_that('DATA FIM NULL',{
  TESTE <- dts_seq_data(list(ano = 2019),NULL)
  expect_equal(TESTE,c('2019'))
})


test_that('Sequencia errada',{
  expect_error(dts_seq_data(list(ano = 2023,mes= 09),list(ano = 2023,mes= 02)))
})

test_that('DATA FIM NAO INFORMADA',{
  expect_error(dts_seq_data(list(ano = 2023,mes= 09)))
})

test_that('Erro de digitaçao',{
  expect_error(dts_seq_data(list(ano = 20231,mes= 09),list(ano = 20203,mes= 02)))
})

test_that('Erro de digitaçao',{
  expect_error(dts_seq_data(list(ano = 20231,mes= 09),list(ano = 20203,mes= 02)))
})


# DF gerencia os arquivos
if(curl::has_internet() == T){
  test_that('DADOS CORRETOS',{
    TESTE <- dts_files_wb('SIH','RD','AC',c(202301:202304))
    expect_equal(nrow(TESTE),4)
    expect_equal(TESTE$lnk_final[3],"ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDAC2303.dbc")
    expect_equal(TESTE$lnk_final[1],"ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RDAC2301.dbc")
  })

  test_that('DADOS CORRETOS',{
    TESTE <- dts_files_wb('SIA','PA','SP',202409)
    expect_equal(nrow(TESTE),3)
    expect_equal(TESTE$arquivos,c('PASP2409a.dbc','PASP2409b.dbc','PASP2409c.dbc'))
  })

  # todas as variaveis ja foram testadas antes, o que reduz a chance de erros
  test_that('DADOS CORRETOS',{
    expect_error(dts_files_wb('CNES','LT','SP',205009),'Erro - TIPO / UF / PERIODO NAO DISPONIVEL  NO MOMENTO')
  })
}

# Funçao que filtra
df <- data.frame(
  uf = c("MG",'SP','MG', "SP", 'AC',"RJ"),
  valor = c(10, 20, 30,40,50,60),
  pais = 'BR',
  stringsAsFactors = FALSE
)

test_that('Filtro correto',{
  teste <- dts_filter_Df(list(coluna = 'uf',valor='MG'),df)
  expect_equal(nrow(teste),2)
  expect_equal(teste$valor,c(10,30))
})


test_that('Filtro correto',{
  teste <- dts_filter_Df(list(coluna = 'uf',valor=c('AC','SP')),df)
  expect_equal(nrow(teste),3)
  expect_equal(teste$valor,c(20,40,50))
})

test_that('Filtro nao aplicado, valor faltando',{

  expect_warning(
    dts_filter_Df(list(coluna = "uf"), df),
    "Filtro inválido. Esperado uma lista com 'coluna' e 'valor'. Filtro não aplicado."
  )
})


test_that('Filtro nao aplicado, coluna errada',{

  expect_warning(
    dts_filter_Df(list(coluna = "PROC",valor = 'MG'), df),
    "Filtro inválido. Coluna selecionada não encontrada na base. Filtro não aplicado."
  )
})

# selecionando colunas

test_that('Colunas Selecionadas',{
  teste <- dts_select_col(c('pais','uf'),df)
  expect_equal(ncol(teste),2)
  expect_equal(nrow(teste),6)
  expect_equal(names(teste),c('pais','uf'))
})

test_that('Colunas ignorada',{
  expect_warning(teste <- dts_select_col(c('pais','uf','area'),df))
  expect_equal(ncol(teste),2)
  expect_equal(nrow(teste),6)
  expect_equal(names(teste),c('pais','uf'))
})


test_that('Colunas ignorada',{
  expect_warning(teste <- dts_select_col(c('area'),df))
  expect_equal(ncol(teste),3)
  expect_equal(nrow(teste),6)
  expect_equal(names(teste),c('uf','valor','pais'))
})


# testando a função que realiza o download

if(curl::has_internet() == T){

  bases <- dts_files_wb('SIH','RD','AC',c(202301:202304))

  test_that("Retorno tem estrutura esperada", {

    res <- dtsus_download_aux(bases, save.dbc = FALSE, open = FALSE)

    expect_type(res, "list")
    expect_true(all(c("files","data") %in% names(res)))
    expect_s3_class(res$files, "data.frame")
    expect_type(res$data, "list")
    expect_true(all(!is.na(res$files$status_download)))
    expect_true(all(res$files$status_download %in%
                      c("Download realizado","Erro no download")))
    expect_length(res$data, 0)
  })

  bases <- dts_files_wb('CNES','LT','AC',c(202101:202103))

  test_that("Retorno tem estrutura esperada", {

    res <- dtsus_download_aux(bases, save.dbc = FALSE, open = T)

    expect_type(res, "list")
    expect_true(all(c("files","data") %in% names(res)))
    expect_s3_class(res$files, "data.frame")
    expect_type(res$data, "list")
    expect_true(all(!is.na(res$files$status_download)))
    expect_true(all(res$files$status_download %in%
                      c("Download realizado","Erro no download")))
    expect_true(length(res$data) > 0)
    expect_true(any(res$files$status_load == "Carregado"))
  })


  test_that("Filtro é aplicado aos dados", {
    bases <- dts_files_wb("CNES","LT","MS",202501)

    res <- dtsus_download_aux(
      bases,
      open = TRUE,
      filtro = list(coluna = "CNES", valor = "9081496")
    )

    expect_true(all(res$data[[1]]$CNES == "9081496"))
  })

  test_that("Colunas selecionadas", {
    bases <- dts_files_wb("SIA",'PA',"ES",202201)

    res <- dtsus_download_aux(
      bases,
      open = TRUE,
      colunas = c("PA_CODUNI","PA_QTDPRO",'PA_PROC_ID'),
      filtro = list(coluna = 'PA_PROC_ID',valor =c('0214010163','0211020060'))
    )

    expect_true(all(res$data[[1]]$CNES %in% c('0214010163','0211020060')))
    expect_true(all(names(res$data[[1]]) %in% c("PA_CODUNI","PA_QTDPRO",'PA_PROC_ID')))
  })

}

test_that("DBC é salvo quando save.dbc = TRUE", {
  bases <- dts_files_wb("SIH","RD","SC",202301)
  pasta <- tempdir()

  res <- dtsus_download_aux(
    bases,
    save.dbc = TRUE,
    pasta.dbc = pasta,
    open = FALSE
  )

  expect_true(
    file.exists(file.path(pasta, bases$arquivos[1]))
  )
})

test_that("Erro em um arquivo não interrompe loop", {

  bases <- dts_files_wb("SIH","RD","AC",c(202301:202303))
  bases$lnk_final[2] <- "ftp://endereco.inexistente.salve.o.sus/erro.dbc"

  res <- suppressWarnings(
    dtsus_download_aux(bases, open = TRUE)
  )

  expect_equal(nrow(res$files), nrow(bases))
  expect_equal(res$files$status_download[2], "Nao Realizado")
  expect_equal(unique(res$files$status_download[c(1,3)]), "Download realizado")
})


# Testando a funcao completa, para todas as bases

base_mappimg <- list(
  # CIH = 'CR',
  # CIHA = 'CIHA',
  # CNES = c("DC","EE","EF","EP","EQ","GM","HB","IN","LT","PF","RC","SR","ST"),
  # ESUS = 'DCCR',
  # PCE = 'PCE',
  # PNI = c("CPNI","DPNI"),
  # PO = 'PO',
  # RESP = 'RESP',
  #
  # SIH = c("ER","RD","RJ","SP"),
  # SIA = c("AB","ABO","ACF","AD","AM","AN","AQ","AR","ATD","PA","PS","SAD"),
  # SIM = c("DO","DOEXT","DOFET","DOINF","DOMAT","DOREXT"),
  SINAN = c("ACBI","ACGR","AIDA","AIDC","ANIM","ANTR","BOTU","CANC","CHAG",
            "CHIK","COLE","COQU","DCRJ","DENG","DERM","DIFT","ESPO","ESQU",
            "EXAN","FMAC","FTIF","HANS","HANT","HEPA","HIVA","HIVC","HIVE",
            "HIVG","IEXO","LEIV","LEPT","LER", "LERD","LTAN","MALA",
            "MENI","MENT","NTRA","PAIR","PEST","PFAN","PNEU","RAIV","ROTA",
            "SDTA","SIFA","SIFC","SIFG","SRC", "TETA","TETN","TOXC","TOXG",
            "TRAC","TUBE","VARC","VIOL","ZIKA"),
  SINASC = c("DN","DNEX"),
  SISCOLO = c("CC","HC"),
  SISMAMA = c("CM","HM"),
  SISPRENATAL = 'PN'
)

ufs <- c('RO','AM','AC','TO','SE','MS','DF')
data <- c(201801:201812,
          201901:201912,
          202001:202012,
          202101:202112,
          202201:202212,
          202301:202312,
          202401:202412)

data_ano <- c(2018:2022)

for( n in 1:length(base_mappimg)) {
  base <- names(base_mappimg[n])

  for(t in base_mappimg[[base]]){

    periodo <- dts_validate_fonte_tipo(base,t)$periodicidade

    uf <- sample(ufs,1)
    per <- sample(data,1)

    if(periodo == 'anual'){
      per <- sample(data_ano,1)
    }

    if(base == 'PCE'){
      uf <- 'MG'
    }

    if(t %in% c('EE','GM','AB','ABO',"SAD",'CIHA','CC','CM','PN','HC')){
      uf <- 'SP'
      per <- 201404
    }
    if(t %in% c('AN')){
      uf <- 'SP'
      per <- 201304
    }
    if(t %in% c('AR')){
      uf <- 'SP'
    }

    if(t %in% c('CR')){
      uf <- 'SP'
      per <- 201004
    }

    if(base %in% 'PNI'){
      uf <- 'MA'
      per <- 2007


    }
    if(t %in% c('PO','DOEXT','DOFET','DOINF','DOMAT','DOREXT') | base %in% c('SINAN','SINASC')){
      uf <- 'BR'
      per <- 2019
    }
    if(t %in% 'AIDA'){
      per <- 2014
    }

    if(t %in% 'DCCR'){
      per <- 2023
      uf <- 'BR'
    }

    print(paste0('Testando ',base, ' - ',t, ' UF: ',uf,' PERIODO ',per))

    dtsus_download(fonte = base,tipo = t,uf = uf,Data_inicio = per,open = T,save.dbc = F)
  }

}
