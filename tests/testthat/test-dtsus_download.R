library('dtsus')
library('testthat')

## Fonte tipo
test_that("Fonte e tipos validados", {

  res <- dts_validate_fonte_tipo(fonte = 'Sih ', tipo = 'rd')
  expect_equal(res$fonte, "SIH")
  expect_equal(res$tipo, "RD")
})

test_that("Erro Fonte invalida", {
  expect_error(dts_validate_fonte_tipo("Gol", "LT"),'ERRO - Foi selecionada uma FONTE inválida.')
})


test_that("Erro Multiplos tipos", {
  expect_error(dts_validate_fonte_tipo("SIA", c("PA",'APAC')),'ERRO - Insira um TIPO válido para esta fonte.')
})

test_that("Erro tipo nao informado", {
  expect_error(dts_validate_fonte_tipo("SIA"),'ERRO - Insira um TIPO válido para esta fonte.')
})

test_that("Erro fonte invalida ", {
  expect_error(dts_validate_fonte_tipo("SIA", "HAHA"),'ERRO - Um TIPO desconhecido foi selecionado.')
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
  res<- dts_validate_data(202305)
  expect_equal(res$ano,2023)
  expect_equal(res$mes,05)
})


test_that('DATA NAO INFORMADA',{
  expect_error(dts_validate_data())
})

test_that('Formato da data errado',{
  expect_error(dts_validate_data('2023/05'))
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
      bases <- dts_files_wb("SIH","RD","AC",202301)

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






}
