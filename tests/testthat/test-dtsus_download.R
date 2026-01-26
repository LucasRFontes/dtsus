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
test_that('DADOS CORRETOS',{
  TESTE <- dts_files_wb('SIH','RD','AC',202301)
  expect_equal()
})
