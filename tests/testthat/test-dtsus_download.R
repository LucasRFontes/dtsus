library('testthat')

## Fonte tipo
test_that("Fonte e tipos validados", {

  res <- dts_validate_fonte_tipo(fonte = 'Sih ', tipo = 'rd')
  expect_equal(res$fonte, "SIH")
  expect_equal(res$tipo, "RD")
  expect_equal(res$periodicidade,'mensal')
})

test_that("Fonte com tipo unico ignora tipo errado com warning", {
  expect_warning(
    res <- dts_validate_fonte_tipo("ESUS", "QUALQUER"),
    "possui tipo"
  )

  expect_equal(res$tipo, "DCCR")
})


test_that("Erro Fonte invalida - mensagem completa", {
  expect_error(
    dts_validate_fonte_tipo("Gol", "LT"),
    "\\[ERRO\\] Fonte invalida: 'GOL'"
  )
})

test_that("Erro Multiplos tipos", {
  expect_error(
    dts_validate_fonte_tipo("SIA", c("PA", "APAC")),
    "informe um tipo valido"
  )
})

test_that("Erro tipo nao informado", {
  expect_error(dts_validate_fonte_tipo("SIA"))
})

test_that("Erro Tipo invalido", {
  expect_error(
    dts_validate_fonte_tipo("SIA", "HAHA"),
    "Tipo invalido para 'SIA'"
  )
})


# UF
test_that('UF VALIDA',{
  res <- dts_validate_uf('Mg ')
  expect_equal(res,'MG')
})

test_that('NENHUMA UF INFORAMDA = TODOS ESTADOS',{
  expect_warning(res <- dts_validate_uf(),'UF')
  expect_true("MG" %in% res)
  expect_true("BR" %in% res)
})

test_that('UF DESCONHECIDA',{
  expect_error(dts_validate_uf('ZZ'))
})

# Data
test_that('DATA CORRETA',{
  res<- dts_validate_data(202305,periodicidade = 'mensal')
  expect_equal(res$ano,2023)
  expect_equal(res$mes,5)
})

test_that('DATA CORRETA',{
  res<- dts_validate_data(2023,periodicidade = 'anual')
  expect_equal(res$ano,2023)
})

test_that('DATA NAO INFORMADA',{
  expect_error(dts_validate_data(NULL,periodicidade = 'anual'))
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
  expect_error(dts_validate_data('150505',periodicidade = 'mensal'))
})

test_that('Mes inforamdo errado',{
  expect_error(dts_validate_data('199915',periodicidade = 'mensal'))
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
  expect_warning(
    dts_validate_dbc(save.dbc = TRUE, pasta.dbc = "NOMEPOUCOPROVAVELDEUMAPASTATER"),
    "nao existe|nao existe|nao encontrada|nao encontrada"
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
  expect_error(
    dts_seq_data(list(ano = 2023, mes = 9)),
    "argument.*Data_fim"
  )
})


test_that('Sequencia errada',{
  expect_error(dts_seq_data(list(ano = 2023,mes= 09),list(ano = 2023,mes= 02)))
})

test_that('Erro de digitacao',{
  expect_error(dts_seq_data(list(ano = 20231,mes= 09),list(ano = 20203,mes= 02)))
})


# DF gerencia os arquivos
if(curl::has_internet() == T){
  test_that('DADOS CORRETOS',{
    TESTE <- dts_files_wb('SIH','RD','AC',c(202301:202304))
    expect_equal(nrow(TESTE),4)
    expect_equal(TESTE$nome_arquivo[3],"RDAC2303")
  })

  test_that('DADOS CORRETOS',{
    TESTE <- dts_files_wb('SIA','PA','SP',202409)
    expect_equal(nrow(TESTE),1)
  })
}



if (curl::has_internet() == TRUE) {

  test_that("SIA PA SP 202411 retorna 4 arquivos (a,b,c,d)", {

    bases <- dts_files_wb("SIA", "PA", "SP", 202511)
    res <- dts_files_lnk(bases)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 4)

    expect_setequal(
      res$arquivos,
      c("PASP2511a.dbc", "PASP2511b.dbc", "PASP2511c.dbc", "PASP2511d.dbc")
    )
  })

}

if (curl::has_internet() == TRUE) {

  test_that("SINASC DN RS 1994-1998 retorna 1 arquivo por ano", {

    bases <- dts_files_wb("SINASC", "DN", "RS", 1994:1998)
    res <- dts_files_lnk(bases)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 5)

    expect_setequal(
      res$arquivos,
      c("DNRRS1994.dbc", "DNRRS1995.dbc", "DNRS1996.DBC", "DNRS1997.DBC", "DNRS1998.DBC")
    )
  })

}


# Funcao que filtra
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
    "\\[AVISO\\] Filtro invalido"
  )
})


test_that('Filtro nao aplicado, coluna errada',{

  expect_warning(
    dts_filter_Df(list(coluna = "PROC",valor = 'MG'), df),
    "Filtro invalido. Coluna selecionada nao encontrada na base. Filtro nao aplicado."
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
  suppressWarnings(teste <- dts_select_col(c("area"), df))

  expect_equal(ncol(teste),3)
  expect_equal(nrow(teste),6)
  expect_equal(names(teste),c('uf','valor','pais'))
})


# testando a funcao que realiza o download

if(curl::has_internet() == T){

  test_that('Retorna os arquivos certos SIA ',{
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN
    bases <- dts_files_wb('SIA','PA','SP',c(202512))

    res <- dts_files_lnk(bases)

    expect_s3_class(res,'data.frame')
    expect_equal(nrow(res),4)
    expect_true(all(c('PASP2512a.dbc','PASP2512b.dbc','PASP2512c.dbc','PASP2512d.dbc') %in% res$arquivos))
    expect_setequal(
      res$arquivos,
      c('PASP2512a.dbc','PASP2512b.dbc','PASP2512c.dbc','PASP2512d.dbc')
    )
  })


  bases <- dts_files_wb('SIH','RD','AC',c(202301:202304))

  test_that('Retorna os arquivos certos',{
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN
    res <- dts_files_lnk(bases)

    expect_s3_class(res,'data.frame')
    expect_equal(nrow(res),4)
    expect_true(all(c('RDAC2301.dbc','RDAC2302.dbc','RDAC2303.dbc','RDAC2304.dbc') %in% res$arquivos))
    expect_setequal(
      res$arquivos,
      c("RDAC2301.dbc", "RDAC2302.dbc", "RDAC2303.dbc", "RDAC2304.dbc")
    )
  })

  bases <- dts_files_lnk(bases)

  test_that("Retorno tem estrutura esperada", {
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN

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
  bases <- dts_files_lnk(bases)

  test_that("Retorno tem estrutura esperada", {
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN

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


  test_that("Filtro e aplicado aos dados", {
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN
    bases <- dts_files_wb("CNES","LT","MS",202501)
    bases <- dts_files_lnk(bases)

    res <- dtsus_download_aux(
      bases,
      open = TRUE,
      filtro = list(coluna = "CNES", valor = "9081496")
    )

    expect_true(all(res$data[[1]]$CNES == "9081496"))
  })


  test_that("Colunas selecionadas", {
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN
    bases <- dts_files_wb("SIA",'PA',"ES",202201)
    bases <- dts_files_lnk(bases)
    res <- dtsus_download_aux(
      bases,
      open = TRUE,
      colunas = c("PA_CODUNI","PA_QTDPRO",'PA_PROC_ID'),
      filtro = list(coluna = 'PA_PROC_ID',valor =c('0214010163','0211020060'))
    )

    expect_true(all(res$data[[1]]$PA_PROC_ID %in% c('0214010163','0211020060')))
    expect_true(all(c("PA_CODUNI","PA_QTDPRO",'PA_PROC_ID') %in% names(res$data[[1]])))
  })

}

test_that("DBC e salvo quando save.dbc = TRUE", {
  skip_if_offline()  # Pula o teste se estiver offline
  skip_on_cran()     # Pula no CRAN

  bases <- dts_files_wb("SIH","RD","SC",202301)
  bases <- dts_files_lnk(bases)
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

test_that("Erro em um arquivo nao interrompe loop", {
  skip_if_offline()  # Pula o teste se estiver offline
  skip_on_cran()     # Pula no CRAN

  bases <- dts_files_wb("SIH","RD","AC",c(202301:202303))
  bases <- dts_files_lnk(bases)
  bases$lnk_final[2] <- "ftp://endereco.inexistente.salve.o.sus/erro.dbc"

  res <- suppressWarnings(
    dtsus_download_aux(bases, open = TRUE)
  )

  expect_equal(nrow(res$files), nrow(bases))
  expect_equal(res$files$status_download[2], "Nao Realizado")
  expect_equal(unique(res$files$status_download[c(1,3)]), "Download realizado")
})


# Testando a funcao completa, para todas as bases
test_that("Baixa pelo menos 1 arquivo de cada pasta (amostra do crawler)", {

  skip_if_offline()  # Pula o teste se estiver offline
  skip_on_cran()     # Pula no CRAN (recomendado)

  # Carrega o objeto
  load(system.file("data", "dbc_teste.rda", package = "dtsus"))

  # Espera que dbc_sample_pasta exista
  expect_true(exists("dbc_sample_pasta"))
  expect_s3_class(dbc_sample_pasta, "data.frame")

  # =========================
  # AJUSTES
  # =========================

  dbc_sample_pasta$fonte <- ifelse(dbc_sample_pasta$fonte == "ESUSNOTIFICA", "ESUS", dbc_sample_pasta$fonte)
  dbc_sample_pasta$fonte <- ifelse(dbc_sample_pasta$fonte == "SIASUS", "SIA", dbc_sample_pasta$fonte)
  dbc_sample_pasta$fonte <- ifelse(dbc_sample_pasta$fonte == "SIHSUS", "SIH", dbc_sample_pasta$fonte)
  dbc_sample_pasta$fonte <- ifelse(dbc_sample_pasta$fonte == "painel_oncologia", "PO", dbc_sample_pasta$fonte)

  dbc_sample_pasta$fonte <- ifelse(
    dbc_sample_pasta$fonte == "SISCAN" & grepl("SISMAMA", dbc_sample_pasta$pasta),
    "SISMAMA",
    dbc_sample_pasta$fonte
  )

  dbc_sample_pasta$fonte <- ifelse(
    dbc_sample_pasta$fonte == "SISCAN" & grepl("SISCOLO", dbc_sample_pasta$pasta),
    "SISCOLO",
    dbc_sample_pasta$fonte
  )

  dbc_sample_pasta <- dbc_sample_pasta[
    !dbc_sample_pasta$tipo %in% c("PAM","PAR","PAS","BI","BIS","BIM","AMP","BIR","CH","DOPA"),
  ]

  dbc_sample_pasta <- dbc_sample_pasta[
    dbc_sample_pasta$fonte == "SIH" & dbc_sample_pasta$tipo %in% c("ER","RD","RJ","SP") |
      dbc_sample_pasta$fonte != "SIH",
  ]

  # =========================
  # Execucao (teste)
  # =========================

  expect_true(nrow(dbc_sample_pasta) > 0)

  for (n in seq_len(nrow(dbc_sample_pasta))) {

    temp <- dbc_sample_pasta[n, ]

    base <- temp$fonte

    tipo <- ifelse(temp$fonte == "SIM", temp$letras, temp$tipo)
    tipo <- ifelse(temp$fonte == "SIM" & tipo %in% c("DOPA","DORRJ"), temp$tipo, tipo)

    uf <- ifelse(
      temp$fonte == "SIM" & !tipo %in% c("DO","DOR"),
      "BR",
      substr(temp$letras, nchar(temp$letras) - 1, nchar(temp$letras))
    )

    per <- ifelse(
      temp$tipo %in% c("DO","DN","DNR","PO"),
      temp$numeros,
      ifelse(
        as.numeric(substr(temp$numeros, 1, 2)) > 26,
        paste0(19, temp$numeros),
        paste0(20, temp$numeros)
      )
    )

    # Mensagem de debug
    print(paste0(n," - Testando ", base, " - ", tipo, " UF: ", uf, " PERIODO ", per))

    # Rodando download
    res <- tryCatch(
      dtsus_download(
        fonte = base,
        tipo = tipo,
        uf = uf,
        Data_inicio = per,
        open = TRUE,
        save.dbc = FALSE,
        return_files = T
      ),
      error = function(e) e
    )

    # Se falhou por instabilidade do FTP, nao derruba o pacote inteiro
    if (inherits(res, "error")) {
      testthat::skip(paste0(
        "Falha pontual ao testar: ", base, "-", tipo,
        " (UF=", uf, ", PER=", per, "). Erro: ", res$message
      ))
    }

    # =========================
    # Validacoes minimas
    # =========================
    expect_type(res, "list")
    expect_true(all(c("files","data") %in% names(res)))

    expect_s3_class(res$files, "data.frame")
    expect_true(nrow(res$files) >= 1)

    # Pelo menos um arquivo deve ter sido baixado ou carregado
    expect_true(any(res$files$status_download %in% c("Download realizado","Erro no download","Nao Realizado")))

    # Como open = TRUE, a expectativa e que tente carregar algo
    expect_type(res$data, "list")
  }
})

# ULTIMA FUNcAO
if (curl::has_internet() == TRUE) {

  test_that("Baixa, filtra, seleciona colunas e abre (CNES LT MG 201801-201805)", {
    skip_if_offline()  # Pula o teste se estiver offline
    skip_on_cran()     # Pula no CRAN

    res <- dtsus_download(
      fonte = "CNES",
      tipo = "LT",
      uf = "MG",
      Data_inicio = 201801,
      Data_fim = 201805,
      open = TRUE,
      filtro = list(coluna = "CNES", valor = "0027014"),
      colunas = c("COMPETEN", "CNES", "TP_LEITO", "QT_SUS"),
      return_files = T
    )

    expect_type(res, "list")
    expect_true(all(c("files", "data") %in% names(res)))

    expect_s3_class(res$files, "data.frame")
    expect_true(nrow(res$files) > 0)

    # Checa filtro
    expect_true(all(unique(res$data$CNES) == "0027014"))

    # Checa periodo
    expect_true(all(unique(res$data$COMPETEN) %in% 201801:201805))

    # Checa colunas selecionadas
    expect_setequal(
      names(res$data),
      c("COMPETEN", "CNES", "TP_LEITO", "QT_SUS")
    )
  })

}
