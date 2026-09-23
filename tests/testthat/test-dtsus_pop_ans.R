#===============================================================================
# TESTES - dtsus_pop_ans()
#===============================================================================

test_that("dtsus_pop_ans rejeita mais de uma UF", {

  expect_error(
    dtsus_pop_ans(
      ano_mes = "202403",
      uf = c("MG", "SP")
    ),
    "Informe apenas uma UF"
  )

})


test_that("dtsus_pop_ans rejeita BR e IG", {

  expect_error(
    dtsus_pop_ans("202403", "BR"),
    "UF estadual"
  )

  expect_error(
    dtsus_pop_ans("202403", "IG"),
    "UF estadual"
  )

})


test_that("dtsus_pop_ans rejeita competencia invalida", {

  expect_error(
    dtsus_pop_ans("202413", "MG")
  )

})


test_that("dtsus_pop_ans rejeita UF invalida", {

  expect_error(
    dtsus_pop_ans("202403", "XX")
  )

})


test_that("dtsus_pop_ans baixa dados da ANS", {

  skip_if_offline()
  skip_on_cran()

  dados <- dtsus_pop_ans(
    ano_mes = "202403",
    uf = "AC",
    quiet = TRUE
  )

  expect_s3_class(dados, "data.frame")
  expect_gt(nrow(dados), 0)
  expect_gt(ncol(dados), 0)

})


test_that("dtsus_pop_ans baixa dados da ANS", {

  skip_if_offline()
  skip_on_cran()

  dados <- dtsus_pop_ans(
    ano_mes = "202507",
    uf = "pr",
    quiet = TRUE
  )

  expect_s3_class(dados, "data.frame")
  expect_gt(nrow(dados), 0)
  expect_gt(ncol(dados), 0)

})
