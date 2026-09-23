#' Populacao beneficiaria de planos de saude da ANS
#'
#' Baixa, extrai e le a base de informacaes consolidadas de
#' beneficiarios de planos de saude disponibilizada pela Agencia
#' Nacional de Saude Suplementar (ANS).
#'
#' @param ano_mes Competencia no formato `"AAAAMM"`.
#'   Exemplo: `"202403"` para marco de 2024.
#' @param uf Sigla da Unidade Federativa. Exemplo: `"MG"`, `"SP"`, `"BA"`.
#' @param quiet Logico. Se `TRUE`, oculta as mensagens e a barra de progresso
#'   do download. O padrao e `FALSE`.
#' @return Um `data.frame` contendo os dados de beneficiarios da ANS
#'   para a UF e competencia informadas.
#'
#'
#'
#' @examples
#' \dontrun{
#' ans <- dtsus_pop_ans("202403", "MG", quiet = FALSE)
#' }
#'
#' @export
dtsus_pop_ans <- function(ano_mes, uf, quiet = FALSE) {

  # ---------------------------------------------------------------------------
  # Validacao das entradas
  # ---------------------------------------------------------------------------

  # Data
  data_valid <- dts_validate_data(ano_mes,periodicidade = 'mensal')

  ano <- sprintf("%04d", data_valid$ano)
  mes <- sprintf("%02d", data_valid$mes)

  ano_mes <- paste0(ano, mes)

  # Valida UF
  uf <- dts_validate_uf(uf)

  # A base da ANS utilizada aqui e organizada por UF
  if (length(uf) != 1) {
    stop("[ERRO] Informe apenas uma UF.", call. = FALSE)}

  if (uf %in% c("BR", "IG")) {
    stop("[ERRO] Para dados da ANS, informe uma UF estadual (ex: MG, SP, BA).",call. = FALSE)
  }

  # Valida internet
  dts_validate_internet()

  # ---------------------------------------------------------------------------
  # Construcao da URL
  # ---------------------------------------------------------------------------
  url_base <- paste0(
    "https://dadosabertos.ans.gov.br/FTP/PDA/",
    "informacoes_consolidadas_de_beneficiarios-024/"
  )

  nome_base <- paste0(
    "pda-024-icb-",uf,"-",ano,"_",mes
  )

  nome_zip <- paste0(nome_base, ".zip")
  nome_csv <- paste0(nome_base, ".csv")

  url <- paste0(
    url_base,
    ano_mes,
    "/",
    nome_zip
  )

  # ---------------------------------------------------------------------------
  # Arquivos temporarios
  # ---------------------------------------------------------------------------

  dir_destino <- tempdir()

  caminho_zip <- file.path(
    dir_destino,
    nome_zip
  )

  caminho_csv <- file.path(
    dir_destino,
    nome_csv
  )

  # Remove os arquivos temporarios ao finalizar a funcao
  on.exit(
    unlink( c(caminho_zip, caminho_csv), force = TRUE ),
    add = TRUE
  )

  # ---------------------------------------------------------------------------
  # Download
  # ---------------------------------------------------------------------------

  status <- tryCatch(
    {
      utils::download.file(
        url,
        destfile = caminho_zip,
        mode = "wb",
        quiet = quiet
      )
    },
    error = function(e) {
      stop(
        paste0(
          "[ERRO] Nao foi possivel baixar os dados da ANS para ",
          uf,
          " na competencia ",
          ano_mes,
          ".\n",
          "URL: ",
          url,
          "\n",
          "Erro: ",
          e$message
        ),
        call. = FALSE
      )
    }
  )

  if (!identical(status, 0L)) {
    stop(
      paste0(
        "[ERRO] O download dos dados da ANS nao foi concluido. ",
        "Verifique se existem dados para a competencia ",
        ano_mes,
        " e UF ",
        uf,
        "."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Descompactando
  # ---------------------------------------------------------------------------

  utils::unzip(
    caminho_zip,
    exdir = dir_destino
  )

  if (!file.exists(caminho_csv)) {
    stop(
      paste0(
        "O arquivo foi baixado, mas o CSV esperado nao foi encontrado: ",
        nome_csv
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Leitura
  # ---------------------------------------------------------------------------
  final <- tryCatch(
    {
      utils::read.csv2(
        caminho_csv,
        fileEncoding = "UTF-8"
      )
    },
    error = function(e_utf) {

      tryCatch(
        {
          utils::read.csv2(
            caminho_csv,
            fileEncoding = "Latin1"
          )
        },
        error = function(e_latin) {

          stop(
            paste0(
              "[ERRO] Nao foi possivel ler o arquivo CSV da ANS.\n",
              "Erro UTF-8: ", conditionMessage(e_utf), "\n",
              "Erro Latin1: ", conditionMessage(e_latin)
            ),
            call. = FALSE
          )

        }
      )
    }
  )

  return(final)
}
