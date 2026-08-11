#' Updates or checks the status of local DBC files
#'
#' This function evaluates whether the DBC files saved on the computer are up to date
#' relative to the data available on the DATASUS server. It can either only
#' check the status (\code{apenas_verificar = TRUE}) or automatically download
#' outdated or missing files.
#'
#' @param fonte Character. Fonte dos dados (ex: "SIH", "SIM", "SINAN").
#' @param tipo Character. Tipo do dado (ex: "RD", "DO", etc.).
#' @param uf Character. Unidade da Federação (ex: "SP", "RJ", "BR" para Brasil).
#' @param Data_inicio Numeric ou character. Start date in the format
#' yyyymm (monthly) or yyyy (annual), depending on the selected dataset.
#' This parameter is required.
#' @param Data_fim Numeric ou character. End date in the same format
#' as \code{Data_inicio}. Default is \code{NULL} (searches for the start
#' date only).
#' @param pasta.dbc Character. Path to the folder where the .DBC files are saved.
#' If \code{NULL}, the function will attempt to validate or request the path.
#' @param apenas_verificar Logical. If \code{TRUE}, the function only checks
#' and returns the status of each file without downloading.
#' If \code{FALSE} (default), it downloads outdated or missing files.
#'
#' @return A \code{data.frame} with the columns \code{nome_arquivo}, \code{fonte},
#' \code{tipo}, \code{uf}, \code{sequencia_datas}, \code{Base_Atualizada}
#' (final status of each file), and \code{status_download} (detail of the
#' download/reconstruction action performed, or \code{NA} when no action was
#' necessary). The return format is the same regardless of the value of
#' \code{apenas_verificar}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates the destination folder path.
#'   \item Checks the internet connection.
#'   \item Lists expected files based on the provided parameters.
#'   \item Checks for the existence of DBC files and cached metadata.
#'   \item For cached files, evaluates whether an update is available on the server.
#'   \item If \code{apenas_verificar = FALSE}, downloads missing or outdated
#'         files, recording the status of each operation.
#' }
#'
#' @examples
#' \dontrun{
#' # Only check the status of SIH files for SP in Jan/2025
#' resultado <- dtsus_refresh(
#'   fonte = "SIH",
#'   tipo = "RD",
#'   uf = "SP",
#'   Data_inicio = 202501,
#'   apenas_verificar = TRUE
#' )
#' print(resultado)
#'
#' # Download outdated files
#' dtsus_refresh(
#'   fonte = "SIM",
#'   tipo = "DO",
#'   uf = "BR",
#'   Data_inicio = 2024,
#'   pasta.dbc = "caminho/para/sua/pasta"
#' )
#' }
#'
#' @export
#'
#' @importFrom curl has_internet
#' @importFrom tools file_path_sans_ext


dtsus_refresh <- function(
    fonte = NA,
    tipo = NA,
    uf = NA,
    Data_inicio = NA,
    Data_fim = NULL,
    pasta.dbc = NULL,
    apenas_verificar = FALSE){

  # 1. Validar a pasta ANTES de montar o request -- evita trabalho
  # desnecessario caso o usuario decida abortar no prompt abaixo
  pasta.dbc <- dts_validate_path(pasta.dbc)

  # prepara o data frame com os arquivos a serem verificados
  prep <- dts_preparar_request(fonte = fonte,
                               tipo = tipo,
                               uf = uf,
                               Data_inicio = Data_inicio,
                               Data_fim = Data_fim)
  files <- prep$files # df com os arquivos a serem baixados

  # Verifica se a internet esta disponivel para realizar a atualizacao
  if (curl::has_internet()) {
    message("[INFO] Conexao com a internet: OK")
  } else {
    stop("[ERRO] Sem conexao com a internet. Verifique sua rede e tente novamente.", call. = FALSE)
  }

  files <- dts_files_lnk(files)

  # Verificando se os arquivos DBC existem e estao salvos
  arquivos <- data.frame(
    arquivo = list.files(path = pasta.dbc, pattern = "\\.dbc$", ignore.case = TRUE),
    Existe.dbc = TRUE
  )

  files$Existe.dbc <- sapply(tools::file_path_sans_ext(files$arquivos), function(x) {
    any(startsWith(toupper(arquivos$arquivo), toupper(x)))
  })

  # Verifica a pasta dtsus_cache
  pasta_cache <- file.path(pasta.dbc, "dtsus_cache")
  cache_existe <- dir.exists(pasta_cache) # verifica se a pasta com os jsons ja existe

  if (!cache_existe) {
    message("[INFO] Nenhum metadado encontrado")
    dir.create(pasta_cache, showWarnings = FALSE)
  }

  arquivos_cache <- data.frame(
    arquivo = list.files(pasta_cache, pattern = "\\.json$"),
    Existe.cache = TRUE
  )

  files$Existe.cache <- sapply(files$nome_arquivo, function(x) {
    any(startsWith(toupper(arquivos_cache$arquivo), toupper(x)))
  })

  idx <- seq_len(nrow(files))

  files$Base_Atualizada <- NA
  files$status_download <- NA_character_

  for (i in idx) {
    # Caso 1: dbc existe mas falta cache -- reconstroi o metadado a
    # partir do arquivo local. Perde a data/hora do download original,
    # mas ganha uma base (hash/tamanho) para comparar com o servidor.
    if (files$Existe.dbc[i] && !files$Existe.cache[i]) {

      caminho_local <- file.path(
        pasta.dbc,
        arquivos$arquivo[startsWith(toupper(arquivos$arquivo), toupper(files$nome_arquivo[i]))][1]
      )

      files$status_download[i] <- "Reconstruido (dbc local, sem registro de download)"

      reconstruido_ok <- tryCatch({
        dts_salvar_metadados(files[i, ], temp = caminho_local, pasta.dbc = pasta.dbc)
        TRUE
      }, error = function(e) {
        warning(sprintf(
          "[AVISO] Nao foi possivel reconstruir o metadado de %s: %s",
          files$nome_arquivo[i], conditionMessage(e)
        ), call. = FALSE)
        FALSE
      })

      if (reconstruido_ok) {
        files$Existe.cache[i] <- TRUE
        message("[INFO] Metadado reconstruido a partir do arquivo local: ", files$nome_arquivo[i])
      }
      # se falhar, Existe.cache[i] continua FALSE e o caso cai no
      # bloco "nem dbc nem cache" mais abaixo -- fora do escopo do
      # refresh, mas de forma explicita (Base_Atualizada = NA)
    }

    # Fluxo normal: dbc + cache (original ou reconstruido no Caso 1)
    if (files$Existe.cache[i]) {

      files$Base_Atualizada[i]  <- dts_verificar_atualizacao(files[i, ], pasta.dbc = pasta.dbc,
                                        tentativas = 5, espera_seg = 1)$status




    } else {
      # Caso 2 (apos remocao), Caso 1 (reconstrucao que falhou) ou
      # Caso 3: nem dbc nem cache -- fora do escopo do refresh, nao ha
      # nada para verificar
      files$Base_Atualizada[i] <- NA
    }
  }


  # Caso seja apenas para verificar, o resultado ja eh exportado
  if(isTRUE(apenas_verificar)){
    files <- unique(files[c('nome_arquivo','fonte','tipo','uf','sequencia_datas','Base_Atualizada','status_download')])
    return(files)
  }else{

    ## verificando quem precisa de atualizacao##
    files_atualizar <- files[
      files$Existe.dbc &
        !is.na(files$Base_Atualizada) &
        files$Base_Atualizada != "atualizado",
    ]


    files_atualizar <- unique(files_atualizar[c('nome_arquivo','fonte','tipo','uf','sequencia_datas','Base_Atualizada','status_download')])


    # Casos em que nao existe o dbc
    files_sem_dbc <- files[!files$Existe.dbc,]
    if(nrow(files_sem_dbc)>0){
      files_sem_dbc$Base_Atualizada <- 'DBC Nao encontrado'
    }
    files_sem_dbc <-  unique(files_sem_dbc[c('nome_arquivo','fonte','tipo','uf','sequencia_datas','Base_Atualizada','status_download')])

    # atualizadas
    atualizada <- unique(files[c('nome_arquivo','fonte','tipo','uf','sequencia_datas','Base_Atualizada','status_download')])
    atualizada <- atualizada[atualizada$Base_Atualizada == "atualizado",]

    # BUGFIX (#4): dbc existe mas Base_Atualizada ficou NA (sem cache e
    # reconstrucao falhou) -- antes esses registros sumiam do resultado
    # final. Agora entram com status proprio, sem tentativa de download
    # (nao ha base de comparacao confiavel).
    files_sem_metadado <- files[
      files$Existe.dbc & is.na(files$Base_Atualizada),
    ]

    if(nrow(files_sem_metadado)>0){
      files_sem_metadado$Base_Atualizada <- "Metadado indisponivel"
    }

    files_sem_metadado <- unique(files_sem_metadado[c('nome_arquivo','fonte','tipo','uf','sequencia_datas','Base_Atualizada','status_download')])

    files <- rbind(atualizada, files_atualizar, files_sem_dbc, files_sem_metadado)

    # Fazendo a atualizacao:
    idx <- seq_len(nrow(files))

    for (i in idx) {
      status_atual <- files$Base_Atualizada[i]
      if (status_atual %in% c("desatualizado", "DBC Nao encontrado")) {
        tryCatch({
          # BUGFIX (#3): antes o retorno do download nao era conferido --
          # a linha "baixado" rodava mesmo que o download tivesse falhado
          # internamente (dtsus_download engole erro por arquivo e so
          # reporta via status_download). Agora capturamos o resultado e
          # so marcamos sucesso se TODOS os arquivos daquela chamada
          # realmente baixaram.
          res <- dtsus_download(
            fonte = files$fonte[i],
            tipo = files$tipo[i],
            uf = files$uf[i],
            Data_inicio = files$sequencia_datas[i],
            open = FALSE,
            save.dbc = TRUE,
            pasta.dbc = pasta.dbc,
            return_files = TRUE
          )

          sucesso <- length(res$files$status_download) > 0 &&
            all(res$files$status_download == "Download realizado")


          files$Base_Atualizada[i] <- if (sucesso) "baixado" else "Falha no download"
          files$status_download[i] <- if (sucesso) "Download realizado" else "Erro no download"


        }, error = function(e) {
          files$Base_Atualizada[i] <<- paste0("Falha no download: ", conditionMessage(e))
          files$status_download[i]  <<- "Erro no download"

          warning(sprintf("Falha ao baixar %s: %s", files$nome_arquivo[i], conditionMessage(e)), call. = FALSE)
        })
      }
    }

  }
  return(files)
}
