#' Atualiza ou verifica a situação dos arquivos DBC locais
#'
#' Esta função avalia se os arquivos DBC salvos no computador estão atualizados
#' em relação aos dados disponíveis no servidor do DATASUS. Ela pode apenas
#' verificar o status (\code{apenas_verificar = TRUE}) ou realizar o download
#' automático dos arquivos desatualizados ou inexistentes.
#'
#' @param fonte Character. Fonte dos dados (ex: "SIH", "SIM", "SINAN").
#' @param tipo Character. Tipo do dado (ex: "RD", "DO", etc.).
#' @param uf Character. Unidade da Federação (ex: "SP", "RJ", "BR" para Brasil).
#' @param Data_inicio Numeric ou character. Data inicial no formato \code{AAAAMM}
#'     (bases mensais) ou \code{AAAA} (bases anuais), conforme a periodicidade da
#'     fonte selecionada. Este parâmetro é obrigatório.
#' @param Data_fim Numeric ou character. Data final no mesmo formato de
#'     \code{Data_inicio}. Padrão é \code{NULL} (busca apenas a data de início).
#' @param pasta.dbc Character. Caminho para a pasta onde os arquivos .DBC estão
#'     salvos. Se \code{NULL}, a função tentará validar ou solicitar o caminho.
#' @param apenas_verificar Logical. Se \code{TRUE}, a função apenas verifica e
#'     retorna o status de cada arquivo, sem realizar download. Se \code{FALSE}
#'     (padrão), baixa os arquivos desatualizados ou faltantes.
#'
#' @return Um \code{data.frame} com as colunas \code{nome_arquivo}, \code{fonte},
#'   \code{tipo}, \code{uf}, \code{sequencia_datas}, \code{Base_Atualizada}
#'   (status final de cada arquivo) e \code{status_download} (detalhe da acao
#'   de download/reconstrucao realizada, ou \code{NA} quando nenhuma acao foi
#'   necessaria). O formato de retorno e o mesmo independente do valor de
#'   \code{apenas_verificar}.
#'
#' @details
#' A função realiza as seguintes etapas:
#' \enumerate{
#'   \item Valida o caminho da pasta de destino.
#'   \item Verifica a conexão com a internet.
#'   \item Lista os arquivos esperados com base nos parâmetros fornecidos.
#'   \item Verifica a existência dos arquivos DBC e dos metadados em cache.
#'   \item Para arquivos com cache, avalia se há atualização disponível no servidor.
#'   \item Se \code{apenas_verificar = FALSE}, baixa os arquivos que estão
#'         desatualizados ou ausentes, registrando o status de cada operação.
#' }
#'
#' @examples
#' \dontrun{
#' # Apenas verificar o status dos arquivos de SIH para SP em Jan/2025
#' resultado <- dtsus_refresh(
#'   fonte = "SIH",
#'   tipo = "RD",
#'   uf = "SP",
#'   Data_inicio = 202501,
#'   apenas_verificar = TRUE
#' )
#' print(resultado)
#'
#' # Baixar arquivos desatualizados
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
