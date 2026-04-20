
dts_filter_Df <- function(filtro, df) {

  # Sem filtro = retorna sem warning
  if (is.null(filtro)) return(df)

  # Estrutura minima
  if (!is.list(filtro) || !all(c("coluna", "valor") %in% names(filtro))) {
    warning("[AVISO] Filtro invalido. Esperado uma lista com 'coluna' e 'valor'. Filtro nao aplicado.", call. = FALSE)
    return(df)
  }

  # Normaliza
  filtro$coluna <- trimws(as.character(filtro$coluna))

  # Coluna vazia
  if (is.null(filtro$coluna) || is.na(filtro$coluna) || filtro$coluna == "") {
    warning("[AVISO] Filtro invalido. Campo 'coluna' nao preenchido. Filtro nao aplicado.", call. = FALSE)
    return(df)
  }

  # Coluna nao existe
  if (!filtro$coluna %in% names(df)) {
    warning("[AVISO] Filtro invalido. Coluna selecionada nao encontrada na base. Filtro nao aplicado.", call. = FALSE)
    return(df)
  }

  # Valor vazio
  if (is.null(filtro$valor) || length(filtro$valor) == 0) {
    warning("[AVISO] Filtro invalido. Nenhum valor informado. Filtro nao aplicado.", call. = FALSE)
    return(df)
  }

  # Aplicando filtro
  df_out <- df[as.character(df[[filtro$coluna]]) %in% as.character(filtro$valor), , drop = FALSE]

  # Se filtrou e zerou
  if (nrow(df_out) == 0) {
    warning("[AVISO] O filtro foi aplicado, mas nenhum registro foi encontrado.", call. = FALSE)
  }

  return(df_out)
}



# selecionando apenas as colunas necessarias
dts_select_col <- function(colunas, df) {

  if(is.null(colunas)){
    return(df)}
  faltantes <- setdiff(colunas, names(df))
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[AVISO] Coluna(s) nao encontrada(s) e ignorada(s): %s",
      paste(faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  cols_ok <- intersect(colunas, names(df))

  if (length(cols_ok) == 0) {
    warning("[AVISO] Nenhuma coluna valida encontrada.", call. = FALSE)
    return(df)
  }

  if(length(cols_ok) > 0){
    df[, cols_ok, drop = FALSE]
  }else{df}
}
