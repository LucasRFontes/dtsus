# Populacao beneficiaria de planos de saude da ANS

Baixa, extrai e le a base de informacaes consolidadas de beneficiarios
de planos de saude disponibilizada pela Agencia Nacional de Saude
Suplementar (ANS).

## Usage

``` r
dtsus_pop_ans(ano_mes, uf, quiet = FALSE)
```

## Arguments

- ano_mes:

  Competencia no formato `"AAAAMM"`. Exemplo: `"202403"` para marco de
  2024.

- uf:

  Sigla da Unidade Federativa. Exemplo: `"MG"`, `"SP"`, `"BA"`.

- quiet:

  Logico. Se `TRUE`, oculta as mensagens e a barra de progresso do
  download. O padrao e `FALSE`.

## Value

Um `data.frame` contendo os dados de beneficiarios da ANS para a UF e
competencia informadas.

## Examples

``` r
if (FALSE) { # \dontrun{
ans <- dtsus_pop_ans("202403", "MG", quiet = FALSE)
} # }
```
