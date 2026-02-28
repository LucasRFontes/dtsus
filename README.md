
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtsus

<!-- badges: start -->
<!-- badges: end -->

Um jeito rápido e controlado de acessar os dados do DATASUS em R.

O **dtsus** foi desenvolvido para simplificar o acesso aos microdados públicos disponibilizados pelo DATASUS. O pacote permite baixar, salvar e ler arquivos disponibilizados pelo DATASUS, com aplicação prévia de filtros e seleção de colunas, tornando o processo mais eficiente e reduzindo o processamento desnecessário.

Além disso, o dtsus mantém explícito o que foi efetivamente realizado na extração e preparação dos dados, favorecendo fluxos de trabalho reprodutíveis e com total controle sobre as etapas executadas.

## Instalação

O pacote `dtsus` depende do pacote `read.dbc`, que não está disponível no CRAN.
Primeiro instale o read.dbc
```r
install.packages("remotes")

# Instalar dependência
remotes::install_github("danicat/read.dbc")
```
Depois instale o dtsus

```r
# Instalar dtsus
remotes::install_github("LucasRFontes/dtsus")
```

## Example

This is a basic example which shows you how to solve a common problem:


#library(dtsus)
## basic example code
```

