# dtsus

Um jeito rápido e controlado de acessar os dados do DATASUS em R.

O **dtsus** foi desenvolvido para simplificar o acesso aos microdados públicos disponibilizados pelo DATASUS. O pacote permite baixar, salvar e ler arquivos disponibilizados pelo DATASUS, com aplicação prévia de filtros e seleção de colunas, tornando o processo mais eficiente e reduzindo o processamento desnecessário.

Além disso, o **dtsus** mantém explícito o que foi efetivamente realizado na extração e preparação dos dados, favorecendo fluxos de trabalho reprodutíveis e com total controle sobre as etapas executadas.

---

## Instalação

O pacote **dtsus** depende do pacote `read.dbc`, que não está disponível no CRAN.

Primeiro, instale a dependência:

```r
install.packages("remotes")

# Instalar read.dbc
remotes::install_github("danicat/read.dbc")
```

Em seguida, instale o **dtsus**:

```r
# Instalar dtsus
remotes::install_github("LucasRFontes/dtsus")
```

---

## Exemplo

### 📌 1. Leitos cadastrados no CNES (RJ – Janeiro/2023)

Download da base contendo os leitos cadastrados no CNES, referente a janeiro de 2023, para o estado do Rio de Janeiro:

```r
library(dtsus)

CNES <- dtsus_download(
  fonte = "CNES",
  tipo = "LT",
  uf = "RJ",
  Data_inicio = 202301
)

files <- CNES$files  # arquivos baixados
dados <- CNES$data   # base de dados carregada
```

---

### 📌 2. Internações Hospitalares (PA – Nov/2024 a Fev/2025)

Download dos dados de Internação Hospitalar do Pará, de novembro de 2024 a fevereiro de 2025, selecionando apenas as colunas de CNES, Diagnóstico Principal e Município de Residência:

```r
library(dtsus)

SIH <- dtsus_download(
  fonte = "SIH",
  tipo = "RD",
  Data_inicio = 202411,
  Data_fim = 202502,
  uf = "PA",
  colunas = c("CNES", "DIAG_PRINC", "MUNIC_RES")
)

files <- SIH$files  # arquivos baixados


### 📌 3. Informações sobre Mortalidade (MG – 2020)

Download dos dados do Sistema de Informações sobre Mortalidade (SIM) de Minas Gerais, ano de 2020.

Neste exemplo:

- O download é realizado, mas os dados **não são carregados no R** (`open = FALSE`)
- Os arquivos são salvos no formato original `.dbc` (`save.dbc = TRUE`)
- É possível definir o diretório onde os arquivos serão armazenados (`pasta.dbc = "caminho/da/pasta"`)

```r
library(dtsus)

SIM <- dtsus_download(
  fonte = "SIM",
  tipo = "DO",
  uf = "MG",
  Data_inicio = 2020,
  open = FALSE,
  save.dbc = TRUE
)

files <- SIM$files  # arquivos baixados
```
