# Updates or checks the status of local DBC files

This function evaluates whether the DBC files saved on the computer are
up to date relative to the data available on the DATASUS server. It can
either only check the status (`apenas_verificar = TRUE`) or
automatically download outdated or missing files.

## Usage

``` r
dtsus_refresh(
  fonte = NA,
  tipo = NA,
  uf = NA,
  Data_inicio = NA,
  Data_fim = NULL,
  pasta.dbc = NULL,
  apenas_verificar = FALSE
)
```

## Arguments

- fonte:

  Character. Fonte dos dados (ex: "SIH", "SIM", "SINAN").

- tipo:

  Character. Tipo do dado (ex: "RD", "DO", etc.).

- uf:

  Character. Unidade da Federação (ex: "SP", "RJ", "BR" para Brasil).

- Data_inicio:

  Numeric ou character. Start date in the format yyyymm (monthly) or
  yyyy (annual), depending on the selected dataset. This parameter is
  required.

- Data_fim:

  Numeric ou character. End date in the same format as `Data_inicio`.
  Default is `NULL` (searches for the start date only).

- pasta.dbc:

  Character. Path to the folder where the .DBC files are saved. If
  `NULL`, the function will attempt to validate or request the path.

- apenas_verificar:

  Logical. If `TRUE`, the function only checks and returns the status of
  each file without downloading. If `FALSE` (default), it downloads
  outdated or missing files.

## Value

A `data.frame` with the columns `nome_arquivo`, `fonte`, `tipo`, `uf`,
`sequencia_datas`, `Base_Atualizada` (final status of each file), and
`status_download` (detail of the download/reconstruction action
performed, or `NA` when no action was necessary). The return format is
the same regardless of the value of `apenas_verificar`.

## Details

The function performs the following steps:

1.  Validates the destination folder path.

2.  Checks the internet connection.

3.  Lists expected files based on the provided parameters.

4.  Checks for the existence of DBC files and cached metadata.

5.  For cached files, evaluates whether an update is available on the
    server.

6.  If `apenas_verificar = FALSE`, downloads missing or outdated files,
    recording the status of each operation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Only check the status of SIH files for SP in Jan/2025
resultado <- dtsus_refresh(
  fonte = "SIH",
  tipo = "RD",
  uf = "SP",
  Data_inicio = 202501,
  apenas_verificar = TRUE
)
print(resultado)

# Download outdated files
dtsus_refresh(
  fonte = "SIM",
  tipo = "DO",
  uf = "BR",
  Data_inicio = 2024,
  pasta.dbc = "caminho/para/sua/pasta"
)
} # }
```
