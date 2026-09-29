# Accessing and Processing DATASUS Microdata

Downloads public health datasets from the DATASUS FTP server
(ftp.datasus.gov.br), supports data preprocessing and filtering,
optionally saves the DBC files, and returns the downloaded data as a
data.frame.

## Usage

``` r
dtsus_download(
  fonte = NA,
  tipo = NA,
  uf = NA,
  Data_inicio = NA,
  Data_fim = NULL,
  open = TRUE,
  filtro = NULL,
  colunas = NULL,
  save.dbc = FALSE,
  save.json = TRUE,
  pasta.dbc = NULL,
  return_files = T
)
```

## Arguments

- fonte:

  Character. The abbreviation of the health information system to be
  accessed, e.g. "CNES", "SIH", "SIA".

- tipo:

  Character. The abbreviation of the file type to be accessed, e.g.
  "LT", "RD".

- uf:

  Character. A UF code or a vector of UF codes (e.g. "MG", "SP", "BR").

- Data_inicio:

  Numeric or character. Start date in the format yyyymm (monthly) or
  yyyy (annual), depending on the selected dataset.

- Data_fim:

  Numeric or character. End date in the format yyyymm (monthly) or yyyy
  (annual), depending on the selected dataset.

- open:

  Logical. If TRUE, the downloaded files are read and returned as data.

- filtro:

  List. Optional filter specification with two fields:
  `list(coluna = "COL", valor = c("X","Y"))`.

- colunas:

  Character. Optional vector of columns to keep.

- save.dbc:

  Logical. If TRUE, saves the downloaded DBC files locally.

- save.json:

  Logical. If TRUE, saves a JSON metadata file alongside the DBC file.
  Requires `save.dbc = TRUE`. Defaults to TRUE.

- pasta.dbc:

  Character. Path to the output directory where the DBC files will be
  saved. Defaults to the current working directory if not provided.

- return_files:

  Logical. If TRUE, returns a list with both the file index and the
  downloaded data. If FALSE, returns only the downloaded data.

## Value

If `return_files = TRUE`, returns a list with:

- files:

  A data.frame containing the indexed files, download links and status
  information.

- data:

  A data.frame with the downloaded microdata (only if `open = TRUE`).

If `return_files = FALSE`, returns only the `data` object.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- dtsus_download(
  fonte = "CNES",
  tipo = "LT",
  uf = "MG",
  Data_inicio = 201801,
  Data_fim = 201803,
  open = TRUE,
  save.dbc = FALSE,
  return_files = TRUE
)

head(res$data)
} # }
```
