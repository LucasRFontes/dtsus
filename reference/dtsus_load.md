# Loads and Processes DATASUS Microdata

Reads previously downloaded DATASUS DBC files from a local directory,
applies optional filtering and column selection, and returns the data as
a data.frame.

## Usage

``` r
dtsus_load(
  fonte = NA,
  tipo = NA,
  uf = NA,
  Data_inicio = NA,
  Data_fim = NULL,
  pasta.dbc = NULL,
  filtro = NULL,
  colunas = NULL,
  return_files = TRUE,
  verbose = FALSE
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

- pasta.dbc:

  Character. Path to the directory where the DBC files are stored.
  Defaults to the current working directory if not provided.

- filtro:

  List. Optional filter specification with two fields:
  `list(coluna = "COL", valor = c("X","Y"))`.

- colunas:

  Character. Optional vector of columns to keep.

- return_files:

  Logical. If TRUE, returns a list with file metadata and the loaded
  data. If FALSE, returns only the data.frame.

- verbose:

  Logical. If TRUE, prints progress messages during file reading.

## Value

If `return_files = TRUE`, returns a list with:

- files:

  A data.frame containing file names, paths, and load status.

- data:

  A data.frame with the loaded microdata (or NULL if none loaded).

If `return_files = FALSE`, returns only the data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- dtsus_load(
  fonte = "CNES",
  tipo = "LT",
  uf = "MG",
  Data_inicio = 201801,
  Data_fim = 201803,
  verbose = TRUE
)

head(res$data)
} # }
```
