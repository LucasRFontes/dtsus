# Changelog

## dtsus 0.3.0

### Novas funcionalidades

- Adiciona
  [`dtsus_pop_ans()`](https://lucasrfontes.github.io/dtsus/reference/dtsus_pop_ans.md),
  para download e leitura dos dados consolidados de beneficiários de
  planos de saúde disponibilizados pela Agência Nacional de Saúde
  Suplementar (ANS).

### Melhorias

- Adiciona validação interna de conexão com a internet por meio de
  `dts_validate_internet()`.
- Amplia o escopo do pacote para fontes complementares utilizadas em
  análises do Sistema Único de Saúde.
- Atualiza documentação e exemplos no README.

## dtsus 0.2.0

- [`dtsus_refresh()`](https://lucasrfontes.github.io/dtsus/reference/dtsus_refresh.md):
  corrige inconsistência de retorno — agora sempre retorna um
  `data.frame` (antes retornava `list(files = ...)` quando
  `apenas_verificar = FALSE`).
- [`dtsus_refresh()`](https://lucasrfontes.github.io/dtsus/reference/dtsus_refresh.md):
  adiciona a coluna `status_download` ao resultado.
- Corrige erro de [`rbind()`](https://rdrr.io/r/base/cbind.html) que
  ocorria quando não havia arquivos ausentes ou sem metadado (colunas
  inconsistentes entre os blocos combinados).
- Corrige uso incorreto de `<<-` que impedia a atualização do status de
  arquivos baixados com sucesso.
- Corrige nome de argumento incorreto na chamada de
  `dts_salvar_metadados()` durante a reconstrução de metadados a partir
  de arquivos locais.
- Remove `dtsus_get()` (função incompleta; será reintroduzida em versão
  futura).

## dtsus 0.1.1

- Versão inicial publicada.
