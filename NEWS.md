# dtsus 0.2.0

* `dtsus_refresh()`: corrige inconsistência de retorno — agora sempre retorna
  um `data.frame` (antes retornava `list(files = ...)` quando `apenas_verificar = FALSE`).
* `dtsus_refresh()`: adiciona a coluna `status_download` ao resultado.
* Corrige erro de `rbind()` que ocorria quando não havia arquivos ausentes
  ou sem metadado (colunas inconsistentes entre os blocos combinados).
* Corrige uso incorreto de `<<-` que impedia a atualização do status de
  arquivos baixados com sucesso.
* Corrige nome de argumento incorreto na chamada de `dts_salvar_metadados()`
  durante a reconstrução de metadados a partir de arquivos locais.
* Remove `dtsus_get()` (função incompleta; será reintroduzida em versão futura).

# dtsus 0.1.1

* Versão inicial publicada.
