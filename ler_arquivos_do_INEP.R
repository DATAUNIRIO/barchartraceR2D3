#f<-function(x,pos) subset(x, CO_IES=="693")
#INEP_2024 <-readr::read_delim_chunked(local_dados_2024,DataFrameCallback$new(f), delim = ";",locale = locale("pt"), chunk_size = 5000)
#Encoding(INEP_2024$NO_CURSO)<- "UTF-8"
#Encoding(INEP_2024$NO_CURSO) <- "latin1"


list.files("mcs",recursive = TRUE,pattern = "\\.CSV$")

library(readr)
library(dplyr)

anos = 2009:2024
# Criar lista para armazenar resultados
dados_inep <- list()
# Função de processamento (ajuste conforme necessário)
f <- function(x, pos) {
  # Exemplo de processamento básico
  x %>%
    mutate(across(where(is.character), ~iconv(., from = "latin1", to = "UTF-8")))
}


# Processar cada ano
for (ano in anos) {
  cat("Processando ano:", ano, "\n")
  
  # Construir caminho do arquivo (ajuste o padrão conforme seus arquivos)
  local_dados = paste0("mcs/Educ Superior ",ano, "/dados/MICRODADOS_CADASTRO_CURSOS_", ano, ".CSV")

  # Verificar se arquivo existe
  if (file.exists(local_dados)) {
    dados_inep[[as.character(ano)]] <- readr::read_delim_chunked(
      local_dados,
      DataFrameCallback$new(f),
      delim = ";",
      locale = locale("pt", encoding = "latin1"),
      chunk_size = 5000
    )
    cat("✅ Ano", ano, "processado com sucesso\n")
  } else {
    cat("❌ Arquivo não encontrado para ano", ano, "\n")
  }
}

#saveRDS(dados_inep,file = 'dados_inep_2009_2024.Rds')

dados_inep = readRDS('dados_inep_2009_2024.Rds')

setdiff(names(dados_inep[["2024"]]),names(dados_inep[["2023"]]))
setdiff(names(dados_inep[["2023"]]),names(dados_inep[["2024"]]))
setdiff(names(dados_inep[["2023"]]),names(dados_inep[["2021"]]))

dados_inep[["2023"]] = dados_inep[["2023"]] %>% select(-IN_COMUNITARIA,-IN_CONFESSIONAL)
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-IN_COMUNITARIA,-IN_CONFESSIONAL,-QT_ING_RVPPI,-QT_ING_RVQUILO)
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_ING_RVREFU,-QT_ING_RVPOVT,-QT_ING_RVIDOSO,-QT_ING_RVINTERN) 
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_ING_RVMEDAL,-QT_ING_RVTRANS,-QT_MAT_RVPPI,-QT_MAT_RVQUILO)
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_MAT_RVREFU,-QT_MAT_RVPOVT,-QT_MAT_RVIDOSO,-QT_MAT_RVINTERN) 
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_MAT_RVMEDAL,-QT_MAT_RVTRANS,-QT_CONC_RVPPI,-QT_CONC_RVQUILO) 
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_CONC_RVREFU,-QT_CONC_RVPOVT,-QT_CONC_RVIDOSO,-QT_CONC_RVINTERN)
dados_inep[["2024"]] = dados_inep[["2024"]] %>% select(-QT_CONC_RVMEDAL,-QT_CONC_RVTRANS)

#----------------------------------------------------------------------------------------

library(purrr)
# Remover variáveis específicas
dados_inep <- map(dados_inep, ~ select(.x, -any_of(c("QT_INSCRITO_TOTAL_DIURNO",
  "QT_INSCRITO_TOTAL_NOTURNO",  "QT_INSCRITO_TOTAL_EAD",  "QT_INSC_VG_NOVA",
  "QT_INSC_PROC_SELETIVO",  "QT_INSC_VG_REMANESC",  "QT_INSC_VG_PROG_ESPECIAL",
  "QT_ING",  "QT_ING_FEM",  "QT_ING_MASC",  "QT_ING_DIURNO",  "QT_ING_NOTURNO",
  "QT_ING_VG_NOVA",  "QT_ING_VESTIBULAR",  "QT_ING_ENEM",  "QT_ING_AVALIACAO_SERIADA",
  "QT_ING_SELECAO_SIMPLIFICA",  "QT_ING_EGR",  "QT_ING_OUTRO_TIPO_SELECAO",
  "QT_ING_PROC_SELETIVO",  "QT_ING_VG_REMANESC",  "QT_ING_VG_PROG_ESPECIAL",
  "QT_ING_OUTRA_FORMA",  "QT_ING_0_17",  "QT_ING_18_24",  "QT_ING_25_29",
  "QT_ING_30_34",  "QT_ING_35_39",  "QT_ING_40_49",  "QT_ING_50_59",  "QT_ING_60_MAIS",
  "QT_ING_BRANCA",  "QT_ING_PRETA",  "QT_ING_PARDA",  "QT_ING_AMARELA",  "QT_ING_INDIGENA",
  "QT_ING_CORND"))))


dados_inep <- map(dados_inep, ~ select(.x, -any_of(c("QT_VG_TOTAL_DIURNO",
  "QT_VG_TOTAL_NOTURNO",  "QT_VG_TOTAL_EAD",  "QT_VG_NOVA",  "QT_VG_PROC_SELETIVO",
  "QT_VG_REMANESC",  "QT_VG_PROG_ESPECIAL"))))

dados_inep <- map(dados_inep, ~ select(.x, -any_of(c("QT_MAT_FEM","QT_MAT_MASC",
  "QT_MAT_DIURNO","QT_MAT_NOTURNO", "QT_MAT_0_17","QT_MAT_18_24","QT_MAT_25_29",
  "QT_MAT_30_34","QT_MAT_35_39","QT_MAT_40_49","QT_MAT_50_59","QT_MAT_60_MAIS",
  "QT_MAT_BRANCA","QT_MAT_PRETA","QT_MAT_PARDA","QT_MAT_AMARELA", "QT_MAT_INDIGENA",
  "QT_MAT_CORND"))))

dados_inep <- map(dados_inep, ~ select(.x, -any_of(c("QT_CONC_FEM","QT_CONC_MASC",
  "QT_CONC_DIURNO","QT_CONC_NOTURNO","QT_CONC_0_17","QT_CONC_18_24","QT_CONC_25_29",
  "QT_CONC_30_34","QT_CONC_35_39","QT_CONC_40_49","QT_CONC_50_59","QT_CONC_60_MAIS",
  "QT_CONC_BRANCA", "QT_CONC_PRETA","QT_CONC_PARDA","QT_CONC_AMARELA","QT_CONC_INDIGENA",
  "QT_CONC_CORND","QT_ING_NACBRAS", "QT_ING_NACESTRANG","QT_MAT_NACBRAS", "QT_MAT_NACESTRANG",
  "QT_CONC_NACBRAS","QT_CONC_NACESTRANG", "QT_ALUNO_DEFICIENTE","QT_ING_DEFICIENTE",
  "QT_MAT_DEFICIENTE","QT_CONC_DEFICIENTE", "QT_ING_FINANC","QT_ING_FINANC_REEMB",
  "QT_ING_FIES","QT_ING_RPFIES","QT_ING_FINANC_REEMB_OUTROS", "QT_ING_FINANC_NREEMB",
  "QT_ING_PROUNII", "QT_ING_PROUNIP", "QT_ING_NRPFIES", "QT_ING_FINANC_NREEMB_OUTROS",
  "QT_MAT_FINANC","QT_MAT_FINANC_REEMB","QT_MAT_FIES","QT_MAT_RPFIES","QT_MAT_FINANC_REEMB_OUTROS",
  "QT_MAT_FINANC_NREEMB", "QT_MAT_PROUNII", "QT_MAT_PROUNIP", "QT_MAT_NRPFIES", "QT_MAT_FINANC_NREEMB_OUTROS",
  "QT_CONC_FINANC", "QT_CONC_FINANC_REEMB", "QT_CONC_FIES", "QT_CONC_RPFIES",
  "QT_CONC_FINANC_REEMB_OUTROS","QT_CONC_FINANC_NREEMB","QT_CONC_PROUNII","QT_CONC_PROUNIP",
  "QT_CONC_NRPFIES","QT_CONC_FINANC_NREEMB_OUTROS"))))

dados_inep <- map(dados_inep, ~ select(.x, -any_of(c("QT_ING_PROCESCPUBLICA",
  "QT_ING_PROCESCPRIVADA","QT_ING_PROCNAOINFORMADA","QT_MAT_PROCESCPUBLICA",
  "QT_MAT_PROCESCPRIVADA","QT_MAT_PROCNAOINFORMADA","QT_CONC_PROCESCPUBLICA",
  "QT_CONC_PROCESCPRIVADA","QT_CONC_PROCNAOINFORMADA","QT_PARFOR",
  "QT_ING_PARFOR","QT_MAT_PARFOR","QT_CONC_PARFOR", "QT_APOIO_SOCIAL","QT_ING_APOIO_SOCIAL",
  "QT_MAT_APOIO_SOCIAL","QT_CONC_APOIO_SOCIAL", "QT_ATIV_EXTRACURRICULAR","QT_ING_ATIV_EXTRACURRICULAR",
  "QT_MAT_ATIV_EXTRACURRICULAR","QT_CONC_ATIV_EXTRACURRICULAR", "QT_MOB_ACADEMICA",
  "QT_ING_MOB_ACADEMICA", "QT_MAT_MOB_ACADEMICA", "QT_CONC_MOB_ACADEMICA"))))

dados_inep <- map(dados_inep, ~ {
  .x %>%mutate(across(any_of(c("CO_CINE_AREA_GERAL","CO_CINE_AREA_ESPECIFICA","CO_CINE_AREA_DETALHADA")), as.character))
})
dados_inep = bind_rows(dados_inep)

saveRDS(dados_inep,file = 'dados_inep_2009_2024_reduzido.Rds')
