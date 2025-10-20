library(httr)
library(rvest)
library(purrr)

# URL da página de microdados
url_base <- "https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior"

# ler a página
pg <- read_html(url_base)

# extrair os links de download — no html os links são tipo download.inep.gov.br
links <- pg %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  # filtrar apenas os que parecem ser “Microdados do Censo da Educação Superior ano …”
  keep(~ grepl("\\.zip$", .))
  #keep(~ grepl("download\\.inep\\.gov\\.br.*Microdados.*Censo.*Superior.*\\.zip$", .))


# opcional: mostrar os links encontrados
print(links)

# criar pasta para salvar
dir.create("microdados_censo_superior", showWarnings = FALSE)

# função para fazer download
baixar <- function(link) {
  fname <- basename(link)
  dest <- file.path("microdados_censo_superior", fname)
  if (!file.exists(dest)) {
    message("Baixando: ", fname)
    RETRY("GET", link, write_disk(dest, overwrite = TRUE), times = 5, pause_base = 5)
  } else {
    message("Arquivo já existe: ", fname)
  }
}

# aplicar para todos os links
walk(links, baixar)

message("Downloads completos!")

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

library(fs)

pasta_zip <- "microdados_censo_superior"
arquivos_zip <- list.files(pasta_zip, pattern = "\\.zip$", full.names = TRUE)

for (arq in arquivos_zip) {
  destino <- file.path(pasta_zip)
  dir.create(destino, showWarnings = FALSE)
  
  message("Extraindo via 7-Zip: ", basename(arq))
  system2("7z", args = c("x", shQuote(arq), paste0("-o", shQuote(pasta_zip)), "-y"))
}



