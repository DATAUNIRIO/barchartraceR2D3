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
#install.packages("zip")

library(zip)

# Função com zip::unzip
extrair_zip_latin1_v2 <- function(arquivo_zip, pasta_destino = 'C:/Users/08451589707/Documents/mcs/') {
  # Criar pasta de destino
  if (!dir.exists(pasta_destino)) {
    dir.create(pasta_destino)
  }
  
  # Extrair com zip::unzip (geralmente lida melhor com encoding)
  zip::unzip(arquivo_zip, exdir = pasta_destino)
  
  return(pasta_destino)
}

# Extrair todos os arquivos
arquivos_zip <- list.files(path='C:/Users/08451589707/Documents/microdados_censo_superior/' , pattern = "\\.zip$", full.names = TRUE)
lapply(arquivos_zip, extrair_zip_latin1_v2)

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# Correcao de nome de pasta

# Correção direta para o padrão que você mencionou
corrigir_nomes_educacao <- function(caminho = 'C:/Users/08451589707/Documents/mcs/') {
  pastas <- list.dirs(caminho, full.names = FALSE, recursive = FALSE)
  
  for (pasta in pastas) {
    # Padrão específico para "Educa��o"
    if (grepl("Educa..o", pasta)) {
      nome_corrigido <- gsub("Microdados do Censo da Educa..o", "Educ", pasta)
      
      # Tentar correção completa com iconv
      nome_corrigido <- tryCatch({
        iconv(nome_corrigido, from = "latin1", to = "UTF-8")
      }, error = function(e) {
        nome_corrigido
      })
      
      caminho_original <- file.path(caminho, pasta)
      caminho_novo <- file.path(caminho, nome_corrigido)
      
      if (!file.exists(caminho_novo)) {
        file.rename(caminho_original, caminho_novo)
        cat(sprintf("Corrigido: '%s' -> '%s'\n", pasta, nome_corrigido))
      }
    }
  }
}

corrigir_nomes_educacao()

