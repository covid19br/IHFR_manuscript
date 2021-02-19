#' Função para ler base da sivep
#'
#' @param dir Caractere. Caminho relativo para o diretório onde está o dado
#' @param escala Caractere. Escala de análise aceita: `"pais"`, `"estado"`, `"municipio"`
#' @param geocode Caractere. Geocode IBGE do estado ou município. Município pde ter 6 ou 7 dígitos
#' @param data Caractere. Data no formato  "%Y_%m_%d". Quando NULL (padrão) pega a data mais recente
#' @param residentes Filtar por residentes ou por local da notificação
#' @param sigla sigla UF
#' @param ... Qualquer parâmetro de `read.csv()`
#' @export
#' @importFrom utils unzip count.fields
#' @importFrom lubridate parse_date_time as_date

read.sivep <- function(dir, # diretorio onde esta o dado
                       escala,
                       sigla,
                       geocode,
                       data,  #formato com _
                       residentes = TRUE,
                       ...) {
  # lendo os dados
  file.name <- list.files(dir, pattern = paste0(".*", data, ".(csv|zip)"), full.names = TRUE)
  # múltiplos matches são possíveis
  file.name <- file.name[1]
  # detecta e lida com arquivo zip
  if (endsWith(file.name, '.zip')) {
    is_zip = TRUE
    file.name <- unzip(file.name, basename(gsub('.zip$', '.csv', file.name)))
  } else{
    is_zip = FALSE
  }
  # detecta separador
  linha1 <- readLines(file.name, n = 2)[2]
  if (count.fields(textConnection(linha1), sep = ',') >
      count.fields(textConnection(linha1), sep = ';')) {
    sep <- ','
  } else
    sep <- ';'
  #dados <- read_delim(file = file.name, delim = sep, ...)
  dados <- readr::read_delim(file = file.name,
                             delim = sep,
                             escape_double = FALSE,
                             trim_ws = TRUE,
                             n_max = 1)
  nomes <- names(dados)
  cols <- rep("c", length(nomes))
  names(cols) <- nomes
  cols.list <- as.list(cols)
  dados <- readr::read_delim(file = file.name,
                             delim = sep,
                             col_types = cols.list,
                             escape_double = FALSE,
                             trim_ws = TRUE)
  dados <- data.frame(dados)
  # conveniencia mudando para minusculas
  names(dados) <- tolower(names(dados))
  names(dados) <- gsub(" ", "_", names(dados))
  names(dados) <- gsub("\\.", "_", names(dados))

  if (is_zip)
    file.remove(file.name)

  # filtro por estados ou municipio
  if (escala != "pais") {
    df <- geocode_ibge
    #geocode <- as.numeric(geocode)
    municipio.code <- sapply(df$id, function(x) substr(x, start = 1, stop = 6))
    micro.code   <- df$microrregiao.id
    meso.code    <- df$microrregiao.mesorregiao.id
    estado.code  <- df$microrregiao.mesorregiao.UF.id
    estado.sigla <- df$microrregiao.mesorregiao.UF.sigla
    n.geo <- nchar(geocode)
    n.geocodes <- c(estado = 2, meso = 4, micro = 5, municipio = 7)
    if (escala == "municipio" & n.geo == 7) {
      geocode <- substr(geocode, start = 1, stop = 6)
      n.geo <- nchar(geocode)
    }
    if (escala == "estado") {
      estados <- c('12','27','13','16','29','23','53','32','52','21','31','50','51','15','25','26','22','41','33','24','11','14','43','42','28','35','17')
      names(estados) <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
      if (!is.null(sigla) & is.null(geocode)) geocode <-  estados[names(estados) == sigla]#o geocode nao importa tanto para estados porque tem sg_uf
      if (is.null(sigla) & !is.null(geocode)) sigla <- names(estados[estados == geocode])
      dados <- dados[dados$sg_uf == sigla, ]
    }
    if (escala == "meso") {
      co.muns <- municipio.code[meso.code == geocode] #ast: aqui pode acontecer a mesma coisa com NAs que PIK apontou?
      if (residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
    if (escala == "micro") {
      co.muns <- municipio.code[micro.code == geocode]
      if (residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
    if (escala == "municipio") {
      if (residentes)
        dados <- dados[which(dados$co_mun_res == as.numeric(geocode)), ]
      else
        dados <- dados[which(dados$co_mun_not == as.numeric(geocode)), ]
    }
    if (escala == "drs") {
      drs <- read.csv(paste0('./dados/DRS_', sigla, '.csv'))
      co.muns <- drs[drs$DRS == geocode, 'id']
      if (residentes)
        dados <- dados[dados$co_mun_res %in% co.muns, ]
      else
        dados <- dados[dados$co_mun_not %in% co.muns, ]
    }
  }
  # formata datas
  # muda nome de colunas e formata datas
  dt.cols <- names(dados)[grepl("dt_", names(dados))]
  ## usa lubridate
  dados[, dt.cols] <- lapply(dados[, dt.cols],
                             function(x)
                               as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs"))))
  return(dados)
}
