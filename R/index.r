#' Reporta inconsistência em um registro.
#'
#' @param record_id ID do registro.
#' @param description Descrição da inconsistência encontrada.
#' @examples
#' report_error("v0x0f01f-ex4a-4a75-a2e2-a99b6f161gh1", "Variável IDADE com valor 456")
report_error <- function(record_id, description) {
  endpoint <- "https://api.fulcrumpl.us/api/inconsistencies"

  req_body <- list(inconsistency = list(record_id = record_id, description = description))

  request <- httr::POST(url = endpoint, body = req_body, encode = c("json"))

  txt <- httr::content(request, as = "text")
  parsed_data <- jsonlite::fromJSON(txt)

  if (request$status_code == 200 || request$status_code == 201) {
    cat("Erro reportado com sucesso.")
  } else {
    cat("Falha: ", parsed_data$error$message)
  }
}
