% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/index.r
\name{report_error}
\alias{report_error}
\title{Reporta inconsistência em um registro.}
\usage{
report_error(record_id, description)
}
\arguments{
\item{record_id}{ID do registro.}

\item{description}{Descrição da inconsistência encontrada.}
}
\description{
Reporta inconsistência em um registro.
}
\examples{
report_error("v0x0f01f-ex4a-4a75-a2e2-a99b6f161gh1", "Variável IDADE com valor 456")
}

