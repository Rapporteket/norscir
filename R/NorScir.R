#' All R resources neede for NorScir at Rapporteket
#'
#' An implemetation of all R related resources for NorScir at
#' Rapporteket, including R-functions for analysis and plotting,
#' noweb-files and even artwork needed for producing R-dependent reports
#'
#' @section Sample data:
#' The package contains a set of sample data that can be used for development
#' purposes. Not to be used in a PRODUCTION environment!
#'
#'
#' @section Functions:
#' Standard R-functions for processing registry data
#'
#' @section Noweb-files:
#' LaTeX docs weaved with R-code to produce reports. These are accessed by
#' \code{system.file}, \emph{E.g}
#' \code{system.file("NSsamelDok.Rnw", package = "NorScir")}. You may process
#' the noweb files directly using \emph{Sweave} or \emph{knitr}, \emph{e.g}
#' \code{knitr::knit(system.file("NSsamleDok.Rnw", package = "NorScir"))}, or
#' by using the wrapper of the \emph{rapbase} package, \emph{e.g.}
#' \code{rapbase::RunNoweb(nowebFileName, packageName, weaveMethod = "knitr")}.
#'
#' @section Artwork:
#' Artwork is accessed in the same way as for the noweb-files, \emph{e.g}
#' \code{system.file("NorSCIR_logo_blue_norsk.pdf", package = "NorScir")}
#'
#' @examples
#' data(NorScirSampleData)
#' names(NorScirSampleData)
#' NSFigAndeler(RegData = NorScirSampleData, valgtVar = "Alder", reshID = 123456, hentData=0)
#' knitr::knit(system.file("NSsamleDok.Rnw", package = "NorScir"))
#'
#' @docType package
#' @name norscir
NULL
