#' Gererere månedsrapport for abonnement
#'
#' @param rnwFil navn på Rnw-fila som skal kjøres ('mndRapp.Rnw')
#' @param brukernavn abonnentens brukernavn ved bestilling
#' @param reshID - reshID som abonnenten var logget inn med ved bestilling
#' @param datoFra - startdato for data som hentes til bruk i rapporten
#' @param datoTil - sluttdato for data som hentes til bruk i rapporten
#'
#' @export
abonnement <- function(rnwFil, brukernavn='ukjent', reshID=0, register='norscir',
                       datoFra=Sys.Date()-400, datoTil=Sys.Date()) {

  # raplog::subLogger(author = brukernavn, registryName = register,
  #               reshId = reshID[[1]],
  #               msg = paste0("1)starter abonnementkjøring: ", rnwFil))

  AlleTab <- nordicscir::getRealData(register = register)
  AlleTab <- nordicscir::processAllData(AlleTab, register = register)
  attach(AlleTab)

  filbase <- substr(rnwFil[[1]], 1, nchar(rnwFil[[1]])-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn)[[1]], '.Rnw')
  src <- normalizePath(system.file(rnwFil[[1]], package='nordicscir'))

  # raplog::subLogger(author = brukernavn, registryName = 'NorScir',
  #                   reshId = reshID,
  #                   msg = "2) filbase, tmpFile, src ok")

  setwd(tempdir()) # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(input=tmpFile)

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0( getwd(), '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf') #

  # raplog::subLogger(author = brukernavn, registryName = 'NorScir',
  #                                      reshId = reshID[[1]],
  #                                      msg = paste("5) Leverer abonnementsfil: ", utfil))
  return(utfil)
}

