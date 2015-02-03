#' Støttefunksjon for NorScir
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og
#' utvalgsteksten.
#'
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: ''(alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param datoFra <- '2010-01-01'. Min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '3000-05-25'
#' @param traume ???
#' @param AIS ???
#' @param fargepalett ???
#' @export



NSLibUtvalg <- function(RegData, datoFra='2010-01-01', datoTil='3000-05-25', minald=0, maxald=120,
		erMann='', traume='', AIS='', fargepalett='BlaaOff')
{
#Definerer registerspesifikke variable................
RegData$InnDato <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")	#"%d.%m.%Y"
RegData$Alder <- RegData$AlderAar
#RegData$ShNavn <- factor(RegData$ReshId, labels=c('Haukeland', 'Sunnaas', 'St.Olav'))


#Hvis "Variabel" ikke definert
if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
Ninn <- dim(RegData)[1]
indVarMed <- intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')),
				which(RegData$Variabel != ''))
indSkjemaUt <- which(RegData$SkjemaID != 1)	#NB: Kan senere bli variabelspesifikk!!!
indAldUt <- which(RegData$Alder <= minald | RegData$Alder >= maxald)
indDatoUt <- setdiff(1:Ninn,
			which(RegData$InnDato > as.POSIXlt(datoFra) & RegData$InnDato < as.POSIXlt(datoTil))) #Får bort NA
traumeValgBort <- switch(traume, ja = c(6,9) , nei = c(1:5,9), alle = NULL) #6 ikke-tr, 1:5 traumer, 9 ukjent
indTrUt <-  which(RegData$Scietiol %in% traumeValgBort)
indKjUt <- if (erMann %in% 0:1) {which(RegData$isMale != erMann)} else {indKjUt <- NULL}
indAISut <- if (length(which(AIS %in% c(LETTERS[1:5],'U')))>0) {
		setdiff(1:Ninn, which(RegData$AAis %in% AIS))} else {NULL}
indMed <- intersect(setdiff(1:Ninn, c(indSkjemaUt, indAldUt, indDatoUt, indTrUt, indKjUt, indAISut)),
			indVarMed)
RegData <- RegData[indMed,]

utvalgTxt <- c(paste('Innleggelsesperiode: ',
	min(RegData$InnDato, na.rm=T), ' til ', max(RegData$InnDato, na.rm=T), sep='' ),
	if ((minald>0) | (maxald<120)) {
		paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
	if (traume %in% c('ja','nei')) {paste('Traume:', traume)},
	if (erMann %in% 0:1){paste('Kjønn: ', c('kvinner', 'menn')[erMann+1], sep='')},
	if (length(which(AIS %in% c(LETTERS[1:5],'U')))>0) {paste('AIS, inn: ', paste(AIS, collapse=','), sep='')} )


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
return(invisible(UtData))
}
