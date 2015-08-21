#' Søylediagram som sammenligner fordeling (vha. stablede søyler) av valgt variabel
#'
#' Søylediagram som sammenligner fordeling (vha. stablede søyler) av valgt variabel
#' hos fra to ulike utvalg, f.eks. eget/resten, før/etter
#' (Kan velge å ikke ha med sammenligninga)
#' Søylediagram som viser andeler av valgt variabel:
#' NevrNivaaInn' - Nevrologisk nivå ved innskrivelse, sml. eget, andre
#' NevrNivaaUt' - Nevrologisk nivå ved utskrivelse, sml. eget, andre
#' NevrNivaaInnUt' - sml. Nevrologisk nivå ved inn- og utskrivelse
#' Hvilke kombinasjonsmuligheter har vi?
#' Inn / Ut (Velge: bare eget/hele landet)
#' Eget / resten av landet (Velge: Inn eller Ut)
#'
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret
#' @param libkat - sti til bibliotekkatalog
#' @param outfile - navn på fil figuren skrives ned til
#' @param reshID - avdelingsid for egen avdeling, må angis
#' Brukerstyrt i Jasper:
#' @param valgtVar - Må velges: ... NevrNivaaInnUt, NevrNivaaInn, NevrNivaaUt
#' @param enhetsUtvalg - 1:eget sykehus, 0:hele landet (standard) Kun for valgtVar=='NevrNivaaInnUt'
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', standard: ikke valgt
#' @param datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2013-05-25'
#' @export

FigAndelStabel <- function(RegData, outfile='', libkat, valgtVar,
                           datoFra='2010-01-01', datoTil='3000-01-01',minald=0,
                           maxald=120, erMann='', traume='',
                           enhetsUtvalg=enhetsUtvalg , reshID)
{

RegData <- NSLoadRegData()

#------------Gjøre utvalg-------------------------
#Definerer funksjonsspesifikke variable................

if (valgtVar %in% c('NevrNivaaInn','NevrNivaaUt')) {
  RegData$Variabel <- as.numeric(RegData[ ,valgtVar])
  }
else {
  RegData$Variabel <- RegData$NevrNivaaInn-RegData$NevrNivaaUt
  }  #Vil bare ha pasienter som har reg. både inn og ut.

#Gjør utvalg (Manglende data i variablene tas høyde for i variabeldef.)
Utvalg <- NSLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                      minald=minald, maxald=maxald, erMann=erMann,
                      traume=traume, AIS='')

RegData <- Utvalg$RegData
utvalgTxt <- Utvalg$utvalgTxt

grtxt <- ''
grtxt2 <- ''


#Sml eget/resten av landet.
if (valgtVar %in% c('NevrNivaaInn','NevrNivaaUt')) {
  ShNavn <- switch(as.character(enhetsUtvalg),    '0' = 'Hele landet',
                   '1' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]),
                   '2' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))
  RegData$AIS <- switch(valgtVar,
                        NevrNivaaInn = as.character(RegData$AAis),
                        NevrNivaaUt = as.character(RegData$FAis))
  #Slå sm D og E
  RegData <- RegData[RegData$AIS %in% LETTERS[1:5], ]     #Bare de med kategori A:E, ikke U
  RegData$AIS[which(RegData$AIS %in% c('D', 'E'))] <- 'D+E'
  #ind <- list(Sh=which(RegData$ReshId == reshID),
  #                       Rest=which(RegData$ReshId != reshID))
  indHoved <- if (enhetsUtvalg == 0) {1:dim(RegData)[1]} else {which(RegData$ReshId == reshID)}
  indRest <-  if (enhetsUtvalg == 1) {which(RegData$ReshId != reshID)} else {NULL}
  #Alternativt bruke to datasett?
  #RegDataLand <- RegData
  #Andre <- if (enhetsUtvalg==1) {'Andre'} else {NA}
  ShNavnTittel <- if (enhetsUtvalg != 1) {ShNavn} else {NA}
  grtxt1 <- if (enhetsUtvalg==1) {c(ShNavn, 'Andre')} else {NA} #c(ShNavn,Andre)
  grtxt2 <- c('C1-4', 'C5-8', 'T,L,S')
  subtxt <- 'Nevrologisk nivå'
  Skala <- c('A','B','C','D+E')
  #Bedre å lage en variabel med nevrologiske kategorier
  indC14 <- which(RegData$Variabel %in% 1:4)
  indC58 <- which(RegData$Variabel %in% 5:8)
  indTLS <- which(RegData$Variabel %in% 9:30)
  Ngr <- cbind(
    table(factor(RegData$AIS)[intersect(indHoved, indC14)]),        #C0104Sh
    table(factor(RegData$AIS)[intersect(indRest, indC14)]), #C0104Rest
    table(factor(RegData$AIS)[intersect(indHoved, indC58)]),        #C0508Sh
    table(factor(RegData$AIS)[intersect(indRest, indC58)]), #C0508Rest
    table(factor(RegData$AIS)[intersect(indHoved, indTLS)]),        #TLSSh
    table(factor(RegData$AIS)[intersect(indRest, indTLS)])) #TLSRest
}



if (valgtVar == 'NevrNivaaInnUt') {
  #Tar bort de som ikke har fått AIS-nivå el nivå U:
  RegData <- RegData[RegData$AAis %in% LETTERS[1:5], ]    #Bare de med kategori A:E, ikke U
  RegData <- RegData[RegData$FAis %in% LETTERS[1:5], ]
  RegData$Var1 <- as.character(RegData$AAis)
  RegData$Var2 <- as.character(RegData$FAis)
  #Slå sm D og E
  RegData$Var1[which(RegData$Var1 %in% c('D', 'E'))] <- 'D+E'
  RegData$Var2[which(RegData$Var2 %in% c('D', 'E'))] <- 'D+E'
  if (enhetsUtvalg==0) {
    ShNavnTittel <- 'Hele landet'
  } else { #Både enhetsUtvalg 1 og 2 gir eget sykehus.
    ind <- which(RegData$ReshId == reshID)
    RegData <- RegData[ind,]
    ShNavnTittel <- as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])}
  subtxt <- 'Nevrologisk nivå'
  grtxt1 <- rep(c('Inn','Ut'))
  grtxt2 <- c('C1-4', 'C5-8', 'T,L,S')
  Skala <- c('A','B','C','D+E')
  RegData$Var1 <- factor(RegData$Var1, levels=c(LETTERS[1:3],'D+E'))
  RegData$Var2 <- factor(RegData$Var2, levels=c(LETTERS[1:3],'D+E'))
  indGr1 <- intersect(which(RegData$NevrNivaaInn %in% 1:4), which(RegData$NevrNivaaUt %in% 1:4))
  indGr2 <- intersect(which(RegData$NevrNivaaInn %in% 5:8), which(RegData$NevrNivaaUt %in% 5:8))
  indGr3 <- intersect(which(RegData$NevrNivaaInn %in% 9:30), which(RegData$NevrNivaaUt %in% 9:30))
  Ngr <- cbind(
    table(RegData$Var1[indGr1]),    #C0104inn
    table(RegData$Var2[indGr1]),    #C0104ut
    table(RegData$Var1[indGr2]),    #C0508inn
    table(RegData$Var2[indGr2]),    #C0508ut
    table(RegData$Var1[indGr3]),    #TLSinn
    table(RegData$Var2[indGr3]))    #TLSut
}

#Definere grupperingsvariabel med gyldige grupper i utgangspunktet i stedet for å gjøre det i indGr?


#------------------------- Beregninger -----------------------------------------
#utvalg <- c('Sh', 'Rest') #som Var1 og Var 2
#Andeler <- list(Sh = 0, Rest =0)
#ind_sh <-which(as.numeric(RegData$AvdReshID)==egenReshID)
#ind_resten <- which(as.numeric(RegData$AvdReshID)!=egenReshID)
#Ngr <- cbind('Egen avdeling'=table(RegData[ind_sh, variabel]),
#			'Landet forøvrig'= table(RegData[ind_resten, variabel]))
N <- colSums(Ngr)
AndelStabel <- round(prop.table(Ngr,2)*100, 1)


if (dim(RegData)[1]==0)	{# | (NSh==0 & enhetsUtvalg==1)) {
	#-----------Figur---------------------------------------
rapbase::figtype(outfile)
	plot.new()
	title(main=tittel)
	text(0.5, 0.7, 'Ingen egne data registrert for dette utvalget',cex=1.3)
	if ( outfile != '') {dev.off()}
} else {

#-----------Figur---------------------------------------
#Inn parametre: subtxt, grtxt1, grtxt2, tittel, libkat, AndelStabel

#Plottspesifikke parametre:
FigTypUt <- rapbase::figtype(outfile, fargepalett=Utvalg$fargepalett)
farger <- FigTypUt$farger

par('fig'=c(0, 1, 0, 0.9))
pos <- barplot(AndelStabel, space=c(0,rep(c(0.1,1),2),0.1), col=farger, border=NA, xlim=c(0,10),
		ylim=c(0,110), names.arg=NULL, ylab="Andel (%)")
legend('right', rev(Skala), bty='n', ncol=1, cex=1, xjust=0, fill=rev(farger), border=rev(farger))	#, title=tLeg)

if ((valgtVar %in% c('NevrNivaaInn','NevrNivaaUt')) & (reshID == 0)) {
	N[c(1,3,5)]=''
	text(pos, 102.6, paste0(c('','N='), N), cex=0.9)} else {
	text(pos, 102.6, paste('N=', N, sep=''), cex=0.9)}	 #Antall over søyla
mtext(at=pos, grtxt1, side=1, las=1, cex=0.8, adj=0.5, line=0.3)	#Tekst under søylene
mtext(at=c(1, 4.1, 7.2), grtxt2, side=1, cex=0.9, adj=0.5, line=1.3)
mtext(subtxt, side=1, cex=1, adj=0.5, line=2.7)

utvpos <- 5.2
avst <- 0.8
krymp <- 0.8
title(tittel, line=1, font.main=1)
mtext(utvalgTxt[1], side=3, las=1, cex=krymp, adj=0, line=utvpos, col=farger[1])
mtext(utvalgTxt[2], side=3, las=1, cex=krymp, adj=0, line=utvpos-avst, col=farger[1])
mtext(utvalgTxt[3], side=3, las=1, cex=krymp, adj=0, line=utvpos-2*avst, col=farger[1])
mtext(utvalgTxt[4], side=3, las=1, cex=krymp, adj=0, line=utvpos-3*avst, col=farger[1])
mtext(utvalgTxt[5], side=3, las=1, cex=krymp, adj=0, line=utvpos-4*avst, col=farger[1])

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#------------------------------------------------------------------------------

}	#end if
}	#end function
