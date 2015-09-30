#' Funksjon som genererer en figur med gjennomsnitt/median
#'
#' Funksjon som genererer en figur med gjennomsnitt/median for hvert sykehus
#' og kan ta inn ulike numeriske variable. Funksjonen er delvis skrevet for å
#' kunne brukes til andre grupperingsvariable enn sykehus.
#'
#' @param RegData Ei dataramme med alle nødvendige variable fra registeret
#' @param libkat Sti til bibliotekkatalog (utgår)
#' @param outfile - navn på fil figuren skrives ned til
#' @param valgtVar Må velges: ... Alder, DagerRehab, DagerTilRehab, OpphTot[HosptlDy],
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', standard: ikke valgt
#' @param AIS - AISgrad ved innleggelse alle(''), velge en eller flere fra A,B,C,D,E,U
#' @param datoFra <- '2010-01-01'. Min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2013-05-25'
#' @param valgtMaal - 'Med' = median. Alt annet gir gjennomsnitt
#'
#' @export

FigMeanMed <- function(RegData, valgtVar, valgtMaal='Gjsn', datoFra='01-01-2010', datoTil='31-12-2050',
		AIS='', minald=0, maxald=130, erMann='', traume='', libkat, outfile='',
    hentData=1) {

# in a package, these will no longer be needed
#source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
#source(paste(libkat, 'NSLibUtvalg.R', sep=''), encoding="UTF-8")

  if (hentData == 1) {
    RegData <- NSLoadRegData()
  }

#Definerer funksjonssperifikke variable................
grVar <- 'ShNavn'
RegData[ ,grVar] <- factor(RegData[ ,grVar])	#, labels=c('Haukeland', 'St.Olav', 'Sunnaas'))

if (valgtVar %in% c('DagerRehab', 'DagerTilRehab')) {
	RegData$Variabel <- RegData[ ,valgtVar] }
if (valgtVar == 'Alder') {RegData$Variabel <- RegData$AlderAar}
if (valgtVar == 'OpphTot') {RegData$Variabel <- RegData$HosptlDy}
if (valgtVar == 'Permisjon') {RegData$Variabel <- RegData$OutOfHosptlDy
				RegData <- RegData[RegData$OutOfHosptlDy>0,]}	#Bare vits i å se på de som faktisk har permisjon

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
Utvalg <- NSLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann, traume=traume, AIS=AIS)
RegData <- Utvalg$RegData
utvalgTxt <- Utvalg$utvalgTxt

N <- dim(RegData)[1]
Ngrense <- 5		#Minste antall registreringer for at et sykehus skal bli vist
if(N > 0) {Nsh <- table(RegData[ ,grVar])}	else {Nsh <- 0}

Nshtxt <- paste(' (', as.character(Nsh),')', sep='') #paste('N=', as.character(Nsh), sep='')
indShUt <- which(Nsh < Ngrense)
if (length(indShUt)==0) { indShUt <- 0}
Nshtxt[indShUt] <- paste(' (<', Ngrense,')',sep='')	#paste('N<', Ngrense,sep='')


vt <- switch(valgtVar, Alder='alder',
					DagerRehab='antall dager med rehabilitering',
					DagerTilRehab='antall dager før rehabilitering',
					OpphTot= 'totalt opphold',
					Permisjon = 'permisjonstid (for de som har hatt permisjon)')

if (valgtMaal=='Med') {
t1 <- 'Median ' } else {t1 <- 'Gjennomsnittlig '}

tittel <- c(paste(t1, vt, sep=''),
			'med 95% konfidensintervall')

#-----------Figur---------------------------------------
if 	( max(Nsh) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
rapbase::figtype(outfile)
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
	title(main=tittel, cex=0.95)
	legend('topleft',utvalgTxt, bty='n', cex=0.9)
	} else {
	tekst <- 'Ingen registrerte data for dette utvalget' }
	text(0.5, 0.5, tekst,cex=1)	#, family="sans")
if ( outfile != '') {dev.off()}
} else {
#--------------------------------------------------------


#Kommer ut ferdig sortert!
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
	MedIQR$stats[ ,indShUt] <- 0
	MedIQR$conf[ ,indShUt] <- 0
	sortInd <- order( MedIQR$stats[3,], decreasing=TRUE)
	Midt <- as.numeric(MedIQR$stats[3, sortInd])
	KIned <- MedIQR$conf[1, sortInd]
	KIopp <- MedIQR$conf[2, sortInd]
#	MidtHele <- median(RegData$Variabel)
	MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
	MidtHele <- as.numeric(MedIQRHele$stats[3])
	KIHele <- MedIQRHele$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
#roughly a 95% confidence interval for the difference in two medians.


} else {	#Gjennomsnitt blir standard.
	Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
	SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Nsh)
	Gjsn[indShUt] <- 0
	SE[indShUt] <- 0
	sortInd <- order(Gjsn, decreasing=TRUE)
	Midt <- as.numeric(Gjsn[sortInd])
	KIned <- Gjsn[sortInd] - 2*SE[sortInd]
	KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
	MidtHele <- mean(RegData$Variabel)
	KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
}

ShNavnSort <- paste(names(Nsh)[sortInd], Nshtxt[sortInd], sep='')
plotdata <- c(Midt, KIned, KIopp)

AntSh <- length(which(Midt>0))
#soyletxt <- c(round(Midt[1:AntSh],1), rep('',length(Nsh)-AntSh))
xmax <-  min(1.1*max(plotdata), 1.5*max(Midt))
xlabt <- switch(valgtVar, Alder='alder (år)',
					DagerRehab='dager',
					DagerTilRehab='dager før rehabilitering',
					OpphTot= 'dager',
					Permisjon = 'permisjonstid (dager)')


#--------------------------FIGUR---------------------------------------------------
FigTypUt <- rapbase::figtype(outfile, fargepalett=Utvalg$fargepalett)
farger <- FigTypUt$farger
par('fig'=c(0.1, 1, 0, 0.91))
	#plot.new()
	pos <- barplot(Midt, horiz=T, border=NA, col=farger[3], #main=tittel,
		xlim=c(0,xmax), ylim=c(0,3.9),
			font.main=1, xlab=paste(t1, vt, sep=''), las=1, cex.names=0.8) 	#xlim=c(0,ymax),
	indShUtPlot <- AntSh+(1:length(indShUt))
	posKI <- pos[1:AntSh]
	ybunn <- 0.1
	ytopp <- 3.7	#c(0, max(posKI) + min(posKI))
	polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
		col=farger[4], border=farger[4])
	lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=3)
	legend('topright', xjust=1, fill=c(farger[4],'white'), border=c(farger[4],'white'), cex=0.8, #lwd=2, #col=farger[2],
		legend='95% konf.int., alle sykehus', bty='o', bg='white', box.col='white')
	barplot(Midt, horiz=T, border=NA, col=farger[3], 	#main=tittel,
		xlim=c(0, xmax), add=TRUE,
			font.main=1, xlab=paste(t1, vt, sep=''), las=1, cex.names=0.5) 	#xlim=c(0,ymax),
	title(tittel, line=1, font.main=1, cex.main=1.2)

#	text(x=0, y=pos+0.1, soyletxt, las=1, cex=0.8, adj=0, col=farger[1])
	utvpos <- 6
	mtext(utvalgTxt[1], side=3, col=farger[1], las=1, cex=0.9, adj=0, line=utvpos)
	mtext(utvalgTxt[2], side=3, col=farger[1],las=1, cex=0.9, adj=0, line=utvpos-0.8)
	mtext(utvalgTxt[3], side=3, col=farger[1],las=1, cex=0.9, adj=0, line=utvpos-1.6)
	mtext(utvalgTxt[4], side=3, col=farger[1],las=1, cex=0.9, adj=0, line=utvpos-2.4)
	mtext(utvalgTxt[5], side=3, col=farger[1],las=1, cex=0.9, adj=0, line=utvpos-3.2)

	mtext(at=pos+0.1, ShNavnSort, side=2, las=1, cex=0.9, adj=1, line=0.25)	#Hvis vil legge på navn som eget steg
	options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
	arrows(x0=Midt[-indShUtPlot]*0.999, y0=posKI, x1=KIopp[-indShUtPlot], y1=posKI,
		length=0.5/max(pos), code=2, angle=90, lwd=2, col=farger[1])
	arrows(x0=Midt[-indShUtPlot]*1.001, y0=posKI, x1=KIned[-indShUtPlot], y1=posKI,
		length=0.5/max(pos), code=2, angle=90, lwd=2, col=farger[1])
par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
