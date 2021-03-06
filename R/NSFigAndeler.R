#' Søylediagram som viser fordeling av valgt variabel
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 15-årige grupper 
#'     \item AAis: A-E+U, innleggelse
#'     \item FAis: A-E+U, kontroll
#'     \item DagerRehab: Lengde på rehab.opphold (Utskrevet – Innl. rehab dato)
#'     \item DagerTilRehab: Tid fra skadedato til oppstart rehab [trauma/ikke], 5d interv
#'     \item OpphTot: Lengde på opphold – totalt (HosptlDy) 20d… >200, 
#'     \item UtTil: Hva pasienten skrives ut til. [PlaceDis]
#'     \item SkadeArsak: Årsak til skade [variabel: Scietiol]
#'     \item Pustehjelp[VentAssi].
#'    }
#' 
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret.
#' @param outfile - navn på fil figuren skrives ned til.
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet.
#' @param valgtVar - variabelen det skal genereres resultat for 
#' @param datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2013-05-25'
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', standard: ikke valgt
#' @param AIS - AISgrad ved innleggelse alle(''), velge en eller flere fra A,B,C,D,E,U
#' @param enhetsUtvalg - 1:eget sykehus, 0:hele landet (standard) Kun for valgtVar=='NevrNivaaInnUt'
#' @param hentData Settes til 1 (standard) om data skal lastes i funksjonen.
#'    Settes til en annen verdi om data leveres til funksjonen gjennom 'RegData',
#'    eksempelvis ved bruk av eksempeldatasettet eller ved kall fra andre
#'    funksjoner der data allerede er tilgjengelig.
#' 
#' @export

NSFigAndeler <- function(RegData, outfile='', valgtVar,
                         datoFra='2010-01-01', datoTil='2050-01-01', AIS='',
                         minald=0, maxald=120, erMann='', traume='',
                         enhetsUtvalg=1, reshID, hentData=1, preprosess=1) {

  if (hentData == 1) {
    RegData <- NSRegDataSQL()
  }
if (preprosess == 1) {
      RegData <- NSPreprosesser(RegData)
      }

  RegData$Variabel <- RegData[ ,valgtVar]

  shtxt <- switch(as.character(enhetsUtvalg), 	'0' = 'Hele landet',
                  '1' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]),
                  '2' = as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))

  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

  #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
  Utvalg <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                        erMann=erMann, traume=traume, AIS=AIS)
  RegData <- Utvalg$RegData
  utvalgTxt <- Utvalg$utvalgTxt

  N <- dim(RegData)[1]


  #-----------Må ha en del som er registerspesifikk, så må selve plottet være i pakken, dvs. funksjoner.
  cexgr <- 1.1
  retn <- 'V'
  txtretn <- 1
  grtxt <- ''
  grtxt2 <- ''
  utvalg <- c('Sh', 'Rest')
  Andeler <- list(Sh = 0, Rest =0)
  Nutv <- N


  #Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
  if (valgtVar %in% c('AAis', 'FAis')) {
        #-1: Velg verdi, 1:A Komplett skade, 2:B Inkomplett, 3:C Inkomplett, 4:D Inkomplett, 5:E Normal, 
        #9: U Ukjent eller ikke anvendbar
        tittel <- paste('Fordeling av', switch(valgtVar, AAis = 'AIS ved innleggelse', FAis = 'AIS ved utskriving'))
    grtxt <- c('A','B','C','D','E','U')
    # Tar med -1 i Ukjent
    RegData$Variabel[RegData$Variabel==-1] <- 9
    subtxt <- 'AIS kategori'
    RegData$Variabel <- factor(RegData$Variabel, levels = c(1:5,9), labels = grtxt) 
  }
  
  if (valgtVar == 'Pustehjelp') {
    tittel <- 'Ventilasjonsstøtte'
    #gr <- (0:3,9) - Kodene som registereres. Nå bare 0:3?
    grtxt <- c('Nei', 'Mindre enn 24t/dag', 'Hele døgnet', 'Ukjent ant timer')
    subtxt <- ''
    RegData$Variabel <- factor(as.numeric(RegData$Variabel), levels=c(0:3), labels = grtxt)
    retn <- 'H'
  }
  if (valgtVar=='Alder') {
    tittel <- 'Aldersfordeling'
    gr <- c(0,16,31,46,61,76,200)	#c(seq(0, 90, 15), 120)
    RegData$Variabel <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('[0,15]','[16,30]','[31,45]','[46,60]','[61,75]','76+')
    #	grtxt <- c(levels(RegData$Variabel)[1:(length(gr)-2)], '76+')
    cexgr <- 0.9
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='Permisjon') {
    tittel <- 'Antall døgn ute av sykehus'
    gr <- c(0,1,7,14,21,28,35, 1000)
    grmax <- '50+'
    RegData$Variabel <- cut(RegData$Permisjon, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('0','1-7','8-14','15-21','22-28','29-35', '35+')
    cexgr <- 0.9
    subtxt <- 'Antall døgn'
  }
  if (valgtVar %in% c('DagerRehab', 'DagerTilRehab', 'OpphTot')) {
    
      grmax <- switch(valgtVar,
            DagerRehab= '360+',
            DagerTilRehab = '100+',
            OpphTot = '300+')
      gr <- switch(valgtVar, 
            DagerRehab = c(seq(0, 180, 20), 360, 1000),
            DagerTilRehab = c(seq(0, 100, 10), 1000),
            OpphTot = c(seq(0, 300, 30), 1000))
    RegData$Variabel <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt2 <- c(levels(RegData$Variabel)[1:(length(gr)-2)], grmax)
    tittel <- switch(valgtVar, 
                     DagerRehab='Antall dager med spesialisert rehabilitering', 
                     DagerTilRehab = 'Tid fra innleggelse til spesialisert rehabilitering',
                     OpphTot = 'Antall døgn innlagt på sykehus')
    cexgr <- 0.9
    txtretn <- 2
    subtxt <- 'Antall døgn'
  }	#Variable med antall dager


  if (valgtVar == 'SkadeArsak') {
    tittel <- 'Skadeårsaker'
    #gr <- (1:6,9) - Kodene som registereres
    RegData$Variabel[which(RegData$SkadeArsak==9)] <- 7
    grtxtAlle <- c('Idrett', 'Vold', 'Transport', 'Fall', 'Andre traumer',
                   'Ikke-traumatisk', 'Uspesifisert')
    grtxt <- grtxtAlle
    subtxt <- 'Utskrevet til'
    RegData$Variabel <- factor(as.numeric(RegData$Variabel), levels=1:7, labels = grtxtAlle)
    retn <- 'H'
  }
  if (valgtVar == 'UtTil') {
    tittel <- 'Utskrevet til'
    #gr <- (1:10,99) - Kodene som registereres
    RegData$Variabel[which(RegData$UtTil==99)] <- 11
    grtxtAlle <- c('Hjem', 'Sykehus', 'Pleiehjem', 'Omsorgsbolig', 'Bofellesskap',
                   'Kriminalomsorg', 'Hotell', 'Bostedsløs', 'Avdød', 'Annet', 'Ukjent')
    grtxt <- grtxtAlle
    subtxt <- 'Utskrevet til'
    RegData$Variabel <- factor(as.numeric(RegData$Variabel), levels=1:11, labels = grtxtAlle)
    #Vurder om skal ta med bare de som er registrert
    #grtxt <- grtxtAlle[as.numeric(names(table(as.numeric(RegData$PlaceDis))))] #De som er reg.
    retn <- 'H'
  }


  #--------------- Gjøre beregninger ------------------------------
  medSml <- 0
  utvalg <- c('Sh', 'Rest')	#Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
  Andeler <- list(Sh = 0, Rest =0)

  #Hvis det skal gjøres sammenligning:
  if (enhetsUtvalg == 1) {
    indSh <-which(RegData$ReshId == reshID)
    indRest <- which(RegData$ReshId != reshID)
    RegDataLand <- RegData
    ind <- list(Sh=indSh, Rest=indRest)
    medSml <- 1
  }

  Nrest <- 0
  for (teller in 1:(medSml+1)) {
    if (medSml == 1) {
      RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
    }

    if (teller == 1) {Andeler$Sh <- round(table(RegData$Variabel)/length(RegData$Variabel)*100,2)
                      Nsh <- dim(RegData)[1]}
    if (teller == 2) {Andeler$Rest <- round(table(RegData$Variabel)/length(RegData$Variabel)*100,2)
                      Nrest <- dim(RegData)[1]}
  }

  #-----Hvis få registreringer: ---------------------
  if (Nsh < 5 | (medSml ==1 & Nrest<5)) {
    FigTypUt <- rapbase::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 egne registreringer,', cex=1.2)
    text(0.5, 0.5, 'eller færre enn 5 i sammenlikningsgruppa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    #-----------Figur---------------------------------------
    #Inn parametre: subtxt, grtxt, grtxt2, tittel, Andeler
    #Plottspesifikke parametre:

    #Plottspesifikke parametre:
    FigTypUt <- rapbase::figtype(outfile, fargepalett=Utvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Sh)), '%)', sep='')
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeSh <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1.1	#Størrelse på legendtekst

    if (retn == 'V' ) {
      #Vertikale søyler eller linje
      if (grtxt2 == '') {grtxt2 <- paste('(', sprintf('%.1f',Andeler$Sh), '%)', sep='')}
      ymax <- max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=txtretn, ylab="Andel pasienter (%)",	#main=tittel,
                     cex.lab=cexleg, sub=subtxt, cex.sub=cexleg,	col=fargeSh, border='white', ylim=c(0, ymax))	#farger[c(1,3)] #names.arg=grtxt, cex.names=cexgr,
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=txtretn, cex=cexgr-0.1, adj=0.5, line=1.5)
      if (medSml == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=NA,
               lwd=lwdRest, ncol=2, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', N,')', sep=''),
               border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
      }
    }

    if (retn == 'H') {
      #Horisontale søyler
      ymax <- antGr*1.4
      xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
      #par('fig'=c(0.1, 1, 0, 0.9))
      pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
                     cex.lab=cexleg,col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0,ymax))	#
      mtext(at=pos+0.1, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)	#text=rev(grtxt)
      if (medSml == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('topleft', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=NA,
               lwd=lwdRest, ncol=2, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', N,')', sep=''),
               border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
      }
    }

    title(tittel, line=1, font.main=1, cex.main=1.5)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=cexleg-0.1, adj=0, col=farger[1], line=c(3+0.9*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }
}
