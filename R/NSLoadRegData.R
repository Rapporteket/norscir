#' Provide global dataframe for NorScir
#'
#' Provides NorScir data from staging
#'
#' @return RegData data frame
#' @export

NSLoadRegData <- function() {

  registryName <- "NorScir"
  dbType <- "mysql"

  query <- "SELECT
     Scietiol,
     isVrtbrInj + '0' AS isVrtbrInj,
     isAssocInj +'0' AS isAssocInj,
     isSpnlSurg + '0' AS isSpnlSurg,
     VentAssi,
     PlaceDis,
     ASensLvlAreaL,
     ASensLvlAreaR,
     AMtrLvlAreaL,
     AMtrLvlAreaR,
     AAis,
     FSensLvlAreaL,
     FSensLvlAreaR,
     FMtrLvlAreaL,
     FMtrLvlAreaR,
     FAis,
     cast(BirthDt as CHAR(10)) AS BirthDt,
     cast(InjuryDt as CHAR(10)) AS InjuryDt,
     cast(AdmitDt as CHAR(10)) AS AdmitDt,
     cast(DischgDt as CHAR(10)) AS DischgDt,
     cast(ANeuExmDt as CHAR(10)) AS ANeuExmDt,
     cast(FNeuExmDt as CHAR(10)) AS FNeuExmDt,
     cast(QolDt as CHAR(10)) AS QolDt,
     cast(AdmitRehDt as CHAR(10)) AS AdmitRehDt,
     ANeuNoMeasure+'0' AS ANeuNoMeasure,
     FNeuNoMeasure+'0' AS FNeuNoMeasure,
     InjuryDateUnknown+'0' AS InjuryDateUnknown,
     OutOfHosptlDy+'0' AS OutOfHosptlDy,
     HosptlDy+'0' AS HosptlDy,
     isMale+'0' AS isMale,
     SatGenrl+'0' AS SatGenrl,
     SatPhys+'0' AS SatPhys,
     SatPsych+'0' AS SatPsych,
     SkjemaID+'0' AS SkjemaID,
     ReshId+'0' AS ReshId,
     DagerRehab+'0' AS DagerRehab,
     DagerTilRehab+'0' AS DagerTilRehab,
     AlderAar+'0' AS AlderAar,
     ShNavn AS ShNavn,
     NevrNivaaInn+'0' AS NevrNivaaInn,
     NevrNivaaUt+'0' AS NevrNivaaUt
FROM
     norScir
WHERE
     SkjemaID=1"

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
