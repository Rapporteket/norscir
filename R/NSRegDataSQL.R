#' Provide global dataframe for NorScir
#'
#' Provides NorScir data from staging
#'
#' @return RegData data frame
#' @export

NSRegDataSQL <- function() {

  registryName <- "NorScir"
  dbType <- "mysql"

  query <- "SELECT
     Scietiol,
     isVrtbrInj,
     isAssocInj,
     isSpnlSurg,
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
     BirthDt,
     InjuryDt,
     AdmitDt,
     DischgDt,
     ANeuExmDt,
     FNeuExmDt,
     QolDt,
     AdmitRehDt,
     ANeuNoMeasure,
     FNeuNoMeasure,
     InjuryDateUnknown,
     OutOfHosptlDy,
     HosptlDy,
     isMale,
     SatGenrl,
     SatPhys,
     SatPsych,
     SkjemaID,
     ReshId,
     DagerRehab,
     DagerTilRehab,
     AlderAar,
     ShNavn,
     NevrNivaaInn,
     NevrNivaaUt
FROM
     MainFormDataContract"

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
