##' @title Get All Functions of Statistics of ITGM
##' @description this function allows you to obtain all functions available in ITGM for model evaluation.
##' @return collection of functions statistics
##' @import Fgmutils
##' @export
getAllEstatisticsFn <- function(){
  return(c(estatisticas,
           estatisticasBIAS,
           estatisticasBiasPERCENTUAL,
           estatisticasCE,
           estatisticasCORR,
           estatisticasCorrPERCENTUAL,
           estatisticasCV,
           estatisticasCvPERCENTUAL,
           estatisticasMAE,
           estatisticasR2,
           estatisticasResiduoPERCENTUAL,
           estatisticasResiduos,
           estatisticasRMSE,
           estatisticasRmsePERCENTUAL,
           estatisticasRRMSE))
}
