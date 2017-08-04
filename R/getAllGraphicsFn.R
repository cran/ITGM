##' @title Get All Functions of Graphics of ITGM
##' @description this function allows you to obtain all functions available in ITGM for model evaluation.
##' @return collection of functions graphics
##' @import Fgmutils
##' @export
getAllGraphicsFn <- function(){
  return(c(getggplot2GraphicObservadoXEstimado,
           getGraphicHistogram,
           getGraphicObservadoXEstimado,
           getGraphicResiduoAbs,
           getGraphicResiduoPerc,
           plotRB,
           getGraphicVolumeTotal,
           getggplot2GraphicObservadoXEstimadoTotal))
}
