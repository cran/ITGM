##' @title Project Volume based in Ages
##' @description this function provides a list of volume projections in a future age or from one to another future age
##' @param firstAge early age. if only one age use NaN
##' @param lastAge late age for project or the age at which one wants to get the volume
##' @param fitDAP an adjustment of the return type of a function lm() from dap
##' @param fitHT an adjustment of the return type of a function lm() from ht
##' @param base a dataset to project
##' @param mapper the mapping for the name of the old fields age, dap and ht in base
##' @param calcVolume function to calc volume based base, dap e ht,  default calculaVolumeDefault of Fgmutils
##' @param withoutBaseFields want returned projected volume no contains the fields of the base? default no
##' @export
project <- function(firstAge = NaN, lastAge, fitDAP, fitHT, base, mapper = list(age1="idade1", age2="idade2", dap1="dap1", dap2="dap2", ht1="ht1", ht2="ht2"), calcVolume = calculaVolumeDefault, withoutBaseFields = F){

  if (is.nan(firstAge)) firstAge = lastAge

  retorno = list()

  for (i in firstAge:lastAge) {
    b2 = base
    b2[, mapper$age2] = i
    b2$volume1 = calcVolume( dap = base[, mapper$dap1],  ht = base[, mapper$ht1], base)
    b2$volume2 = calcVolume(
      dap = (b2$dap2 = predizer(  fitDAP, newdata = b2 )),
      ht = (b2$ht2 = predizer(  fitHT, newdata = b2  )),
      base
    )
    if (!withoutBaseFields)
      eval(parse(text = paste0("retorno[[", i, "]] = b2")))
    else
      eval(parse(text = paste0("retorno$Volume_", i, " = b2$volume2")))
    base[, mapper$dap1] = b2$dap2
    base[, mapper$ht1] = b2$ht1
    base[, mapper$age1] = i
  }

  return(retorno)
}
