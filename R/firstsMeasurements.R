##' @title First Measurements per Group
##' @description this function generates a field "primeiraMedicao" that contains the lowest age for each item in the group both derived from dtFrame
##' @param dtFrame It is the database that contains the data
##' @param group is the field that represents the groups default parcela
##' @param age default idadearred is the name of field containing the values of the group
##' @return data.frame dtFrame with the field "primeiraMedicao"
##' @import data.table
##' @import plyr
##' @import sqldf
##' @import Fgmutils
##' @import gsubfn
##' @examples
##' dtf = data.frame(
##' grupo = c(1,2,3,4,1,2,3,4),
##' medicoes= c(10,20,30,40,5,30,1,52))
##' firstsMeasurements(dtf, group = "grupo", age = "medicoes")
##' @export
firstsMeasurements <- function(dtFrame, group = "parcela", age = "idadearred"){

  dfParcelasPrimeirasMedicoes = fn$sqldf("select $group, min($age) as primeiraMedicao from dtFrame group by $group")

  dtFrame = atualizaCampoBase(camposAtualizar = "primeiraMedicao", baseAgrupada = dfParcelasPrimeirasMedicoes,
                              baseAtualizar = dtFrame, keys = group)
  return(data.frame(dtFrame))
}
