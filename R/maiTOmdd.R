##' @title Individual Tree Model to Diameter Distribution Model
##' @description this function add fields to returned base "inventario" in order to make it usable for diameter distribution models
##' @param projeto is the field that contains the cod project of individuals
##' @param talhao is the field that contains the cod of project subdivision
##' @param parcela is the field that contains the cod of talhao subdivision
##' @param fila is the field that contains the cod row where the tree is
##' @param cova is the field that contains the cod pit where the tree is
##' @param fuste is the field that contains the cod shaft of tree
##' @param idade is the field that contains the age of individuals the observation
##' @param dap is the field that contains the diameter of individuals the observation
##' @param volume is the field that contains the volume of individuals the observation
##' @param espacamento is the field that contains the distance in METROS between one and another individual for ex.:c("3 x 3", "3,3 x 3", ...
##' @param amplitude default 1 is the amplitude of diameter classes
##' @param verbose use TRUE to status presentation
##' @return data.table what is "inventario" with some added fields
##' @import data.table
##' @import sqldf
##' @export
maiTOmdd <- function(projeto, talhao, parcela, fila, cova, fuste, #data of identification
                     idade, dap, volume, espacamento,             #characteristic of the individual
                     amplitude = 1, verbose = FALSE){             #options to execution
  if (verbose) ini = Sys.time()

  if (verbose) print("coletando os campos necessarios...")
  dtPrincipal <- data.table(
    projeto=projeto, talhao=talhao, parcela=parcela, fila=fila, cova=cova, fuste=fuste, idade=idade, dap=dap, volume=volume, espacamento=espacamento
  )

  if (verbose) print("gerando idetificador individual...")
  dtPrincipal$cod_id = paste0(dtPrincipal$projeto, "_", dtPrincipal$talhao, "_",  dtPrincipal$parcela, "_", dtPrincipal$fila, "_", dtPrincipal$cova, "_", dtPrincipal$fuste)

  if (verbose) print("adicionando colunas...")
  dtPrincipal$cmd = gsub(gsub(dtPrincipal$espacamento, pattern = "x", replacement = "*"), pattern = ",", replacement = ".")
  dtPrincipal$area = sapply(dtPrincipal$cmd, FUN = function(X){ return(eval(parse(text = X))) })
  dtPrincipal$NHa=round(10000/dtPrincipal$area)
  dtPrincipal$idadearred = round(dtPrincipal$idade)

  if (verbose) print("criando base de classes...")
  baseClasses <- sqldf("SELECT COUNT(*) AS N, MIN(dap) AS limiteMin, SUM(volume) AS VolumeTotal, idadearred, parcela FROM dtPrincipal GROUP BY idadearred, parcela")

  if (verbose) print("classificando dap...")
  dtPrincipal = merge(dtPrincipal, data.table(baseClasses), by = c("parcela", "idadearred"))
  dtPrincipal$classeDAP = (floor(dtPrincipal$limiteMin)+(amplitude/2)+(amplitude*floor((dtPrincipal$dap-dtPrincipal$limiteMin)/amplitude)))

  if (verbose) print("contabilizando por classe...")
  baseClasses <- sqldf("SELECT COUNT(*) AS NCLASSES, SUM(NHa) AS NhaClasse, SUM(area) AS areaClasse, idadearred, parcela, classeDAP FROM dtPrincipal GROUP BY idadearred, parcela, classeDAP")
  dtPrincipal = merge(dtPrincipal, data.table(baseClasses), by= c("parcela", "idadearred", "classeDAP"))

  if (verbose) print("calculando PROBABLILIDADE por classes...")
  dtPrincipal$PROBABILIDADE = dtPrincipal$NCLASSES / dtPrincipal$N

  if (verbose) print("calculando Volume por classes...")
  dtPrincipal$VolumeClasse =  dtPrincipal$PROBABILIDADE * dtPrincipal$VolumeTotal
  dtPrincipal$volumeClasseHA = dtPrincipal$VolumeClasse / (dtPrincipal$areaClasse / 10000)

  if (verbose) print("obtendo classe dap da primeira medicao...")
  baseClasses <- sqldf(
    "SELECT classeDAP AS classeDAPpriMed, MIN(idadearred), cod_id FROM dtPrincipal GROUP BY cod_id" )
  dtPrincipal = merge(dtPrincipal, data.table(baseClasses[, c("classeDAPpriMed", "cod_id")]), by = "cod_id")

  if (verbose) print(paste0("concluido com sucesso : ", round(as.numeric(Sys.time() - ini), 2), " sgs"))
  return ( data.table(
    #	"cod_id" = dtPrincipal$cod_id,
    "projeto" = dtPrincipal$projeto,
    "talhao" = dtPrincipal$talhao,
    "parcela" = dtPrincipal$parcela,
    "fila" = dtPrincipal$fila,
    "cova" = dtPrincipal$cova,
    "fuste" = dtPrincipal$fuste,
    "idade" = dtPrincipal$idade,
    #	"idadearred" = dtPrincipal$idadearred,
    "dap" = dtPrincipal$dap,
    "classeDAP" = dtPrincipal$classeDAP,
    "classeDAPpriMed" = dtPrincipal$classeDAPpriMed,
    #	"limiteMin" = dtPrincipal$limiteMin,
    "probabilidade" = dtPrincipal$PROBABILIDADE,
    "N" = dtPrincipal$N,
    "NCLASSES" = dtPrincipal$NCLASSES,
    "volume" = dtPrincipal$volume,
    "VolumeTotal" = dtPrincipal$VolumeTotal,
    "VolumeClasse" = dtPrincipal$VolumeClasse,
    "volumeClasseHA" = dtPrincipal$volumeClasseHA,
    #	"espacamento" = dtPrincipal$espacamento,
    #	"area" = dtPrincipal$area,
    "NHa" = dtPrincipal$NHa,
    "NhaClasse" = dtPrincipal$NhaClasse
  ))
}
