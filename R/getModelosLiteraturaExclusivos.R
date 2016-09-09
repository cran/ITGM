##' @title get Models Literature Exclusives
##' @description This function creates and returns an array with the usual models mapped to the mapping vector
##' @param mapeamento list of names fields of database will work "idade1", "idade2", "bai1", "s"
##' @return will be returned list of function with exclusive model
##' @export
getModelosLiteraturaExclusivos <- function(mapeamento = c("idade1", "idade2", "bai1", "s")){
  mlg = getModelosLiteraturaGenericos()
  pienaareschiver <- mlg[[1]](I1 = mapeamento[1], I2=mapeamento[2])
  amaroetal <- mlg[[2]](I1 = mapeamento[1], I2=mapeamento[2])
  richardszeide <- mlg[[3]](I1 = mapeamento[1], I2=mapeamento[2])
  schumachertome <- mlg[[4]](I1 = mapeamento[1], I2=mapeamento[2])
  bellacamposleite <- mlg[[5]]( I1 = mapeamento[1], I2=mapeamento[2], BAI=mapeamento[3], S=mapeamento[4])
  return (c(pienaareschiver, amaroetal, richardszeide, schumachertome, bellacamposleite))
}
