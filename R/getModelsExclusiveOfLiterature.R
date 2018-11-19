##' @title get Models Literature Exclusives
##' @description This function creates and returns an array with the usual models mapped to the mapping vector
##' @param mapper list of names fields of database will work "idade1", "idade2", "bai1", "s"
##' @return will be returned list of function with exclusive model
##' @export
getModelsExclusiveOfLiterature <- function(mapper = c("idade1", "idade2", "bai1", "s")){
  mlg = getModelosLiteraturaGenericos()
  pienaareschiver <- mlg[[1]](I1 = mapper[1], I2=mapper[2])
  amaroetal <- mlg[[2]](I1 = mapper[1], I2=mapper[2])
  richardszeide <- mlg[[3]](I1 = mapper[1], I2=mapper[2])
  schumachertome <- mlg[[4]](I1 = mapper[1], I2=mapper[2])
  bellacamposleite <- mlg[[5]]( I1 = mapper[1], I2=mapper[2], BAI=mapper[3], S=mapper[4])
  return (c(pienaareschiver, amaroetal, richardszeide, schumachertome, bellacamposleite))
}
