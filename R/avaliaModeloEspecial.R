##' @title Avalia Model Special
##' @description This function is a wizard to use the functionValueValueAdvanced2 configuration: a list of configurations that will be passed as parameter to function avaliaVolumeAvancado2
##' @param configuracao It is the list of settings
##' @return result of avaliaVolumeAvancado2
##' @import Fgmutils
##' @export
avaliaModeloEspecial <- function(configuracao){

  if(is.null(configuracao$basePredicao)){
    stop("ERROR: enter the prediction dataframe.")
  }

  if(is.null(configuracao$baseProjecao)){
    stop("ERROR: report the projection dataframe.")
  }

  if(is.null(configuracao$modelos)){
    stop("ERROR: inform the models to be evaluated.")
  }

  if(is.null(configuracao$estatisticas)){
    stop("ERROR: tell statistics to get.")
  }

  ##remover parametros adicionais de configuracao
  ##

  parametros = "basePredicao = configuracao$basePredicao, baseProjecao = configuracao$baseProjecao, modelos = configuracao$modelos, estatisticas = configuracao$estatisticas";

  others = setdiff(names(configuracao), c("basePredicao","baseProjecao","modelos","estatisticas"))
  for (other in others){
    parametros = paste0(parametros, ", ", other, " = configuracao$", other)
  }

  callAvalia = paste0("saida = avaliaVolumeAvancado2(", parametros, ")");
  relatorio = "relatorio.avaliaModeloEspecial.json";
  if(!is.null(configuracao$salvar$relatorio))
    relatorio = configuracao$salvar$relatorio;

  env <- new.env()
  assign("configuracao", configuracao, envir = env)
  runAndReport(script = callAvalia, report = relatorio, environment = env)

  return (get("saida", envir = env));
}
