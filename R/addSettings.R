##' @title Add Settings to list param
##' @description This function assigns if the parameter does not exist or adds the parameter to the configuration. The variable assigned to the parameter is present in the last line of the script
##' @param script Script is a string that contains the script
##' @param tipo Type is one of the possibilities of parameter of evaluationVolumeAdvanced like: "models" or "graphs" or "statistics" or "save"
##' @param configuracao Configuration is a list that contains the settings for evaluation in the function EvaluateSpecial ()
##' @return list of settings
##' @export
addSettings <- function(script, tipo, configuracao = NULL){
  if(is.null(configuracao))
    configuracao = list()
  pt1 = ""
  pt2 = ""
  if((tipo == "modelos") && (eval(parse(text = paste0("!is.null(configuracao$", tipo, ") || length(configuracao$", tipo, ") > 0"))))){
    pt1 = paste0("union(configuracao$", tipo, ", ")
    pt2 = ")"
  }
  env <- new.env()
  saida = runAndReport(script = script, report = "relatorio.addSettings.json", environment = env)

  env <- new.env()
  assign("configuracao", configuracao, envir = env)
  assign("saida", saida, envir = env)

  if(tipo != "mapeamento" && tipo != "salvar" && tipo != "graficos" && tipo != "estatisticas" || (eval(parse(text = paste0("is.null(configuracao$", tipo, ")")))))
    eval(parse(text = paste0(  "configuracao$", tipo, " = ", pt1, "saida", pt2  )), envir = env)
  else{
    eval(parse(text = paste0( "tmp = configuracao$", tipo)), envir = env)
    for (name in names(saida)) {
      eval(parse(text = paste0( "tmp$", name, " = union(saida$", name, ", tmp$", name, ")")), envir = env)
    }
    eval(parse(text = paste0( "configuracao$", tipo, " = tmp")), envir = env)
  }
  return (get("configuracao", envir = env))
}
