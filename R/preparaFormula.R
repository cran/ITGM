##' @title Prepare formula
##' @description this function prepare formula to use in models
##' @param formula It is the string formula to use in modelo
##' @return formula handled
##' @export
preparaFormula <- function(formula){
  if(grepl("^y2~y1", formula))
    return (formula);
  back = formula
  sep = NULL
  if(grepl("^\\w*~.*", formula, perl = T)){ ##caso modelo y2~y1+b1
    sep = "~"
  }else
    if(grepl("^\\w*=.*", formula, perl = T)){ ##caso modelo y2=y1+b1
      sep = "="
    }else{
      stop("tipo de formula invalida, use 'y2~y1+b1' ou 'y2=y1+b1'....")
    }
  form = strsplit(formula, sep)[[1]]
  y2 = form[1]
  y1 = gsub(pattern = "\\W.*$", replacement = "", x = form[2], perl = T)
  formula = gsub(pattern = y1, replacement = "y1", x = formula)
  formula = gsub(pattern = y2, replacement = "y2", x = formula)
  warning(paste("FORMULA INVALIDA:", back, "ALTERADO PARA ", formula))
  return (formula)
}
