##' @title get Generic Model Literature
##' @description This function creates and returns an array with the usual models of literature
##' @return will be returned list of models generic
##' @export
getModelosLiteraturaGenericos <- function(){
  
  ps <- criaModeloGenerico(nome ="pienaar e schiver", formula = "y2~y1*exp(-b0*(((I2)^b1)-((I1)^b1)))", funcaoRegressao = "nlsLM", variaveis = c("I1", "I2"), palpite = "b0=1, b1=-0.05", requires = "minpack.lm")
  
  ae <- criaModeloGenerico(nome ="amaro et al", formula = "y2~y1+((b0/(1+exp(b1-b2*I2)))-(b0/(1+exp(b1-b2*I1))))", funcaoRegressao = "nlsLM", variaveis = c("I1", "I2"), palpite = "b0=1, b1=1, b2=0.5", requires = "minpack.lm")
  
  rz <- criaModeloGenerico(nome ="richards zeide", formula = "y2~y1+((b0/(1+exp((b1-b2*I2)*1/b3)))-(b0/(1+exp((b1-b2*I1)*1/b3))))", funcaoRegressao = "nlsLM", variaveis = c("I1", "I2"), palpite = "b0=1, b1=1, b2=1, b3=1", requires = "minpack.lm")
  
  st <- criaModeloGenerico(nome ="schumacher tome", formula = "y2~y1+exp(b0-(b1*I2))-exp(b0-(b1*I1))", funcaoRegressao = "nlsLM", variaveis = c("I1", "I2"), palpite = "b0=1, b1=-0.05", requires = "minpack.lm")
  
  bcl <- criaModeloGenerico(nome ="adaptado bella e campos e leite", formula = "y2~y1+(b0+b1*((1/I2)-(1/I1))+b2*BAI+b3*S)", funcaoRegressao = "nlsLM", variaveis = c("I1", "I2", "BAI", "S"), palpite = "b0=-1, b1=-1, b2 = -1, b3 = 0.05", requires = "minpack.lm")
  
  return (c(ps, ae, rz, st, bcl))
}
