% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criaModeloGenerico2.R
\name{criaModeloGenerico2}
\alias{criaModeloGenerico2}
\title{Create function with generic model}
\usage{
criaModeloGenerico2(
  nome,
  formula,
  funcaoRegressao,
  variaveis,
  palpite = NULL,
  maisParametros = NULL,
  requires = NULL
)
}
\arguments{
\item{nome}{is the name of model}

\item{formula}{is the string formula begin with y2~y1}

\item{funcaoRegressao}{is the function that will make the regression, ex.: 'nlsLM'}

\item{variaveis}{list variables that are present in the model that are field database}

\item{palpite}{param start of funcaoRegressao}

\item{maisParametros}{string add in funcaoRegressao, ex lm(y2~y1, data=base, maisParametros)}

\item{requires}{list of string of packges used to work with funcaoRegressao}
}
\value{
will be returned function with generic model to map to a base
}
\description{
This function creates a generic model that will be a funcao that has parameters for the variables that can be mapped to each different base. her return will be a generic model that should be mapped to be used by the function avaliaEstimativas
}
