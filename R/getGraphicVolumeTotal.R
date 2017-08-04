##' @title Get rbokeh grapic observed versus estimated
##' @description this function displays/saves/returns a Graphical rbokeh illustrating the difference between the observed and estimated
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param showTestF draw results of test F in graphic?
##' @param save If you want to save enter the directory as a string
##' @param labsX label x
##' @param labsy label y
##' @param vetorial save picture in vector type? (Default TRUE)
##' @param desc description of plot geted from avaliavolumeavancado
##' @param data database to retireve fields to id
##' @param mapeamento name of fields on base
##' @param ... optionals params to plot graphic in ITGM
##' @return will be returned a rbokeh graphic
##' @import sqldf
##' @importFrom grDevices dev.off png postscript
##' @importFrom graphics abline mtext
##' @importFrom stats scatter.smooth
##' @export
getGraphicVolumeTotal <- function (titulo = "observadoXestimado",
                                   nome = "observadoXestimado",
                                   observado,
                                   estimado,
                                   showTestF = TRUE,
                                   save = NULL,
                                   labsX = "observado",
                                   labsy = "estimado",
                                   vetorial = T,
                                   desc = NULL,
                                   data = NULL,
                                   mapeamento = list(idade2 = "idadearred2", parcela = "parcela", areacorr = "areacorr"),
                                   ...)
{

  if (!is.null(data)){


    if (!is.null(save)) {
      if (vetorial)
        postscript(paste0(save, nome, "ObservadoXEstimado total.postscript"))
      else png(paste0(save, nome, "ObservadoXEstimado total.png"))
    }

    tipo = strsplit(desc, '\\.')[[1]][[3]]
    labsX = eval(parse(text = paste('expression(paste("', tipo, ' estimado ", m^3, " ", ha^-1))')))
    labsy = eval(parse(text = paste('expression(paste("', tipo, ' ", m^3, " ", ha^-1))')))

    data$observado = observado
    data$estimado = estimado
    data$idade = data[, mapeamento$idade2]

    df = sqldf(paste("SELECT ", mapeamento$parcela, " AS parcela, idade, SUM(observado) AS volumeTotal2, SUM(estimado) AS volumeTotal2EstMAI, ", mapeamento$areacorr, " AS areacorr FROM data GROUP By ", mapeamento$parcela, ", idade"))
    df$volumeTotal2HA = (10000/df$areacorr) * df$volumeTotal2
    df$volumeTotal2EstMAIHA = (10000/df$areacorr) * df$volumeTotal2EstMAI

    scatter.smooth(df$volumeTotal2HA, df$volumeTotal2EstMAIHA, main = paste(titulo, " total por ", mapeamento$parcela), xlab = labsX, ylab = labsy, pch = 18)
    abline(0, 1, lty="dashed")

    if (showTestF == TRUE) {
      coeficientes = lm(df$volumeTotal2EstMAIHA ~ df$volumeTotal2HA)
      mtext("Test F:", side = 3, line = 0.8)
      eval(parse(text = paste0("mtext(expression(paste(\n                                beta, \"0 = ",
                               round(coeficientes$coefficients[1], digits = 3),
                               " \",", "beta, \"1 = ", round(coeficientes$coefficients[2],
                                                             digits = 3), "\"", ")), side=3, line=-0.1)")))
    }
    if (!is.null(save)) {
      dev.off()
    }
  }
}
