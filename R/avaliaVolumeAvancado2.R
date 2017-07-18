## Copyright (C) 2016  Clayton Vieira Fraga Filho
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
##' @title evaluates Volume Advanced
##' @description this function performs an assessment of estimates of a variable as the forcefulness with expected
##' @param base data.frame with data in case of fragment base
##' @param basePredicao data.frame with data to use in ajust
##' @param baseProjecao data.frame with data to use in validation
##' @param mapeamento name of field eight and diameter
##' @param modelos list of exclusive for base models
##' @param salvar list of param to save the files
##' @param graficos list of param to plot graphics
##' @param estatisticas list of param to caclc estatistics
##' @param forcePredict force the calculation without using predict?
##' @param dividirEm how divide the base in training and validation
##' @param percentualDeTreino how many percent will stay in the training group?
##' @param agruparPor name field of base is group of individuals
##' @param fnCalculaVolume list of estatistics results
##' @param rmColsSuspicious remove fields b0, b1, b2, b2 ... of bases?
##' @return will be returned a result of statistics and ranking of volume
##' @import Fgmutils
##' @importFrom grDevices dev.new
##' @importFrom graphics par
##' @importFrom utils capture.output write.csv
##' @export
avaliaVolumeAvancado2 <- function (
  base = NULL,
  basePredicao = NULL,
  baseProjecao = NULL,
  mapeamento = list(dap1 = "dap1", dap2 = "dap2", ht1 = "ht1", ht2 = "ht2"),
  modelos = NULL,
  salvar = NULL,
  graficos = NULL,
  estatisticas = NULL,
  forcePredict = F,
  dividirEm = NULL,
  percentualDeTreino = 0.7,
  agruparPor = NULL,
  fnCalculaVolume = calculaVolumeDefault,
  rmColsSuspicious = F) {

  if (!is.null(basePredicao) && !is.null(baseProjecao))
    base = basePredicao
  if (is.null(base))
    stop("informe a base que deseja usar para extrair as informacoes")
  if (is.null(modelos))
    stop("informe modelos a serem avaliados vide ITGM::getModelosLiteraturaExclusivos()")
  save = dirDAP = dirHT = dirVOLUME = NULL
  if (!is.null(salvar)) {
    save = dirDAP = dirHT = dirVOLUME =  salvar$diretorio
  }
  if (!is.null(salvar) && !is.null(salvar$diretorioDAP))
    dirDAP = paste0(salvar$diretorio, salvar$diretorioDAP)
  if (!is.null(salvar) && !is.null(salvar$diretorioHT))
    dirHT = paste0(salvar$diretorio, salvar$diretorioHT)
  if (!is.null(salvar) && !is.null(salvar$diretorioVOLUME))
    dirVOLUME = paste0(salvar$diretorio, salvar$diretorioVOLUME)

  suspCols = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9")
  if (rmColsSuspicious){
    if (!is.null(base))
      base = removeColsSuspicious(base, intersect(names(base), suspCols))
    if (!is.null(basePredicao))
      basePredicao = removeColsSuspicious(basePredicao, intersect(names(basePredicao), suspCols))
    if (!is.null(baseProjecao))
      baseProjecao = removeColsSuspicious(baseProjecao, intersect(names(baseProjecao), suspCols))
  }

  colunas = c()
  if(!is.null(dividirEm)) colunas = union(colunas, dividirEm)
  if(!is.null(agruparPor)) colunas = union(colunas, agruparPor)

  for (i in 1:length(modelos)) colunas = union(colunas, getColumnsOfBase(base,
                                                                         modelos[[i]]()[[2]]))

  if (length((diff = intersect(colunas, suspCols))) > 0)
    stop(paste0("A base contem campos com nomes suspeitos: <",
                toString(diff), "> sao coeficientes de algum modelo? sugest: renomeie-os ou use rmColsSuspicious = T"))
  if (!is.null(baseProjecao) && length((diff = intersect(names(baseProjecao), suspCols))) > 0)
    stop(paste0("A base de projecao contem campos com nomes suspeitos: <",
                toString(diff), "> sao coeficientes de algum modelo? sugest: renomeie-os:
                \n baseProjecao2 = baseProjecao[, setdiff(names(baseProjecao),  c(\"", toString(diff), "\"))] ou use rmColsSuspicious = T"))
  bw = list()
  eval(parse(text = paste0("bw$", union(colunas, mapeamento),
                           " = base$", union(colunas, mapeamento))))
  baseW = as.data.frame(bw)

  baseTreino = basePredicao
  if (is.null(basePredicao) || is.null(baseProjecao)){
    dftv = separaDados(baseW, dividirEm, percTraining = percentualDeTreino)
    baseTreino = dftv$treino
    baseProjecao = dftv$validacao
  }
  ranking = data.frame(b0 = double(), b1 = double(), rankingB0 = double(),
                       rankingB1 = double())
  dfestatisticas = list()
  nomes = list()
  if(!is.null(agruparPor)){
    volumesPreditos = data.frame(k = baseProjecao[, agruparPor])
    names(volumesPreditos)[[1]] = agruparPor
  }

  estatisticasV = estatisticasVAj = estatisticas ##estatisitcas do volume
  estatisticasV$baseDoAjuste = baseProjecao ##base para obtencao dos campos
  estatisticasVAj$baseDoAjuste = baseTreino

  if (!is.null(graficos) && !is.null(graficos$funcoes)) {
    rw = length(modelos)
    cl = 6 * length(graficos$funcoes)
    for (i in 1:length(graficos$funcoes))
      if (all.equal(graficos$funcoes[[i]], getggplot2GraphicObservadoXEstimado) == T) {
        cl = cl - 6
        break;
      }
    if (cl > 0 && is.null(save)){
      dev.new(width = cl*2, height = rw * 2)
      par(mfrow = c(rw, cl))
    }
    nms = c()
    for(nm in 1:(6*rw))
      nms = union(nms, paste0("g", nm))
    j = 1
  }

  comparativoAjuste = list()
  comparativoAjuste$estatisticasComparativoGeral = list(
    estatisticas = data.frame(
      DAP_OBSERVADO = basePredicao[, mapeamento$dap2],
      HT_OBSERVADO = basePredicao[, mapeamento$ht2],
      VOLUME_OBSERVADO = fnCalculaVolume(dap = basePredicao[, mapeamento$dap2], ht = basePredicao[, mapeamento$ht2])),
    estatisticasDoModelo = data.frame())
  comparativoAjuste$estatisticasComparativoDAP = list(
    estatisticas = data.frame(DAP_OBSERVADO = basePredicao[, mapeamento$dap2]),
    estatisticasDoModelo = data.frame())
  comparativoAjuste$estatisticasComparativoHT = list(
    estatisticas = data.frame(HT_OBSERVADO = basePredicao[, mapeamento$ht2]),
    estatisticasDoModelo = data.frame())
  comparativoAjuste$estatisticasComparativoVolume = list(
    estatisticas = data.frame(
      VOLUME_OBSERVADO = comparativoAjuste$estatisticasComparativoGeral$estatisticas$VOLUME_OBSERVADO),
    estatisticasDoModelo = data.frame())

  comparativoVal = list()
  comparativoVal$estatisticasComparativoGeral = list(
    estatisticas = data.frame(
      DAP_OBSERVADO = baseProjecao[, mapeamento$dap2],
      HT_OBSERVADO = baseProjecao[, mapeamento$ht2],
      VOLUME_OBSERVADO = fnCalculaVolume(dap = baseProjecao[, mapeamento$dap2], ht = baseProjecao[, mapeamento$ht2])),
    estatisticasDoModelo = data.frame())
  comparativoVal$estatisticasComparativoDAP = list(
    estatisticas = data.frame(DAP_OBSERVADO = baseProjecao[, mapeamento$dap2]),
    estatisticasDoModelo = data.frame())
  comparativoVal$estatisticasComparativoHT = list(
    estatisticas = data.frame(HT_OBSERVADO = baseProjecao[, mapeamento$ht2]),
    estatisticasDoModelo = data.frame())
  comparativoVal$estatisticasComparativoVolume = list(
    estatisticas = data.frame(
      VOLUME_OBSERVADO = comparativoVal$estatisticasComparativoGeral$estatisticas$VOLUME_OBSERVADO),
    estatisticasDoModelo = data.frame())


  for (i in 1:length(modelos)) {
    modelo = modelos[[i]]
    estatisticasV$formula = estatisticasVAj$formula =  modelo()[[2]]
    nomes[i] = gsub("\\s", "", modelo()[[1]], perl = T)
    dirToSaveVol = dirToSaveVolvalidacao = dirToSaveDAP = dirToSaveHT = dirToSaveDAPvalidacao = dirToSaveHTvalidacao =  NULL

    ##id (ajuste/validacao).(nome do modelo).(dap/ht/volume)
    id = gsub("[.]", "*", modelo()[[1]], perl = T)

    if (!is.null(save)){
      dirToSaveDAP = paste0(dirDAP, nomes[i], "/")
      dirToSaveHT = paste0(dirHT, nomes[i], "/")
      dirToSaveVol = paste0(dirVOLUME, nomes[i], "/")
      dirToSaveDAPvalidacao = paste0(dirToSaveDAP, "validacao/")
      dirToSaveHTvalidacao = paste0(dirToSaveHT, "validacao/")
      dirToSaveVolvalidacao = paste0(dirToSaveVol, "validacao/")
    }

    grafic1 = grafic2 = grafic3 =  grafic4 =  grafic5 = grafic6 = graficos
    if (!is.null(graficos)) {

      if (!is.null(save)){
        grafic1$save = dirToSaveDAP
        grafic2$save = dirToSaveHT
        grafic3$save = dirToSaveVolvalidacao
        grafic4$save = dirToSaveDAPvalidacao
        grafic5$save = dirToSaveHTvalidacao
        grafic6$save = dirToSaveVol
      }

      grafic1$titulo = grafic1$nome = paste0(nomes[i],
                                             " DAP Ajuste")
      grafic2$titulo = grafic2$nome = paste0(nomes[i],
                                             " HT Ajuste")
      grafic3$titulo = grafic3$nome = paste0(nomes[i],
                                             " Volume Validacao")
      grafic6$titulo = grafic6$nome = paste0(nomes[i],
                                             " Volume Ajuste")
      grafic4$titulo = grafic4$nome = paste0(nomes[i],
                                             " DAP Validacao")
      grafic5$titulo = grafic5$nome = paste0(nomes[i],
                                             " HT Validacao")

      grafic1$desc = paste0('ajuste.', id, ".dap")
      grafic2$desc = paste0('ajuste.', id, ".ht")
      grafic6$desc = paste0('ajuste.', id, ".volume")
      grafic4$desc = paste0('validacao.', id, ".dap")
      grafic5$desc = paste0('validacao.', id, ".ht")
      grafic3$desc = paste0('validacao.', id, ".volume")

      grafic1$data = grafic2$data = grafic6$data = baseTreino
      grafic3$data = grafic4$data = grafic5$data = baseProjecao

      grafic1$nomeParaExibir = nms[[j]]
      grafic4$nomeParaExibir = nms[[j + 1]]
      grafic2$nomeParaExibir = nms[[j + 2]]
      grafic5$nomeParaExibir = nms[[j + 3]]
      grafic3$nomeParaExibir = nms[[j + 4]]
      grafic6$nomeParaExibir = nms[[j + 5]]
      j = j + 6
    }


    estatistica =
      avaliaEstimativas(
        observado = comparativoVal$estatisticasComparativoGeral$estatisticas$VOLUME_OBSERVADO,
        estimado = (comparativoVal$estatisticasComparativoGeral$estatisticas[,paste0("VOLUME_ESTIMADO_", nomes[i])] =
                      (comparativoVal$estatisticasComparativoVolume$estatisticas[,paste0("VOLUME_ESTIMADO_", nomes[i])]=
                         (fnCalculaVolume(dap = ((ajusteDAPVAL = avaliaEstimativas(observado = baseProjecao[, mapeamento$dap2],
                                                                                   estimado =
                                                                                     (comparativoVal$estatisticasComparativoGeral$estatisticas[,paste0("DAP_ESTIMADO_", nomes[i])] =
                                                                                        (comparativoVal$estatisticasComparativoDAP$estatisticas[,paste0("DAP_ESTIMADO_", nomes[i])] =
                                                                                           (predizer(
                                                                                             ajuste = (ajusteDAP = (avaliaEstimativas(observado = baseTreino[, mapeamento$dap2],
                                                                                                                                      estimado = NULL,
                                                                                                                                      ajuste = modelo(y1 = mapeamento$dap1, y2 = mapeamento$dap2, base = baseTreino),
                                                                                                                                      graficos = grafic1,
                                                                                                                                      estatisticas = estatisticas,
                                                                                                                                      salvarEm = dirToSaveDAP,
                                                                                                                                      nome = paste0("ajuste DAP ", nomes[i]))))$ajuste,
                                                                                             newdata = baseProjecao,
                                                                                             force = forcePredict)))),
                                                                                   graficos = grafic4,
                                                                                   estatisticas = estatisticasV,
                                                                                   salvarEm = dirToSaveDAPvalidacao,
                                                                                   nome = paste0("validacao DAP ", nomes[i])))$estimado),
                                          ht = ((ajusteHTVAL = avaliaEstimativas(observado = baseProjecao[, mapeamento$ht2],
                                                                                 estimado =
                                                                                   (comparativoVal$estatisticasComparativoGeral$estatisticas[,paste0("HT_ESTIMADO_", nomes[i])] =
                                                                                      (comparativoVal$estatisticasComparativoHT$estatisticas[,paste0("HT_ESTIMADO_", nomes[i])] =
                                                                                         (predizer(
                                                                                           ajuste = (ajusteHT = (avaliaEstimativas(observado = baseTreino[, mapeamento$ht2],
                                                                                                                                   estimado = NULL,
                                                                                                                                   ajuste = modelo(y1 = mapeamento$ht1, y2 = mapeamento$ht2, base = baseTreino),
                                                                                                                                   graficos = grafic2,
                                                                                                                                   estatisticas = estatisticas,
                                                                                                                                   salvarEm = dirToSaveHT,
                                                                                                                                   nome = paste0("ajuste HT ", nomes[i]))))$ajuste,
                                                                                           newdata = baseProjecao,
                                                                                           force = forcePredict)))),
                                                                                 graficos = grafic5,
                                                                                 estatisticas = estatisticasV,
                                                                                 salvarEm = dirToSaveHTvalidacao,
                                                                                 nome = paste0("validacao HT ", nomes[i])))$estimado))))),
        graficos = grafic3,
        estatisticas = estatisticasV,
        salvarEm = dirToSaveVolvalidacao,
        nome = paste0("validacao Volume ", nomes[i]))

    estatistica$estatisticas$ajusteDAP = ajusteDAP$ajuste
    estatistica$estatisticas$ajusteHT = ajusteHT$ajuste
    estatt = estatistica$estatisticas$estatisticas
    estatistica$estatisticas$estatisticas = cbind(baseProjecao,
                                                  estatistica$estatisticas$estatisticas)
    eval(parse(text = paste0("dfestatisticas$", nomes[i],
                             " = estatistica")))
    if(!is.null(agruparPor))
      eval(parse(text = paste0("volumesPreditos$", nomes[i],
                               " = estatistica$estimado")))
    ranking = rbind(ranking, estatistica$ranking)
    comparativoAjuste$estatisticasComparativoGeral$estatisticas[,paste0("DAP_ESTIMADO_", nomes[i])] = ajusteDAP$estimado
    comparativoAjuste$estatisticasComparativoGeral$estatisticas[,paste0("HT_ESTIMADO_", nomes[i])] = ajusteHT$estimado
    comparativoAjuste$estatisticasComparativoGeral$estatisticas[,paste0("VOLUME_ESTIMADO_", nomes[i])] = fnCalculaVolume(dap = ajusteDAP$estimado, ht = ajusteHT$estimado)
    comparativoAjuste$estatisticasComparativoDAP$estatisticas[,paste0("DAP_ESTIMADO_", nomes[i])] = ajusteDAP$estimado
    comparativoAjuste$estatisticasComparativoHT$estatisticas[,paste0("HT_ESTIMADO_", nomes[i])] = ajusteDAP$estimado
    comparativoAjuste$estatisticasComparativoVolume$estatisticas[,paste0("VOLUME_ESTIMADO_", nomes[i])]= fnCalculaVolume(dap = ajusteDAP$estimado, ht = ajusteHT$estimado)

    estatt2 = avaliaEstimativas(observado = comparativoAjuste$estatisticasComparativoGeral$estatisticas$VOLUME_OBSERVADO,
                                estimado = comparativoAjuste$estatisticasComparativoGeral$estatisticas[,paste0("VOLUME_ESTIMADO_", nomes[i])],
                                estatisticas = estatisticasVAj,
                                salvarEm = dirToSaveVol,
                                graficos = grafic6,
                                nome = paste0("ajuste Volume ", nomes[i]))

    for(name in names(estatt2$estatisticas$estatisticas)){
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoGeral$estatisticas$", name, "_", nomes[i], " = estatt2$estatisticas$estatisticas", name)))
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoVolume$estatisticas$", name, "_", nomes[i], " = estatt2$estatisticas$estatisticas", name)))
    }

    for(name in names(ajusteDAP$estatisticas$estatisticas)){
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoGeral$estatisticas$DAP_", name, "_", nomes[i], " = ajusteDAP$estatisticas$estatisticas$", name)))
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoDAP$estatisticas$", name, "_", nomes[i], " = ajusteDAP$estatisticas$estatisticas$", name)))
    }

    for(name in names(ajusteHT$estatisticas$estatisticas)){
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoGeral$estatisticas$HT_", name, "_", nomes[i], " = ajusteHT$estatisticas$estatisticas$", name)))
      eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoHT$estatisticas$", name, "_", nomes[i], " = ajusteHT$estatisticas$estatisticas$", name)))
    }


    if (i == 1){
      comparativoAjuste$estatisticasComparativoGeral$estatisticasDoModelo = data.frame(name = estatt2$estatisticas$estatisticasDoModelo$name)
      comparativoAjuste$estatisticasComparativoVolume$estatisticasDoModelo = data.frame(name = estatt2$estatisticas$estatisticasDoModelo$name)
      comparativoAjuste$estatisticasComparativoDAP$estatisticasDoModelo = data.frame(name = ajusteDAP$estatisticas$estatisticasDoModelo$name)
      comparativoAjuste$estatisticasComparativoHT$estatisticasDoModelo = data.frame(name = ajusteHT$estatisticas$estatisticasDoModelo$name)
    }


    eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoGeral$estatisticasDoModelo$value_", nomes[i],
                             " = estatt2$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoDAP$estatisticasDoModelo$value_", nomes[i],
                             " = ajusteDAP$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoHT$estatisticasDoModelo$value_", nomes[i],
                             " = ajusteHT$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoAjuste$estatisticasComparativoVolume$estatisticasDoModelo$value_", nomes[i],
                             " = estatt2$estatisticas$estatisticasDoModelo$value")))



    for(name in names(estatt)){
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoGeral$estatisticas$", name, "_", nomes[i], " = estatt$", name)))
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoVolume$estatisticas$", name, "_", nomes[i], " = estatt$", name)))
    }

    for(name in names(ajusteDAPVAL$estatisticas$estatisticas)){
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoGeral$estatisticas$DAP_", name, "_", nomes[i], " = ajusteDAPVAL$estatisticas$estatisticas$", name)))
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoDAP$estatisticas$", name, "_", nomes[i], " = ajusteDAPVAL$estatisticas$estatisticas$", name)))
    }

    for(name in names(ajusteHTVAL$estatisticas$estatisticas)){
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoGeral$estatisticas$HT_", name, "_", nomes[i], " = ajusteHTVAL$estatisticas$estatisticas$", name)))
      eval(parse(text = paste0("comparativoVal$estatisticasComparativoHT$estatisticas$", name, "_", nomes[i], " = ajusteHTVAL$estatisticas$estatisticas$", name)))
    }

    if (i == 1){
      comparativoVal$estatisticasComparativoGeral$estatisticasDoModelo = data.frame(name = estatistica$estatisticas$estatisticasDoModelo$name)
      comparativoVal$estatisticasComparativoVolume$estatisticasDoModelo = data.frame(name = estatistica$estatisticas$estatisticasDoModelo$name)
      comparativoVal$estatisticasComparativoDAP$estatisticasDoModelo = data.frame(name = ajusteDAPVAL$estatisticas$estatisticasDoModelo$name)
      comparativoVal$estatisticasComparativoHT$estatisticasDoModelo = data.frame(name = ajusteHTVAL$estatisticas$estatisticasDoModelo$name)
    }


    eval(parse(text = paste0("comparativoVal$estatisticasComparativoGeral$estatisticasDoModelo$value_", nomes[i],
                             " = estatistica$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoVal$estatisticasComparativoDAP$estatisticasDoModelo$value_", nomes[i],
                             " = ajusteDAPVAL$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoVal$estatisticasComparativoHT$estatisticasDoModelo$value_", nomes[i],
                             " = ajusteHTVAL$estatisticas$estatisticasDoModelo$value")))
    eval(parse(text = paste0("comparativoVal$estatisticasComparativoVolume$estatisticasDoModelo$value_", nomes[i],
                             " = estatistica$estatisticas$estatisticasDoModelo$value")))
  }


  salvarComparativo <- function(comparativoAjuste, comparativoVal, local, prefixo){
    write.csv(x = comparativoAjuste$estatisticas, file = paste0(local, "comparativo ( ", prefixo, " ) de AJUSTE de modelos - estatisticas.csv"))
    write.csv(x = comparativoAjuste$estatisticasDoModelo, file = paste0(local, "comparativo ( ", prefixo, " ) de AJUSTE de modelos - estatistica dos modelos.csv"))
    write.csv(x = comparativoVal$estatisticas, file = paste0(local, "comparativo ( ", prefixo, " ) da VALIDACAO dos modelos - estatisticas.csv"))
    write.csv(x = comparativoVal$estatisticasDoModelo, file = paste0(local, "comparativo ( ", prefixo, " ) da VALIDACAO dos modelos - estatistica dos modelos.csv"))
  }

  if(!is.null(save))
    salvarComparativo(comparativoAjuste$estatisticasComparativoGeral, comparativoVal$estatisticasComparativoGeral, save, "GERAL")
  if(!is.null(dirDAP))
    salvarComparativo(comparativoAjuste$estatisticasComparativoDAP, comparativoVal$estatisticasComparativoDAP, dirDAP, "DAP")
  if(!is.null(dirHT))
    salvarComparativo(comparativoAjuste$estatisticasComparativoHT, comparativoVal$estatisticasComparativoHT, dirHT, "HT")
  if(!is.null(dirVOLUME))
    salvarComparativo(comparativoAjuste$estatisticasComparativoVolume, comparativoVal$estatisticasComparativoVolume, dirVOLUME, "VOLUME")

  rownames(ranking) = nomes
  ranking = ranking[order(ranking$rankingB0, ranking$rankingB1),]
  ranking$rank = list(1:nrow(ranking))[[1]]
  rank = ranking[, c("rank", "b0", "b1")]
  if (!is.null(salvar))
    capture.output(rank, file = paste0(salvar$diretorio,
                                       " rank Test F.txt"))
  print(rank)

  if(!is.null(agruparPor)){
    str = paste0("SELECT ", agruparPor, ", SUM(", nomes[1], ") AS '",
                 nomes[1], "'")
    if (2 <= length(nomes))
      for (i in 2:length(nomes)) str = paste0(str, ", SUM(",
                                              nomes[i], ") AS '", nomes[i], "'")
      str = paste0(str, "from volumesPreditos GROUP BY ", agruparPor)
      if (!is.null(save)){
        write.csv(x = basePredicao, file = paste0(save, "basePredicao.csv"))
        write.csv(x = baseProjecao, file = paste0(save, "baseProjecao.csv"))
      }
      print("agrupando...")
      volumesPreditos = sqldf(str)

      print("atribuindo...")
      data = atualizaCampoBase(camposAtualizar = setdiff(names(volumesPreditos),
                                                         agruparPor), baseAgrupada = volumesPreditos, baseAtualizar = baseW,
                               keys = agruparPor)
  }else{
    data = list(predicao = baseTreino, projecao = baseProjecao)
  }

##  if (!is.null(graficos)) {
 ##  if (cl > 0){
  ##    x11(width = cl*2, height = rw * 2)
  ##    par(mfrow = c(1, 1))
  ##  }
  ##  for (i in 1:length(graficos$funcoes))
  ##    if (all.equal(graficos$funcoes[[i]], getggplot2GraphicObservadoXEstimado) == T) {
  ##      require(gridExtra)
  ##      eval(parse(text =
   ##                  paste("grid.arrange(", toString(nms), ", ncol=", 6, ", nrow=", rw, ")")))
   ##     break;
    #  }
## }
  return(list(estatisticas = dfestatisticas, base = data, ranking = rank, comparativoAjuste = comparativoAjuste, comparativoValidacao = comparativoVal))
}
