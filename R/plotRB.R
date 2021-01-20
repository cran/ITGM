##' @title Get rbokeh grapic observed versus estimated
##' @description this function displays/saves/returns a Graphical rbokeh illustrating the difference between the observed and estimated
##' @param titulo is the title graphic
##' @param nome name of file case save
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param id list containing 'id' of individuals
##' @param save If you want to save enter the directory as a string
##' @param labsX label x
##' @param labsy label y
##' @param showTestF draw results of test F in graphic?
##' @param colID label of ids
##' @param sobnome optional name to append of file name graphic
##' @param data database to retireve fields to id
##' @param diretorio database to retireve fields to id
##' @param datadiretorio optional abstract path of directory
##' @param HTTPS script in https?
##' @param totalizar generat graphic of volume total
##' @param campoID name of parcela field in data
##' @param mapeamento name of fields on base
##' @param ... optionals params to plot graphic in ITGM
##' @return will be returned a rbokeh graphic
##' @import rbokeh
##' @import randomcoloR
##' @importFrom stats lm smooth.spline
##' @export
plotRB <- function(
  titulo = "observadoXestimado",
  nome = "observadoXestimado",
  observado,
  estimado,
  id = NULL,
  save = NULL,
  labsX = "observado",
  labsy = "estimado",
  showTestF = TRUE,
  colID = 'idLabel',
  sobnome = "ObservadoXEstimado",
  data = NULL,
  diretorio = NULL,
  datadiretorio = NULL,
  HTTPS = FALSE,
  totalizar = NULL,
  campoID = NULL,
  mapeamento = list(idade2 = "idadearred2", parcela = "parcela", areacorr = "areacorr"),
  ...) {

  if (is.null(campoID) && !is.null(id) && !is.null(data)) {
    for(name in names(data))
      ifelse(FALSE %in% (data[,name] == id), "", {campoID = name;})
  }

  if(is.null(campoID) && !is.null(id) && !is.null(data))
    stop("INFORME ID na variavel data! impossivel obter campo ID! PLOTBR")

  if(!is.null(campoID)) {
    id = data[, campoID]
    if(colID == 'idLabel')
      colID = campoID
  }

  if (!is.null(data) && !is.null(totalizar)) {

    data$observado = observado
    data$estimado = estimado
    data$idade = data[, mapeamento$idade2]
    cp = ifelse(is.null(campoID), "", { paste0(campoID, ", "); })
    df = sqldf(paste("SELECT ", cp, mapeamento$parcela, " AS parcela, idade, SUM(observado) AS volumeTotal2, SUM(estimado) AS volumeTotal2EstMAI, ", mapeamento$areacorr, " AS areacorr FROM data GROUP By ", mapeamento$parcela, ", idade"))
    df$volumeTotal2HA = (10000/df$areacorr) * df$volumeTotal2
    df$volumeTotal2EstMAIHA = (10000/df$areacorr) * df$volumeTotal2EstMAI

    observado = df$volumeTotal2HA
    estimado = df$volumeTotal2EstMAIHA
    if(!is.null(campoID)){
      id = df[, campoID]
    }
  }

  p <- figure(title = titulo, width = 500, height = 500, xlab = labsX, ylab = labsy)

  if (showTestF) {
    p <- ly_abline(fig = p, lm(estimado ~ observado), type = 2, legend = "teste F", color="blue")
  }

  if (is.null(id)) {
    p <- ly_points(fig = p, observado, estimado, hover = eval(parse(text = paste('list(', labsX,'= observado, ', labsy, ' = estimado)'))))
  }else{
    dftmp = data.frame(x = unique(id))
    dftmp$y = randomColor((length(dftmp$x)), luminosity = c("dark"))
    cores = sapply(id, function(X) { return(dftmp$y[dftmp$x == X]) })
    p <- ly_points(fig = p, observado, estimado, fill_color=cores, fill_alpha = 0.6, line_color = cores, line_alpha = 1, hover = eval(parse(text = paste('list(', labsX,'= observado, ', labsy, ' = estimado, ', colID, ' = id )'))))
  }

  p <- ly_abline(fig = p, 0, 1, width = 2) %>%
    ly_lines(smooth.spline(observado,estimado), line_width=5, line_alpha=0.5, line_join="round", color="blue") %>%
    tool_box_select() %>%
    tool_lasso_select()

  if (!is.null(save)){

    opcoes =base = dir = func = ''

    lab = '<script type="text/javascript">\n'
    divs = '<table style="position: absolute; top: 30px; left: 520px;">\n<tbody>\n<tr style="vertical-align: baseline;">\n<td>Id:</td><td id="select">\n</td>\n</tr>\n<tr style="vertical-align: baseline;">\n<td>Id:</td><td id="select2">\n</td>\n</tr>\n<tr style="vertical-align: baseline;">\n<td>\n<div id="checkboxes"></div>\n</td>\n'

    if (!is.null(data)){
      f=  paste0(save, 'base.rbokeh.csv')
      if (is.null(data$observado) || is.null(data$estimado)) {
        d2 = cbind(data, data.frame(observado = observado, estimado = estimado))
        write.csv(file = f, x = d2)
      }
      opcoes = paste0("var opcoes = ['", gsub(", ", "', '", toString(names(data))), "'];\n")

      if(!is.null(datadiretorio))
        f = datadiretorio
      base = paste0("var database = '", f, "';\n")
      dir = paste0("var diretorio = '", diretorio, "';\n")
      func = paste0('var funcao = \'titulo = "', titulo,
                    '", labsX = "', labsX,
                    '", labsy = "', labsy,
                    '", colID = "', colID,
                    '", showTestF = ', toString(showTestF), '\';\n')
      lab = paste0(lab, opcoes, base, dir, func)
    }

    if (!is.null(id)) {
      lab = paste0(lab, 'var label = {', toString(paste0('"', cores, '":"', id, '"')), '};\n')
      if (length(id) > 19)
        for (k in 1:(length(id) %/% 20)){
          divs = paste0(divs, '<td><div id="checkboxes-', k, '"></div></td>\n')
        }
    }

    lab = paste0(lab, '</script>\n')

    divs = paste0(divs, '</tr>\n</tbody>\n</table>\n')

    m = (rbokeh::renderRbokeh(p))()
    elID = gsub("\".*", "", gsub(".*elementid\":\"", "", m))
    mdID = gsub("\".*", "", gsub(".*modelid\":\"", "", m))
    dcID = gsub("\".*", "", gsub(".*docid\":\"", "", m))
    jSon = gsub(".*\",\"docs_json\"", "", m)
    sc <- ifelse(HTTPS, "s", "")
    scp <- ifelse(HTTPS, "9", "8")
    str = paste0("<!DOCTYPE html>\n<html>\n<head>\n
		<script src='http", sc ,"://cdn.pydata.org/bokeh/release/bokeh-0.12.2.min.js'></script>\n
		<script src='http", sc ,"://itgm.mikeias.net:809", scp, "/js/bokeh.js'></script>\n
		<link href='http", sc ,"://cdn.pydata.org/bokeh/release/bokeh-0.12.2.min.css' rel='stylesheet'></head>\n",
                 lab,
                 '\n<body>\n<div id="htmlwidget_container">\n<div id="htmlwidget-',
                 elID , '" class="rbokeh html-widget" style=" width: 500px; height: 500px;">\n</div>\n</div>\n', divs ,'
		<script type="application/json" data-for="htmlwidget-',
                 elID , '">{"x":{"elementid":"', elID , '","modeltype":"Plot","modelid":"',
                 mdID , '","docid":"', dcID , '","docs_json"', jSon, '</script>\n</body>\n</html>')

    cat(str, file=paste0(save, nome, " ", sobnome, ".html"))
  }

  return (p)
}
