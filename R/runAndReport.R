##' @title Evaluate avaliation and report result
##' @description This function tries to execute the script and stores the result of the execution in a report script is a string that contains the script in R to be executed report is the file to be saved with the report the function saves a file in the local directory with report name, Which is a JSON relatory.
##' @param script It is the script to evaluate
##' @param report is the name of file to save reports
##' @param environment the environment to evaluate script
##' @param showWarnings show thrown warnings?
##' @return will be returned implicit var
##' @export
runAndReport <- function (script, report, environment, showWarnings = F)
{
  if (showWarnings) {
    write(paste0("{\"call\":\"", gsub("\n", ";", gsub("\"", "'",
                                                      script)), "\",\"start\":\"", format(Sys.time(), "%a %d %b %Y %X"),
                 "\""), file = report)
    result = tryCatch({
      ptm <- proc.time()
      eval(parse(text = script), envir = environment)
    },
    warning = function(w) {
      write(paste0(",\"warning-time\":\"", format(Sys.time(),
                                                  "%a %d %b %Y %X"), "\",\"warning\":\"", gsub("\n",
                                                                                               " ", gsub("\"", "'", w)), "\""), file = report, append = T)
    },
    error = function(e) {
      write(paste0(",\"error-time\":\"", format(Sys.time(),
                                                "%a %d %b %Y %X"), "\",\"error\":\"", gsub("\n",
                                                                                           " ", gsub("\"", "'", e)), "\""), file = report, append = T)
    }, finally = {
      k = (proc.time() - ptm)
      write(paste0(",\"end\":\"", format(Sys.time(), "%a %d %b %Y %X"),
                   "\", ", gsub(" ", "", toString(paste("\"", names(k),
                                                        "\":\"", strsplit(toString(k), ",")[[1]], "\""))),
                   "}"), file = report, append = T)
    })
  } else {

    write(paste0("{\"call\":\"", gsub("\n", ";", gsub("\"", "'",
                                                      script)), "\",\"start\":\"", format(Sys.time(), "%a %d %b %Y %X"),
                 "\""), file = report)
    result = tryCatch({
      ptm <- proc.time()
      eval(parse(text = script), envir = environment)
    },
    error = function(e) {
      write(paste0(",\"error-time\":\"", format(Sys.time(),
                                                "%a %d %b %Y %X"), "\",\"error\":\"", gsub("\n",
                                                                                           " ", gsub("\"", "'", e)), "\""), file = report, append = T)
    }, finally = {
      k = (proc.time() - ptm)
      write(paste0(",\"end\":\"", format(Sys.time(), "%a %d %b %Y %X"),
                   "\", ", gsub(" ", "", toString(paste("\"", names(k),
                                                        "\":\"", strsplit(toString(k), ",")[[1]], "\""))),
                   "}"), file = report, append = T)
    })

  }
}
