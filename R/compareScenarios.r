#' Compare scenarios
#'
#' The function builds a rodeo-based model and runs it for one
#' or multiple scenarios. Individual scenarios can differ by the
#' initial state or the values of parameters.
#'
#' @param workbook File path of the worksheet holding the model. See the
#'   \code{\link{buildFromWorkbook}} for details on the expected file
#'   contents. See below for further details.
#' @param scenarios Character vector with each element defining the name
#'   of a 'scenario'. See details below.
#' @param times Numeric vector defining the times for which the future
#'   state of the system is computed.
#' @param plot.vars Logical. Plot the dynamics of all state variables?
#' @param plot.pros Logical. Plot the dynamics of process rates?
#' @param leg Keyword to set the position of the legend (if plots are created).
#'
#' @return A data frame with at least three columns and one row for each
#'   time requested via the \code{times} argument. The first column
#'   indicates the scenario, the second column holds the time, and further
#'   columns hold the values of state variables and process rates.
#'
#' @note For each scenario listed in the vector \code{scenarios}, the sheet
#'    'vars' in \code{workbook} must contain a designated column. The name
#'    of that column must be 'value.' followed by the scenario identifier,
#'    for example 'value.default' if the scenario was titled 'default'.
#'    The same applies to the sheet 'pars' in \code{workbook}.
#'
#' @seealso Look at \code{\link{buildFromWorkbook}} for building a model
#'   without running it immediately.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' x <- compareScenarios(
#'   system.file("models/oxygen.xlsx", package="rodeoEasy"),
#'   scenarios=c("default","alternative"),
#'   times=seq(0, 12, 0.1),
#'   plot.vars=TRUE
#')

compareScenarios <- function(workbook, scenarios, times,
  plot.vars=FALSE, plot.pros=FALSE, leg="topright") {
  # check inputs
  if (length(scenarios) < 1)
    stop("'scenarios' must be a character vector of length >= 1")  
  if ((length(times) < 2) || (!is.numeric(times)))
    stop("'times' must be a numeric vector of length >= 2")  
  # create model
  m <- buildFromWorkbook(workbook)
  # initialize results
  out <- NULL
  # process all scenarios
  for (s in scenarios) {
    # set initial values
    x <- m$getVarsTable()
    colname <- paste0("value.",s)
    if (!colname %in% names(x))
      stop(paste0("missing column '",colname,"' in sheet 'vars'",
        " of workbook '",workbook,"'")) 
    m$setVars(stats::setNames(x[,colname], x[,"name"]))
    # set parameters
    x <- m$getParsTable()
    if (!colname %in% names(x))
      stop(paste0("missing column '",colname,"' in sheet 'pars'",
        " of workbook '",workbook,"'")) 
    m$setPars(stats::setNames(x[,colname], x[,"name"]))
    # simulate
    out <- rbind(out, data.frame(scenario=s,
      m$dynamics(times=times, fortran=FALSE)))
  }
  # plot if requested
  if (plot.vars || plot.pros) {
    items <- c(if (plot.vars) m$namesVars() else c(),
      if (plot.pros) m$namesPros() else c())
    ncells <- ceiling(sqrt(length(items)))
    graphics::par(mfrow=c(ncells, ncells))
    clr <- stats::setNames(grDevices::colorRampPalette(c("steelblue4","khaki",
      "darkorange"))(length(scenarios)), scenarios)
    for (it in items) {
      plot(range(times), range(out[,it]), type="n", xlab="time", ylab=it) 
      fn <- function(x, item) {
        graphics::lines(x[,"time"], x[,item], col=clr[unique(x[,"scenario"])])
      }
      by(out, out[,"scenario"], fn, item=it)
    }
    graphics::legend(leg, bty="n", lty=1, col=clr, legend=names(clr))
    graphics::par(mfrow=c(1,1))
  }
  # return simulation output for further processing
  out
}
