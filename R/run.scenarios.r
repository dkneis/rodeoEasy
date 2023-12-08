#' Run a model, possibly for multiple scenarios
#'
#' The function executes a model for one to many scenarios and optionally
#' visualizes the outcome. Individual scenarios can differ by the initial state
#' or the values of parameters.
#'
#' @param model A model object returned by the \code{\link{build}} function.
#'   See the examples below.
#' @param scenarios Either \code{NULL} or a named list, each element of which
#'   defines a scenario to be considered. In the latter case, list elements must
#'   be (named) numeric vectors used to update the initial values and parameters
#'   provided as defaults in the workbook from which the model was imported.
#'   The vectors for the individual scenarios can differ in length but their
#'   length could also be consistent across scenarios (so you could also pass a
#'   \code{data.frame}). If \code{scenarios} is set to \code{NULL}, only the
#'   default scenario will be run. See details and examples.
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
#' @note If the \code{scenarios} argument is used to update initial values and /
#' or parameters, the following applies: For each parameter not being included
#'   (by name) in the vector for a particular scenario, the default value
#'   will be used (as stored in the workbook from which the model was imported).
#'   The same is true for the initial values of variables. See the examples
#'   below.
#'
#' @seealso Look at \code{\link{build}} for how to create the necessary input
#'   for the \code{model} argument.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' # build the model
#' m <- build(system.file("models/oxygen.xlsx", package="rodeoEasy"))
#' 
#' # run with defaults
#' x <- run.scenarios(model=m, times=seq(0, 12, 0.1),
#'   scenarios=NULL
#' )
#' 
#' # run with updated values 
#' x <- run.scenarios(model=m, times=seq(0, 12, 0.1),
#'   scenarios=list(warm=c(kd=1.25), cold.windy=c(kd=1, ka=5))
#' )

run.scenarios <- function(model, times, scenarios=NULL,
  plot.vars=TRUE, plot.pros=FALSE, leg="topright") {
  # check inputs
  if (class(model)[1] != "rodeo")
    stop("'model' not of class 'rodeo'; Did you run 'build'?")
  if (!(is.null(scenarios) || is.list(scenarios)))
    stop("'scenarios' must be a list or NULL")
  all.named <- function(x) {!is.null(names(x)) && all(names(x) != "")}
  if (is.list(scenarios) && !all.named(scenarios))
    stop("if 'scenarios' is a list, elements must be named")
  if (is.list(scenarios) && !all(sapply(scenarios, is.numeric)))
    stop("if 'scenarios' is a list, elements must be numeric vectors")
  if (is.list(scenarios) && !all(sapply(scenarios, all.named)))
    stop("if 'scenarios' is a list of numeric vectors, all vector elements must be named")
  if ((length(times) < 2) || (!is.numeric(times)))
    stop("'times' must be a numeric vector of length >= 2")  
  # get defaults
  v.def <- stats::setNames(model$getVarsTable()[,"default"], model$getVarsTable()[,"name"])
  p.def <- stats::setNames(model$getParsTable()[,"default"], model$getParsTable()[,"name"])
  # initialize results
  out <- NULL
  # process scenarios
  if (is.null(scenarios)) {
    model$setVars(v.def)
    model$setPars(p.def)
    out <- rbind(out, data.frame(scenario="default",
      model$dynamics(times=times, fortran=FALSE)))
  } else {
    for (s in names(scenarios)) {
      # set defaults first
      v.scn <- v.def
      p.scn <- p.def
      # update initial values
      for (item.name in names(scenarios[[s]])) {
        if (item.name %in% names(p.scn)) {
          p.scn[item.name] <- scenarios[[s]][[item.name]]
        } else if (item.name %in% names(v.scn)) {
          v.scn[item.name] <- scenarios[[s]][[item.name]]
        } else {
          stop(paste0("a value has been specified for item '",item.name,
            "' in scenario '",s,"' but the model has no parameter or state ",
            "variable of that name"))
        }
      }
      model$setVars(v.scn)
      model$setPars(p.scn)
      out <- rbind(out, data.frame(scenario=s,
        model$dynamics(times=times, fortran=FALSE)))
    }
  }
  # plot if requested
  if (plot.vars || plot.pros) {
    items <- c(if (plot.vars) model$namesVars() else c(),
      if (plot.pros) model$namesPros() else c())
    nc <- if (length(items) == 1) 1 else if (length(items) <= 6) 2 else 3
    nr <- ceiling(length(items) / nc)
    graphics::par(mfrow=c(nr, nc))
    if (is.null(scenarios)) {
      clr <- c(default="black")
    } else {
      clr <- stats::setNames(grDevices::colorRampPalette(c("steelblue4","khaki",
        "darkorange"))(length(scenarios)), names(scenarios))
    }
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
