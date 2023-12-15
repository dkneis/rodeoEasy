#' Visualize scenarios for a single variable
#'
#' The function takes a data frame returned by \code{\link{run.scenarios}}
#' and plots the dynamics of a single variable. The different scenarios
#' are distinguished by color.
#'
#' @param x A data frame returned by \code{\link{run.scenarios}}.
#' @param variable The name of a single variable (character string).
#'   A column with that name must be present in \code{x}.
#' @param xlab Label for the x axis (character string).
#' @param ylab Label for the y axis (character string).
#' @param ylog Logical. If \code{TRUE}, the y axis will be log-scaled.
#' @param ylim Either a numeric vector of length two used to trim the y axis
#'   or \code{NULL} to adjust the range automatically. 
#'
#' @return A vector of colors with element names corresponding to the
#'   names of scenarios. This is useful for building a legend.
#'
#' @seealso Look at \code{\link{run.scenarios}} to create a suitable
#'   input data frame.
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
#' # implement specific functions used in RHS of ODE
#' DO_sat <- function(T) { exp(7.712 - 1.314 * log(T + 45.93)) }
#' 
#' # run scenarios WITHOUT plotting
#' x <- run.scenarios(model=m, times=seq(0, 12, 0.1),
#'   scenarios=list(warm=c(kd=1.25), cold.windy=c(kd=1, ka=5)),
#'   plot.vars=FALSE
#' )
#' 
#' # custom plotting
#' par(mfrow=c(1,2))
#' show.scenarios(x, "DO")
#' keyColors <- show.scenarios(x, "OM")
#' legend("topright", bty="n", lty=1, col=keyColors,
#'   legend=names(keyColors))
#' par(mfrow=c(1,1))

show.scenarios <- function(x, variable, xlab="time", ylab=variable,
  ylog=FALSE, ylim=NULL) {
  if (!is.data.frame(x))
    stop("'x' must be a data frame")
  if (!all(c("time","scenario",variable) %in% names(x)))
    stop(paste0("missing column(s) in input data frame 'x': ",
      "need at least 'scenario', 'time' and '",variable,"'"))
  if ((length(variable) != 1) || (!is.character(variable)))
    stop("'variable' must be a character string")
  plot(x[,"time"], x[,variable], type="n", xlab=xlab, ylab=ylab,
    ylim=ylim, log=if (ylog) "y" else "", yaxt=if (ylog) "n" else "s")
  if (ylog) {
    pow <- -12:12
    graphics::axis(2, at=10^pow, labels=as.expression(sapply(pow, function(p) {
      substitute(10^e, list(e=p))})), las=2)
  }
  colorCodes <- grDevices::colorRampPalette(c("steelblue4","khaki2",
    "darkorange","brown"))(length(unique(x[,"scenario"])))
  names(colorCodes) <- unique(x[,"scenario"])
  addLine <- function(x) {
    graphics::lines(x[,"time"], x[,variable],
      col=colorCodes[unique(x[,"scenario"])])
  }
  by(x, x[,"scenario"], addLine)
  return(colorCodes)
}
