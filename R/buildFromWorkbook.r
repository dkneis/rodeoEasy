#' Build model from workbook
#'
#' The function builds a rodeo-based model from a workbook without
#' triggering a simulation.
#'
#' @param workbook File path of the worksheet holding the model. This can be
#'   a file with either extension '.xlsx' or '.ods'. See below for the
#'   mandatory file contents.
#'
#' @return An object of class \code{\link[rodeo]{rodeo}}.
#'
#' @note The file provided as \code{workbook} must contain at least the four
#'   mandatory sheets:
#' \itemize{
#'   \item{'vars'} Defines the state variables of the model. Mandatory columns
#'      are 'name', 'unit', 'description'.
#'   \item{'pars'} Defines the parameters of the model. Mandatory columns
#'      are the same as for sheet 'vars'.
#'   \item{'funs'} Declares the names of functions used in the model
#'      equations. Mandatory columns are the same as for sheet 'vars'.
#'   \item{'eqns'} Specifies the model equations. Mandatory columns
#'      are 'name', 'unit', 'description', 'rate' plus one column for
#'      every state variable of the model. The 'rate' columns holds the
#'      process rate expressions and columns named after state variables
#'      contain the corresponding stoichiometric factors.
#' }
#' The best way to understand the contents of the workbook is to study the
#' examples in the folder 'models' shipped with the package. Type
#' \code{system.file("models", package="rodeoEasy")} at the R prompt to see
#' where the folder 'models' is installed on your system.
#'
#' @seealso Look at \code{\link{compareScenarios}} for building and running
#'   a model with a single call.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' x <- buildFromWorkbook(
#'   system.file("models/oxygen.xlsx", package="rodeoEasy")
#' )


buildFromWorkbook <- function(workbook) {
  # check inputs
  if (!file.exists(workbook))
    stop(paste0("file provided as 'workbook' not found: '",workbook,"'"))
  # read sheets
  sheets <- c("vars", "pars", "funs", "eqns")
  x <- list()
  for (s in c(sheets)) {
    tryCatch({
      if (grepl(workbook, pattern=".+[.]xlsx$")) {
        x[[s]] <- as.data.frame(readxl::read_excel(workbook, sheet=s))
      } else if (grepl(workbook, pattern=".+[.]ods$")) {
        x[[s]] <- readODS::read_ods(workbook, sheet=s)
      } else {
        stop(paste0("file '",workbook,"' not in '.xlsx' or '.ods' format")) 
      }
    }, error= function(e) {
      stop(paste0("failed to read required sheet '",s,"' from '",workbook,"'"))
    })
  }
  # separate processes and stoichiometry
  x[["stoi"]] <- with(x, as.matrix(eqns[,vars[,"name"]]))
  rownames(x[["stoi"]]) <- with(x, eqns[,"name"])
  x[["pros"]] <- with(x, eqns[,c("name","unit","rate","description")])
  names(x[["pros"]])[names(x[["pros"]]) == "rate"] <- "expression"
  x[["eqns"]] <- NULL
  # build and compile model object
  m <- with(x, rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=stoi,
    asMatrix=T, dim=1))
  m$compile(fortran=FALSE)
  # return object
  m
}
