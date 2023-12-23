#' Build a model from the contents of a workbook
#'
#' The function builds a \code{\link[rodeo]{rodeo}}-based model by importing
#' all declarations and equations from a workbook established with common
#' spreadsheet software.
#'
#' @param workbook File path of the workbook. The file type is guessed from
#'   the extension which must be '.xlsx' or '.ods'. See below for the mandatory
#'   worksheets that must be present in the workbook.
#'
#' @return An object of class \code{\link[rodeo]{rodeo}}.
#'
#' @note The file provided as \code{workbook} must contain at least the four
#'   mandatory sheets:
#' \itemize{
#'   \item{'vars'} Defines the state variables of the model. Mandatory columns
#'      are 'name', 'unit', 'description', 'default'.
#'   \item{'pars'} Defines the parameters of the model. Mandatory columns
#'      are the same as for sheet 'vars'.
#'   \item{'funs'} Declares and possibly defines functions used in the model
#'      equations. Mandatory columns are 'name' and 'code'.
#'   \item{'eqns'} Specifies the model equations. Mandatory columns
#'      are 'name', 'unit', 'description', 'rate' plus one column for
#'      every state variable of the model. The 'rate' columns holds the
#'      process rate expressions and columns named after state variables
#'      contain the corresponding stoichiometric factors.
#' }
#' The best way to understand the contents of the workbook is to study the
#' examples in the folder 'models' shipped with the package. Type
#' \code{system.file("models", package="rodeoEasy")} at the R prompt to see
#' where this folder is installed on your system.
#' 
#' The sheet 'funs' may contain a mixture of both intrinsic and user defined
#' functions. Intrinsic functions can be declared with a single line and the
#' column 'code' can be filled with an R comment like, e.g. '# intrinsic',
#' to show that the existing function will not be overloaded.
#'
#' @seealso Look at \code{\link{run.scenarios}} and other functions of this
#'   package for how to run a simulation using the built model.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' x <- build(
#'   system.file("models/oxygen.xlsx", package="rodeoEasy")
#' )


build <- function(workbook) {
  # check inputs
  if (!file.exists(workbook))
    stop(paste0("file provided as 'workbook' not found: '",workbook,"'"))
  # needed sheets and columns
  sheets.needed <- list(
    vars = c("name","unit","description","default"),
    pars = c("name","unit","description","default"),
    funs = c("name","code"),
    eqns = c("name","unit","description","rate")
  )
  # import sheets
  x <- list()
  for (s in names(sheets.needed)) {
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
  # update needed columns in sheet with equations
  sheets.needed[["eqns"]] <- c(sheets.needed[["eqns"]], x[["vars"]][,"name"])
  # check sheets
  for (s in names(sheets.needed)) {
    missing <- sheets.needed[[s]][! sheets.needed[[s]] %in% names(x[[s]])]
    if (length(missing) > 0) {
      stop(paste0("table in sheet '",s,"' of '",workbook,
        "' is lacking mandatory column(s): '",paste(missing, collapse="', '"),"'"))
    }
  }
  # separate processes and stoichiometry
  x[["stoi"]] <- with(x, as.matrix(eqns[,vars[,"name"]]))
  rownames(x[["stoi"]]) <- with(x, eqns[,"name"])
  x[["pros"]] <- with(x, eqns[,c("name","unit","rate","description")])
  names(x[["pros"]])[names(x[["pros"]]) == "rate"] <- "expression"
  x[["eqns"]] <- NULL
  # make user-defined functions available
  for (f in unique(x[["funs"]][,"name"])) {
    code <- x[["funs"]][x[["funs"]][,"name"] == f, "code"]
    code <- paste(code, collapse="\n")
    tryCatch(
      eval(parse(text=code), envir=globalenv())
    , error = function(e) {
      stop(paste0("function '",f,"' define on sheet 'funs' of workbook '",
        workbook,"' could not be parsed and evaluated"))
    })
  }
  x[["funs"]] <- data.frame(name=unique(x[["funs"]][,"name"]),
    unit="not provided; see possible commments in code", description="see code")
  # build and compile model object
  m <- with(x, rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=stoi,
    asMatrix=T, dim=1))
  m$compile(fortran=FALSE)
  
  # return object
  m
}
