#' spinebil
#'
#' Functions to evaluate the performance of projection pursuit
#'    index functions using tour methods.
#'
#' @seealso
#'
#' The main functions are:
#' \itemize{
#'   \item [getTrace()]
#'   \item [profileRotation()]
#'   \item [compareSmoothing()]
#'   \item [timeSequence()]
#'   \item [squintAngleEstimate()]
#' }
#'
#' @keywords internal
"_PACKAGE"


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
globalVariables(c("alpha",
                  "value",
                  "angle",
                  "method",
                  "indexMean"))
