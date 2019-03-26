#' Fix data types in parameters dataframe.
#'
#' @description It converts all columns (but the \code{label} one) of a parameters dataframe created by \code{\link{parametersDataframe}} into type numeric.
#'
#' @usage parametersDataframe(x)
#'
#' @param x dataframe resulting from \code{\link{parametersDataframe}}.
#'
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return Same dataframe provided in argument \code{x} but with fixed data types.
#'
#' @seealso \code{\link{parametersDataframe}}
#'
#' @examples
#'
#' parameters <- parametersDataframe(rows=1)
#' parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#' parameters <- fixParametersTypes(x=parameters)
#'
#' @export
fixParametersTypes <- function(x){
  #sets all columns but the first one into numeric
  x[, 2:ncol(x)] <- sapply(x[, 2:ncol(x)], as.numeric)
  return(x)
}
