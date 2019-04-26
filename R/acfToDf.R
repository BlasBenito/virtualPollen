#' Computes temporal autocorrelation in a vector, and returns a dataframe for easy plotting.
#'
#' @description It reads a vector representing a time series, applies \code{\link{acf}} for a given number of lags
#'
#' @usage acfToDf(
#'   x = NULL,
#'   lag.max = 100,
#'   length.out = 10
#'   )
#'
#' @param x numeric vector. Must represent a variable sampled at regular times.
#' @param lag.max integer, number of lags over which to compute temporal autocorrelation.
#' @param length.out integer, total number of lags to consider for plotting. Should be a subset of \code{lag.max}.
#'
#' @details This function computes temporal autocorrelation of a given vector using \code{\link{acf}}, and returns a dataframe ready for easy plotting with \code{\link{plotAcf}}.
#'
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A dataframe with the columns:
#' #' \itemize{
#'   \item \emph{lag}: numeric, lag in the time units of \code{x} with a maximum determined by \code{lag.max}, and a number of unique values determined by \code{length.out}
#'   \item \emph{acf}: Pearson correlation index returned by the \code{\link{acf}} for a given number of lags for the given lag.
#'   \item \emph{ci.max}: Maximum value of the confidence interval of \code{acf}.
#'   \item \emph{ci.min}: Minimum value of the confidence interval of \code{acf}.
#' }
#'
#' @seealso \code{\link{acf}}, \code{\link{plotAcf}}
#'
#' @examples
#'
#'#getting a driver
#'data(driverA)
#'
#'#computing temporal autocorrelations
#'x.df <- acfToDf(
#'   x = driverA,
#'   lag.max = 1000,
#'   length.out = 100
#')
#'str(x.df)
#'
#'#plotting output
#'plotAcf(x.df)
#'
#' @export
acfToDf <- function(x = NULL,
                   lag.max = 100,
                   length.out = 10){


  #CHECKS INPUT VECTOR
  #----------------------

  if(!is.vector(x) | length(x) == 0 | is.null(x)){
    x = as.vector(x)
    if(is.vector(x) == FALSE){
      stop("acfToDf: the argument x is not a vector.")
    }
  }


  #CHECKS lag.max
  #----------------------

  if(lag.max > length(x)){
    lag.max <- length(x) - 1 #default behavior of acf()
  }


  #CHECKS length.out
  #----------------------

  if(length.out > lag.max){
    length.out <- lag.max
  }


  #COMPUTING AUTOCORRELATION AND CONFIDENCE INTERVALS
  #----------------------

  #computing autocorrelation
  acf.output <- acf(x, lag.max=lag.max, plot=FALSE)

  #computes confidence interval (same equation as in plot.acf())
  acf.ci <- qnorm((1 + 0.95)/2)/sqrt(acf.output$n.used)


  #PREPARING OUTPUT DATAFRAME
  #----------------------

  #generating dataframe with all lags
  acf.df <- data.frame(lag=acf.output$lag, acf=acf.output$acf, ci.max=acf.ci, ci.min=-acf.ci)

  #resampling to reduce the number of lines to be plotted
  acf.df <- acf.df[floor(seq(1, nrow(acf.df), length.out = length.out)),]

  return(acf.df)

}
