#' Plots results of \code{\link{acfToDf}}.
#'
#' @description Plots a dataframe resulting from \code{\link{acfToDf}} by using \code{\link[ggplot2]{ggplot2}}.
#'
#' @usage plotAcf(
#'   x = NULL,
#'   plot.title = ""
#'   )
#'
#' @param x dataframe, output of \code{\link{acfToDf}}
#' @param plot.title string, title of the output plot.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A ggplot object
#'
#' @seealso \code{\link{acfToDf}}
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
plotAcf <- function(x = NULL,
                   plot.title = ""){

  library(cowplot)

  acf.plot = ggplot(data = x, aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ci.max), color="red", linetype="dashed") +
    geom_hline(aes(yintercept = ci.min), color="red", linetype="dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    ggtitle(plot.title) +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))

return(acf.plot)

}
