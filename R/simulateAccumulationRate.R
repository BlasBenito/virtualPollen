#' Simulates a virtual sediment accumulation rate.
#'
#' @description Generates a virtual sediment accumulation rate to be applied to the results of \code{\link{simulatePopulation}}.
#'
#'
#' @usage simulateAccumulationRate(
#'   seed=50,
#'   time=1:1000,
#'   output.min=10,
#'   output.max=40,
#'   direction=1,
#'   plot=TRUE
#'   )
#'
#' @param seed integer, seed to be used by \code{\link{set.seed}} to configure the state of the pseudo-random number generator. It defines the shape of the curve.
#' @param time vector of time values (ideally the same used to generate the simulations). \strong{Important}: the \code{time} column goes from "left to right", meaning that oldest samples have the lowest values of age/time, and viceversa.
#' @param output.min numeric, in years per centimetre, minimum sediment accumulation rate (10 by default).
#' @param output.max numeric, in years per centimetre, maximum sediment accumulation rate (40 bu default).
#' @param direction integer, values 1 or -1, to invert the resulting accumulation rate.
#' @param plot boolean, plots output accumulation rate if \code{TRUE}.
#'
#' @details The accumulation rate curve is generated through a random walk smoothed by a GAM model. The value of the \code{seed} argument changes the shape of the curve, but the user has no more control than trying different values to achieve a curve closer to the desired one. If \code{plot} is set to \code{TRUE}, the accumulation rate curve is printed on screen, but not exported to pdf.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A dataframe like \code{\link{accumulationRate}}, with the following columns:
#' \itemize{
#'   \item \emph{time}: numeric, time or age of the given case.
#'   \item \emph{accumulation.rate}: numeric, in years per centimetre, simulated accumulation rate.
#'   \item \emph{grouping}: integer, grouping variable to aggregate together (with \code{\link{aggregateSimulation}}) samples deposited in the same centimetre according \emph{accumulation.rate}.
#' }
#'
#' @seealso \code{\link{simulatePopulation}}, \code{\link{aggregateSimulation}}
#'
#' @examples
#'
#'acc.rate <- simulateAccumulationRate(
#'  seed = 50,
#'  time = 1:1000,
#'  output.min = 10,
#'  output.max = 40,
#'  direction = 1,
#'  plot = TRUE
#'  )
#'
#'str(acc.rate)
#'
#' @export
simulateAccumulationRate <- function(seed = 50,
                                     time = 1:1000,
                                     output.min = 10,
                                     output.max = 40,
                                     direction = 1,
                                     plot = TRUE
                                     ){

#setting random seed for repeatibility
set.seed(seed)

#generating a random walk
accumulation.rate <- cumsum(sample(c(-0.1, 0, 0.1), max(time), TRUE))
if(direction == -1){
  accumulation.rate = rev(accumulation.rate)
  }

#fitting a gam to the data and predicting a smoothed accumulation rate curve
temp.data <- data.frame(accumulation.rate, time)
temp.gam <- gam(accumulation.rate ~ s(time, k = 10), data = temp.data)
accumulation.rate <- predict(temp.gam, type = "response")

#scaling it between given bounds
accumulation.rate <- rescaleVector(x = as.vector(accumulation.rate), new.min = output.min, new.max = output.max, integer = TRUE)
accumulation.rate <- as.vector(accumulation.rate)

#plotting data
temp.df  <-  data.frame(time, accumulation.rate)
if(plot == TRUE){
  temp.plot  <-  ggplot(data = temp.df, aes(x = time, y = accumulation.rate)) +
    geom_line(color = viridis(10)[3], size = 0.5) +
    geom_ribbon(aes(ymin = 0, ymax = accumulation.rate), fill = viridis(10)[3], alpha = 0.3) +
    xlab("Time") +
    ylab("Acc. rate") +
    scale_y_continuous(breaks = seq(0, output.max, by = 10)) +
    scale_x_continuous(breaks = seq(0, max(time), by = max(time)/5))
  print(temp.plot)
}

#generating a grouping variable (consecutive numbers with same value are put in separated groups)
#applying rle to identify groups of consecutivee integers
accumulation.rate.rle <- rle(accumulation.rate)
accumulation.rate.rle <- data.frame(value = accumulation.rate.rle$values, length = accumulation.rate.rle$lengths)

#using rle as guide to build the groups
accumulation.rate.groups <- vector()
start.group <- 0
for(i in 1:nrow(accumulation.rate.rle)){
  value <- accumulation.rate.rle[i, "value"]
  length <- accumulation.rate.rle[i, "length"]
  times <- start.group + (1:round(length/value, 0))
  accumulation.rate.groups <- c(accumulation.rate.groups, rep(times, each = value))
  start.group <- max(times)
}
accumulation.rate.groups <- accumulation.rate.groups[1:max(time)]

output <- data.frame(time = time, accumulation.rate = accumulation.rate, grouping = accumulation.rate.groups)

return(output)

} #end of function
