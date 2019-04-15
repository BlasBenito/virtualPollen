#' Reinterpolates aggregated simulations into regular time.
#'
#' @description Takes the output of \code{\link{aggregateSimulation}}, and interpolates it into a regular time grid.
#'
#'
#' @usage toRegularTime(
#'   x,
#'   time.column="Time",
#'   interpolation.interval,
#'   columns.to.interpolate=c("Suitability", "Driver.A", "Pollen")
#'   )
#'
#' @param x list of dataframes (generally the output of \code{\link{aggregateSimulation}}) or single dataframe  with irregular time series.
#' @param time.column character string, default value is "Time".
#' @param interpolation.interval integer, in years, time length encompassed by each sample.
#' @param columns.to.interpolate character string or character vector, columns of simulation output to be interpolated. Any subset of: "Pollen", "Population.mature", "Population.immature", "Population.viable.seeds", "Suitability", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immature", "Driver.A", "Driver.B".
#'
#' @details This function fits a \code{\link{loess}} model of the form \code{y ~ x}, where \code{y} is any column given by \code{columns.to.interpolate} and \code{x} is the column given by the \code{time.column} argument. The model is used to interpolate column \code{y} on a regular time series of intervals equal to \code{interpolation.interval}. If \code{x} is a matrix-like list returned by \code{\link{aggregateSimulation}} (on results of \code{\link{simulateAccumulationRate}} and \code{\link{simulatePopulation}}), the first column of the matrix will already have a regular time column, and therefore nothing will be done with this column of the list.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return If \code{x} is a list of dataframes, the function returns a list with the same structure as the input list. If \code{x} is a dataframe, the function returns a dataframe. In any case, output dataframes have the columns "Time" (now regular), and any column listed in \code{columns.to.interpolate}.
#'
#' @seealso \code{\link{simulateAccumulationRate}}, \code{\link{aggregateSimulation}}
#'
#' @examples
#'#loading data
#'data(simulation)
#'
#'#generating accumulation rate
#'acc.rate <- simulateAccumulationRate(
#'  seed=50,
#'  time=1:1000,
#'  output.min=10,
#'  output.max=40,
#'  direction=1,
#'  plot=TRUE
#'  )
#'
#'#aggregating simulated data
#'sim.output.aggregated <- aggregateSimulation(
#'  simulation.output=sim.output,
#'  accumulation.rate=acc.rate,
#'  sampling.intervals=3
#'  )
#'
#'#comparing simulations
#'sim.output.regular <- toRegularTime(
#'  x=sim.output.aggregated,
#'  time.column="Time",
#'  interpolation.interval=20,
#'  columns.to.interpolate=c("Driver.A", "Pollen")
#'  )
#'
#' @export
toRegularTime <- function(x, time.column="Time", interpolation.interval=10, columns.to.interpolate=c("Suitability", "Driver.A", "Pollen")){

  #list dimensions if x is list
  if(inherits(x , "list")==TRUE){
    x.rows <- 1:dim(x)[1]
    x.columns <- 1:dim(x)[2]
  }

  if(inherits(x , "dataframe")==TRUE){
    x.rows <- 1
    x.columns <- 1
  }

  #iterating through list elements
  #virtual taxa
  for(x.row in x.rows){
    #aggregation levels
    for(x.column in x.columns){

      #getting the dataframe
      temp <- x[[x.row, x.column]]
      temp <- temp[temp$Period=="Simulation", ]

      #computing age extremes
      min.time <- 0
      max.time <- max(temp[,time.column])

      #reference time to interpolate into
      reference.time <- round(seq(min.time, max.time, by=interpolation.interval), 0)

      #empty dataset to store interpolation
      temp.interpolated <- data.frame(time=reference.time)
      names(temp.interpolated)<-time.column

      #iterating through columns
      for (column.to.interpolate in columns.to.interpolate){

        #do not interpolate non-numeric columns
        if (!is.numeric(temp[, column.to.interpolate])){
          next
        }

        #interpolation
        interpolation.formula <- as.formula(paste(column.to.interpolate, "~", time.column, sep=" "))

        #iteration through span values untill R-squared equals 0.9985 (R-squared equal to 1 may throw errors)
        span.values <- seq(50/nrow(temp), 5/nrow(temp), by = -0.0005)
        for(span in span.values){

          interpolation.function <- loess(interpolation.formula, data = temp, span = span, control = loess.control(surface = "direct"))

          #check fit
          if(cor(interpolation.function$fitted, temp[, column.to.interpolate]) >=  0.9985){break}

        }

        #interpolation
        interpolation.result <- predict(interpolation.function, newdata = reference.time, se = FALSE)

        #constraining the range of the interpolation result to the range of the reference data
        interpolation.range <- range(temp[, column.to.interpolate])
        interpolation.result[interpolation.result < interpolation.range[1]] <- interpolation.range[1]
        interpolation.result[interpolation.result > interpolation.range[2]] <- interpolation.range[2]

        #putting the interpolated data back in place
        temp.interpolated[, column.to.interpolate] <- interpolation.result

      }#end of iteration through columns

      temp.interpolated$Period <- "Simulation"

      if(inherits(x , "list")==TRUE){
        x[[x.row, x.column]] <- temp.interpolated
      }

      if(inherits(x , "data.frame")==TRUE){
        x <- temp.interpolated
      }
    }
  }

  return(x)
}
