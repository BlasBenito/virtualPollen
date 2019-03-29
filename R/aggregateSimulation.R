#' Aggregates the output of \code{\link{simulatePopulation}}.
#'
#' @description Takes the output of \code{\link{simulatePopulation}} and aggregates it into centimetres
#'
#'
#' @usage aggregateSimulation(
#'   simulation.output=NULL,
#'   accumulation.rate=NULL,
#'   sampling.intervals=1
#'   )
#'
#' @param simulation.output list, output of \code{\link{simulatePopulation}}.
#' @param accumulation.rate dataframe, output of \code{\link{simulateAccumulationRate}}.
#' @param sampling.intervals numeric or numeric vector, in centimetres, depth interval or intervals between consecutive samples. If 1, all samples are returned, if 2, returned samples are separated by 1 cm.
#'
#' @details The function uses the values in the \code{grouping} column of the \code{\link{simulateAccumulationRate}} output to aggregate together (by computing the \code{mean}) as many samples as cases in \code{grouping} have the same identificator. Output samples are identified by the average age of the samples within the given centimetre.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A list of dataframes with as many rows as virtual taxa were produced by \code{\link{simulatePopulation}}, and several columns. First column is the original data. Second column is the data aggregated into centimetres according to the result of \code{\link{simulateAccumulationRate}}. Each additional column is the data of the second column resampled as given by the \code{sampling.intervals} argument. See example below for more clarity.
#'
#' @seealso \code{\link{simulateAccumulationRate}}, \code{\link{simulatePopulation}}
#'
#' @examples
#'#generating driver
#'driver <- simulateDriver(
#'  random.seed = 10,
#'  time = 1:1000,
#'  autocorrelation.length = 200,
#'  output.min = 0,
#'  output.max = 100,
#'  rescale = TRUE
#'  )
#'
#'#preparing parameters
#'parameters <- parametersDataframe(rows=2)
#'parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#'parameters <- fixParametersTypes(x=parameters)
#'
#'#simulating population dynamics
#'sim.output <- simulatePopulation(
#'  parameters=parameters,
#'  driver.A=driver,
#'  driver.B=NULL
#'  )
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
#'  sampling.intervals=3)
#'
#'#comparing simulations
#'par(mfrow=c(3,1))
#'#notice the subsetting of the given column of the input list
#'plot(sim.output.aggregated[[1,1]]$Time,
#'  sim.output.aggregated[[1,1]]$Pollen,
#'  type="l",
#'  xlim = c(0, 1000),
#'  main="Annual"
#'  )
#'plot(sim.output.aggregated[[1,2]]$Time,
#'  sim.output.aggregated[[1,2]]$Pollen,
#'  type="l",
#'  xlim = c(0, 1000),
#'  main="1cm"
#'  )
#'plot(sim.output.aggregated[[1,3]]$Time,
#'  sim.output.aggregated[[1,3]]$Pollen,
#'  type="l",
#'  xlim = c(0, 1000),
#'  main="3cm"
#'  )
#'
#'#check differences in nrow
#'nrow(sim.output.aggregated[[1,1]]) #original data
#'nrow(sim.output.aggregated[[1,2]]) #1cm
#'nrow(sim.output.aggregated[[1,3]]) #3cm intervals
#'
#' @export
aggregateSimulation=function(simulation.output=NULL, accumulation.rate=NULL, sampling.intervals=1){

  #get list names
  names.col.1 <- names(simulation.output)

  if(is.null(accumulation.rate)){stop("The argument accumulation.rate is empty.")}
  if(is.null(simulation.output)){stop("The argument simulation.output is empty.")}

  #function to aggregate a dataframe
  applyAccumulationRate = function(x){
    temp = x
    temp = temp[temp$Period=="Simulation", ]
    temp$Period = NULL
    temp.aggregated = aggregate(x=temp, by=list(group=accumulation.rate$grouping), FUN=mean)
    temp.aggregated$group = NULL
    temp.aggregated$Period = "Simulation"
    return(temp.aggregated)
  }

  #function to apply sampling intervals
  applySamplingIntervals=function(x){
    nrow.x <- nrow(x)
    sampling.sequence <- seq(1, nrow.x, by=sampling.interval)
    output.df <- x[sampling.sequence, ]
    return(output.df)
  }

  #apply accumulation rate
  accrate.list=lapply(X=simulation.output, FUN=applyAccumulationRate)

  #apply sampling intervals
  intervals.list <- list()
  if(!is.null(sampling.intervals)){

    for(sampling.interval in sampling.intervals){

      intervals.list = cbind(intervals.list, lapply(X=accrate.list, FUN=applySamplingIntervals))

    }

    #adding lists together
    output.list = cbind(simulation.output, accrate.list, intervals.list)
  } else {
    #adding lists together
    output.list = cbind(simulation.output, accrate.list)
  }

  names(output.list) = NULL

  return(output.list)

}
