#' Aggregates the output of \code{\link{simulatePopulation}}.
#'
#' @description Takes the output of \code{\link{simulatePopulation}} and aggregates it into centimetres by following a sediment accumulation rate produced by \code{\link{simulateAccumulationRate}}. It further samples it at given depth intervals. It intends to simulate a pseudo-realistic sedimentation of the pollen produced by the simulation, and to apply a pollen-sampling pattern to a virtual pollen core.
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
#' @param sampling.intervals integer, numeric vector, depth interval or intervals between consecutive samples in centimetres. If 1, all samples are returned, if 2, returned samples are separated by 1 cm.
#'
#' @details The function uses the values in the \code{grouping} column of the \code{\link{simulateAccumulationRate}} output to aggregate together (by computing the \code{mean}) as many samples as cases in \code{grouping} have the same identificator. Output samples are identified by the average age of the samples within the given centimetre.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A list of dataframes with as many rows as virtual taxa were produced by \code{\link{simulatePopulation}}, and as many columns as number of \code{sampling.intervals} defined by the user.
#'
#' @seealso \code{\link{simulateAccumulationRate}}, \code{\link{simulatePopulation}}
#'
#' @examples
#'
#'#getting example data
#'data(simulation)
#'data(accumulationRate)
#'
#'#aggregating first simulation outcome
#'sim.output.aggregated <- aggregateSimulation(
#'  simulation.output = simulation[1],
#'  accumulation.rate = accumulationRate,
#'  sampling.intervals = c(2,6))
#'
#'#comparing simulations
#'par(mfrow = c(3,1))
#'#notice the subsetting of the given column of the input list
#'plot(sim.output.aggregated[[1,1]]$Time,
#'  sim.output.aggregated[[1,1]]$Pollen,
#'  type = "l",
#'  xlim = c(500, 1000),
#'  main = "Annual"
#'  )
#'plot(sim.output.aggregated[[1,2]]$Time,
#'  sim.output.aggregated[[1,2]]$Pollen,
#'  type = "l",
#'  xlim = c(500, 1000),
#'  main = "2cm"
#'  )
#'plot(sim.output.aggregated[[1,3]]$Time,
#'  sim.output.aggregated[[1,3]]$Pollen,
#'  type = "l",
#'  xlim = c(500, 1000),
#'  main = "6cm"
#'  )
#'
#'#check differences in nrow
#'nrow(sim.output.aggregated[[1,1]]) #original data
#'nrow(sim.output.aggregated[[1,2]]) #2cm
#'nrow(sim.output.aggregated[[1,3]]) #6cm intervals
#'
#' @export
aggregateSimulation <- function(simulation.output = NULL,
                                accumulation.rate = NULL,
                                sampling.intervals = 1){

  if(is.null(accumulation.rate)){stop("The argument accumulation.rate is empty.")}
  if(is.null(simulation.output)){stop("The argument simulation.output is empty.")}

  #function to aggregate a dataframe
  applyAccumulationRate <- function(x){
    temp = x
    temp = temp[temp$Period=="Simulation", ]
    temp$Period = NULL
    temp.aggregated = aggregate(x = temp, by = list(group = accumulation.rate$grouping), FUN = mean)
    temp.aggregated$group = NULL
    temp.aggregated$Period = "Simulation"
    return(temp.aggregated)
  }

  #function to apply sampling intervals
  applySamplingIntervals <- function(x){
    nrow.x = nrow(x)
    sampling.sequence = seq(1, nrow.x, by = sampling.interval)
    output.df = x[sampling.sequence, ]
    return(output.df)
  }

  #apply accumulation rate
  accrate.list <- lapply(X = simulation.output, FUN = applyAccumulationRate)

  #apply sampling intervals
  intervals.list <- list()
  if(!is.null(sampling.intervals)){

    for(sampling.interval in sampling.intervals){

      intervals.list <- cbind(intervals.list, lapply(X = accrate.list, FUN = applySamplingIntervals))

    }

    #adding lists together
    output.list <- cbind(simulation.output, accrate.list, intervals.list)
  } else {
    #adding lists together
    output.list <- cbind(simulation.output, accrate.list)
  }

  names(output.list) <- NULL

  return(output.list)

}
