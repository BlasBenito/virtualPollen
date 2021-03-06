% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toRegularTime.R
\name{toRegularTime}
\alias{toRegularTime}
\title{Reinterpolates aggregated simulations into regular time.}
\usage{
toRegularTime(
  x = NULL,
  time.column = "Time",
  interpolation.interval = NULL,
  columns.to.interpolate = c("Suitability",
                             "Driver.A",
                             "Pollen")
  )
}
\arguments{
\item{x}{list of dataframes (generally the output of \code{\link{aggregateSimulation}}) or single dataframe  with irregular time series.}

\item{time.column}{character string, default value is "Time".}

\item{interpolation.interval}{integer, in years, time length encompassed by each sample.}

\item{columns.to.interpolate}{character string or character vector, columns of simulation output to be interpolated. Any subset of: "Pollen", "Population.mature", "Population.immature", "Population.viable.seeds", "Suitability", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immature", "Driver.A", "Driver.B".}
}
\value{
If \code{x} is a list of dataframes, the function returns a list with the same structure as the input list. If \code{x} is a dataframe, the function returns a dataframe. In any case, output dataframes have the columns "Time" (now regular), and any column listed in \code{columns.to.interpolate}. \strong{Important}: as in the input data, the \code{time} column of the output data has lower time for oldest samples and higher time for newest samples.
}
\description{
Takes the output of \code{\link{aggregateSimulation}}, and interpolates it into a regular time grid.
}
\details{
This function fits a \code{\link{loess}} model of the form \code{y ~ x}, where \code{y} is any column given by \code{columns.to.interpolate} and \code{x} is the column given by the \code{time.column} argument. The model is used to interpolate column \code{y} on a regular time series of intervals equal to \code{interpolation.interval}. If \code{x} is a matrix-like list returned by \code{\link{aggregateSimulation}} (on results of \code{\link{simulateAccumulationRate}} and \code{\link{simulatePopulation}}), the first column of the matrix will already have a regular time column, and therefore nothing will be done with this column of the list.
}
\examples{

\dontrun{
#getting example data
data(simulation)
data(accumulationRate)

#aggregating first simulation outcome
sim.output.aggregated <- aggregateSimulation(
 simulation.output = simulation[1],
 accumulation.rate = accumulationRate,
 sampling.intervals = c(2,6))

#to regular time
sim.output.aggregated <- toRegularTime(
 x=sim.output.aggregated,
 time.column ="Time",
 interpolation.interval = 10,
 columns.to.interpolate = c("Suitability", "Pollen")
)

#comparing simulations
par(mfrow = c(3,1))
#notice the subsetting of the given column of the input list
plot(sim.output.aggregated[[1,1]]$Time,
    sim.output.aggregated[[1,1]]$Pollen,
    type = "l",
    xlim = c(500, 1000),
    main = "Annual"
)
plot(sim.output.aggregated[[1,2]]$Time,
    sim.output.aggregated[[1,2]]$Pollen,
    type = "l",
    xlim = c(500, 1000),
    main = "2cm"
)
plot(sim.output.aggregated[[1,3]]$Time,
    sim.output.aggregated[[1,3]]$Pollen,
    type = "l",
    xlim = c(500, 1000),
    main = "6cm"
)

#check differences in nrow
nrow(sim.output.aggregated[[1,1]]) #original data
nrow(sim.output.aggregated[[1,2]]) #2cm
nrow(sim.output.aggregated[[1,3]]) #6cm intervals
}

}
\seealso{
\code{\link{simulateAccumulationRate}}, \code{\link{aggregateSimulation}}
}
\author{
Blas M. Benito  <blasbenito@gmail.com>
}
