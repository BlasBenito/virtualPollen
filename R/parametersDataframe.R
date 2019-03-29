#' Generates a template dataframe to contain simulation parameters.
#'
#' @description Generates the dataframe structure needed to contain the parameters used as input for the \code{\link{simulatePopulation}} function.
#'
#' @usage parametersDataframe(rows=1)
#'
#' @param rows integer, number of rows in the output dataframe.
#'
#' @details The resulting dataframe can either be filled manually through vectors, as shown in the example (but this requires to use the function \code{\link{fixParametersTypes}} once the dataframe is completed), or can be edited manually in Rstudio by installing the \href{https://cran.r-project.org/web/packages/editData/README.html}{editData} package.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A dataframe filled with \code{NA} values and the columns:
#' \itemize{
#'   \item label: to store names (character string) of the virtual taxa.
#'   \item maximum.age: integer, maximum possible age of the individuals in years.
#'   \item reproductive.age: integer, age of sexual maturity in years.
#'   \item fecundity: integer, number of maximum viable seeds produced by a mature individual under fully suitable conditions.
#'   \item growth.rate: numeric, parameter of the logistic growth function.
#'   \item pollen.control: numeric in the interval [0, 1]. If 0, pollen productivity depends on environmental suitability only. The larger the number, biomass takes over environmental suitability in determining pollen productivity.
#'   \item maximum.biomass: integer, maximum biomass of the individuals.
#'   \item carrying.capacity: integer, maximum sum of biomass of the individuals. Very large carrying capacities plus a low maximum.biomass generates too many individuals for the simulation to remain efficient. Try to set carrying.capacity and maximum.biomass to carrying.capacity divided by biomass returns a number lower than 1000 (and even better if it is closer to 100).
#'   \item driver.A.weight: numeric in the interval [0, 1], represents the relative influence of the driver on environmental suitability.
#'   \item driver.B.weight: numeric in the interval [0, 1], represents the relative influence of the driver on environmental suitability. The sum of weights of drivers A and B should be 1.
#'   \item niche.A.mean: numeric, in the same units as driver A. It is the mean of the normal function defining the response of the virtual taxa to driver A.
#'   \item niche.A.sd: numeric, in the same units as driver A. It is the standard deviation of the normal function defining the response of the virtual taxa to driver A.
#'   \item niche.B.mean: as above, but for driver B.
#'   \item niche.B.sd: as above, but for driver B.
#'   \item autocorrelation.length.A: numeric, only useful if several drivers generated with different autocorrelation lengths are available (and identified by the column \code{autocorrelation.length}) in the \code{drivers} argument provided to the \code{\link{simulatePopulation}} function.
#'   \item autocorrelation.length.B: same as above.
#' }
#'
#' @seealso \code{\link{simulatePopulation}}, \code{\link{fixParametersTypes}}
#'
#' @examples
#'
#' parameters <- parametersDataframe(rows=1)
#'parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#'
#' @export
parametersDataframe = function(rows=1){

  if(is.integer(rows)==FALSE){rows = ceiling(rows)}
  if(rows==0){rows = 1}

  output.df = data.frame(label=rep(NA, rows),
                         maximum.age=rep(NA, rows),
                         reproductive.age=rep(NA, rows),
                         fecundity=rep(NA, rows),
                         growth.rate=rep(NA, rows),
                         pollen.control=rep(NA, rows),
                         maximum.biomass=rep(NA, rows),
                         carrying.capacity=rep(NA, rows),
                         driver.A.weight=rep(NA, rows),
                         driver.B.weight=rep(NA, rows),
                         niche.A.mean=rep(NA, rows),
                         niche.A.sd=rep(NA, rows),
                         niche.B.mean=rep(NA, rows),
                         niche.B.sd=rep(NA, rows),
                         autocorrelation.length.A=rep(NA, rows),
                         autocorrelation.length.B=rep(NA, rows),
                         stringsAsFactors = FALSE)
  return(output.df)

}
