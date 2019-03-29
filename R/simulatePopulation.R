#' Simulates population dynamics for virtual species with different traits.
#'
#' @description This function takes as input a dataframe of parameters defining virtual taxa produced by \code{\link{parametersDataframe}} and \code{\link{fixParametersTypes}}, a driver or drivers generated with \code{\link{simulateDriver}}, and simulates population dynamics at yearly resolution for the time-length defined by the driver or drivers. The model relies on the following set of assumptions.
#'
#' \itemize{
#'   \item  The spatial structure of the population is not important to explain its pollen productivity. This is an operative assumption, to speed-up model execution.
#'   \item  The environmental niche of the species follows a Gaussian distribution, characterized by a mean (niche optimum, also niche position) and a standard deviation (niche breadth or tolerance).
#'   \item  Different drivers can have a different influence on the species dynamics, and that influence can be defined by the user by tuning the weights of each driver.
#'   \item  Environmental suitability, expressed in the range [0, 1], is the result of an additive function of the species niches (normal function defined by the species' mean and standard deviation for each driver), the drivers' values, and the relative influence of each driver (driver weights).
#'   \item  Pollen productivity is a function of the individual's biomass and environmental suitability, so under a hypothetical constant individual's biomass, its pollen production depends linearly on environmental suitability values.
#'   \item  Effective fecundity is limited by environmental suitability. Low environmental suitability values limit recruitment, acting as an environmental filter. Therefore, even though the fecundity of the individuals is fixed by the fecundity parameter, the overall population fecundity is limited by environmental suitability.
#' }
#'
#' @usage simulatePopulation(
#'   parameters=NULL,
#'   species="all",
#'   driver.A=NULL,
#'   driver.B=NULL,
#'   drivers=NULL,
#'   burnin=TRUE
#'   )
#'
#' @param parameters dataframe with parameters.
#' @param species if "all" or "ALL", all species in "parameters" are simulated It also accepts a vector of numbers representing the rows of the selected species, or a vector of names of the selected species.
#' @param driver.A numeric vector with driver values.
#' @param driver.B numeric vector with driver values.
#' @param drivers dataframe with drivers. It should have the columns: \emph{age}
#' @param burnin boolean, generates a warming-up period for the population model of a length of five times the maximum age of the virtual taxa.
#'
#' @details The model starts with a population of 100 individuals with random ages, in the range [1, maximum age], taken from a uniform distribution (all ages are equiprobable). For each environmental suitability value, including the burn-in period, the model performs the following operations:
#'
#' \itemize{
#'   \item Aging: adds one year to the age of the individuals.
#'   \item Mortality due to senescence: individuals reaching the maximum age are removed from the simulation.
#'   \item Local extinction and immigration If the number of individuals drops to zero, the population is replaced by a "seed bank" of #' 100 individuals with age zero, and the simulation jumps to step 7.. This is intended to simulate the arrival of seeds from nearby regions, and will only lead to population growth if environmental suitability is higher than zero.
#'   \item Plant growth: Applies a plant growth equation to compute the biomass of every individual.
#'   \item Carrying capacity: If maximum population biomass is reached, individuals are iteratively selected for removal according to a mortality risk curve computed by the equation \eqn{P_{m} = 1 - sqrt(a/A)}, were \emph{Pm} is the probability of mortality, \emph{a} is the age of the given individual, and \emph{A} is the maximum age reached by the virtual taxa. This curve gives removal preference to younger individuals, matching observed patterns in natural populations.
#'   \item Pollen productivity: In each time step the model computes the pollen productivity (in relative values) of the population using the equation \eqn{P_{t} = \sum x_{it} \times max(S_{t}, B)}, where \emph{t} is time (a given simulation time step), \emph{P} is the pollen productivity of the population at a given time, \emph{x_{i}} represents the biomass of every adult individual, \emph{S} is the environmental suitability at the given time, \emph{B} is the contribution of biomass to pollen productivity regardless of environmental suitability (\emph{pollen.control} parameter in the simulation, 0 by default). If \emph{B} equals 1, \emph{P} is equal to the total biomass sum of the adult population, regardless of the environmental suitability. If \emph{B} equals 0, pollen productivity depends entirely on environmental suitability values.
#'   \item Reproduction: Generates as many seeds as reproductive individuals are available multiplied by the maximum fecundity and the environmental suitability of the given time.
#' }
#'The model returns a table with climatic suitability, pollen production, and population size (reproductive individuals only) per simulation year. Figure 10 shows the results of the population model when applied to the example virtual species.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A list of dataframes, each one of them with the results of one simulation. Each dataframe has the columns:
#' \itemize{
#'   \item \emph{Time} integer, ages in years. Negative ages indicate the burn-in period.
#'   \item \emph{Pollen} numeric, pollen counts
#'   \item \emph{Population.mature} numeric, number of mature individuals.
#'   \item \emph{Population.immatre} numeric, number of immature individuals.
#'   \item \emph{Population.viable.seeds} numeric, number of viable seeds generated each year.
#'   \item \emph{Suitability} numeric, environmental suitability computed from the driver by the normal function/s defining the taxon niche.
#'   \item \emph{Biomass.total} numeric, overall biomass of the population.
#'   \item \emph{Biomass.mature} numeric, sum of biomass of mature individuals.
#'   \item \emph{Biomass.immature} numeric, sum of biomass of immature individuals.
#'   \item \emph{Mortality.mature} numeric, number of mature individuals dead each year.
#'   \item \emph{Mortality.immature} numeric, same as above for immature individuals.
#'   \item \emph{Driver.A} numeric, values of driver A.
#'   \item \emph{Driver.B} numeric, values of driver B, if available, and NA otherwise.
#'   \item \emph{Period} qualitative, with value "Burn-in" for burn-in period, and "Simulation" otherwise.
#' }
#'
#' @seealso \code{\link{parametersDataframe}}, \code{\link{fixParametersTypes}}, \code{\link{plotSimulation}}
#'
#' @examples
#'
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
#'parameters[2,] <- c("Species 1", 500, 100, 10, 0.02, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#'parameters <- fixParametersTypes(x=parameters)
#'
#'#simulating population dynamics
#'sim.output <- simulatePopulation(
#'  parameters=parameters,
#'  driver.A=driver
#'  )
#'
#'#checking output for Species 1
#'str(sim.output[[1]])
#'
#' @export
simulatePopulation = function(parameters=NULL,
                              species="all",
                              driver.A=NULL,
                              driver.B=NULL,
                              drivers=NULL,
                              burnin=TRUE){


  #CHECKING INPUT DATA
  #-------------------

  #CHECKING parameters
  if(is.null(parameters) == TRUE | is.data.frame(parameters) == FALSE){

    stop("The argument 'parameters' empty.")

  } else {

    if(sum(!(colnames(parameters) %in% c("label", "maximum.age", "reproductive.age", "fecundity", "growth.rate", "pollen.control", "maximum.biomass", "carrying.capacity", "driver.A.weight", "driver.B.weight", "niche.A.mean", "niche.A.sd", "niche.B.mean", "niche.B.sd", "autocorrelation.length.A", "autocorrelation.length.B"))) != 0){
      stop(paste("The following column/s of 'parameters' seem to be missing: ", colnames(parameters)[colnames(parameters) %not-in% c("label", "maximum.age", "reproductive.age", "fecundity", "growth.rate", "pollen.control", "maximum.biomass", "carrying.capacity", "driver.A.weight", "driver.B.weight", "niche.A.mean", "niche.A.sd", "niche.B.mean", "niche.B.sd", "autocorrelation.length.A", "autocorrelation.length.B")], sep=""))

    }
  }


  #function to check if driver B is available
  is.driver.B.available <- function(driver.B){
    if(is.null(driver.B)==TRUE  | is.vector(driver.B)==FALSE){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }


  #CHECKING drivers, driver.A, driver.B
  if(is.null(drivers)==TRUE | is.data.frame(drivers)==FALSE){

    #checking driver B
    driver.B.available = is.driver.B.available(driver.B)

    #checking driver.A
    if(is.null(driver.A)==TRUE | is.vector(driver.A)==FALSE){

        if(driver.B.available==FALSE){
          stop("No drivers have been provided.")
        }

      } else {

        drivers.input="vector"

        #create fake driver.B if absent
        if(driver.B.available==FALSE){
          driver.B = rep(1, length(driver.A))
        }

      }

    } else {
      #CHECKING drivers dataframe

      #checking columns
      if(sum(!(colnames(drivers) %in% c("time", "driver", "autocorrelation.length", "value"))) != 0){
        stop(paste("The following column/s of 'drivers' seem to be missing: ", colnames(parameters)[colnames(parameters) %not-in% c("time", "driver", "autocorrelation.length", "value")], sep=""))
      } else {

          #switch to dataframe input
          drivers.input="data.frame"

          #giving preference to dataframe format
          driver.A = NULL
          driver.B = NULL
          driver.B.available = TRUE
        }
    }


  #CHECKING AND SELECTING species
  #----------------
  #creating dictionary of species names and indexes
  names.dictionary = data.frame(name=parameters$label, index=1:nrow(parameters))

  #if null or "all", selects all species
  if(is.null(species) | species %in% c("all", "All", "ALL")){

    selected.species = names.dictionary$index

  } else {

    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){
      stop("You have selected species that are not available in the parameters table.")
      }

    #correct species names or indexes
    if(species %in% names.dictionary$names){
      selected.species = names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species = species

    }
  }


  #generating output list
  #----------------
  output.list = list()


  #function to rescale suitability
  #----------------
  rescaleSuitability=function(predicted.density, max.observed.density){
    new.min=0
    new.max=1
    old.min=0
    old.max=max.observed.density
    scaled.density=((predicted.density - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min
    return(scaled.density)
  }


  #ITERATING THROUGH SPECIES
  #----------------
  for(i in selected.species){

    message(paste("Simulating taxon: ", parameters[i, "label"], sep=""), "\n")


    #dataframe rows into list
    parameters.list = list()
    for(j in 1:ncol(parameters)){
      parameters.list[[paste0(colnames(parameters)[j])]] = parameters[i,j]
    }


    #parameters from list to environment
    list2env(parameters.list, envir=environment())


    #GETTING DRIVER VALUES
    #IF DRIVERS PROVIDED AS DATAFRAME
    if(drivers.input=="data.frame"){

      #if the autocorrelation.lengt available in parameters for species i is not in drivers, the first autocorrelation length available in drivers is assigned
      if(!(autocorrelation.length.A %in% unique(drivers$autocorrelation.length)) & !(autocorrelation.length.B %in% unique(drivers$autocorrelation.length))){
        message(paste("Autocorrelation lengths in parameters do not match autocorrelation lengths in drivers, I am getting the first value of autocorrelation.length available in drivers: ", unique(drivers$autocorrelation.length)[1], sep=""))
        autocorrelation.length.A = autocorrelation.length.B = unique(drivers$autocorrelation.length)[1]

      }

      #getting driver values
      driver.A.ready = drivers[drivers$driver=="A" & drivers$autocorrelation.length==autocorrelation.length.A, "value"]
      driver.B.ready = drivers[drivers$driver=="B" & drivers$autocorrelation.length==autocorrelation.length.B, "value"]

      #checking if drivers are NA
      if(sum(is.na(driver.A.ready))==length(driver.A.ready)){
        stop("Driver A is made of NA, something is wrong with the drivers argument.")
      }

      if(sum(is.na(driver.B.ready))==length(driver.B.ready)){
        driver.B.ready = rep(1, length(driver.A.ready))
        driver.B.weight=0
        message("Driver B is missing, setting driver.B.weight to 0.")
      }

    }


    #if input drivers are vectors
    if(drivers.input=="vector"){
      driver.A.ready = driver.A

      #setting driver.B.weight to 0 if driver.B was missing
      if(driver.B.available==FALSE){
        driver.B.ready = rep(1, length(driver.A.ready))
        driver.B.weight=0
        message("Driver B is missing, setting driver.B.weight to 0.")
      } else {
        driver.B.ready = driver.B
      }
    }


    #checking niche parameters
    if(is.na(niche.A.sd)==TRUE | niche.A.sd == 0){niche.A.sd = 1}
    if(is.na(niche.B.sd)==TRUE | niche.B.sd == 0){niche.B.sd = 1}
    if(is.na(niche.A.mean)==TRUE){niche.A.mean = 0}
    if(is.na(niche.B.mean)==TRUE){niche.B.mean = 0}


    #COMPUTING MAXIMUM DENSITY (output of normal function) OF EACH DRIVER
    max.possible.density.driver.A = dnorm(niche.A.mean, mean=niche.A.mean, sd=niche.A.sd)
    max.possible.density.driver.B = dnorm(niche.B.mean, mean=niche.B.mean, sd=niche.B.sd)


    #computes suitability over driver.A using dnorm, niche.A.mean, and niche.A.sd, and multiplies it by driver.A.weight
    suitability.A = rescaleSuitability(dnorm(driver.A.ready, mean=niche.A.mean, sd=niche.A.sd), max.possible.density.driver.A) * driver.A.weight

    #same over driver.B
    suitability.B = rescaleSuitability(dnorm(driver.B.ready, mean=niche.B.mean, sd=niche.B.sd), max.possible.density.driver.B) * driver.B.weight

    #sums the results of both is driver.B is available
    suitability = suitability.A + suitability.B

    #rounding to three decimal places
    suitability = round(suitability, 3)


    #BURN-IN PERIOD ADDED TO SUITABILITY
    if(burnin==TRUE){

      burnin.suitability = jitter(c(rep(1, maximum.age*5), seq(1, suitability[1], length.out = maximum.age*5)), amount=0.01)
      burnin.suitability[burnin.suitability < 0]=0
      burnin.suitability[burnin.suitability > 1]=1
      length.burnin.suitability=length(burnin.suitability)
      burnin.suitability = c(burnin.suitability, suitability)

    } else {

      burnin.suitability = suitability

    }


    #VECTORS TO SAVE RESULTS
    pollen.count = vector()
    population.mature = vector()
    population.immature = vector()
    population.seeds = vector()
    population.biomass = vector()
    population.biomass.mature = vector()
    population.biomass.immature = vector()
    mortality.mature = vector()
    mortality.immature = vector()


    #SCALING AGE
    reproductive.age = reproductive.age / maximum.age
    scaled.year = 1/maximum.age
    maximum.age.original = maximum.age
    maximum.age = 1


    #STARTING POPULATION
    population = sample(seq(0, 1, by=scaled.year), 100, replace=TRUE)


    #EXECUTING SIMULATION, one iteration per suitaiblity value
    #----------------
    for(suitability.i in burnin.suitability){

      #aging
      population = population + scaled.year

      #death due to senescence
      population = population[population < maximum.age]

      #population drops to 0
      if (length(population) == 0){

        #local extinction, replaces population with a seedbank
        population = rep(0, floor(100 * suitability.i))

        #adds 0 to the output vectors
        pollen.count = c(pollen.count, 0)
        population.mature = c(population.mature, 0)
        population.immature = c(population.immature, 0)
        population.seeds = c(population.seeds, 0)
        population.biomass = c(population.biomass, 0)
        population.biomass.mature = c(population.biomass.mature, 0)
        population.biomass.immature = c(population.biomass.immature, 0)
        mortality.mature = c(mortality.mature, 0)
        mortality.immature = c(mortality.immature, 0)

        #jumps to next iteration
        next
      }

      #PLANT GROWTH
      biomass =  maximum.biomass / (1 + maximum.biomass * exp(- (growth.rate * suitability.i) * (population * maximum.age.original)))

      #MORTALITY
      individuals.removed = vector()

      #carrying capacity is reached
      while(sum(biomass) > carrying.capacity){

        #removes random individual (curvilinear risk curve)
        individual.to.remove = sample(x=length(population), size=1L, replace=TRUE, prob=1 - sqrt(population))

        #adds the removed individuals to the list
        individuals.removed = c(individuals.removed, population[individual.to.remove])

        #removing individuals
        population = population[-individual.to.remove]
        biomass = biomass[-individual.to.remove]

      }#end of while

      #indexes of adult individuals
      adults = population > reproductive.age

      #producing seeds
      seeds = rep(0, floor(sum((biomass[adults]/maximum.biomass) * fecundity) * suitability.i))

      #filling output vectors
      #pollen count
      pollen.count = c(pollen.count, sum(biomass[adults]) * max(suitability.i, pollen.control))
      population.mature = c(population.mature, sum(adults))
      population.immature = c(population.immature, sum(population <= reproductive.age))
      population.seeds = c(population.seeds, length(seeds))
      population.biomass = c(population.biomass, sum(biomass))
      population.biomass.mature = c(population.biomass.mature, sum(biomass[adults]))
      population.biomass.immature = c(population.biomass.immature, sum(biomass[!adults]))
      mortality.mature = c(mortality.mature, sum(individuals.removed > reproductive.age))
      mortality.immature = c(mortality.immature, sum(individuals.removed <= reproductive.age))

      #joining seeds to the population
      population=c(population, seeds)

    } #end of loop through suitability values


    #removing drivers that were not used
    if(driver.A.weight == 0){
      driver.A.write=rep(NA, length(driver.A.ready))
    } else {
      driver.A.write=driver.A.ready
    }

    if(driver.B.weight == 0 | driver.B.available == FALSE){
      driver.B.write=rep(NA, length(driver.B.ready))
    } else {
      driver.B.write=driver.B.ready
    }

    #data frame
    output.df = data.frame(Time=c(-length.burnin.suitability:-1, 1:(length(suitability))),
                           Pollen=pollen.count,
                           Population.mature=population.mature,
                           Population.immature=population.immature,
                           Population.viable.seeds=population.seeds,
                           Suitability=burnin.suitability,
                           Biomass.total=population.biomass,
                           Biomass.mature=population.biomass.mature,
                           Biomass.immature=population.biomass.immature,
                           Mortality.mature=mortality.mature,
                           Mortality.immature=mortality.immature,
                           Driver.A=c(rep(NA, length.burnin.suitability), driver.A.write),
                           Driver.B=c(rep(NA, length.burnin.suitability), driver.B.write),
                           Period=c(rep("Burn-in", length.burnin.suitability), rep("Simulation", length(suitability))))


    #mergest with output.list
    output.list[[parameters[i, "label"]]]=output.df

  } #end of iteration through selected species

  return(output.list)

}
