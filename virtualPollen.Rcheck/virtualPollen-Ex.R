pkgname <- "virtualPollen"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "virtualPollen-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('virtualPollen')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("acfToDf")
### * acfToDf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: acfToDf
### Title: Computes temporal autocorrelation in a vector, and returns a
###   dataframe for easy plotting.
### Aliases: acfToDf

### ** Examples


#getting a driver
data(driverA)

#computing temporal autocorrelations
x.df <- acfToDf(
  x = driverA,
  lag.max = 1000,
  length.out = 100
)
str(x.df)

#plotting output
plotAcf(x.df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("acfToDf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aggregateSimulation")
### * aggregateSimulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aggregateSimulation
### Title: Aggregates the output of 'simulatePopulation'.
### Aliases: aggregateSimulation

### ** Examples


#getting example data
data(simulation)
data(accumulationRate)

#aggregating first simulation outcome
sim.output.aggregated <- aggregateSimulation(
 simulation.output = simulation[1],
 accumulation.rate = accumulationRate,
 sampling.intervals = c(2,6))

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




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aggregateSimulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("compareSimulations")
### * compareSimulations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compareSimulations
### Title: Compares different simulations produced by 'simulatePopulation'.
### Aliases: compareSimulations

### ** Examples


#getting example data
data(simulation)

#compare taxa 1, 2, and 3.
compareSimulations(simulation.output = simulation,
 species = c(1, 2, 3),
 columns = c("Pollen", "Suitability"),
 time.zoom = c(1000, 2000)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compareSimulations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fixParametersTypes")
### * fixParametersTypes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fixParametersTypes
### Title: Fix data types in a parameters dataframe.
### Aliases: fixParametersTypes

### ** Examples


parameters <- parametersDataframe(rows=1)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, 600, 600)
parameters <- fixParametersTypes(x=parameters)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fixParametersTypes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parametersCheck")
### * parametersCheck

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parametersCheck
### Title: Plots main simulation parameters.
### Aliases: parametersCheck

### ** Examples

#getting data
data(parameters)
data(drivers)

#plotting parameters
parametersCheck(
 parameters = parameters,
 drivers = drivers
 )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parametersCheck", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parametersDataframe")
### * parametersDataframe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parametersDataframe
### Title: Generates a template dataframe to contain simulation parameters.
### Aliases: parametersDataframe

### ** Examples


#generating the template
parameters <- parametersDataframe(rows=1)

#filling it with a vector
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, 600, 600)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parametersDataframe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotAcf")
### * plotAcf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotAcf
### Title: Plots results of 'acfToDf'.
### Aliases: plotAcf

### ** Examples


#getting a driver
data(driverA)

#computing temporal autocorrelations
x.df <- acfToDf(
  x = driverA,
  lag.max = 1000,
  length.out = 100
)
str(x.df)

#plotting output
plotAcf(x.df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotAcf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotSimulation")
### * plotSimulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotSimulation
### Title: Plots results of 'simulatePopulation'.
### Aliases: plotSimulation

### ** Examples


#getting example data
data(simulation)

#plot first simulation
plotSimulation(simulation.output = simulation[[1]])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotSimulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rescaleVector")
### * rescaleVector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rescaleVector
### Title: Rescales a vector within given bounds.
### Aliases: rescaleVector

### ** Examples

#generating example data
x = rnorm(100)

#as float
x.float <- rescaleVector(
  x = x,
  new.min = 0,
  new.max = 100,
  integer = FALSE
  )

#as integer
x.integer <- rescaleVector(
  x = x,
  new.min = 0,
  new.max = 100,
  integer = TRUE
  )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rescaleVector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulateAccumulationRate")
### * simulateAccumulationRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulateAccumulationRate
### Title: Simulates a virtual sediment accumulation rate.
### Aliases: simulateAccumulationRate

### ** Examples


acc.rate <- simulateAccumulationRate(
 seed = 50,
 time = 1:1000,
 output.min = 10,
 output.max = 40,
 direction = 1,
 plot = TRUE
 )

str(acc.rate)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulateAccumulationRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulateDriver")
### * simulateDriver

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulateDriver
### Title: Generates a random time series with temporal autocorrelation.
### Aliases: simulateDriver

### ** Examples


x <- simulateDriver(
  random.seed = 30,
  time = 1:10000,
  autocorrelation.length = 100,
  output.min = -10,
  output.max = 20,
  rescale = TRUE
  )

#plots output
plot(x, type = "l")

#checks temporal autocorrelation
acf(x, lag.max = 300)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulateDriver", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulateDriverS")
### * simulateDriverS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulateDriverS
### Title: Generates drivers for 'simulatePopulation'.
### Aliases: simulateDriverS

### ** Examples


drivers <- simulateDriverS(
 random.seeds=c(60, 120),
 time=1:10000,
 autocorrelation.lengths=c(200, 600),
 output.min=c(0,0),
 output.max=c(100, 100),
 driver.names=c("A", "B"),
 filename=NULL
)

str(drivers)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulateDriverS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulatePopulation")
### * simulatePopulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulatePopulation
### Title: Simulates population dynamics for virtual species with different
###   traits.
### Aliases: simulatePopulation

### ** Examples


#getting data
data(parameters)
data(driverA)

#simulating population dynamics
# of first taxon in parameters
# for first 500 values of driverA
sim.output <- simulatePopulation(
 parameters=parameters[1,],
 driver.A=driverA[1:500]
 )

#checking output
str(sim.output)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulatePopulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("toRegularTime")
### * toRegularTime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: toRegularTime
### Title: Reinterpolates aggregated simulations into regular time.
### Aliases: toRegularTime

### ** Examples


## Not run: 
##D #getting example data
##D data(simulation)
##D data(accumulationRate)
##D 
##D #aggregating first simulation outcome
##D sim.output.aggregated <- aggregateSimulation(
##D  simulation.output = simulation[1],
##D  accumulation.rate = accumulationRate,
##D  sampling.intervals = c(2,6))
##D 
##D #to regular time
##D sim.output.aggregated <- toRegularTime(
##D  x=sim.output.aggregated,
##D  time.column ="Time",
##D  interpolation.interval = 10,
##D  columns.to.interpolate = c("Suitability", "Pollen")
##D )
##D 
##D #comparing simulations
##D par(mfrow = c(3,1))
##D #notice the subsetting of the given column of the input list
##D plot(sim.output.aggregated[[1,1]]$Time,
##D     sim.output.aggregated[[1,1]]$Pollen,
##D     type = "l",
##D     xlim = c(500, 1000),
##D     main = "Annual"
##D )
##D plot(sim.output.aggregated[[1,2]]$Time,
##D     sim.output.aggregated[[1,2]]$Pollen,
##D     type = "l",
##D     xlim = c(500, 1000),
##D     main = "2cm"
##D )
##D plot(sim.output.aggregated[[1,3]]$Time,
##D     sim.output.aggregated[[1,3]]$Pollen,
##D     type = "l",
##D     xlim = c(500, 1000),
##D     main = "6cm"
##D )
##D 
##D #check differences in nrow
##D nrow(sim.output.aggregated[[1,1]]) #original data
##D nrow(sim.output.aggregated[[1,2]]) #2cm
##D nrow(sim.output.aggregated[[1,3]]) #6cm intervals
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("toRegularTime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
