library(virtualPollen)

#TEST: rescaleVector PASSED
##################################

#help test
help(rescaleVector)

#test example
x = rnorm(100)
x.float <- rescaleVector(x = x, new.min = 0, new.max = 10, integer = FALSE)
x.integer <- rescaleVector(x = x, new.min = 0, new.max = 10, integer = TRUE)
plot(x.float, type="l")
lines(x.integer, col="red")

#new.max lower than new.min test
x.float <- rescaleVector(x = x, new.min = 10, new.max = 0, integer = FALSE)
plot(x.float, type="l")


#TEST: simulateDriver
##################################

#help test
help(simulateDriver)

#test example with rescale = TRUE
x.rescaled <- simulateDriver(random.seed = 30,
                    time = 1:1000,
                    autocorrelation.length = 200,
                    output.min = -10,
                    output.max = 20,
                    rescale = TRUE)
plot(x.rescaled, type="l")
acf(x.rescaled, lag.max = 300)

#test example with rescale = FALSE
x <- simulateDriver(random.seed = 30,
                    time = 1:1000,
                    autocorrelation.length = 200,
                    output.min = -10,
                    output.max = 20,
                    rescale = FALSE)
plot(x, type="l")
acf(x, lag.max = 300)

par(mfrow=c(2,1))
plot(x, type="l")
plot(x.rescaled, type="l")


#TEST simulateDriverS
#################################
drivers.test <- simulateDriverS(random.seeds=c(60, 120),
                time=1:10000,
                autocorrelation.lengths=c(200, 600, 1800),
                output.min=c(0,0),
                output.max=c(100, 100),
                driver.names=c("A", "B"),
                filename=NULL
                )


#TEST: acfToDf and plotAcf
##################################

x <- simulateDriver(random.seed = 10, time = 1:1000, autocorrelation.length = 200, output.min = -10, output.max = 20, rescale = TRUE)
x.df <- acfToDf(x = x, lag.max = 300, length.out = 100)
plot.x.df <- plotAcf(x.df)
plot.x.df


#TEST parametersDataframe AND fixParametersTypes
#################################
parameters <- parametersDataframe(rows=1)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 10000, 1, 0, 50, 10, 0, 0, 600, 0)
parameters <- fixParametersTypes(x=parameters)


#TEST parametersCheck
################################
data(parameters)
data(drivers)

#plotting parameters
parametersCheck(parameters=parameters, drivers=drivers)


#TEST simulatePopulation
################################
data(drivers)
data(driverA)
data(driverB)
data(parameters)

sim.output <- simulatePopulation(parameters=parameters, drivers=drivers)
sim.output <- simulatePopulation(parameters=parameters, driver.A=driverA, driver.B=driverB, species=1)
str(sim.output[[2]])

x11()
plotSimulation(simulation.output = sim.output, species=1)

x11()
plotSimulation(simulation.output = sim.output, species=2)


#TEST plotSimulation and compareSimulations
#############################
driver <- simulateDriver(random.seed = 10, time = 1:1000, autocorrelation.length = 200, output.min = 0, output.max = 100, rescale = TRUE)

#preparing parameters
parameters <- parametersDataframe(rows=2)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, 600, 0)
parameters[2,] <- c("Species 2", 500, 100, 10, 0.02, 0, 100, 1000, 1, 0, 50, 10, 0, 0, 600, 0)
parameters <- fixParametersTypes(x=parameters)

#simulating population dynamics
sim.output <- simulatePopulation(parameters=parameters, driver.A=driver, driver.B=NULL)
str(sim.output[[1]])

#plotting result
plotSimulation(simulation.output = sim.output)
plotSimulation(simulation.output = sim.output, species=2, panels = c("Suitability", "Pollen"), filename="/home/blas/test.pdf")

#comparing simulations
compareSimulations(simulation.output = sim.output, time.zoom = c(400, 600))


#TEST simulateAccumulationRate
###########################
acc.rate <- simulateAccumulationRate(seed=50, time=1:1000, output.min=10, output.max=40, direction=1, plot=TRUE)
str(acc.rate)


#TEST aggregateSimulation
##########################
#generating driver
driver <- simulateDriver(random.seed = 10, time = 1:10000, autocorrelation.length = 200, output.min = 0, output.max = 100, rescale = TRUE)

#preparing parameters
parameters <- parametersDataframe(rows=1)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 10000, 1, 0, 50, 10, 0, 0, 600, 0)
parameters <- fixParametersTypes(x=parameters)

#simulating population dynamics
sim.output <- simulatePopulation(parameters=parameters, driver.A=driver, driver.B=NULL)

#generating accumulation rate
acc.rate <- simulateAccumulationRate(seed=50, time=1:10000, output.min=10, output.max=40, direction=1, plot=TRUE)

#aggregating simulated data
sim.output.aggregated <- aggregateSimulation(simulation.output=sim.output, accumulation.rate=acc.rate, sampling.intervals=3)

#comparing simulations
par(mfrow=c(3,1))
#note that both lines only differentiate in the column taken from the input list
plot(sim.output.aggregated[[1,1]]$Time, sim.output.aggregated[[1,1]]$Pollen, type="l", xlim = c(0, 1000), main="Annual")
plot(sim.output.aggregated[[1,2]]$Time, sim.output.aggregated[[1,2]]$Pollen, type="l", xlim = c(0, 1000), main="1cm")
plot(sim.output.aggregated[[1,3]]$Time, sim.output.aggregated[[1,3]]$Pollen, type="l", xlim = c(0, 1000), main="3cm")

#check differences in nrow
nrow(sim.output.aggregated[[1,1]])
nrow(sim.output.aggregated[[1,2]])
nrow(sim.output.aggregated[[1,3]])

#to regular time
sim.output.regular <- toRegularTime(x=sim.output.aggregated, time.column="Time", interpolation.interval=10, columns.to.interpolate=c("Driver.A", "Pollen"))

#check differences in nrow
nrow(sim.output.regular[[1,1]])
nrow(sim.output.regular[[1,2]])
nrow(sim.output.regular[[1,3]])
