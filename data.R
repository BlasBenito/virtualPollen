
#GENERATING DRIVER DATA EXAMPLES
################################
#time
time <- 1:10000
driver.list=c("A", "B")
autocorrelation.length.list=c(200, 600, 1800)
data.ranges<-data.frame(driver=driver.list, output.min=c(0, 0), output.max=c(100, 100), random.seed=c(60, 120))

#dataframes to store drivers.10k
drivers.10k <- drivers.10k.temp <- data.frame(time=numeric(), driver=character(), autocorrelation.length=numeric(), value=numeric())

#looping through drivers.10k and memory lengths
for(driver in driver.list){
  for(autocorrelation.length in autocorrelation.length.list){

    #FILLING drivers.10k.temp
    #---------------------------------------
    #fill drivers.10k.temp with time and grouping
    drivers.10k.temp[max(time), ] <- NA
    drivers.10k.temp$time <- time

    #fill with parameter values
    drivers.10k.temp$driver <- rep(driver, nrow(drivers.10k.temp))
    drivers.10k.temp$autocorrelation.length <- rep(autocorrelation.length, nrow(drivers.10k.temp))

    #fill values of the driver
    simulated.driver <- simulateDriver(
      random.seed=data.ranges[data.ranges$driver==driver, "random.seed"],
      time=time,
      autocorrelation.length=autocorrelation.length,
      output.min=data.ranges[data.ranges$driver==driver, "output.min"],
      output.max=data.ranges[data.ranges$driver==driver, "output.max"]
    )

    #getting the driver values
    drivers.10k.temp$value <- rescaleVector(x=simulated.driver, new.max = 100, new.min = 0)

    #merging with main dataframes
    drivers.10k <- rbind(drivers.10k, drivers.10k.temp)

  }
}

drivers <- drivers.10k
driverA <- drivers.10k[drivers.10k$driver=="A" & drivers.10k$autocorrelation.length==600, "value"]
driverB <- drivers.10k[drivers.10k$driver=="B" & drivers.10k$autocorrelation.length==600, "value"]


#GENERATING PARAMETERS DATA EXAMPLE
###################################
parameters <- parametersDataframe(rows=2)
parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, 600, 600)
parameters[2,] <- c("Species 2", 500, 100, 10, 0.02, 0, 100, 1000, 0.5, 0.5, 50, 10, 70, 20, 600, 600)
parameters <- fixParametersTypes(x=parameters)


#GENERATING SIMULATION
###################################
simulation <- simulatePopulation(parameters=parameters, drivers=NULL, driver.A=driverA, driver.B=driverB)
plotSimulation(simulation.output=simulation)

save(drivers, file = "data/drivers.RData")
save(driverA, file = "data/driverA.RData")
save(driverB, file = "data/driverB.RData")
save(parameters, file = "data/parameters.RData")
save(simulation, file = "data/simulation.RData")
