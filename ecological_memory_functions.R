###############################################################################################
#Computes the temporal autocorrelation of a vector, and returns a dataframe ready to be plotted in ggplot
#x: target vector
#lag.max: maximum number of lags to compute temporal autocorrelation
#length.out: number of rows of the output dataframe, to control de density of lines plotted

acfToDf = function(x, lag.max, length.out){
  
  #computes acf
  acf.output = acf(x, lag.max=lag.max, plot=FALSE)
  
  #computes confidence interval (same equation as in plot.acf())
  acf.ci = qnorm((1 + 0.95)/2)/sqrt(acf.output$n.used)
  
  #creates and fills dataframe
  acf.df = data.frame(lag=acf.output$lag, acf=acf.output$acf, ci.max=acf.ci, ci.min=-acf.ci)
  
  #resamples the data to reduce the number of lines to be plotted
  if(nrow(acf.df) > 20){
    acf.df = acf.df[floor(seq(1, nrow(acf.df), length.out = length.out)),]
  }
  
  return(acf.df)
}


###############################################################################################
#Generates time series (to be used as drivers) with a given length 
#random.seed: a integer for set.seed(), to ensure experiment reproducibility.
#time: a vector (i.e. 1:1000), representing the length of the output.
#autocorrelation.length: length of the desired temporal autocorrelation structure, in the same units as *time*.
#output.min: minimum value of the output.
#output.max: maximum value of the output.

simulateDriver = function(random.seed=50, time=NULL, autocorrelation.length=100, output.min=0, output.max=100){
  
  set.seed(random.seed)
  
  #generates values from a normal distribution with a given memory length
  if (autocorrelation.length < length(time)/2){
    
    driver = filter(rnorm(max(time)), filter=rep(1, autocorrelation.length), circular=TRUE)
    
  } else {
    #constant memory
    #the first part cumsum(blah blah) is a straight line with an ascending trend
    #the second is an error term
    driver = cumsum(rep(1 , max(time))) + cumsum(rnorm(n=max(time), mean=0, sd=max(time) / 500))
  }
  
  # #rescale driver to the interval output.min output.max
  driver = rescaleVector(x=driver, new.min=output.min, new.max=output.max)
  
  #converts to vector
  driver = as.vector(driver)
  
  return(driver)
  
}


###############################################################################################
#Re-scales a vector between given bounds
#x: input vector
#new.min: new minimum value
#new.max: new maximum value
#integer: if TRUE, returned vector is of type integer

rescaleVector=function(x, new.min=0, new.max=100, integer=FALSE){
  #data extremes
  old.min=min(x)
  old.max=max(x)
  
  #scaling
  x=((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min
  
  if(integer==TRUE){
    x=round(x, 0)
  }
  
  return(x)
}


##################################################################
#GENERATES AN EMPTY PARAMETERS DATAFRAME
#rows: numer of rows desired in the parameters dataframe

parametersDataframe = function(rows){
  
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


##################################################################
#PLOTS PARAMETERS TO BE USED IN A SIMULATION
#parameters: the parameters dataframe
#speccies: if "all" or "ALL", all species in "parameters" are plotted. It also accepts a vector of numbers representing the rows of the selected species, or a vector of names of the selected species.
#driver.A: vector of values of a driver.
#driver.B: vector of values of a driver.
#drivers: dataframe with drivers.
#filename: filename of the output pdf.

parametersCheck = function(parameters, species="all", driver.A=NULL, driver.B=NULL, drivers=NULL, filename=NULL){
  
  require("ggplot2")
  require("cowplot")
  require("viridis")
  
  #CHECKING DATAFRAME
  #if it's  not a dataframe
  if(!is.data.frame(parameters)){stop("params.df is not a dataframe.")}
  
  #if no rows
  if(nrow(parameters)==0){stop("params.df has no rows.")}
  
  #colnames
  if(sum(colnames(parameters) %in% c("maximum.age", "reproductive.age", "growth.rate", "maximum.biomass", "carrying.capacity", "fecundity", "driver.A.weight", "driver.B.weight", "niche.A.mean", "niche.A.sd", "niche.B.mean", "niche.B.sd" ))!=12){
    stop("params.df is missing some columns. Column names should be: maximum.age, reproductive.age, growth.rate, maximum.biomass, carrying.capacity, fecundity, driver.A.weight, driver.B.weight, niche.A.mean, niche.A.sd, niche.B.mean, niche.B.sd")
  }
  
  #dataframe to store data
  plot.df = data.frame(Species=character(), Driver=character(), Driver.density.x=numeric(), Driver.density.y=numeric(), Driver.weights=numeric(), Value=numeric(), Suitability=numeric(), Age=numeric(), Biomass=numeric(), Reproductive.age=numeric(), Fecundity=numeric())
  
  #SELECTING SPECIES
  #----------------
  #creating dictionary of species names and indexes
  names.dictionary = data.frame(name=parameters$label, index=1:nrow(parameters))
  
  #if null or "all"
  if(is.null(species) | species=="all" | species=="ALL" | species=="All"){
    selected.species = names.dictionary$index
  } else {
    
    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){stop("You have selected species that are not available in the parameters table.")}
    
    #correct species names or indexes
    if(species %in% names.dictionary$names){
      selected.species = names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species = species
    }
  }
  
  #defining driver input
  if(is.null(drivers)){drivers.input="two.tables"} else {drivers.input="one.table"}
  
  #ITERATING THROUGH SPECIES
  for(i in selected.species){
    
    #GETTING DRIVER DATA
    if(drivers.input=="one.table"){
      driver.A = drivers[drivers$driver=="A" & drivers$autocorrelation.length==parameters[i, "autocorrelation.length.A"], "value"]
      driver.B = drivers[drivers$driver=="B" & drivers$autocorrelation.length==parameters[i, "autocorrelation.length.B"], "value"]
    }
    
    #if no drivers, stop
    if(length(driver.A)==0 & length(driver.B==0)){stop("Drivers not available, at least driver.A is required.")}
    if(length(driver.A)>0 & length(driver.B)>0){driver.B.available=TRUE}else{driver.B.available=FALSE}

    
    #preparing driver.A
    density.driver.A = density(driver.A, from=min(driver.A), to=max(driver.A), n=100, bw=max(driver.A)/100)
    density.driver.A.y = (density.driver.A$y - min(density.driver.A$y)) / (max(density.driver.A$y) - min(density.driver.A$y))
    driver.A.range = seq(min(driver.A), max(driver.A), length.out = 100)
    niche.A = dnorm(x=driver.A.range, mean=parameters[i, "niche.A.mean"], sd=parameters[i, "niche.A.sd"])
    niche.A = niche.A / max(niche.A)
    driver.A.weight = parameters[i, "driver.A.weight"]
    
    #preparing driver.B
    if(driver.B.available){
    density.driver.B = density(driver.B, from=min(driver.B), to=max(driver.B), n=100, bw=max(driver.B)/100)
    density.driver.B.y = (density.driver.B$y - min(density.driver.B$y))/ (max(density.driver.B$y) - min(density.driver.B$y))
    driver.B.range = seq(min(driver.B), max(driver.B), length.out = 100)
    niche.B = dnorm(x=driver.B.range, mean=parameters[i, "niche.B.mean"], sd=parameters[i, "niche.B.sd"])
    niche.B = niche.B / max(niche.B)
    driver.B.weight = parameters[i, "driver.B.weight"]
    } 
    
    #computing biomass
    age = seq(0, parameters[i, "maximum.age"], length.out = 100)
    biomass =  parameters[i, "maximum.biomass"] / (1 +  parameters[i, "maximum.biomass"] * exp(-  parameters[i, "growth.rate"] * age))
    
    #preparing data for plotting
    if(driver.B.available){
    plot.df.temp = data.frame(Species=rep(paste(parameters[i, "label"], sep=""), 100),
                              Driver=c(rep("Driver A", 100), rep("Driver B", 100)),
                              Driver.density.x=c(density.driver.A$x, density.driver.B$x),
                              Driver.density.y=c(density.driver.A.y, density.driver.B.y),
                              Driver.weights = c(rep(driver.A.weight, 100), rep(driver.B.weight, 100)),
                              Value=c(driver.A.range, driver.B.range), 
                              Suitability=c(niche.A, niche.B), 
                              Age=age, 
                              Biomass=biomass, 
                              Reproductive.age=rep(parameters[i, "reproductive.age"], 100),
                              Fecundity=rep(parameters[i, "fecundity"], 100))
    } else {
      plot.df.temp = data.frame(Species=rep(paste(parameters[i, "label"], sep=""), 100),
                                Driver=c(rep("Driver A", 100)),
                                Driver.density.x=c(density.driver.A$x),
                                Driver.density.y=c(density.driver.A.y),
                                Driver.weights = c(rep(driver.A.weight, 100)),
                                Value=driver.A.range, 
                                Suitability=niche.A, 
                                Age=age, 
                                Biomass=biomass, 
                                Reproductive.age=rep(parameters[i, "reproductive.age"], 100),
                                Fecundity=rep(parameters[i, "fecundity"], 100))
    }
    
    
    #putting together with main dataframe
    plot.df = rbind(plot.df, plot.df.temp)
    
  }#end of iterations
  
  plot.df$Suitability = round(plot.df$Suitability, 2)
  plot.df[plot.df$Suitability==0, "Suitability"]=NA
  
  color.palette = viridis(10)
  
  niche.plot = ggplot(data=plot.df, aes(x=Value, y=Suitability, group=Species)) + 
    geom_ribbon(data=plot.df, aes(ymin=0, ymax=Driver.density.y), color="gray80", fill="gray80", alpha=0.5) +
    geom_ribbon(data=plot.df, aes(ymin=0, ymax=Suitability, alpha=Driver.weights), colour=NA, fill=color.palette[3]) +
    geom_line(data=plot.df, aes(x=Value, y=Driver.density.y), color="gray80", alpha=0.5) +
    facet_grid(Species~Driver) + 
    scale_alpha_continuous(range=c(0, 1)) +
    xlab("Driver values") + 
    ylab("Environmental suitability") + 
    theme(strip.background.y = element_blank(), strip.text.y = element_blank(), legend.position="none", text = element_text(size=12), strip.background = element_rect(fill=NA), panel.spacing = unit(1, "lines"))
  
  fecundity.plot = ggplot(data=plot.df, aes(x=Species, y=Fecundity, group=Species)) + 
    geom_hline(aes(yintercept=Fecundity), size=10, color="gray80", alpha=0.5) + 
    geom_hline(aes(yintercept=Fecundity), size=2, color=color.palette[3]) +
    facet_wrap(facets="Species", ncol=1, strip.position="right") + 
    theme(strip.background.y = element_blank(), strip.text.y = element_blank(), text = element_text(size=12), panel.spacing = unit(1, "lines")) +
    scale_y_continuous(limits=c(0, max(plot.df$Fecundity))) + 
    xlab("")
  
  growth.plot = ggplot(data=plot.df, aes(x=Age, y=Biomass, group=Species)) + 
    geom_ribbon(ymin=0, ymax=plot.df$Biomass, color="gray80", fill="gray80", alpha=0.5) + 
    geom_line(aes(x=Reproductive.age, y=Biomass), color=color.palette[3], size=2, alpha=0.8) + 
    facet_wrap(facets="Species", ncol=1, strip.position="right", scales="free_x") + 
    xlab("Age (years)") + 
    ylab("Biomass (relative)") + 
    theme(text = element_text(size=12), panel.spacing = unit(1, "lines"))
  
  joint.plot = plot_grid(niche.plot ,fecundity.plot, growth.plot, ncol=3, rel_widths = c(1 ,0.2, 1), align="h", axis="tb")
  
  title <- ggdraw() + draw_label("Features of virtual species", fontface = 'bold')
  
  
  print(plot_grid(title, joint.plot, ncol = 1, rel_heights = c(0.1, 1)))
  
  #saving to file
  # cowplot::plot_grid(niche.plot, growth.plot, ncol=2)
  
  if(!is.null(filename) & is.character(filename)){
    ggsave(filename=paste(filename, ".pdf", sep=""), width=12, height=2*nrow(parameters))
  }
  
}


##################################################################
#SIMULATES POPULATION BASED ON PARAMETERS FILE
#parameters: a parameters dataframe generated by parametersDataframe()
#species: a number or vector or numbers representing rows in the parameters dataframe, or a string or vector of strings referencing to the "label" column of the parameters dataframe.
#driver.A: a numeric vector of values for a driver
#driver.B: a numeric vector of values for a driver
#drivers: a long format dataframe with the columns "time", "driver" (driver name), "autocorrelation.length" (numeric, autocorrelation length of the driver), and "value" (values of the driver).
#burnin: if FALSE, burn-in period is not considered in the model.

simulatePopulation = function(parameters=NULL,
                              species="all",
                              driver.A=NULL,
                              driver.B=NULL,
                              drivers=NULL,
                              burnin=TRUE){
  
  #defining driver input
  if(is.null(drivers)){drivers.input="two.tables"} else {drivers.input="one.table"}
  
  #default burnin
  if(is.null(burnin)){burnin=TRUE}
  
  #generating output list
  output.list = list()
  
  #function to rescale suitability
  rescaleSuitability=function(predicted.density, max.observed.density){
    new.min=0
    new.max=1
    old.min=0
    old.max=max.observed.density
    scaled.density=((predicted.density - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min
    return(scaled.density)
  }
  
  #SELECTING SPECIES
  #----------------
  #creating dictionary of species names and indexes
  names.dictionary = data.frame(name=parameters$label, index=1:nrow(parameters))
  
  #if null or "all"
  if(is.null(species) | species=="all" | species=="ALL" | species=="All"){
    selected.species = names.dictionary$index
  } else {
    
    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){stop("You have selected species that are not available in the parameters table.")}
    
    #correct species names or indexes
    if(species %in% names.dictionary$names){
      selected.species = names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species = species
    }
  }
  
  #CHECKING TYPE OF drivers INPUT
  if(is.null(drivers)){drivers.input="vector"} else {drivers.input="dataframe"}
  
  #ITERATING THROUGH SPECIES
  #----------------
  for(i in selected.species){
    
    #cat(paste("Simulating species: ", parameters[i, "label"], sep=""), "\n")
    
    #dataframe rows into list
    parameters.list = list()
    for(j in 1:ncol(parameters)){
      parameters.list[[paste0(colnames(parameters)[j])]] = parameters[i,j]
    }
    
    #list to environment
    list2env(parameters.list, envir=environment())
    
    #getting the drivers if only one drivers table is provided
    if(drivers.input=="dataframe"){
      
      #setting autocorrelation length to what is available in drivers
      if(!(autocorrelation.length.A %in% unique(drivers$autocorrelation.length)) & !(autocorrelation.length.A %in% unique(drivers$autocorrelation.length))){
        autocorrelation.length.A = autocorrelation.length.B = unique(drivers$autocorrelation.length)[1]
      }

      driver.A = drivers[drivers$driver=="A" & drivers$autocorrelation.length==autocorrelation.length.A, "value"]
      driver.B = drivers[drivers$driver=="B" & drivers$autocorrelation.length==autocorrelation.length.B, "value"]
      
    }

      #if no drivers, stop
      if(length(driver.A)==0 & length(driver.B==0)){stop("Drivers not available, at least driver.A is required.")}
      
      #checking if driver B is available
      if(length(driver.A)>0 & length(driver.B)>0){
        driver.B.available=TRUE
      } else {
          driver.B.available=FALSE
          driver.A.weight=1
          }
    
    #maximum possible density per driver
    max.possible.density.driver.A = dnorm(niche.A.mean, mean=niche.A.mean, sd=niche.A.sd)
    if(driver.B.available){
      max.possible.density.driver.B = dnorm(niche.B.mean, mean=niche.B.mean, sd=niche.B.sd)
    }
    
    #1 computes suitability over driver.A using dnorm, niche.A.mean, and niche.A.sd, and multiplies it by driver.A.weight
    suitability = rescaleSuitability(dnorm(driver.A, mean=niche.A.mean, sd=niche.A.sd), max.possible.density.driver.A) * driver.A.weight
    
    #2 same over driver.B
    if(driver.B.available){
      suitability.B = rescaleSuitability(dnorm(driver.B, mean=niche.B.mean, sd=niche.B.sd), max.possible.density.driver.B) * driver.B.weight
      
      #sums the results of both is driver.B is available
      suitability = suitability + suitability.B
    }
    
    #rounding to three decimal places
    suitability = round(suitability, 3)
    
    #BURN-IN PERIOD ADDED TO SUITABILITY
    if(burnin==TRUE){
      burnin.suitability = jitter(c(rep(1, maximum.age*5), seq(1, suitability[1], length.out = maximum.age*5)), amount=max(suitability)/100)
      burnin.suitability[burnin.suitability < 0]=0
      burnin.suitability[burnin.suitability > 1]=1
      length.burnin.suitability=length(burnin.suitability)
      burnin.suitability = c(burnin.suitability, suitability)
    }
    
    if(burnin==FALSE){
      burnin.suitability = suitability
    }
    
    #vectors to save results
    pollen.count = vector()
    population.mature = vector()
    population.immature = vector()
    population.seeds = vector()
    population.biomass = vector()
    population.biomass.mature = vector()
    population.biomass.immature = vector()
    mortality.mature = vector()
    mortality.immature = vector()
    
    #scaling age to [0, 1]
    reproductive.age = reproductive.age / maximum.age
    scaled.year = 1/maximum.age
    maximum.age.original = maximum.age
    maximum.age = 1
    
    #starting population
    population = sample(seq(0, 1, by=scaled.year), 100, replace=TRUE)
    
    #iterating through suitability values (including burn-in)
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
      
      #carrying capacity depends on suitability
      # while(sum(biomass) >= carrying.capacity * (suitability.i + 0.001)){
      
      #carrying capacity does not depend on suitability
      while(sum(biomass) > carrying.capacity){
        
        #removes random individual (linear risk curve)
        # individual.to.remove = .Internal(sample(length(population), 2L, TRUE, 1 - population))
        
        #removes random individual (linear risk curve)
        individual.to.remove = .Internal(sample(length(population), 2L, TRUE, 1 - sqrt(population)))
        
        #adds the removed individuals to the list
        individuals.removed = c(individuals.removed, population[individual.to.remove])
        
        #removing individuals
        population = population[-individual.to.remove]
        biomass = biomass[-individual.to.remove]
        
      }#end of while
      
      #indexes of adult individuals
      adults = population > reproductive.age
      
      #producing seeds
      #number of adults * fecundity
      # seeds = rep(0, floor((sum(adults)*fecundity) * suitability.i))
      
      #sum of (age of adults * fecundity) FECUNDITY SCALED BY AGE, +AGE +FECUNDITY
      #seeds = rep(0, floor(sum(population[adults]*fecundity) * suitability.i))
      
      #sum of (fractuibak biomass of adults * fecundity) FECUNDITY SCALED BY AGE, +AGE +FECUNDITY
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
                           Driver.A=c(rep(NA, length.burnin.suitability), driver.A),
                           Driver.B=c(rep(NA, length.burnin.suitability), driver.B),
                           Period=c(rep("Burn-in", length.burnin.suitability), rep("Simulation", length(suitability)))) 

    
    #mergest with output.list
    output.list[[parameters[i, "label"]]]=output.df
    
  } #end of iteration through selected species
  
  #names(output.list) = parameters[selected.species, "label"]
  
  return(output.list)
  
} #end of function


##################################################################
#PLOT RESULTS OF SIMULATIONS
#simulation.output: output of simulatePopulation()
#species: a number or vector or numbers representing rows in the parameters dataframe, or a string or vector of strings referencing to the "label" column of the parameters dataframe.
#burnin: if FALSE, burn-in period is not considered in the model.
#filename: character string, name of output pdf file
#time.zoom: vector of two numbers indicating the beginnign and end of the time interval to be plotted (i.e. "c(5000, 10000)")
#panels: panels to be plotted. Can be a single character or a vector of characters containing any of the following strings: "Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"
#plot.title: string to use as plot title.
#width: plot width in inches.
#text.size: text size of the plot.
#title.size: plot title size.

plotSimulation = function(simulation.output, species="all", burnin=FALSE, filename=NULL, time.zoom=NULL, panels=c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"), plot.title=NULL, width=12, text.size=20, title.size=25, line.size=1){
  
  require(ggplot2)
  require(tidyr)
  
  #checking and setting panels
  if(length(panels)==1){
    if(panels=="all" | panels=="ALL" | is.null(panels) | length(panels)==0 | !is.character(panels)){
      panels=c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen")
    }
  } else {
    if(sum(!(panels %in% c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"))) >= 1){
      warning(paste("There is something wrong with your 'panels' argument. Available panels are ", c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"), " . All panels will be plotted instead"))
      panels=c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen")
    }
  }
  
  #checking time.zoom
  if(!is.null(time.zoom) & length(time.zoom) != 2){stop("Argument time.zoom must be a vector of length two, as in: time.zoom=c(1000, 2000)")}
  
  #list to store plots
  plots.list=list()
  
  #SELECTING SPECIES
  #----------------
  #creating dictionary of species names and indexes
  #getting the data
  if(inherits(simulation.output, "list")){
    names.dictionary = data.frame(name=names(simulation.output), index=1:length(simulation.output))
  } else {
    #fake names.dictionary to be used donwstream when input is a data.frame
    names.dictionary = data.frame(name = 1, index = 1)
  }
  
  #if null or "all"
  if(species=="all" | species=="ALL" | species=="All"){
    selected.species = names.dictionary$index
  } else {
    
    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){stop("You have selected species that are not available in the parameters table.")}
    
    #correct species names or indexes
    if(species %in% names.dictionary$name){
      selected.species = names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species = species
    }
  }
  
  if(inherits(simulation.output, "data.frame")){
    selected.species = 1
  }
  
  
  #ITERATING THROUGH SPECIES
  for(i in selected.species){
    
    #getting the data
    if(inherits(simulation.output, "list")){
      output = simulation.output[[i]]
    }
    
    if(inherits(simulation.output, "data.frame")){
      output = simulation.output
    }
    
    #to long format
    if("Period" %in% colnames(output)){
      output.long = gather(data=output, Variable, Value, 2:(ncol(output)-1))
      
      #removing burn-in period if burnin==FALSE
      if(burnin==FALSE){output.long = output.long[output.long$Period == "Simulation",]}
      
    } else {
      output.long = gather(data=output, Variable, Value, 2:ncol(output))
    }
    
    
    #age limits of the plot
    if(is.null(time.zoom)){
      age.min = 0
      age.max = max(output.long$Time)
    } else {
      age.min = time.zoom[1]
      age.max = time.zoom[2]
      #burning to FALSE to avoid plotting it
      burnin=FALSE
    }
    
    #preparing groups for facets
    output.long$Facets = "Population"
    output.long[output.long$Variable=="Pollen", "Facets"] = "Pollen"
    output.long[grep("Biomass", output.long$Variable), "Facets"] = "Biomass"
    output.long[grep("Mortality", output.long$Variable), "Facets"] = "Mortality"
    output.long[output.long$Variable=="Suitability", "Facets"] = "Suitability"
    output.long[output.long$Variable=="Driver.A", "Facets"] = "Driver A"
    output.long[output.long$Variable=="Driver.B", "Facets"] = "Driver B"
    
    #facets order
    output.long$Facets=factor(output.long$Facets, levels=c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"))
    
    #preparing subgroups for color
    output.long$Color = "Adults"
    output.long[grep("immature", output.long$Variable), "Color"] = "Saplings"
    output.long[grep("total", output.long$Variable), "Color"] = "Total biomass"
    output.long[output.long$Variable=="Pollen", "Color"] = "Pollen"
    output.long[output.long$Variable=="Population.viable.seeds", "Color"] = "Seedlings"
    output.long[output.long$Variable=="Suitability", "Color"] = "Suitability"
    output.long[output.long$Variable=="Driver.A", "Color"] = "Driver A"
    output.long[output.long$Variable=="Driver.B", "Color"] = "Driver B"
    
    #color order
    output.long$Color=factor(output.long$Color, levels=c("Driver A", "Driver B", "Suitability", "Total biomass", "Adults", "Saplings", "Seedlings", "Pollen"))
    
    #palette
    color.palette=c("#2F642A", "#57AD4F", "#000000", "#C45055", "#75E46A", "#4572A9", "gray40", "gray40")
    names(color.palette)=c("Adults", "Saplings", "Total biomass", "Pollen", "Seedlings", "Suitability", "Driver A", "Driver B")
    
    #removing unwanted facets/panels
    output.long = output.long[output.long$Facets %in% panels, ]
    
    #setting up plot title
    if(is.null(plot.title)){plot.title=paste("Taxon: ", names(simulation.output)[i], sep="")}

    
    #plot
    p1 = ggplot(data=output.long, aes(x=Time, y=Value, color=Color)) +
      geom_rect(data=output.long, aes(xmin=min(min(Time), 0), xmax=0, ymin=0, ymax=Inf), inherit.aes=FALSE, fill="gray90") + 
      geom_line(size=line.size) + 
      scale_colour_manual(values=color.palette) + 
      facet_wrap(facets="Facets", scales="free_y", ncol=1, drop=TRUE) + 
      ggtitle(plot.title) + 
      xlab("Time (years)") + 
      ylab("") + 
      geom_vline(xintercept=seq(0, max(output.long$Time), by=200), color="gray") +
      scale_x_continuous(breaks=seq(age.min, age.max, by=age.max/10)) + 
      theme(text = element_text(size=text.size), axis.text = element_text(size=text.size), axis.title = element_text(size=text.size), legend.position="bottom", plot.title = element_text(size = title.size)) + 
      labs(color='Legend') +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      coord_cartesian(xlim=c(age.min, age.max))
    # guides(linetype = guide_legend(override.aes = list(size = 4)))
    # + theme(plot.margin=unit(c(1,3,1,1),"cm"))
    
    plots.list[[i]] = p1
    
  } #end of iteration through species
  
  #plots to screen
  invisible(lapply(plots.list, print))
  
  #plots to pdf
  if(!is.null(filename) & is.character(filename)){
    pdf(paste(filename, ".pdf", sep=""), width=12, height=length(unique(output.long$Facets))*2)
    invisible(lapply(plots.list, print))
    dev.off()
  }
  
  
} #end of plotting function


##################################################################
#PLOTS SIMULATIONS TOGETHER
#simulation.output: result of simulatePopulation
#species: a number or vector or numbers representing rows in the parameters dataframe, or a string or vector of strings referencing to the "label" column of the parameters dataframe.
#filename: character string, name of output pdf file
#time.zoom: vector of two numbers indicating the beginnign and end of the time interval to be plotted (i.e. "c(5000, 10000)")
#columns: columns of the simulations to be plotted together. Can be a single character or a vector of characters containing any of the following strings: "Time", "Pollen", "Population.mature", "Population.immature", "Population.viable.seeds", "Suitability", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immature", "Driver.A", "Driver.B", "Period". 
#plot.title: string to use as plot title.
#width: plot width in inches.
#text.size: text size of the plot.
#title.size: plot title size.

compareSimulations = function(simulation.output, species="all", filename=NULL, columns="Pollen", time.zoom=NULL, width=12, text.size=20, title.size=25, plot.title=""){
  
  require(plyr)
  
  #SELECTING SPECIES
  #----------------
  #creating dictionary of species names and indexes
  #getting the data
  if(inherits(simulation.output, "list")){
    if(is.null(names(simulation.output)) & length(simulation.output)>1){list.names=1:length(simulation.output)} else {list.names=names(simulation.output)}
    names.dictionary = data.frame(name=list.names, index=1:length(simulation.output))
  }
  
  #if null or "all"
  if(species=="all" | species=="ALL" | species=="All"){
    selected.species = names.dictionary$index
  } else {
    
    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){stop("You have selected species that are not available in the parameters table.")}
    
    #correct species names or indexes
    if(species %in% names.dictionary$names){
      selected.species = names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species = species
    }
  }
  
  
  #checking time.zoom
  if(!is.null(time.zoom) & length(time.zoom) != 2){stop("Argument time.zoom must be a vector of length two, as in: time.zoom=c(1000, 2000)")}
  
  #CHECKING COLUMN NAMES
  column.names = c("Time", "Pollen", "Population.mature", "Population.immature", "Population.viable.seeds", "Suitability", "Driver.A", "Driver.B", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immature", "Period") 
  
  if(!(columns %in% column.names)){cat("Warning, the argument columns seem to be wrong. The available column names are:", column.names, ". Returning results based on the column 'Pollen'\n")}
  
  #GETTING THE DATA FROM THE INPUT LIST
  output.df = simulation.output[[selected.species[1]]]
  output.df$Species = names(simulation.output)[selected.species[1]]
  
  #age limits of the plot
  if(is.null(time.zoom)){
    age.min = 0
    age.max = max(output.df$Time)
  } else {
    age.min = time.zoom[1]
    age.max = time.zoom[2]
  }
  
  #ITERATING THROUGH LIST ELEMENTS
  for(i in selected.species[-1]){
    temp = simulation.output[[i]]
    temp$Species = names(simulation.output)[i]
    output.df = rbind.fill(output.df, temp)
  }
  
  #REMOVING BURNIN
  output.df = output.df[output.df$Period=="Simulation", ]
  output.df$Period = NULL
  
  #GETTING THE SPECIES SELECTED BY THE USER
  # output.df = output.df[output.df$Species %in% selected.species, ]
  
  #TO LONG FORMAT
  output.df.long = gather(data=output.df, Variable, Value, 2:(ncol(output.df)-1))
  
  #ORDER OF PANELS TO PLOT
  output.df.long$Variable=factor(output.df.long$Variable, levels=c("Driver.A", "Driver.B","Suitability", "Population.mature", "Population.immature", "Population.viable.seeds", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immagure", "Pollen"))
  
  #GETTING VARIABLES SELECTED BY THE USER
  output.df.long = output.df.long[output.df.long$Variable %in% columns, ]
  
  #plot
  p1 = ggplot(data=output.df.long, aes(x=Time, y=Value, color=Species, group=Species)) +
    geom_line(size=1, alpha=0.5) + 
    scale_colour_viridis(discrete=TRUE, direction=-1, begin = 0, end = 0.95) + 
    facet_wrap(facets="Variable", scales="free_y", ncol=1, drop=TRUE) + 
    ggtitle(plot.title) + 
    xlab("Time (years)") + 
    ylab("") + 
    geom_vline(xintercept=seq(age.min, age.max, by=200), color="gray") +
    scale_x_continuous(breaks=seq(age.min, age.max, by=(age.max-age.min)/10)) + 
    theme(text = element_text(size=text.size), legend.position="bottom", plot.title = element_text(size = title.size)) + 
    labs(color='Legend') +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    coord_cartesian(xlim=c(age.min, age.max))
  
  invisible(print(p1))
  
  if(!is.null(filename)){ggsave(filename, width=width, height=2*length(columns))}
  
}


#########################################################################
#SIMULATE ACCUMULATION RATE
#seed: integer, seed for the pseudo-random number generator. It defines the shape of the curve.
#time: vector of time values (ideally the same used to generate the simulations).
#output.min: numeric, minimum accumulation rate.
#output.max: numeric, maximum accumulation rate.
#direction: values 1 or -1, to invert the path of the resulting accumulation rate.

simulateAccumulationRate=function(seed=50, time=1:10000, output.min=10, output.max=40, direction=1, plot=TRUE){
  
  require(mgcv)
  require(ggplot2)
  require(cowplot)
  
  #setting random seed for repeatibility
  set.seed(seed)
  
  #generating a random walk
  accumulation.rate = cumsum(sample(c(-0.1, 0, 0.1), max(time), TRUE))
  if(direction==-1){accumulation.rate=rev(accumulation.rate)}
  
  #fitting a gam to the data and predicting a smoothed accumulation rate curve
  temp.data=data.frame(accumulation.rate, time)
  temp.gam=gam(accumulation.rate ~ s(time, k=10), data=temp.data)
  accumulation.rate=predict(temp.gam, type="response")
  
  #scaling it between given bounds
  accumulation.rate=rescaleVector(accumulation.rate, new.min=output.min, new.max=output.max, integer=TRUE)
  accumulation.rate=as.vector(accumulation.rate)
  
  #plotting data
  temp.df = data.frame(time, accumulation.rate)
  if(plot==TRUE){
  temp.plot = ggplot(data=temp.df, aes(x=time, y=accumulation.rate)) + 
    geom_line(color=viridis(10)[3], size=0.5) + 
    geom_ribbon(aes(ymin=0, ymax=accumulation.rate), fill=viridis(10)[3], alpha=0.3) +
    xlab("Time") +
    ylab("Acc. rate") +
    scale_y_continuous(breaks=seq(0, output.max, by=10)) +
    scale_x_continuous(breaks=seq(0, max(time), by=2000))
  print(temp.plot)
  }
  
  #generating a grouping variable (consecutive numbers with same value are put in separated groups)
  #applying rle to identify groups of consecutivee integers
  accumulation.rate.rle=rle(accumulation.rate)
  accumulation.rate.rle=data.frame(value=accumulation.rate.rle$values, length=accumulation.rate.rle$lengths)
  
  #using rle as guide to build the groups
  accumulation.rate.groups=vector()
  start.group=0
  for(i in 1:nrow(accumulation.rate.rle)){
    value=accumulation.rate.rle[i, "value"]
    length=accumulation.rate.rle[i, "length"]
    times=start.group + (1:round(length/value, 0))
    accumulation.rate.groups=c(accumulation.rate.groups, rep(times, each=value))
    start.group=max(times)
  }
  accumulation.rate.groups=accumulation.rate.groups[1:max(time)]
  
  output=data.frame(time=time, accumulation.rate=accumulation.rate, grouping=accumulation.rate.groups)
  
  return(output)
  
} #end of function



###############################################################################
#AGGREGATE SIMULATION DATA BY USING THE RESULT OF simulateAccumulationRate()
#simulation.output: output of simulatePopulation
#accumulation.rate: output of simulateAccumulationRate
#sampling intervals: vector of numeric values representing intervals between consecutive samples expressed in centimeters.
#the function returns a matrix-like list, with as many rows as simulations available in simulation output, and as many columns as sampling intervals are requested (plus the original data)

aggregateSimulation=function(simulation.output, accumulation.rate=NULL, sampling.intervals=1){
  
  #get list names
  names.col.1 <- names(simulation.output)
  
  if(is.null(accumulation.rate)){stop("Accumulation rate is missing.")}
  
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
  }
  
  output.list = cbind(simulation.output, accrate.list, intervals.list)
  names(output.list)<-NULL
  return(output.list)
  
}


##############################################################################
#INTERPOLATES A DATAFRAME WITH IRREGULAR TIME INTO REGULAR TIME
#x: list resulting from the function aggregateSimulation.
#time.column: character, name of the input containing time or age.
#interpolation.interval: numeric, interval in years between consecutive samples.
#columns.to.interpolate: vector of character strings defining the columns from each dataframe in x to be interpolated.

toRegularTime <- function(x, time.column, interpolation.interval, columns.to.interpolate=c("Suitability", "Driver.A", "Pollen")){
  
  #list dimensions
  x.rows <- 1:dim(x)[1]
  x.columns <- 2:dim(x)[2] #from 2 to avoid changing the original sim
  
  # pdf("interpolation.pdf", width=6, height=4, pointsize=12)
  
  for(x.row in x.rows){
    for(x.column in x.columns){
      
      #getting the dataframe
      temp <- x[[x.row, x.column]]
      temp <- temp[temp$Period=="Simulation", ]
      
      #computing age extremes
      min.time = 0
      max.time = max(temp[,time.column])
      
      #reference time to interpolate into
      reference.time = round(seq(min.time, max.time, by=interpolation.interval), 0)
      
      #empty dataset to store interpolation
      temp.interpolated = data.frame(time=reference.time)
      names(temp.interpolated)<-time.column
      
      #iterating through columns
      for (column.to.interpolate in columns.to.interpolate){
        
        #do not interpolate non-numeric columns
        if (!is.numeric(temp[, column.to.interpolate])){
          next
        }
        
        #interpolation
        interpolation.formula = as.formula(paste(column.to.interpolate, "~", time.column, sep=" "))
        
        #iteration through span values untill R-squared equals 1
        span.values=seq(50/nrow(temp), 5/nrow(temp), by=-0.0005)
        for(span in span.values){
          
          interpolation.function = loess(interpolation.formula, data=temp, span=span, control=loess.control(surface="direct"))
          
          #check fit
          if(cor(interpolation.function$fitted, temp[, column.to.interpolate]) >=  0.9985){break}
          
        }
        
        #interpolation
        interpolation.result = predict(interpolation.function, newdata=reference.time, se=FALSE)
        
        #constraining the range of the interpolation result to the range of the reference data
        interpolation.range<-range(temp[, column.to.interpolate])
        interpolation.result[interpolation.result < interpolation.range[1]] = interpolation.range[1]
        interpolation.result[interpolation.result > interpolation.range[2]] = interpolation.range[2]
        
        #putting the interpolated data back in place
        temp.interpolated[, column.to.interpolate] = interpolation.result
        
        #test plot
        # plot(temp[,time.column], temp[,column.to.interpolate], type="l", main=paste(x.row, x.column, column.to.interpolate, sep=" , "))
        # lines(temp.interpolated[,time.column], temp.interpolated[,column.to.interpolate], col="red")
        
      }#end of iteration through columns
      
      #removing the time column
      # temp.interpolated[, time.column]=NULL
      
      temp.interpolated$Period = "Simulation"
      
      x[[x.row, x.column]] <- temp.interpolated
    }
  }
  
  # dev.off()
  
  return(x)
}



##############################################################
#GENERATES LAGGED DATA FROM A SIMULATION RESULT
#simulation.data: dataframe produced by the simulation
#response: string, name of the response column (usually, "Pollen")
#drivers: string, driver name or names.
#time: string, column name of the time column
#lags: numeric vector with the lags, in years. Example: c(10, 20, 30) for a lag every 10 years.
#time.zoom: vector of two numbers indicating the beginnign and end of the time interval to be plotted (i.e. "c(5000, 10000)")
prepareLaggedData = function(simulation.data, response, drivers, time, lags, time.zoom=NULL, scale=FALSE){
  
  # #divide lags by 10 if the time resolution is 10 years
  # if(simulation.data[2, time] - simulation.data[1, time] != 1){
  #   lags = lags/10
  # }
  
  #computing data resolution to adjust lags for the annual resolution dataset
  temporal.resolution = simulation.data[2, time] - simulation.data[1, time]
  
  #converting lags from years to cases to be used as lags
  #if lag is 20 and temporal.resolution is 20, the first case before a target case is selected as lag 1
  lags.to.rows = round(lags/temporal.resolution, 0)
  
  #adds 0 to lags if it's not 
  if(!(0 %in% lags)){
    lags.to.rows = c(0, lags.to.rows)
    lags = c(0, lags)
  }
  
  #apply time.zoom if so
  if(!is.null(time.zoom) & is.vector(time.zoom) & is.numeric(time.zoom) & length(time.zoom) == 2){
    simulation.data = simulation.data[simulation.data[, time] >= time.zoom[1] & simulation.data[, time] <= time.zoom[2], ]
  }
  
  #response lags
  response.lags = do.call("merge", lapply(lags.to.rows, function(lag.to.row) lag(as.zoo(simulation.data[,response]), -lag.to.row)))
  
  #naming columns
  colnames(response.lags) = paste("Response", lags, sep = "_")
  
  #driver lags
  for(driver in drivers){
    
    driver.lags = do.call("merge", lapply(lags.to.rows, function(lag.to.row) lag(as.zoo(simulation.data[,driver]), -lag.to.row)))
    
    #naming columns
    colnames(driver.lags) = paste(driver, lags, sep = "_")
    
    #joining with response lags
    response.lags = cbind(response.lags, driver.lags)
    
  }
  
  #removing NA
  response.lags = as.data.frame(response.lags)
  response.lags$time = simulation.data[, time]
  response.lags = na.omit(response.lags)
  time = response.lags$time
  response.lags$time = NULL
  
  #scaling data
  if(scale==TRUE){
    response.lags = data.frame(scale(response.lags), time)
  } else {
    response.lags = data.frame(response.lags, time)
  }
  
  return(response.lags)
  
}



#########################################################################
#INTERACTION PLOT
#plots the interaction between two predictors in a random forest model
#model: a ranger, randomForest, or rpart model
#data: dataframe used to fit the model
#x: character, name of variable to plot on the x axis
#y: character, name of variable to plot on the y axis
#grid: numeric, resolution of the plot grid

plotInteraction <- function(model, data, x, y, z, grid = 100, point.size.range=c(0.1, 1), print=TRUE){
  
  require(cowplot)
  require(ggplot2)
  require(viridis)
  
  #from https://stackoverflow.com/a/49167211
  #to ensure that scale_fill_viridis returns a transparent color scale
  vir_lite = function(cols, ds=0.4, dv=0.7) {
    cols = rgb2hsv(col2rgb(cols))
    cols["v", ] = cols["v", ] + dv*(1 - cols["v", ])
    cols["s", ] = ds*cols["s", ]
    apply(cols, 2, function(x) hsv(x[1], x[2], x[3]))
  }
  
  #generating grid
  newdata <- expand.grid(seq(min(data[[x]]), max(data[[x]]), length.out = grid), seq(min(data[[y]]), max(data[[y]]), length.out = grid))
  colnames(newdata) <- c(x, y)
  
  #setting the other variables to their mean
  other_vars <- setdiff(names(data), c(x, y))
  n <- nrow(data)
  for(i in other_vars){
    # newdata[, i] <- data[, i][sample(n, n)]
    newdata[, i] <- mean(data[, i])
  }
  
  #predicting different types of models
  if(class(model)=="ranger"){
    require(ranger)
    newdata$prediction <- predict(model, newdata)$predictions
    } 
  if(class(model)=="rpart"){
    require(rpart)
    newdata$prediction <- predict(model, newdata, type="vector")
    
    }
  if(!(class(model) %in% c("ranger", "rpart"))){
    require(randomForest)
    newdata$prediction <- predict(model, newdata, type="response")
  }
  
  #if more than 15 unique predictions, 
  if(length(unique(newdata$prediction)) < 15){
    
    newdata$prediction <- round(newdata$prediction, 0)
    newdata$prediction <- as.factor(newdata$prediction)
    
    p1 <- ggplot(newdata, aes_string(x=x, y=y)) +
      geom_raster(aes(fill=prediction), alpha=0.3) + 
      scale_fill_viridis(discrete=TRUE) +
      guides(fill = guide_legend(override.aes = list(alpha = 0.2))) +
      geom_point(data=data, aes_string(x=x, y=y, color=z, size=z, alpha=z), shape=16) +
      scale_color_gradient(low="white", high="black", guide=FALSE) +
      scale_size_continuous(range=point.size.range) +
      scale_alpha_continuous(guide=FALSE) +
      labs(fill="Predicted", size="Observed")
  
  } else {
    
    p1 <- ggplot(newdata, aes_string(x=x, y=y)) +
      geom_raster(aes(fill = prediction), alpha=0.8) + 
      scale_fill_gradientn(colors=vir_lite(viridis(10))) + 
      geom_point(data=data, aes_string(x=x, y=y, color=z, size=z, alpha=z), shape=16) +
      scale_color_gradient(low="white", high="black", guide=FALSE) +
      scale_size_continuous(range=point.size.range) +
      scale_alpha_continuous(guide=FALSE) +
      labs(fill="Predicted", size="Observed")
  }
  
  if(print==TRUE){print(p1)}else{return(p1)}
  
}



###########################################################################
#COMPUTES AN ECOLOGICAL MEMORY PATTERN FROM LAGGED DATA
#lagged.data: a lagged dataset resulting from prepareLaggedData()
#drivers: a string or vector of strings with variables to be used as predictors in the model (i.e. c("Suitability", "Driver.A"))
#add.random: if TRUE, adds a random term to the model, useful to assess the significance of the variable importance scores
#random.mode: either "white.noise" or "autocorrelated".
#repetitions: integer, number of random forest models to fit
#response: character string, name of the response variable (typically, "Response_0")
#lags: numeric vector with lags in years (i.e c(20, 40, 60))
#subset.response: character string with values "up", "down" or "none", triggers the subsetting of the input dataset. "up" only models memory on cases where the response's trend is positive, "down" selectes cases with negative trends, and "none" selects all cases.
computeMemory = function(lagged.data, drivers, add.random=TRUE, random.mode="autocorrelated", repetitions=10, response="Response", subset.response="none"){
  
  #required libraries
  require(ranger)
  require(stringr)
  
  #removing age column
  lagged.data$time = NULL
  
  #removing variables not in drivers
  if(length(drivers)>1){driver.string=paste(drivers, collapse="|")} else {driver.string=drivers}
  string.pattern = paste(response, "|", driver.string, sep="")
  lagged.data = lagged.data[, grepl(string.pattern, colnames(lagged.data))]
  
  #multicollinearity
  multicollinearity = data.frame(vif(lagged.data[, 2:ncol(lagged.data)]))
  multicollinearity = data.frame(variable=rownames(multicollinearity), vif=multicollinearity[,1])
  
  #object to store outputs
  importance.list = list()
  pseudo.R2 = vector()
  predictions.list = list()
  
  #selects cases where the response goes up or down
  lagged.data$subset.column = NA
  
  #response string (checking if there is a 0 or not in the response)
  if(str_detect(response, "_0")==FALSE){response = paste(response, "_0", sep="")}
  if(!(response %in% colnames(lagged.data))){stop("Response variable not found in the input data.")}
  
  #adding labels
  for(i in 1:(nrow(lagged.data)-1)){
    if(lagged.data[i+1, response] > lagged.data[i, response]){lagged.data[i-1, "subset.column"] = "up"}
    if(lagged.data[i+1, response] < lagged.data[i, response]){lagged.data[i-1, "subset.column"] = "down"}
    if(lagged.data[i+1, response] == lagged.data[i, response]){lagged.data[i-1, "subset.column"] = "stable"}
  }
  
  subset.vector = lagged.data$subset.column
  lagged.data$subset.column = NULL
  
  # cat("Repetition: ")
  
  #iterating through repetitions
  for(i in 1:repetitions){
    
    # cat(i, " ")
    
    #subsetting according to user choice
    if(subset.response == "up"){lagged.data.model = lagged.data[subset.vector=="up", ]}
    if(subset.response == "down"){lagged.data.model = lagged.data[subset.vector=="down", ]}
    if(subset.response == "none" | is.null(subset.response)){lagged.data.model = lagged.data}
    lagged.data.model = na.omit(lagged.data.model)
    
    #adding random column
    if(add.random == TRUE){
      lagged.data.model = addRandomColumn(x=lagged.data)
    }#end of adding random column
    
    #fitting random forest
    model.output = ranger(
      dependent.variable.name = response, 
      data = lagged.data.model, 
      importance = "permutation", 
      scale.permutation.importance = TRUE, 
      replace=FALSE,
      splitrule = "variance", 
      min.node.size = 5, 
      num.trees = 2000,
      num.threads = 8,
      verbose = FALSE,
      mtry = 2
    )
    
    #importance
    importance.list[[i]] = data.frame(t(importance(model.output)))
    
    #prediction
    prediction = predict(object=model.output, data=lagged.data.model, type="response")$predictions
    predictions.list[[i]] = data.frame(t(prediction))
    
    #pseudo R.squared
    pseudo.R2[i] = cor(lagged.data.model[, response], prediction)^2
    
  } #end of repetitions
  
  #computing stats of repetitions
  #put results together
  importance.df = do.call("rbind", importance.list)
  
  #processing output for plotting
  importance.df = data.frame(Variable=colnames(importance.df), median=apply(importance.df, 2, median), sd=apply(importance.df, 2, sd), min=apply(importance.df, 2, quantile, probs=0.05), max=apply(importance.df, 2, quantile, probs=0.95))
  
  #separating variable name from lag
  importance.df = transform(importance.df, test=do.call(rbind, strsplit(as.character(importance.df$Variable),'_',fixed=TRUE)), stringsAsFactors=F)
  importance.df$Variable=NULL
  names(importance.df)[5:6] = c("Variable", "Lag")
  
  #removing the word "Random" fromt he lag column
  importance.df[importance.df$Variable == importance.df$Lag, "Lag"] = 0
  
  #repeating the random variable
  if(add.random == TRUE){
    importance.df = rbind(importance.df, importance.df[rep(which(importance.df$Variable=="Random"), each=length(na.omit(unique(importance.df$Lag)))-1),])
    importance.df[importance.df$Variable=="Random", "Lag"] = na.omit(unique(importance.df$Lag))
  }
  
  #setting the floor of random at 0
  importance.df[importance.df$Variable=="Random", "min"] = 0
  
  #setting the median of random to 0 if it is negative (only important when white.noise is selected)
  if(random.mode=="white.noise" & importance.df[importance.df$Variable=="Random", "median"][1] < 0){importance.df[importance.df$Variable=="Random", "median"] = 0}
  
  #variable as factor
  if(add.random==TRUE){
    importance.df$Variable = factor(importance.df$Variable, levels=c("Response", drivers, "Random"))
  } else {
    importance.df$Variable = factor(importance.df$Variable, levels=c("Response", drivers))
  }
  
  #lag to numeric
  importance.df$Lag = as.numeric(importance.df$Lag)
  
  #aggregating predictions
  predictions.aggregated = do.call("rbind", predictions.list)
  predictions.aggregated = data.frame(variable=colnames(predictions.aggregated), median=apply(predictions.aggregated, 2, median), sd=apply(predictions.aggregated, 2, sd), min=apply(predictions.aggregated, 2, quantile, probs=0.05), max=apply(predictions.aggregated, 2, quantile, probs=0.95))
  predictions.aggregated$variable = NULL
  
  #output
  output.list = list()
  output.list$memory = importance.df
  output.list$R2 = pseudo.R2
  output.list$prediction = predictions.aggregated
  output.list$multicollinearity = multicollinearity
  
  return(output.list)
}


###########################################################################
#ADDS A RANDOM COLUMN TO A LAGGED DATASET TO ASSESS SIGNIFICANCE OF VARIABLE IMPORTANCE SCORES
#x: dataframe to be used as input for random forest
#response: character string, name of the response variable (typically, "Response_0")
#mode:  either "white.noise" or "autocorrelated".
addRandomColumn = function(x, random.mode="autocorrelated"){
  
  if(random.mode=="autocorrelated"){
    #generating the data
    x$Random = as.vector(rescaleVector(filter(rnorm(nrow(x)), 
                                          filter=rep(1, sample(1:floor(nrow(x)/4), 1)), 
                                          method="convolution", 
                                          circular=TRUE), new.max = 1, new.min=0))
  } 
  
  if(random.mode=="white.noise"){
    x$Random = rnorm(nrow(x))
  }
  
  return(x)
}



########################################################################
#PLOTTING MEMORY OUTPUT
#memory.output: result of computeMemory()
#title: string, title for the plot
plotMemory = function(memory.output, title="Ecological memory pattern", legend.position="right"){
  
  #to dataframe
  memory.output.df <- memory.output$memory
  
  #plot
  plot.memory = ggplot(data=memory.output.df, aes(x=Lag, y=median, group=Variable, color=Variable, fill=Variable)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, colour=NA) + 
    geom_line(alpha=0.6, size=1.5) +
    scale_color_viridis(discrete=TRUE) +
    scale_fill_viridis(discrete=TRUE) + 
    scale_x_continuous(breaks=unique(memory.output.df$Lag)) +
    xlab("Lag (years)") + 
    ylab("Relative importance") +
    theme(strip.text.x = element_text(size = 12), legend.position = legend.position, axis.text.x = element_text(size=12)) +
    ggtitle(title) 
  
  return(plot.memory)
}


########################################################################################
#EXTRACTS FEATURES FROM AN ANALYSIS OBJECT
#analysis.output: either a dataframe resulting from experimentToTable() or a single dataframe resulting from computeMemory().
#exogenous.component: string, name of the variable defining the exogenous component
#endogenous.component: string, name of the variable defining the endogenous component
#sampling.subset: only used when analysis.output is the result of runExperiment(). String with the dataset type, one of: "Annual", "1cm", "2cm", "6cm", "10cm".

extractMemoryFeatures <- function(analysis.output, exogenous.component, endogenous.component, sampling.subset=NULL){
  
  #entry point
  x=analysis.output
  
  #checking if it is a memory object or not
  if(is.null(x$memory)){
  
  #factors to character
  x$label = as.character(x$label)
  x$Variable = as.character(x$Variable)
  
  #subsetting by sampling
  if(!is.null(sampling.subset)){
    x=x[x$sampling==sampling.subset, ]
  }
  
  #identifying available groups
  taxa=unique(x$label)
  sampling=unique(x$sampling)
  
  #memory object switch to false
  is.memory.object=FALSE
  
  } else {
    #if it is a memory object
    taxa=1
    is.memory.object=TRUE
    
    #if there is no sampling column, we add it
    if(is.null(x$sampling)){
      x$sampling <- "this"
      sampling.subset <- "this"
      
      #identifying available groups (repeated code because I am sick of this shit)
      taxa=unique(x$label)
      sampling=unique(x$sampling)
    }
    
  }
  
  #dataframe to store results
  nas=rep(NA, (length(taxa) * length(sampling)))
  output.df=data.frame(label=nas, strength.endogenous=nas, strength.exogenous=nas, strength.concurrent=nas, length.endogenous=nas, length.exogenous=nas, dominance.endogenous=nas, dominance.exogenous=nas, maximum.age=nas, fecundity=nas, niche.mean=nas, niche.sd=nas, sampling=nas, stringsAsFactors = FALSE)
  
  #row counter
  row.counter = 0
  
  #iterating through taxa and sampling
  for(taxon in taxa){
    for(samp in sampling){
      
    #+1 to the row counter
    row.counter = row.counter + 1
      
    #subsetting the taxon
    if(is.memory.object==FALSE){
    x.temp=x[x$label==taxon, ]
    x.temp=x.temp[x.temp$sampling==samp, ]
    } else {
      x.temp=x$memory
    }
    
    #random median
    random.median = round(x.temp[x.temp$Variable=="Random", "median"][1], 2)
    
    #number of lags
    lags = unique(x.temp$Lag)
    lags = lags[lags!=0]
    
    #computing memory strength (difference betweenn component and median of the random term)
    strength.concurrent = x.temp[x.temp$Variable==exogenous.component & x.temp$Lag==0, "median"] - random.median
    x.temp=x.temp[x.temp$Lag!=0,] #removing lag 0
    strength.endogenous = max(x.temp[x.temp$Variable==endogenous.component, "median"]) - random.median
    strength.exogenous = max(x.temp[x.temp$Variable==exogenous.component, "median"]) - random.median
    
    #computing memory length: number of lags above the median of the random component
    length.endogenous = sum(x.temp[x.temp$Variable==endogenous.component, "median"] > random.median) / length(lags)
    length.exogenous = sum(x.temp[x.temp$Variable==exogenous.component, "median"] > random.median) / length(lags)
    
    #computing component dominance
    endogenous=x.temp[x.temp$Variable==endogenous.component & x.temp$Lag %in% lags, "median"]
    exogenous=x.temp[x.temp$Variable==exogenous.component & x.temp$Lag %in% lags, "median"]
    #values below random.median to zero
    endogenous[endogenous < random.median] = 0
    exogenous[exogenous < random.median] = 0
    #values
    dominance.endogenous = sum(endogenous > exogenous) / length(lags)
    dominance.exogenous =  sum(exogenous > endogenous) / length(lags)
    
    #params
    if(is.memory.object==FALSE){
      maximum.age = x.temp$maximum.age[1]
      fecundity = x.temp$fecundity[1]
      niche.mean = x.temp$niche.A.mean[1]
      niche.sd = x.temp$niche.A.sd[1]
    } else {
      maximum.age = fecundity = niche.mean = niche.sd = NA
    }
    
    #filling dataframe
    output.df[row.counter, ] = c(taxon, strength.endogenous, strength.exogenous, strength.concurrent, length.endogenous, length.exogenous, dominance.endogenous, dominance.exogenous, maximum.age, fecundity, niche.mean, niche.sd, samp)
    
    } #end of iteration through sampling
  } #end of iteration through taxa
  
  #to numeric
  output.df[, 2:(ncol(output.df)-1)] = sapply(output.df[, 2:(ncol(output.df)-1)], as.numeric)
  
  return(output.df)
  
}



#RUN EXPERIMENT
########################################################################
#simulations.file: list, output of simulatePopulation()
#selected.rows: numeric vector, rows of simulations.file to be analyzed.
#selected.columns: numeric vector, colums (datasets) of simulations.file to be analyzed.
#parameters.file: dataframe, parameters used in simulatePopulation() to produce simulations.file.
#parameters.names: vector of strings with names of traits and niche features to be included in the analysis (i.e. c("maximum.age", "fecundity", "niche.A.mean", "niche.A.sd"))
#sampling.names: vector of character strings with the names of the columns of simulations.file (columns are selected through selected.columns). Typically c("Annual", "1cm", "2cm", "6cm", "10cm")
#driver.column: character string, name of the column to be considered as driver (either "Suitability" or "Driver.A").
#response.column: character string defining the response variable, typically "Response_o".
#subset.response: character string with values "up", "down" or "none", triggers the subsetting of the input dataset in computeMemory(). "up" only models memory on cases where the response's trend is positive, "down" selectes cases with negative trends, and "none" selects all cases.
#age.column: character string, name of the time/age column.
#age.zoom: numeric vector with two numbers defining the time/age extremes of the time interval of interest.
#lags: numeric vector with the lags to be used in the experiment.
#repetitions: number of random forest models to be fitted by computeMemory().

runExperiment = function(simulations.file, selected.rows, selected.columns, parameters.file, parameters.names, sampling.names, driver.column, response.column, subset.response="none", time.column, time.zoom=NULL, lags, repetitions){
  
  require(HH)
  ############
  #Example of argument values
  # simulations.file = simulation.2k.interpolated
  # selected.rows = c(1, 7, 13)
  # selected.columns = 1:5
  # parameters.file = parameters.10k
  # parameters.names = "maximum.age"
  # sampling.names = c("annual", "1cm", "2cm", "6cm", "10cm")
  # driver.column = "Suitability"
  # response.column = "Pollen"
  # subset.response = NULL
  # time.column = "Time"
  # time.zoom = c(500, 1000)
  # lags = seq(10, 210, by=20)
  # repetitions = 10
  ############
  
  #subsetting simulations file
  data.list = simulations.file[selected.rows, selected.columns]
  output.list = data.list
  
  #subsetting parameters file
  parameters.list = parameters.file[selected.rows, ]
  
  #generating names matrix
  #-----------------------
  #getting parameter names and values
  if(length(parameters.names==1)){
    
    temp.parameters = data.frame(parameters.list[, parameters.names])
    colnames(temp.parameters) = parameters.names
    
  } else {
    
    temp.parameters = parameters.list[, parameters.names]
    
  }
  
  #joining parameter name and parameter values
  for(current.column in 1:ncol(temp.parameters)){
    
    temp.parameters[,current.column] = paste(colnames(temp.parameters)[current.column], ": ", temp.parameters[,current.column], sep="")
    
  }
  
  #different parameters together
  if(length(parameters.names) > 1){
    temp.parameters = data.frame(parameters=apply(temp.parameters[ , ] , 1 , paste , collapse = "; " ))
  }
  
  #to matrix
  temp.parameters = as.matrix(temp.parameters)
  
  #parameters with sampling names
  sampling.names.matrix = matrix(rep(sampling.names, length(selected.rows)), nrow=length(selected.rows), ncol=length(selected.columns), byrow = TRUE)
  
  #these are the simulation names
  simulation.names = matrix(paste(temp.parameters, sampling.names.matrix, sep="; sampling: "), nrow=length(selected.rows), ncol=length(selected.columns), byrow = FALSE)
  
  #RUNNING ANALYSIS FOR EACH CASE
  if(is.null(dim(data.list))){
    list.rows=length(data.list)
    list.columns=1
  } else {
    list.rows=dim(data.list)[1]
    list.columns=dim(data.list)[2]
  }
  
  for(data.list.row in 1:list.rows){
    for(data.list.column in 1:list.columns){
      
      #info 
      #cat(paste("\n Row: ", data.list.row, "; Column: ", data.list.column, "; Model: ", simulation.names[data.list.row, data.list.column], "\n", sep=""))
      
      #retrieves data
      if(list.columns > 1){
        simulation.data = data.list[[data.list.row, data.list.column]]
      } else {
        simulation.data = data.list[[data.list.row]]
      }
      
      #adds lags
      simulation.data.lagged = prepareLaggedData(simulation.data=simulation.data, 
                                                 response=response.column, 
                                                 drivers=driver.column, 
                                                 time=time.column, 
                                                 lags=lags, 
                                                 time.zoom=time.zoom)
      
      #fitting model
      simulation.data.memory = computeMemory(lagged.data=simulation.data.lagged,
                                             drivers=driver.column,
                                             repetitions=repetitions,
                                             add.random=TRUE,
                                             random.mode="autocorrelated",
                                             response="Response",
                                             subset.response=subset.response)
      
      #saves result
      if(list.columns > 1){
        output.list[[data.list.row, data.list.column]] = simulation.data.memory
      } else {
        output.list[[data.list.row]] = simulation.data.memory
      }
      
      #clears RAM space
      gc()
      
    } #end of iterations through rows
  } #end of iterations through columns
  
  list.to.return = list()
  list.to.return$names = simulation.names
  list.to.return$output = output.list
  return(list.to.return)
  
} #end of function




########################################################################
#PLOT EXPERIMENT plots result of runExperiment()
#experiment.output: list resulting from runExperiment().
#parameters.file: dataframe with parameters of the virtual taxa.
#experiment.title: character string with the plot title
#sampling.names: vector of character strings with the names of the columns of simulations.file (columns are selected through selected.columns). Typically c("Annual", "1cm", "2cm", "6cm", "10cm")
#legend.position: character string. One of: "bottom", "right".
#R2: boolean (TRUE/FALSE): whether to print pseudo R-squared in the model title or not.
#filename: a path with file name and extension included, to save pdf file.

plotExperiment = function(experiment.output, parameters.file, experiment.title, sampling.names, legend.position, R2=NULL, filename=NULL, strip.text.size=12, axis.x.text.size=8, axis.y.text.size=12, axis.x.title.size=14, axis.y.title.size=14, title.size=18){
  
  simulation.df = experimentToTable(experiment.output=experiment.output, 
                                    parameters.file=parameters, 
                                    sampling.names=sampling.names, 
                                    R2=R2)
  
  #order of name as it comes in the dataset
  simulation.df$name = factor(simulation.df$name, levels=unique(simulation.df$name))
  
  #plot
  experiment.plot <- ggplot(data=simulation.df, aes(x=Lag, y=median, group=Variable, color=Variable, fill=Variable)) +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3, colour=NA) + 
    geom_line(alpha=0.6, size=1.5) +
    scale_color_viridis(discrete=TRUE) +
    scale_fill_viridis(discrete=TRUE) + 
    scale_x_continuous(breaks=unique(simulation.df$Lag)) +
    facet_wrap("name", ncol = length(unique(simulation.df$sampling)), scales="free_y") + 
    xlab("Lag (years)") + 
    ylab("Relative importance") +
    theme(strip.text.x = element_text(size = strip.text.size), 
          legend.position = legend.position, 
          axis.text.x = element_text(size=axis.x.text.size),
          axis.text.y = element_text(size=axis.y.text.size),
          axis.title.x = element_text(size=axis.x.title.size),
          axis.title.y = element_text(size=axis.y.title.size),
          plot.title = element_text(size=title.size)) +
    ggtitle(experiment.title) 
  
  if(!is.null(filename) & is.character(filename)){
    ggsave(filename=paste(filename, ".pdf", sep=""), width=length(unique(simulation.df$sampling))*4, height=1.5*nrow(experiment.output$output))
  }
  
  return(experiment.plot)
  
}




################################################################
#EXPERIMENTS TO TABLE transforms output of runExperiment() into a single table
#experiment.output: list, result of runExperiment().
#parameters.file: dataframe, parameters of the virtual taxa.
#column.names: 
experimentToTable = function(experiment.output, parameters.file, sampling.names=NULL, R2=TRUE){
  
  #objects to store results
  df.list = list()
  df.list.index = 0
  
  #assessing the number of columns of experiment.input
  if(is.null(dim(experiment.output$output))){
    list.rows=length(experiment.output$output)
    list.columns=1
    species.names = names(experiment.output$output)
  } else {
    list.rows=dim(experiment.output$output)[1]
    list.columns=dim(experiment.output$output)[2]
    species.names = dimnames(experiment.output$output)[[1]]
  }
  
  #getting species names
  # species.names = dimnames(experiment.output$output)[[1]]
  # if(is.null(species.names))
  # if(length(species.names)!=list.rows){warning("There's something wrong with experiment.output dimensions")}
  # 
  for (current.row in 1:list.rows){
    for (current.column in 1:list.columns){
      
      #getting experiment data
      if(list.columns > 1){
        
        #getting the memory output
        temp.data = experiment.output$output[[current.row,current.column]]$memory
        
        #adding R2 to name if required
        if(R2==TRUE){
          temp.data$name = paste(experiment.output$names[[current.row,current.column]], "; R2 ", round(mean(experiment.output$output[[current.row,current.column]]$R2), 2), "±", round(sd(experiment.output$output[[current.row,current.column]]$R2), 2), sep="")
        } else {
          temp.data$name = experiment.output$names[[current.row,current.column]]
        }
        
        #getting pseudo R squared
        temp.data$R2mean = mean(experiment.output$output[[current.row,current.column]]$R2)
        temp.data$R2sd = sd(experiment.output$output[[current.row,current.column]]$R2)
        
        #getting average multicollinearity
        temp.data$VIFmean = mean(experiment.output$output[[current.row,current.column]]$multicollinearity$vif)
        temp.data$VIFsd = sd(experiment.output$output[[current.row,current.column]]$multicollinearity$vif)
        
        #if only one column
      } else {
        
        #getting the memory output
        temp.data = experiment.output$output[[current.row]]$memory
        
        #getting R2
        if(R2==TRUE){
          temp.data$name = paste(experiment.output$names[[current.row]], "; R2 ", round(mean(experiment.output$output[[current.row]]$R2), 2), "±", round(sd(experiment.output$output[[current.row]]$R2), 2), sep="")
        } else {
          temp.data$name = experiment.output$names[[current.row]]
        }
        
        #getting pseudo R squared
        temp.data$R2mean = mean(experiment.output$output[[current.row]]$R2)
        temp.data$R2sd = sd(experiment.output$output[[current.row]]$R2)
        
        #getting average multicollinearity
        temp.data$VIFmean = mean(experiment.output$output[[current.row]]$multicollinearity$vif)
        temp.data$VIFsd = sd(experiment.output$output[[current.row]]$multicollinearity$vif)
        
      }
      
      #adding parameters
      temp.data = cbind(temp.data, as.data.frame(lapply(parameters.file[parameters.file$label == species.names[current.row], ], rep, nrow(temp.data))))
      
      #adding resampling (sampling.names)
      temp.data$sampling = sampling.names[current.column]
      
      #adding dataframe to list
      df.list.index = df.list.index + 1
      df.list[[df.list.index]] = temp.data
    }
  }
  
  #puttind dataframe together
  simulation.df = do.call("rbind", df.list)
  
  #abbreviate the name field
  simulation.df$name = gsub(pattern="maximum.age", replacement="ma", x=simulation.df$name)
  simulation.df$name = gsub(pattern="reproductive.age", replacement="sma", x=simulation.df$name)
  simulation.df$name = gsub(pattern="sampling", replacement="smp", x=simulation.df$name)
  simulation.df$name = gsub(pattern=":", replacement="", x=simulation.df$name)
  simulation.df$name = gsub(pattern="fecundity", replacement="f", x=simulation.df$name)
  simulation.df$name = gsub(pattern="growth.rate", replacement="gr", x=simulation.df$name)
  simulation.df$name = gsub(pattern="niche.", replacement="", x=simulation.df$name)
  simulation.df$name = gsub(pattern="driver.", replacement="", x=simulation.df$name)
  simulation.df$name = gsub(pattern=".weight", replacement="w", x=simulation.df$name)
  simulation.df$name = gsub(pattern="Annual", replacement="annual", x=simulation.df$name)
  simulation.df$name = gsub(pattern=".mean", replacement="m", x=simulation.df$name)
  simulation.df$name = gsub(pattern=".sd", replacement="sd", x=simulation.df$name)
  simulation.df$name = gsub(pattern="autocorrelation.length.A", replacement="Aac", x=simulation.df$name)
  simulation.df$name = gsub(pattern="autocorrelation.length.B", replacement="Bac", x=simulation.df$name)
  
  return(simulation.df)
  
}
