#' Compares different simulations produced by \code{\link{simulatePopulation}}.
#'
#' @description Plots together the results of different taxa produced by a single run of \code{\link{simulatePopulation}}.
#'
#'
#' @usage compareSimulations(simulation.output=NULL, species="all", filename=NULL, columns="Pollen", time.zoom=NULL, width=12, text.size=20, title.size=25, plot.title="")
#'
#' @param simulation.output output of \code{\link{simulatePopulation}}.
#' @param species a number or vector or numbers representing rows in the parameters dataframe, or a string or vector of strings referencing to the "label" column of the parameters dataframe.
#' @param burnin if \code{FALSE}, burn-in period is not considered in the model.
#' @param filename character string, name of output pdf file. If NULL or empty, no pdf is produced.
#' @param time.zoom vector of two numbers indicating the beginnign and end of the time interval to be plotted (i.e. "c(5000, 10000)")
#' @param columns character string or vector of character strings with these possible values: "Pollen", "Population.mature", "Population.immature", "Population.viable.seeds", "Suitability", "Biomass.total", "Biomass.mature", "Biomass.immature", "Mortality.mature", "Mortality.immature", "Driver.A", "Driver.B".
#' @param plot.title character string to use as plot title.
#' @param width plot width in inches.
#' @param text.size text size of the plot.
#' @param title.size plot title size.
#'
#' @details The user can decide what virtual taxa to plot (argument \code{species}), and what information to show throught the \code{columns} argument. Output is plotted on screen by default, and printed to pdf if the \code{filename} argument is filled.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#'
#' @seealso \code{\link{simulatePopulation}}, \code{\link{plotSimulation}}
#'
#' @examples
#'
#'driver <- simulateDriver(random.seed = 10, time = 1:1000, autocorrelation.length = 200, output.min = 0, output.max = 100, rescale = TRUE)
#'#preparing parameters
#'parameters <- parametersDataframe(rows=2)
#'parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 10000, 1, 0, 50, 10, 0, 0, 600, 0)
#'parameters[2,] <- c("Species 2", 500, 100, 10, 0.02, 0, 100, 10000, 1, 0, 50, 10, 0, 0, 600, 0)
#'parameters <- fixParametersTypes(x=parameters)
#'
#'#simulating population dynamics
#'sim.output <- simulatePopulation(parameters=parameters, driver.A=driver)
#'
#'#plot simulation
#'compareSimulations(simulation.output = sim.output)
#'
#' @export
compareSimulations = function(simulation.output=NULL, species="all", filename=NULL, columns="Pollen", time.zoom=NULL, width=12, text.size=20, title.size=25, plot.title=""){

  require(plyr)
  require(ggplot2)
  require(viridis)
  require(cowplot)

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
    geom_line(size=0.5, alpha=0.5) +
    scale_colour_viridis(discrete=TRUE, direction=-1, begin = 0, end = 0.8) +
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
