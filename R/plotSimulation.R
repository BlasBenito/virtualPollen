#' Plots results of \code{\link{simulatePopulation}}.
#'
#' @description This function takes as input a list of dataframes or a single dataframe resulting from the execution of \code{\link{simulatePopulation}}, and plots the resulting time series of pollen abundance, number of individuals, biomass, driver, and environmnetal suitability.
#'
#'
#' @usage plotSimulation(
#'   simulation.output=NULL,
#'   species="all",
#'   burnin=FALSE,
#'   filename=NULL,
#'   time.zoom=NULL,
#'   panels=c("Driver A",
#'            "Driver B",
#'            "Suitability",
#'            "Population",
#'            "Mortality",
#'            "Biomass",
#'            "Pollen"
#'            ),
#'  plot.title=NULL,
#'  width=12,
#'  text.size=20,
#'  title.size=25,
#'  line.size=1
#'  )
#'
#' @param simulation.output output of \code{\link{simulatePopulation}}.
#' @param species a number or vector or numbers representing rows in the parameters dataframe, or a string or vector of strings referencing to the "label" column of the parameters dataframe.
#' @param burnin if \code{FALSE}, burn-in period is not considered in the model.
#' @param filename character string, name of output pdf file. If NULL or empty, no pdf is produced.
#' @param time.zoom vector of two numbers indicating the beginnign and end of the time interval to be plotted (i.e. "c(5000, 10000)")
#' @param panels character string or vector of character strings with these possible values: "Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen".
#' @param plot.title character string to use as plot title.
#' @param width plot width in inches.
#' @param text.size text size of the plot.
#' @param title.size plot title size.
#' @param line.size size of lines in plots.
#'
#' @details The user can decide what virtual taxa to plot (argument \code{species}), and what information to show throught the \code{panels} argument. Output is plotted on screen by default, and printed to pdf if the \code{filename} argument is filled.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#'
#' @seealso \code{\link{simulatePopulation}}, \code{\link{compareSimulations}}
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
#'parameters <- parametersDataframe(rows = 2)
#'parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#'parameters[2,] <- c("Species 1", 500, 100, 10, 0.02, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#'parameters <- fixParametersTypes(x = parameters)
#'
#'#simulating population dynamics
#'sim.output <- simulatePopulation(
#'  parameters = parameters,
#'  driver.A = driver
#'  )
#'
#'#plot simulation
#'plotSimulation(simulation.output = sim.output)
#'
#' @export
plotSimulation <- function(
  simulation.output = NULL,
  species = "all",
  burnin = FALSE,
  filename = NULL,
  time.zoom = NULL,
  panels = c("Driver A",
           "Driver B",
           "Suitability",
           "Population",
           "Mortality",
           "Biomass",
           "Pollen"),
  plot.title = NULL,
  width = 12,
  text.size = 20,
  title.size = 25,
  line.size = 1){

  require(tidyr)
  require(ggplot2)
  require(tidyr)
  require(cowplot)

  #checking and setting panels
  if(length(panels) == 1){
    if(panels == "all" | panels == "ALL" | panels == "All" | is.null(panels) | length(panels) == 0 | !is.character(panels)){
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
  if(species == "all" | species == "ALL" | species == "All"){
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

      #removing burn-in period if burnin == FALSE
      if(burnin == FALSE){output.long = output.long[output.long$Period == "Simulation",]}

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
    output.long[output.long$Variable == "Pollen", "Facets"] = "Pollen"
    output.long[grep("Biomass", output.long$Variable), "Facets"] = "Biomass"
    output.long[grep("Mortality", output.long$Variable), "Facets"] = "Mortality"
    output.long[output.long$Variable == "Suitability", "Facets"] = "Suitability"
    output.long[output.long$Variable == "Driver.A", "Facets"] = "Driver A"

    #checking if driver B is empty
    if(sum(is.na(output$Driver.B))!=nrow(output)){
      output.long[output.long$Variable == "Driver.B", "Facets"] = "Driver B"
      #facets order
      output.long$Facets=factor(output.long$Facets, levels=c("Driver A", "Driver B","Suitability", "Population", "Mortality", "Biomass", "Pollen"))
    } else {
      output.long$Facets=factor(output.long$Facets, levels=c("Driver A","Suitability", "Population", "Mortality", "Biomass", "Pollen"))
    }

    #preparing subgroups for color
    output.long$Color = "Adults"
    output.long[grep("immature", output.long$Variable), "Color"] = "Saplings"
    output.long[grep("total", output.long$Variable), "Color"] = "Total biomass"
    output.long[output.long$Variable == "Pollen", "Color"] = "Pollen"
    output.long[output.long$Variable == "Population.viable.seeds", "Color"] = "Seedlings"
    output.long[output.long$Variable == "Suitability", "Color"] = "Suitability"
    output.long[output.long$Variable == "Driver.A", "Color"] = "Driver A"


    #checking if driver B is empty
    if(sum(is.na(output$Driver.B))!=nrow(output)){
      output.long[output.long$Variable == "Driver.B", "Color"] = "Driver B"
      #facets order
      output.long$Color <- factor(output.long$Color, levels = c("Driver A", "Driver B", "Suitability", "Total biomass", "Adults", "Saplings", "Seedlings", "Pollen"))
      #palette
      color.palette <- c("#2F642A", "#57AD4F", "#000000", "#C45055", "#75E46A", "#4572A9", "gray40", "gray40")
      names(color.palette) <- c("Adults", "Saplings", "Total biomass", "Pollen", "Seedlings", "Suitability", "Driver A", "Driver B")
    } else {
      output.long$Color <- factor(output.long$Color, levels = c("Driver A", "Suitability", "Total biomass", "Adults", "Saplings", "Seedlings", "Pollen"))
      #palette
      color.palette <- c("#2F642A", "#57AD4F", "#000000", "#C45055", "#75E46A", "#4572A9", "gray40")
      names(color.palette) <- c("Adults", "Saplings", "Total biomass", "Pollen", "Seedlings", "Suitability", "Driver A")
    }

    #removing unwanted facets/panels
    output.long <-output.long[output.long$Facets %in% panels, ]

    #setting up plot title
    if(is.null(plot.title)){
      plot.title <- paste("Taxon: ", names(simulation.output)[i], sep = "")
      }


    #plot
    p1 <- ggplot(data = output.long, aes(x = Time, y = Value, color = Color)) +
      geom_rect(data = output.long, aes(xmin = min(min(Time), 0), xmax = 0, ymin = 0, ymax = Inf), inherit.aes = FALSE, fill = "gray90") +
      geom_line(size = line.size) +
      scale_colour_manual(values = color.palette) +
      facet_wrap(facets = "Facets", scales = "free_y", ncol = 1, drop = TRUE) +
      ggtitle(plot.title) +
      xlab("Time (years)") +
      ylab("") +
      geom_vline(xintercept = seq(0, max(output.long$Time), by = 200), color = "gray") +
      scale_x_continuous(breaks = seq(age.min, age.max, by = age.max/10)) +
      theme(text  =  element_text(size = text.size), axis.text  =  element_text(size = text.size), axis.title  =  element_text(size = text.size), legend.position = "bottom", plot.title  =  element_text(size  =  title.size)) +
      labs(color = 'Legend') +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      coord_cartesian(xlim = c(age.min, age.max))
    # guides(linetype = guide_legend(override.aes = list(size = 4)))
    # + theme(plot.margin=unit(c(1,3,1,1),"cm"))

    plots.list[[i]] <- p1

  } #end of iteration through species

  #plots to screen
  invisible(lapply(plots.list, print))

  #plots to pdf
  if(!is.null(filename) & is.character(filename)){
    pdf(paste(filename, ".pdf", sep = ""), width = 12, height = length(unique(output.long$Facets))*2)
    invisible(lapply(plots.list, print))
    dev.off()
  }


} #end of plotting function
