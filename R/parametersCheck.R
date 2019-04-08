#' Plots main simulation parameters.
#'
#' @description Plots the normal function/s, fecundity, growth curve, and maturity age, of each virtual taxa in \code{parameters}.
#'
#' @usage parametersCheck(
#'   parameters,
#'   species = "all",
#'   driver.A = NULL,
#'   driver.B = NULL,
#'   drivers = NULL,
#'   filename = NULL
#'   )
#'
#' @param parameters the parameters dataframe.
#' @param species if "all" or "ALL", all species in "parameters" are plotted. It also accepts a vector of numbers representing the rows of the selected species, or a vector of names of the selected species.
#' @param driver.A  numeric vector with driver values.
#' @param driver.B numeric vector with driver values.
#' @param drivers dataframe with drivers
#' @param filename character string, filename of the output pdf.
#'
#' @details The function prints the plot, can save it to a pdf file if \code{filename} is provided, and returns a \code{\link[ggplot2]{ggplot2}} object.
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A \code{\link{ggplot2}} object.
#'
#' @seealso \code{\link{parametersDataframe}}, \code{\link{fixParametersTypes}}
#'
#' @examples
#'#generating driver
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
#'#plotting parameters
#'parametersCheck(
#'  parameters = parameters,
#'  driver.A = driver,
#'  filename = "Parameters.pdf"
#'  )
#'
#' @export
parametersCheck <- function(parameters,
                           species = "all",
                           driver.A = NULL,
                           driver.B = NULL,
                           drivers = NULL,
                           filename = NULL){

  #CHECKING INPUT DATA
  #-------------------

  #CHECKING parameters
  if(is.null(parameters) == TRUE | is.data.frame(parameters) == FALSE){

    stop("The argument 'parameters' empty.")

  } else {

    if(sum(!(colnames(parameters) %in% c("label", "maximum.age", "reproductive.age", "fecundity", "growth.rate", "pollen.control", "maximum.biomass", "carrying.capacity", "driver.A.weight", "driver.B.weight", "niche.A.mean", "niche.A.sd", "niche.B.mean", "niche.B.sd", "autocorrelation.length.A", "autocorrelation.length.B"))) != 0){
      stop(paste("The following column/s of 'parameters' seem to be missing: ", colnames(parameters)[!(colnames(parameters) %in% c("label", "maximum.age", "reproductive.age", "fecundity", "growth.rate", "pollen.control", "maximum.biomass", "carrying.capacity", "driver.A.weight", "driver.B.weight", "niche.A.mean", "niche.A.sd", "niche.B.mean", "niche.B.sd", "autocorrelation.length.A", "autocorrelation.length.B"))], sep=""))

    }
  }


  #CHECKING drivers
  #------------------
  if(is.null(drivers) == FALSE | is.data.frame(drivers) == TRUE){

    #checking columns
    if(sum(!(colnames(drivers) %in% c("time", "driver", "autocorrelation.length", "value"))) != 0){

      stop(paste("The following column/s of 'drivers' seem to be missing: ", colnames(parameters)[colnames(parameters) %not-in% c("time", "driver", "autocorrelation.length", "value")], sep=""))
    }

    } else {
    if(is.null(driver.A) | is.vector(driver.A) | !is.numeric(driver.A)){
      stop("driver.A should be a numeric vector")
    }
    if(is.null(driver.A) | is.vector(driver.A) | !is.numeric(driver.A)){
      message("driver.B not provided")
    } else {
      if(length(driver.A) != length(driver.B)){
        stop("driver.A and driver.B should have the same length.")
      }
    }
  }


  #CHECKING AND SELECTING species
  #----------------
  #creating dictionary of species names and indexes
  names.dictionary <- data.frame(name=parameters$label, index=1:nrow(parameters))

  #if null or "all", selects all species
  if(is.null(species) | species %in% c("all", "All", "ALL")){

    selected.species <- names.dictionary$index

  } else {

    #wrong names or indexes
    if(!(species %in% names.dictionary$name) & !(species %in% names.dictionary$index)){
      stop("You have selected species that are not available in the parameters table.")
    }

    #correct species names or indexes
    if(species %in% names.dictionary$names){
      selected.species <- names.dictionary[names.dictionary$name %in% species, "index"]
    }
    if(species %in% names.dictionary$index){
      selected.species <- species

    }
  }

  #dataframe to store data
  plot.df <- data.frame(Species = character(),
                        Driver = character(),
                        Driver.density.x = numeric(),
                        Driver.density.y = numeric(),
                        Driver.weights = numeric(),
                        Value = numeric(),
                        Suitability = numeric(),
                        Age = numeric(),
                        Biomass = numeric(),
                        Reproductive.age = numeric(),
                        Fecundity = numeric())


  #ITERATING THROUGH SPECIES
  for(i in selected.species){


    #GETTING DRIVER VALUES
    #Drivers provided as dataframe
    if(is.data.frame(drivers) == TRUE){

      #if the autocorrelation.lengt available in parameters for species i is not in drivers, the first autocorrelation length available in drivers is assigned
      if(!(parameters[i, "autocorrelation.length.A"] %in% unique(drivers$autocorrelation.length)) & !(parameters[i, "autocorrelation.length.B"] %in% unique(drivers$autocorrelation.length))){
        message(paste("Autocorrelation lengths in parameters do not match autocorrelation lengths in drivers, I am getting the first value of autocorrelation.length available in drivers: ", unique(drivers$autocorrelation.length)[1], sep=""))
        autocorrelation.length.A <- autocorrelation.length.B <- unique(drivers$autocorrelation.length)[1]

      }

      #getting driver values
      driver.A.ready <- drivers[drivers$driver == "A" & drivers$autocorrelation.length == parameters[i, "autocorrelation.length.A"], "value"]
      driver.B.ready <- drivers[drivers$driver == "B" & drivers$autocorrelation.length == parameters[i, "autocorrelation.length.B"], "value"]

    } else {
      #getting values from vectors
      driver.A.ready <- driver.A
      driver.B.ready <- driver.B
    }


    #checking if drivers are NA
    if(sum(is.na(driver.A.ready)) == length(driver.A.ready)){
      stop("Driver A is made of NA, something is wrong with the drivers argument.")
    }

    if(sum(is.na(driver.B.ready)) == length(driver.B.ready)){
      driver.B.ready <- NULL
      driver.B.weight <- 0
    }

    #checking if drivers have the same length
    if(!is.null(driver.B.ready) & length(driver.A.ready) != length(driver.B.ready)){
      stop("driver.A and driver.B have different lengths.")
    }


    #preparing driver.A
    density.driver.A <- density(x=driver.A.ready, from=min(driver.A.ready), to=max(driver.A.ready), n=100, bw=max(driver.A.ready)/100)
    density.driver.A.y <- (density.driver.A$y - min(density.driver.A$y)) / (max(density.driver.A$y) - min(density.driver.A$y))
    driver.A.range <- seq(min(driver.A.ready), max(driver.A.ready), length.out = 100)
    niche.A <- dnorm(x=driver.A.range, mean=parameters[i, "niche.A.mean"], sd=parameters[i, "niche.A.sd"])
    niche.A <- niche.A / max(niche.A)
    driver.A.weight <- parameters[i, "driver.A.weight"]

    #preparing driver.B
    if(!is.null(driver.B.ready)){
      density.driver.B <- density(x=driver.B.ready, from=min(driver.B.ready), to=max(driver.B.ready), n=100, bw=max(driver.B.ready)/100)
      density.driver.B.y <- (density.driver.B$y - min(density.driver.B$y))/ (max(density.driver.B$y) - min(density.driver.B$y))
      driver.B.range <- seq(min(driver.B.ready), max(driver.B.ready), length.out = 100)
      niche.B <- dnorm(x=driver.B.range, mean=parameters[i, "niche.B.mean"], sd=parameters[i, "niche.B.sd"])
      niche.B <- niche.B / max(niche.B)
      driver.B.weight <- parameters[i, "driver.B.weight"]
    }

    #computing biomass
    age <- seq(0, parameters[i, "maximum.age"], length.out = 100)
    biomass <-  parameters[i, "maximum.biomass"] / (1 +  parameters[i, "maximum.biomass"] * exp(-  parameters[i, "growth.rate"] * age))

    #preparing data for plotting
    if(is.null(driver.B.ready) == FALSE){
      plot.df.temp <- data.frame(Species = rep(paste(parameters[i, "label"], sep = ""), 100),
                                Driver = c(rep("Driver A", 100), rep("Driver B", 100)),
                                Driver.density.x = c(density.driver.A$x, density.driver.B$x),
                                Driver.density.y = c(density.driver.A.y, density.driver.B.y),
                                Driver.weights  =  c(rep(driver.A.weight, 100), rep(driver.B.weight, 100)),
                                Value = c(driver.A.range, driver.B.range),
                                Suitability = c(niche.A, niche.B),
                                Age = age,
                                Biomass = biomass,
                                Reproductive.age = rep(parameters[i, "reproductive.age"], 100),
                                Fecundity = rep(parameters[i, "fecundity"], 100))
    } else {
      plot.df.temp <- data.frame(Species = rep(paste(parameters[i, "label"], sep = ""), 100),
                                Driver = c(rep("Driver A", 100)),
                                Driver.density.x = c(density.driver.A$x),
                                Driver.density.y = c(density.driver.A.y),
                                Driver.weights = c(rep(driver.A.weight, 100)),
                                Value = driver.A.range,
                                Suitability = niche.A,
                                Age = age,
                                Biomass = biomass,
                                Reproductive.age = rep(parameters[i, "reproductive.age"], 100),
                                Fecundity = rep(parameters[i, "fecundity"], 100))
    }


    #putting together with main dataframe
    plot.df <- rbind(plot.df, plot.df.temp)

  }#end of iterations

  plot.df$Suitability <- round(plot.df$Suitability, 2)
  plot.df <- na.omit(plot.df)
  plot.df[plot.df$Suitability == 0, "Suitability"] <- NA

  color.palette <- viridis(10)

  niche.plot <- ggplot(data = plot.df, aes(x = Value, y = Suitability, group = Species)) +
    geom_ribbon(data = plot.df, aes(ymin = 0, ymax = Driver.density.y), color = "gray80", fill = "gray80", alpha = 0.5) +
    geom_ribbon(data = plot.df, aes(ymin = 0, ymax = Suitability, alpha = Driver.weights), colour = NA, fill = color.palette[3]) +
    geom_line(data = plot.df, aes(x = Value, y = Driver.density.y), color = "gray80", alpha = 0.5) +
    facet_grid(Species~Driver) +
    scale_alpha_continuous(range = c(0, 1)) +
    xlab("Driver values") +
    ylab("Environmental suitability") +
    theme(strip.background.y = element_blank(), strip.text.y = element_blank(), legend.position = "none", text = element_text(size = 12), strip.background = element_rect(fill = NA), panel.spacing = unit(1, "lines"))

  fecundity.plot <- ggplot(data = plot.df, aes(x = Species, y = Fecundity, group = Species)) +
    geom_hline(aes(yintercept = Fecundity), size = 10, color = "gray80", alpha = 0.5) +
    geom_hline(aes(yintercept = Fecundity), size = 2, color = color.palette[3]) +
    facet_wrap(facets = "Species", ncol = 1, strip.position = "right") +
    theme(strip.background.y = element_blank(), strip.text.y = element_blank(), text = element_text(size = 12), panel.spacing = unit(1, "lines")) +
    scale_y_continuous(limits = c(0, max(plot.df$Fecundity))) +
    xlab("")

  growth.plot <- ggplot(data = plot.df, aes(x = Age, y = Biomass, group = Species)) +
    geom_ribbon(ymin = 0, ymax = plot.df$Biomass, color = "gray80", fill = "gray80", alpha = 0.5) +
    geom_line(aes(x = Reproductive.age, y = Biomass), color = color.palette[3], size = 2, alpha = 0.8) +
    facet_wrap(facets = "Species", ncol = 1, strip.position = "right", scales = "free_x") +
    xlab("Age (years)") +
    ylab("Biomass (relative)") +
    theme(text = element_text(size = 12), panel.spacing  =  unit(1, "lines"))

  joint.plot <- plot_grid(niche.plot ,fecundity.plot, growth.plot, ncol = 3, rel_widths  =  c(1 ,0.2, 1), align = "h", axis = "tb")

  title <- ggdraw() + draw_label("Features of virtual species", fontface = 'bold')


  print(plot_grid(title, joint.plot, ncol = 1, rel_heights = c(0.1, 1)))

  #saving to file
  # cowplot::plot_grid(niche.plot, growth.plot, ncol=2)

  if(!is.null(filename) & is.character(filename)){
    ggsave(filename = paste(filename, ".pdf", sep = ""), width = 12, height = 2*nrow(parameters))
  }

}
