#' Fix data types in parameters dataframe.
#'
#' @description It converts all columns (but the \code{label} one) of a parameters dataframe created by \code{\link{parametersDataframe}} into type numeric, and checks the coherence of the parameters for each taxon. It provides feedback on the check results on screen for each taxon.
#'
#' @usage fixParametersTypes(x)
#'
#' @param x dataframe resulting from \code{\link{parametersDataframe}}.
#'
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return Same dataframe provided in argument \code{x} but with fixed data types.
#'
#' @seealso \code{\link{parametersDataframe}}
#'
#' @examples
#'
#' parameters <- parametersDataframe(rows=1)
#' parameters[1,] <- c("Species 1", 50, 20, 2, 0.2, 0, 100, 1000, 1, 0, 50, 10, 0, 0, NA, NA)
#' parameters <- fixParametersTypes(x=parameters)
#'
#' @export
fixParametersTypes <- function(x){

  #sets all columns but the first one into numeric
  x[, 2:ncol(x)] <- sapply(x[, 2:ncol(x)], as.numeric)

  #checking columns
  if(!is.character(x$label)){x$label <- as.character(x$label)}

  #checking taxa by taxa
  for (i in 1:nrow(x)){

    message(paste("Checking taxon ", x[i,"label"], sep=""))

    #checking for NA values
    if(sum(is.na(x[i,])) > 0){
      message("There are NA values in the parameters of this taxon, please, fix this and come back here!")
    }

    #checking reproductive and maximum age
    if(x[i, "reproductive.age"] >= x[i, "maximum.age"]){
      message("reproductive.age is >= than maximum.age, setting reproductive.age to half of maximum.age.")
      x[i, "reproductive.age"] <- floor(x[i, "maximum.age"]/2)
    }

    #checking maximum biomass and carrying capacity
    if((x[i, "carrying.capacity"] / x[i, "maximum.biomass"]) < 20){
      message(paste("carrying.capacity for the taxon is too low, increasing it to",  x[i, "maximum.biomass"] * 20, sep=""))
      x[i, "carrying.capacity"] <- x[i, "maximum.biomass"] * 20
    }

    #checking pollen control
    if(x[i, "pollen.control"] > 1 | x[i, "pollen.control"] < 0){
      message("pollen.control must be between 0 and 1, setting it to 0")
      x[i, "pollen.control"] <- 0
    }

    #checking driver weights
    if(x[i, "driver.A.weight"] + x[i, "driver.B.weight"] != 1){
      message("The sum of driver.A.weight and driver.B.weight must be 1.")
    }

    #checking standard deviation of niche functions
    if(x[i, "niche.A.sd"] == 0){
      message("niche.A.sd was 0, changing it to 1.")
      x[i, "niche.A.sd"] <- 1
    }

    if(x[i, "niche.B.sd"] == 0){
      message("niche.B.sd was 0, changing it to 1.")
      x[i, "niche.B.sd"] <- 1
    }


  }

  return(x)
}
