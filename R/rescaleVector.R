#' Rescales a vector within given bounds.
#'
#' @description Takes a numeric vector \code{x} and rescales it within the values given by \code{new.min} and \code{new.max}.
#'
#' @usage rescaleVector(
#'   x = rnorm(100),
#'   new.min = 0,
#'   new.max = 100,
#'   integer = FALSE
#'   )
#'
#' @param x numeric vector to be rescaled.
#' @param new.min numeric, new minimum value for \code{x}. Default is 0.
#' @param new.max numeric, new maximum value for \code{x}. Default is 100.
#' @param integer boolean, if TRUE, output vector is returned as vector of integers. Default is FALSE.
#'
#'
#' @author Blas M. Benito  <blasbenito@gmail.com>
#'
#' @return A vector of the same length as \code{x} rescaled between \code{output.min} and \code{output.max}.
#'
#'
#' @examples
#' #generating example data
#' x = rnorm(100)
#'
#' #as float
#' x.float <- rescaleVector(
#'   x = x,
#'   new.min = 0,
#'   new.max = 100,
#'   integer = FALSE
#'   )
#'
#' #as integer
#' x.integer <- rescaleVector(
#'   x = x,
#'   new.min = 0,
#'   new.max = 100,
#'   integer = TRUE
#'   )
#'
#' @export
rescaleVector <- function(x = rnorm(100),
                          new.min = 0,
                          new.max = 100,
                          integer = FALSE){

  #CHECKS INPUT VECTOR
  #----------------------

  if(!is.vector(x) | length(x) == 0 | is.null(x)){
    stop("rescaleVector: the argument x is not a vector.")
  }

  #OUTPUT MIN AND MAX
  #----------------------

  #checks that min and max are actually min and max and swaps them if not
  if(new.max < new.min){
    temp.min = new.max #new.max to temporary variable
    new.max = new.min #swaps values
    new.min = temp.min #sets new.min to value of temporary variable
  }


  #COMPUTING DATA EXTREMES
  #----------------------

  #data extremes
  old.min = min(x)
  old.max = max(x)


  #SCALING VECTOR
  #----------------------

  x = ((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min


  #FORCES VECTOR INTO INTEGER
  #----------------------

  if(integer==TRUE){
    x = floor(x)
  }

  return(x)

}
