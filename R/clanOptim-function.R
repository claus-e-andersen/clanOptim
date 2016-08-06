#' @title Version function for the clanOptim library
#' @description Version function for the clanOptim library.
#' @usage
#' clanOptim()
#' @name clanOptim
#' @author Claus E. Andersen
#' @return A list of information about the version and functions within clanOptim.
#' @export 
clanOptim <- function(){
  list(name="clanOptim",
       version=0.008,
       date="August 3, 2014 (uploaded August 6, 2016)",
       functions=sort(c("clanOptim",
                        "fix.parameters","optim.start","optim.fit.func","optim.predict",
                        "optim.do.simulations","optim.prediction.statistics","optim.fitting.demo"
       )))
}
