#' @title Keeping selected parameters constant during model fitting with optim
#' @description To facilitate parameter fitting using optim() when some of the parameters are
#'          ocationally held constant. optim is from the MASS library.
#' Notes: The p.fixed names can be given in any order and only the number in the names count. 
#' For example, p2=3.14 is equivalent to ffffff2=3.14. 
#' @usage
#'   fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
#'   fix.parameters(NULL,list(p1=20))        # gives 20
#'   fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
#'   fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
#'   fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
#' @name fix.parameters
#' @author Claus E. Andersen
#' @return A merged parameter vector
#' @param p is a vector with parameters (to be fitted)
#' @param p.fixed is a list with fixed parameters (named p1, p2 etc.) 
#' @param p.fixed.overwrite is the value to use for all the fixed parameters. When is
#' this useful? It is useful when manupulating the standard error vector when
#' all the positions with fixed parameters should be set to zero or NA.
#' For exampel assume pp and ss are the fitted parameters and std. errors obtained
#' with pfix = list(p2=33.2, p3=5.5). To get the full parameter set and the
#' correspopnding std. errors we simply calculate fixed.parameters(pp, pfix)
#' fixed.parameters(ss,pfix, p.fixed.overwrite=NA).
#' @export fix.parameters 
fix.parameters <- function(p, p.fixed = NULL, p.fixed.overwrite = NULL)
{
  # Task   : To facilitate parameter fitting using optim() when some of the parameters are
  #          ocationally held constant.
  # Created: October 26, 2006
  # Revised: January 10, 2008
  # Revised: January 15, 2008
  # Name   : Claus E. Andersen
  # Input:
  #   p = vector with parameters (to be fitted)
  #   p.fixed = a list with fixed parameters (named p1, p2 etc.) 
  #   p.fixed.overwrite = the value to use for all the fixed parameters. When is
  #   this useful? It is useful when manupulating the standard error vector when
  #   all the positions with fixed parameters should be set to zero or NA.
  #   For exampel assume pp and ss are the fitted parameters and std. errors obtained
  #   with pfix = list(p2=33.2, p3=5.5). To get the full parameter set and the
  #   correspopnding std. errors we simply calculate fixed.parameters(pp, pfix)
  #   fixed.parameters(ss,pfix, p.fixed.overwrite=NA).
  # Notes:
  #   The p.fixed names can be given in any order and only
  #   the number in the names count. For example, p2=3.14 is
  #   equivalent to ffffff2=3.14. 
  # Output:
  #   A merged parameter vector
  # Sample calls:
  #   fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
  #   fix.parameters(NULL,list(p1=20))        # gives 20
  #   fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
  #   fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
  #   fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
  pp <- p
  if(!is.null(p.fixed)) {
    i.fixed <- extract.first.number(names(p.fixed))$number
    N <- length(p) + length(p.fixed)
    if(max(i.fixed) > N | !(length(unique(i.fixed)) == length(
      i.fixed))) {
      print(p)
      print(substitute(p.fixed))
      print(i.fixed)
      stop(paste(
        "fix.parameters: The fixed parameters are not valid. Max number of parameters=",
        N))
    }
    pp <- rep(NA, N)
    if(!is.null(p.fixed.overwrite)) {
      pp[i.fixed] <- p.fixed.overwrite
    }
    else {
      pp[i.fixed] <- as.vector(unlist(p.fixed))
    }
    i.non.fixed <- setdiff(1:N, i.fixed)
    pp[i.non.fixed] <- p
  }
  invisible()
  pp
}
