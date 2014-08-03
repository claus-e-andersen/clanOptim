#' @title Perform simulations as to estimate the prediction uncertainty 
#' @description Perform simulations as to estimate the prediction uncertainty
#' @usage
#' See demo
#' @name optim.do.simulations
#' @author Claus E. Andersen
#' @return Syntheric data based on the fitted parameters.
#' @param fm is the fitted model
#' @param df is the dataframe with the data to which the model will be fitted
#' @param fit.func is the function with the fitted function 
#' @param model is the model equation
#' @param N.sims is the number of simulations (repeats)
#' @param tol is the tolerance
#' @param trace controls if the function should be verbose during execution.
#' @export optim.do.simulations
optim.do.simulations <- function(fm, df, fit.func = optim.fit.func, model = NULL, N.sims = 30, tol = 1e-007, trace = FALSE)
{
  # Created: January 13, 2008
  # Revised: January 15, 2008
  # Revised: January 5, 2009
  # tol <- 1e-6
  y.sim <- NULL
  err <- FALSE
  hess <- fm$hessian
  qr <- qr(fm$hessian, tol = tol)
  if(!(qr$rank == dim(hess)[1])) {
    print("Rank too bad")
    err <- TRUE
  }
  cvar <- solve(hess)
  ev <- eigen(cvar, sym = T)$values
  if(!all(ev >=  - tol * abs(ev[1]))) {
    print("Sigma is not positive definite")
    err <- TRUE
  }
  if(err) {
    print("hessian=")
    print(hess)
    print("qr=")
    print(qr)
    y.sim <- "Error"
  }
  else {
    simbetas <- mvrnorm(N.sims, fm$par, cvar)
    y.sim <- NULL
    for(i in 1:N.sims) {
      p <- simbetas[i,  ]
      ss <- fit.func(p, df = df, what = "simulation", model
                     = model)
      if(is.null(y.sim)) {
        y.sim <- ss
      }
      else {
        y.sim <- rbind(y.sim, ss)
      }
    }
  }
  invisible()
  y.sim
}
