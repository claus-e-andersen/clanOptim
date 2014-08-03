#' @title Compute prediction statistics 
#' @description Compute prediction statistics for model fitting in clanOptim.
#' @usage
#' See demo
#' @name optim.prediction.statistics
#' @author Claus E. Andersen
#' @return Prediction statistics based on the simulated data
#' @param sim is the simulated data
#' @param df is the dataframe with the data to which the model will be fitted
#' @param P is the confidence intercal (0.95 means 95%) 
#' @param ylim is a vecor of two elements indicating the min and max of the output.
#' @param na.rm controls if NA-values should be removed
#' @param trace controls if the function should be verbose during execution.
#' @export optim.prediction.statistics
optim.prediction.statistics <- function(sim, df, P = 0.95, ylim = NULL, na.rm = T, trace = F)
{
  # Created: January 13, 2008
  # Revised: January 13, 2008
  # Revised: January 16, 2008
  # Revised: January 5, 2009
  # Name   : Claus E. Andersen
  sim <- as.data.frame(sim)
  if(trace) {
    print("sim:")
    print(sim)
  }
  names(sim) <- 1:dim(sim)[2]
  sim.stack <- stack.for.trellis(sim, names(sim), remove.stacked = T)
  names(sim.stack) <- c("y", "which")
  if(trace) {
    print("sim.stack:")
    print(sim.stack)
  }
  sim.stack$which <- reorder.for.trellis(sim.stack$which)
  df.pi <- data.frame(y.mean = tapply(sim.stack$y, sim.stack$which,
                                      mean, na.rm = na.rm), y.stdev = tapply(sim.stack$y, sim.stack$
                                                                               which, stdev, na.rm = na.rm), y.low = tapply(sim.stack$y,
                                                                                                                            sim.stack$which, quantile, na.rm = na.rm, probs = (1 - P)/
                                                                                                                              2.), y.high = tapply(sim.stack$y, sim.stack$which, quantile,
                                                                                                                                                   na.rm = na.rm, probs = 1 - (1 - P)/2.))
  df.pi <- data.frame(df, df.pi)
  if(trace) {
    print("df.pi:")
    print(df.pi)
  }
  if(!is.null(ylim)) {
    # truncate
    ok <- df.pi$y.low < ylim[1]
    if(sum(ok) > 0)
      df.pi$y.low[ok] <- ylim[1]
    ok <- df.pi$y.low > ylim[2]
    if(sum(ok) > 0)
      df.pi$y.low[ok] <- ylim[2]
    ok <- df.pi$y.high < ylim[1]
    if(sum(ok) > 0)
      df.pi$y.high[ok] <- ylim[1]
    ok <- df.pi$y.high > ylim[2]
    if(sum(ok) > 0)
      df.pi$y.high[ok] <- ylim[2]
    if(trace) {
      print("df.pi (truncated):")
      print(df.pi)
    }
    if(trace) {
      print("ylim:")
      print(ylim)
    }
  }
  # ylim
  invisible()
  df.pi
}
