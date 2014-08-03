#' @title Make prediction withs the fitted function.
#' @description Make predictions with the fitted function.
#' @usage
#'   fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
#'   fix.parameters(NULL,list(p1=20))        # gives 20
#'   fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
#'   fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
#'   fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
#' @name optim.predict
#' @author Claus E. Andersen
#' @return A merged parameter vector
#' @param fm is the fitted model
#' @param df is the dataframe with the data to which the model will be fitted
#' @param fit.func is the function with the fitted function 
#' @param model is the model equation
#' @param trace controls if the function should be verbose during execution.
#' @export optim.predict
optim.predict <- function(fm, df, fit.func = optim.fit.func, model = NULL, trace = F)
{
  # Created: January 13, 2008
  # Revised: January 14, 2008
  # Name   : Claus E. Andersen
  yy <- fit.func(fm$par, df = df, what = "mu", model = model)
  txt <- paste("df$", model$y, " <- yy", sep = "")
  if(trace)
    print(txt)
  eval(parse(text = txt))
  df
}

