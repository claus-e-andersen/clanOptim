#' @title Start function (see demo)
#' @description Start function (see demo)
#' @usage
#'   fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
#'   fix.parameters(NULL,list(p1=20))        # gives 20
#'   fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
#'   fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
#'   fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
#' @name optim.start
#' @author Claus E. Andersen
#' @return A merged parameter vector
#' @param df is the dataframe with the data to which the model will be fitted
#' @param model is the model equation
#' @param p.max is the maximum of iterations
#' @param attach.df controls if the dataframe will be attached or not
#' @param trace controls if the function should be verbose during execution.
#' @export optim.start 
optim.start <- function(df, model = NULL, p.max = 20, attach.df = TRUE, trace = FALSE)
{
  # Created: January 14, 2008
  # Revised: January 15, 2008
  # Name   : Claus E. Andersen
  if(attach.df & !is.null(df)) {
    nam <- names(df)
    txt <- paste(nam, " <- df$", nam, sep = "")
    txt <- paste(txt, collapse = "; ")
    eval(parse(text = txt))
  }
  p <- rep(NA, p.max)
  p <- fix.parameters(p, model$p.fixed)
  txt <- model$start.code
  if(trace)
    print(txt)
  eval(parse(text = txt))
  ok <- !is.na(p)
  dd <- diff(ok)
  if(max(dd) > 0.1) {
    print("p = ")
    print(p)
    print(model$start.code)
    print("Problem. There seems to a break in the parameters?")
    stop()
  }
  p <- p[ok]
  p <- rep(NA, p.max)
  txt <- model$start.code
  if(trace)
    print(txt)
  eval(parse(text = txt))
  ok <- !is.na(p)
  p <- p[ok]
  N <- length(p)
  p.fit.numbers <- 1:N
  ####
  # If some parameters are listed as being fixed then we will remove them from
  # the p-start list. This means that they will not be treated as free parameters
  # that will be fitted by optim.
  if(!is.null(model$p.fixed)) {
    i.fixed <- extract.first.number(names(model$p.fixed))$number
    if(max(i.fixed) > N | !(length(unique(i.fixed)) == length(
      i.fixed))) {
      print(p)
      print(substitute(model$p.fixed))
      print(i.fixed)
      stop(paste(
        "fix.parameters: The fixed parameters are not valid. Max number of parameters=",
        N))
    }
    i.non.fixed <- setdiff(1:N, i.fixed)
    p <- p[i.non.fixed]
    p.fit.numbers <- p.fit.numbers[i.non.fixed]
  }
  model$p.start <- p
  model$p.fit.numbers <- p.fit.numbers
  invisible()
  model
}
