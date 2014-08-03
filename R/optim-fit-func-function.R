#' @title General purpose fitting function for the clanOptim library
#' @description General purpose fitting function. The actual function is supplied
#' using the model argument. The prime use of the function is for
#' nonlinear log-likelihood fitting.
#' 
#' This function (optim.fit.func) has been designed for use in connection with 
#' the mass-optimization procedure called optim(). It can be used both directly 
#' during the optimization process and before or after. Hence the function 
#' provides a general purpose wrappper such that we can easily extract anything 
#' from the Log-likelihood to prediction values for any parameter set p.
#' 
#' See body of function for further details. Also see the demo function.
#' @usage
#'   fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
#'   fix.parameters(NULL,list(p1=20))        # gives 20
#'   fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
#'   fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
#'   fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
#' @name optim.fit.func
#' @author Claus E. Andersen
#' @return A merged parameter vector
#' @param df is the dataframe with the data to which the model will be fitted
#' @param p        : a vector of parameters to be fitted. If three parameters are given 
#              (p = c(0, 2,-5) then the model$code must use p[1], p[2] and p[3]. 
#' @param what: determines what output should be returned. The default is the 
#' log-likelihood value. The following possibilities exist:
#'     'txt'          : a text string describing the function.
#'     'name'         : the name of the function.
#'     'simulation'   : a single random sample of y-values with added noise in accordanme with mu and sigma2.
#'     'mu'           : the expected values (i.e. = the model prediction if p are the fitted parameters).
#'     'sigma2'       : the variance.
#'     'residuals'    : the residials.
#'     'residuals.std': the standardized residuals
#'     'LogLik'       : the log-likelihood value.
#' @param model : a list as shown below  with information about the specific model, start-up parameters etc.
#' @param attach.df: a boolean (TRUE or FALSE) indicationg if df should be attached or not. 
#'              In case it is not attached thet all model$code etc. should refer
#'              to the variable inluding such as "df$dose" rather than just "dose".
#' @param trace    : can be used to create output during each call to the function. 
#' @export optim.fit.func
optim.fit.func <- function(p, df = NULL, what = "LogLik", model = NULL, attach.df = TRUE, trace = FALSE)
{
  # General purpose fitting function. The actual function is supplied
  # using the model argument. The prime use of the function is for
  # nonlinear log-likelihood fitting.
  # Revised: January  9, 2008
  # Revised: January 15, 2008
  # Claus E. Andersen
  # This function (optim.fit.func) has been designed for use in connection with 
  # the mass-optimization procedure called optim(). It can be used both directly 
  # during the optimization process and before or after. Hence the function 
  # provides a general purpose wrappper such that we can easily extract anything 
  # from the Log-likelihood to prediction values for any parameter set p.
  # Arguments:
  #   p        : a vector of parameters to be fitted. If three parameters are given 
  #              (p = c(0, 2,-5) then the model$code must use p[1], p[2] and p[3]. 
  #   what     : determines what output should be returned. The default is the 
  #              log-likelihood value. The following possibilities exist:
  #                'txt' : a text string describing the function.
  #                'name': the name of the function.
  #                'simulation': a single random sample of y-values with added 
  #                              noise in accordanme with mu and sigma2.
  #                'mu': the expected values (i.e. = the model prediction if 
  #                      p are the fitted parameters).
  #                'sigma2': the variance.
  #                'residuals': the residials.
  #                'residuals.std': the standardized residuals
  #                'LogLik': the log-likelihood value.
  #   model    : a list as shown below  with information about the specific model, 
  #              start-up parameters etc.
  #   attach.df: a boolean (T or F) indicationg if df should be attached or not. 
  #              In case it is not attached thet all model$code etc. should refer
  #              to the variable inluding such as "df$dose" rather than just "dose".
  #   trace    : can be used to create output during each call to the function. 
  # The function needs a model, such as these:
  # Model example 1:
  # model.sel <- list(family    = 'normal', 
  #                  name       = "Lin.reg. 2nd order polynom.", 
  #                  y          = "dose.meas", 
  #                  code       = "mu <- p[1]+p[2]*dose+p[3]*dose*dose; sigma2 <- (p[4])^2; ", 
  #                  start.code = "p[1]<- -1;  p[2] <- 3.5; p[3] <- -10; p[4] <- 7.2; p[5] <- 20", 
  #                  Err.code   = NULL, 
  #                  LL.code    = NULL, 
  #                  p.fixed    = NULL, 
  #                  p.names    = NULL)
  # Model example 2:
  # model.sel <- list(family    = 'normal', 
  #                  name       = "Lin.reg. 2nd order polynom.", 
  #                  y          = "dose.meas", 
  #                  code       = "xxx <- dose-p[5]; mu <- p[1]+p[2]*xxx+p[3]*xxx*xxx; sigma2 <- (p[4])^2; ", 
  #                  start.code = "p[1]<- -1;  p[2] <- 3.5; p[3] <- -10; p[4] <- 7.2; p[5] <- 20", 
  #                  Err.code   = "Err <- p[4] < 0", 
  #                  p.fixed    = list(p2=3.5, p4=7.2109), 
  #                  p.names    = c("b0","b1","b2","sigma","Whatever") )
  # where:
  #   family   : is the name of the stastical family. Curently only
  #              normal has been implemented. This means that given for any
  #              values of the independent variables the y-value should be
  #              well described by a normal distribution function with the two 
  #              parameters mu and sigma2 (i.e. variance).
  #   name     : is a simple name for the function.
  #   y        : is a text string with the name of the dependent variable = a column name in df.  
  #   code     : is a text string with the expression for mu and sigma2 (the variance).
  #              The parameters p[1], p[2] etc. and one of more data columns in df can be
  #              used in these expressions. sigma2 = "abs(p[4])" means that abs(p[4]) is an 
  #              estimator of the variance. sigma2 = "p[4]^2" means that abs(p[4]) is the standard deviation.
  #   start.code : is a text string with the expressions for parameters p[1], p[2] etc. The code has two
  #              side effects: (1) It determines the initial (start up) values for these parameters
  #              when optim is called (in so far as optim is called as optim(model$p.start,...)). 
  #              (2) It identifies the free parameters to be fitted. For example, if the start.code includes
  #              an assignment p[3] <- 3.5 then we not only set p[3] to the value 3.5 in the initial
  #              optim-call, we assume implicitly declare p[3] as a free parameter to be fitted by optim.
  #              This could be overruled by the p.fixed list (see below).
  #   p.fixed  : A list of parameters with fixed values. For example, list(p3=44.7) means that p[3] should
  #              be treated as a fixed parameter with value 44.7. If p[3] was already "declared" a free
  #              parameter in the start.code, then p.fixed wins and p[3] is fixed. This is a very importnt
  #              feature of this modelling concept as we can easily change parameters from being free or
  #              fixed.
  #   Err.code : is a text string with code. For example, if p[4] should be positive then we can
  #              use Err.code ="Err <- p[4]<0". The prime objective of this feature is to avoid 
  #              illegal calls and to have a simple and flexible method for telling optim that 
  #              certain parameter ranges should be avoided (for example, by makin the loglikelihood
  #              -Inf if Err ==T as shown below).               
  #   LL.code  : The expression for what should be optimized (the norm). This is normally the
  #              loglikelihood expression:  Err.code = "if(Err){LL <- -Inf} else 
  #              {LL <- 0.5 * sum(log(sigma2) + (y - mu)^2 / sigma2 )}". However, it could also
  #              be a simple sum of squares.
  #   p.names  : an optional vector of physical names of the parameters.
  # note 1     : Why does the function not have a default independent variable x?
  #              This is because there could be more than one independent variable.
  #              However, there can obly be one dependent y-variable.                
  # note 2      : How to get standard errors and how to account for fixed parameters in the final output?
  # After the call to optim (fm <- optim(...), we then do the following as to get all four parameters
  #    p <- fm$par
  #    se <- sqrt(diag(solve(fm$hessian)))
  #    fix.parameters(p,p.fixed.sel)
  #    fix.parameters(se,p.fixed.sel,NA)
  # Ensure that all columns in the data.frame are available for reference without the df$ - i.e. as if
  # df was attached. For large data sets if may be useful to include df$ directly in all formulas
  # and the below pseudo copy of df can then be omitted using attach.df = F.
  if(attach.df & !is.null(df)) {
    nam <- names(df)
    txt <- paste(nam, " <- df$", nam, sep = "")
    txt <- paste(txt, collapse = "; ")
    eval(parse(text = txt))
  }
  res <- NULL
  Err <- F
  if(trace) {
    print("optim.fit.func")
    print(paste("what = ", what))
    print(paste("p = ", paste(p, collapse = " ")))
    if(!is.null(model$p.fixed))
      print(paste("p.fixed = ", paste(model$p.fixed, 
                                      collapse = " ")))
  }
  if(model$family == "normal") {
    # Normal distribution family: y ~ N(mu,sigma2) 
    if(is.element(what, c("name", "txt"))) {
      if(is.element(what, "name"))
        res <- model$name
      if(is.element(what, "txt"))
        res <- paste("y ~ N(mu,sigma2); ", model$
                       code, sep = "")
    }
    else {
      # Lets do some real calculations:
      p <- fix.parameters(p, model$p.fixed)
      eval(parse(text = model$code))
      if(!is.null(model$Err.code)) {
        eval(parse(text = model$Err.code))
      }
      # Only calculate y if it is actually needed
      if(is.element(what, c("residuals", "residuals.std",
                            "LogLik"))) {
        if(is.null(model$y) | !is.element(model$y,
                                          nam)) {
          print(model)
          print("Problem: y is not a valid column name in the data.frame"
          )
          stop()
        }
        y <- eval(parse(text = model$y))
      }
      res <- mu
      if(is.element(what, "simulation")) {
        res <- mu + (sigma2)^0.5 * rnorm(nrow(df))
      }
      if(is.element(what, "mu")) {
        res <- mu
      }
      if(is.element(what, "sigma2")) {
        res <- sigma2
      }
      if(is.element(what, "residuals")) {
        res <- y - mu
      }
      if(is.element(what, "residuals.std")) {
        res <- (y - mu)/sqrt(sigma2)
      }
      # standardized residuals
      if(is.element(what, "LogLik")) {
        if(is.null(model$LL.code)) {
          if(Err) {
            LL <-  - Inf
          }
          else {
            LL <- 0.5 * sum(log(sigma2) +
                              (y - mu)^2/sigma2)
          }
        }
        else {
          eval(parse(text = model$LL.code))
        }
        res <- LL
      }
    }
  }
  # family normal
  invisible()
  res
}

#####################################################################################
