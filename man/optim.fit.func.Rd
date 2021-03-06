% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optim-fit-func-function.R
\name{optim.fit.func}
\alias{optim.fit.func}
\title{General purpose fitting function for the clanOptim library}
\usage{
fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
  fix.parameters(NULL,list(p1=20))        # gives 20
  fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
  fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
  fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
}
\arguments{
\item{p}{: a vector of parameters to be fitted. If three parameters are given}

\item{df}{is the dataframe with the data to which the model will be fitted}

\item{model}{: a list as shown below  with information about the specific model, start-up parameters etc.}

\item{trace}{: can be used to create output during each call to the function.}

\item{what:}{determines what output should be returned. The default is the 
log-likelihood value. The following possibilities exist:
    'txt'          : a text string describing the function.
    'name'         : the name of the function.
    'simulation'   : a single random sample of y-values with added noise in accordanme with mu and sigma2.
    'mu'           : the expected values (i.e. = the model prediction if p are the fitted parameters).
    'sigma2'       : the variance.
    'residuals'    : the residials.
    'residuals.std': the standardized residuals
    'LogLik'       : the log-likelihood value.}

\item{attach.df:}{a boolean (TRUE or FALSE) indicationg if df should be attached or not. 
In case it is not attached thet all model$code etc. should refer
to the variable inluding such as "df$dose" rather than just "dose".}
}
\value{
A merged parameter vector
}
\description{
General purpose fitting function. The actual function is supplied
using the model argument. The prime use of the function is for
nonlinear log-likelihood fitting.

This function (optim.fit.func) has been designed for use in connection with 
the mass-optimization procedure called optim(). It can be used both directly 
during the optimization process and before or after. Hence the function 
provides a general purpose wrappper such that we can easily extract anything 
from the Log-likelihood to prediction values for any parameter set p.

See body of function for further details. Also see the demo function.
}
\author{
Claus E. Andersen
}
