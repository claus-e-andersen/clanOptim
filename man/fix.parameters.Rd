% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix-parameters-function.R
\name{fix.parameters}
\alias{fix.parameters}
\title{Keeping selected parameters constant during model fitting with optim}
\usage{
fix.parameters(c(10,30,40),list(p5=50, p2=20)) # gives 10 20 30 40 50
  fix.parameters(NULL,list(p1=20))        # gives 20
  fix.parameters(c(10,20),list(p3=30))    # gives 10 20 30
  fix.parameters(c(10,20),list(p5=30))    # error - there is no 5th parameter
  fix.parameters(c(10,20),list(p3=30,x3=45))  # error - cant refer to the 3rd parameter more than once
}
\arguments{
\item{p}{is a vector with parameters (to be fitted)}

\item{p.fixed}{is a list with fixed parameters (named p1, p2 etc.)}

\item{p.fixed.overwrite}{is the value to use for all the fixed parameters. When is
this useful? It is useful when manupulating the standard error vector when
all the positions with fixed parameters should be set to zero or NA.
For exampel assume pp and ss are the fitted parameters and std. errors obtained
with pfix = list(p2=33.2, p3=5.5). To get the full parameter set and the
correspopnding std. errors we simply calculate fixed.parameters(pp, pfix)
fixed.parameters(ss,pfix, p.fixed.overwrite=NA).}
}
\value{
A merged parameter vector
}
\description{
To facilitate parameter fitting using optim() when some of the parameters are
         ocationally held constant. optim is from the MASS library.
Notes: The p.fixed names can be given in any order and only the number in the names count. 
For example, p2=3.14 is equivalent to ffffff2=3.14.
}
\author{
Claus E. Andersen
}
