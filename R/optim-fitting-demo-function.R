#' @title Demonstration of functions in clanOptim 
#' @description Demonstrates how optim (from the MASS library) and the functions in clanOptim
#' can be used for non-linear fitting.
#' @usage optim.fitting.demo()
#' @name optim.fitting.demo
#' @author Claus E. Andersen
#' @return Nothing expect side effects.
#' @param x is not used for anything
#' @export optim.fitting.demo
optim.fitting.demo <- function(x){
  # Purpose : to  demonstrate how the optim (from the mass library) can be used for
  #           non-linear fitting. 
  # Created : January 15, 2008
  # Revised : January  5, 2009
  # Revised : September 18, 2011
  # Name    : Claus E. Andersen
  # Sample call: optim.fitting.demo()
  # library(mass)
  # First generate some synthetic data
  NN <- 300
  df4 <- data.frame(dose = seq(-5, 50, length = NN), dose.meas = NA)
  #df4$dose.meas <- 4 + 3*df4$dose -2 * df4$dose*df4$dose + rnorm(NN,mean=0,sd=7.2)
  df4$dose.meas <- 555 * (1 + 3.3 * exp( - df4$dose * 0.1)) + rnorm(
    NN, mean = 0, sd = 70.2)
  fit.func <- optim.fit.func
  plt <- xyplot(dose.meas ~ dose, main = "Data to be fitted", data=df4)
  ## plt <- refplot(df4, dose.meas ~ dose, main = "Data to be fitted")
  print(plt)
  ####################################################################################
  #
  # Defining the selected model
  #
  ####################################################################################
  model.sel <- list(family = "normal", 
                    name   = "Lin.reg. 2nd order polynom.", 
                    y      = "dose.meas", 
                    code   = "xxx <- dose-p[5]; mu <- p[1]+p[2]*xxx+p[3]*xxx*xxx; sigma2 <- (p[4])^2; ",
                    start.code = "p[1]<- -1;  p[2] <- 3.5; p[3] <- -10; p[4] <- 7.2; p[5] <- 20",
                    Err.code   = "Err <- p[4] < 0", 
                    p.fixed = list(p2 = 3.5, p4 = 7.2109), 
                    p.names = c("b0", "b1", "b2", "sigma", "Whatever"))
  
  model.sel <- list(family = "normal", 
                    name   = "Sat. exponential", 
                    y      = "dose.meas", 
                    code   = "mu <- p[1]*(1 + p[2]*exp(-dose*p[3])); sigma2 <- (p[4])^2; ",
                    start.code = "p[1] <- 210;  p[2] <- 1; p[3] <- 1; p[4] <- 50",
                    Err.code = "Err <- p[4] < 0", 
                    p.fixed = NULL, 
                    p.names = c("c.start", "A", "lambda", "sigma"))
  
  main.txt0 <- optim.fit.func(what = "txt", model = model.sel)
  model.sel <- optim.start(df4, model = model.sel, p.max = 20, trace = F)
  ####################################################################################
  #
  # Fitting outside a trellis plot
  #
  ####################################################################################
  # Do the fitting
  fm <- optim(model.sel$p.start, fit.func, method = c("Nelder-Mead","BFGS", "CG", "L-BFGS-B", "SANN")[2], 
              control = list(trace = F, reltol = 1e-016), 
              hessian = T, 
              what = "LogLik", 
              df = df4,
              model = model.sel, trace = F)
  print("Hessian:")
  print(fm$hessian)
  # Combine the fitted and tge fixed parameters
  p <- fm$par
  fix.parameters(p, model.sel$p.fixed)
  se <- sqrt(diag(solve(fm$hessian)))
  fix.parameters(se, model.sel$p.fixed, NA)
  # Create a data frame (df.predict) with model predictions for any range of the independent variable(s)
  df.predict <- data.frame(dose = c(seq(-5, 50.6, by = 0.1)))
  df.predict <- optim.predict(fm, df = df.predict, fit.func, model = 
                                model.sel, trace = F)
  # Created a data frame (df.pi) with prediction intervals for yet another range of the independent variable(s)
  df.pi <- data.frame(dose = seq(-5, 50.6, length = 30))
  y.sim <- optim.do.simulations(fm, df = df.pi, fit.func, model = 
                                  model.sel, N.sims = 2000, trace = F)
  if(!is.null(y.sim)) {
    df.pi <- optim.prediction.statistics(y.sim, df = df.pi, P = 
                                           0.95, ylim = NULL, trace = F)
    df.pi$x <- df.pi$dose
  }
  
  df.pi
  
  plt <- xyplot(dose.meas ~ dose, 
                main = main.txt0, 
                xlim = range(df4$dose, df.pi$dose, df.predict$dose), 
                ylim = range(df4$dose.meas, df.pi$y.low, df.pi$y.high, df.predict$dose.meas),
                panel=function(x,y,...){
                  # Note: these vars are not included: c("df.predict", "df.pi", "model.sel","p", "se") 
                  lpolygon(c(df.pi$x ,rev(df.pi$x)),c(df.pi$y.high,rev(df.pi$y.low)),density=-0.1, col='lightblue1');
                  #lpoints(df.pi$x, df.pi$y.low,  type='l', lty=1, lwd=2, col='red');
                  #lpoints(df.pi$x, df.pi$y.high, type='l', lty=1, lwd=2, col='red');
                  lpoints(df.pi$x, df.pi$y.low,  type='l', lwd=1,col='black')
                  lpoints(df.pi$x, df.pi$y.high, type='l', lwd=1,col='black')
                  lpoints(df.predict$dose, df.predict$dose.meas,type='l', lwd=5, col='blue')
                  panel.xyplot(x,y,col='red',pch=16,cex=0.7,...)
                  dy0 <- - 0.1 # Vertical distance between parameter lines
                  dec0 <- 6    # Number of decimal digits
                  sci0 <- c(-6,6) # When to show the data in so-called scientific mode
                  no <- model.sel$p.fit.numbers 
                  for(i in 1:length(p)){
                    grid.text(paste(model.sel$p.names[no[i]],' = p[',no[i],'] = ',round.ca(p[i],dec0,sci0),' +/- ',round.ca(se[i],dec0,sci0),sep=''), x=0.4, y=0.9 + dy0*i, just=0, gp=gpar(fontsize=10)) 
                  } #for
                },
                data=df4)
  print(plt)
  
  
  ####################################################################################
  #
  # Fitting inside a trellis plot
  #
  ####################################################################################
  plt <- xyplot(dose.meas ~ dose, 
                subset = dose < 40, 
                main = "The data to be fitted (note the subset: dose < 40)",
                data=df4)
  print(plt)
  
  plt <- xyplot(dose.meas ~ dose, 
                subset = dose < 40, 
                panel=function(x,y,subscripts,...){
                  
                  # From S-plus: usr <- par('usr')
                  usr <- c(current.viewport()$xscale, current.viewport()$yscale) 
                  
                  model.sel <- list(family = 'normal', 
                                    name = 'Sat. exponential',
                                    y = 'dose.meas',            
                                    code = 'mu <- p[1]*(1 + p[2]*exp(-dose*p[3])); sigma2 <- (p[4])^2; ',
                                    start.code = 'p[1] <- 210;  p[2] <- 10; p[3] <- 0.05; p[4] <- 50',
                                    Err.code = 'Err <- p[4] < 0',
                                    p.fixed = NULL, #list(p4 = 7), #list(p1=3.5, p2=2.9856, p4=7.2109),
                                    p.names = c('c.start','A','lambda','sigma'))
                  
                  model.sel <- optim.start(df4[subscripts,], model=model.sel, p.max=200, trace=F)
                  
                  fit.func <- optim.fit.func
                  
                  fm <- optim(model.sel$p.start, fit.func, 
                              method=c('Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'SANN')[2], 
                              control=list(trace=F, reltol=1e-16),
                              hessian=T, 
                              what='LogLik', 
                              df=df4[subscripts,], 
                              model=model.sel, trace=F)
                  # Combine the fitted and the fixed parameters
                  p <- fm$par
                  fix.parameters(p,model.sel$p.fixed)
                  se <- sqrt(diag(solve(fm$hessian)))
                  fix.parameters(se,model.sel$p.fixed,NA)
                  # Create a data frame (df.predict) with model predictions for any range of the independent variable(s)
                  df.predict    <- data.frame(dose=c(seq(usr[1],usr[2],by=0.1)))
                  df.predict    <- optim.predict(fm, df=df.predict, fit.func, model=model.sel, trace=F)
                  # Created a data frame (df.pi) with prediction intervals for yet another range of the independent variable(s)
                  ylim0 <- c(usr[3],usr[4])
                  df.pi    <- data.frame(dose=seq(usr[1],usr[2], length=50))
                  y.sim  <- optim.do.simulations(fm, df=df.pi, fit.func, model=model.sel, N.sims=2000, trace=F)
                  if(!y.sim[1]=='Error'){
                    df.pi  <- optim.prediction.statistics(y.sim, df=df.pi, P=0.95, ylim=ylim0, trace=F)
                    df.pi$x <- df.pi$dose
                    lpolygon(c(df.pi$x ,rev(df.pi$x)),c(df.pi$y.high,rev(df.pi$y.low)),density=-0.1, col='lightblue1');
                    lpoints(df.pi$x, df.pi$y.low,  type='l', lty=1, lwd=2, col='blue');
                    lpoints(df.pi$x, df.pi$y.high, type='l', lty=1, lwd=2, col='blue');
                  }
                  
                  lpoints(df.predict$dose, df.predict$dose.meas, type='l', lwd=5, col='red')
                  
                  lpoints(x, y, type='p', lwd=5, col='red')
                  
                  dy0 <- - 0.1
                  dec0 <- 6
                  sci0 <- c(-6,6)
                  no <- model.sel$p.fit.numbers 
                  for(i in 1:length(p)){
                    grid.text(paste(model.sel$p.names[no[i]],' = p[',no[i],'] = ',round.ca(p[i],dec0,sci0),' +/- ',round.ca(se[i],dec0,sci0),sep=''), x=0.4, y=0.9 + dy0*i, just=0, gp=gpar(fontsize=10)) 
                  } 
                  
                }, # panel
                data=df4)
  
  print(plt)
  
  return(NULL)
}
