install.packages("deSolve")
library(deSolve)

expGrowthODE <- function(tt, yy, pars) {
  derivs <- pars['rr'] * yy
  return(list(derivs))
}

output <- lsoda(init, tseq, expGrowthODE, pars)

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.1)

expGrowthOutput <- lsoda(init, tseq, expGrowthODE, pars)
head(expGrowthOutput)

plot(expGrowthOutput[,1], expGrowthOutput[,2], col='blue', type = 'l')



#Mini-exercise 4.1.1
expGrowth <- function(init, tseq, pars){
library(deSolve) #Setup
expGrowthODE <- function(tt, yy, pars) {
  derivs <- pars['rr'] * yy
  return(list(derivs))}
expGrowthOutput <- lsoda(init, tseq, expGrowthODE, pars)
plot(expGrowthOutput[,1], expGrowthOutput[,2], col='blue', type = 'l')

}

#call the function
expGrowth(init<-1, tseq<-seq(0, 20, by<-0.01), pars<-c(rr=0.1))
# this fundtion exhibit the same range of qualitative behaviors as the geometric growth model



#Mini-exercise 4.2.1
logisticGrow<-function(init, tseq, pars){
library(deSolve)
logGrowthODE <- function(tt, yy, pars){
  derivs <- pars["rr"] * yy*(1-yy/pars["KK"])
  return(list(derivs))
}

logGrowthOutput <- lsoda(init, tseq, logGrowthODE, pars)
head(logGrowthOutput)
plot(logGrowthOutput[,1], logGrowthOutput[,2], col = "blue", type = "l")
}

logisticGrow(init<-1, tseq<-seq(0, 20, by<-0.5), pars<-c(rr=0.1, KK=220))
