
NO <- 25
RR <- 1.05
ttMax <- 100

NN <- matrix (NA, nrow=1, ncol=ttMax+1)
NN[1] <- NO
for(tt in 1:ttMax){NN[tt+1] <- RR*NN[tt]}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")


#Mini-exercise 3.2.1
#you have to put a RR < 1 to make the population decline
NO <- 25
RR <- 0.9
ttMax <- 100

NN <- matrix (NA, nrow=1, ncol=ttMax+1)
NN[1] <- NO
for(tt in 1:ttMax) {NN[tt+1] <- RR*NN[tt]}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")

#base in the plot the qualitative behavior does not depend of population size
NO <- 1000
RR <- 1.05
ttMax <- 100

NN <- matrix (NA, nrow=1, ncol=ttMax+1)
NN[1] <- NO
for(tt in 1:ttMax) {NN[tt+1] <- RR*NN[tt]}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")

#base in the plot, RR and time change the behavior of the model. 
#In particular RR can make the population grow or decline, and less generations show the population size variation as constant through time

NO <- 25
RR <- 0.9
ttMax <- 10

NN <- matrix (NA, nrow=1, ncol=ttMax+1)
NN[1] <- NO
for(tt in 1:ttMax) {NN[tt+1] <- RR*NN[tt]}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")


#Exercise 3.2.2
#Function version
geomFun <- function(RR, NO, ttMax){
  NN <- rep(NA,ttMax+1)
  NN[1] <- NO
  for (tt in 1:ttMax) {
    NN[tt+1] <- RR*NN[tt]
  }
  plot(1:(ttMax+1),NN,xlab = "time", ylab = "b", col ='blue')
}
geomFun(NO=10, RR=0.95, ttMax = 100)

