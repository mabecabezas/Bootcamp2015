
NO <- 25
RR <- 1.05
ttMax <- 100
KK <- 100
NN <- matrix (NA, nrow=1, ncol=ttMax+1)
NN[1] <- NO

for(tt in 1:ttMax){NN[tt+1] <- NN[tt]*(1+RR*(1-(NN[tt]/KK)))}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")


#Mini-exercise 3.3.1
discreLogisticFun <- function(RR,NO,KK){
NN <- matrix (NA, nrow=1, ncol=ttMax+1)
ttMax=100
NN[1] <- NO
for(tt in 1:ttMax){NN[tt+1] <- NN[tt]*(1+RR*(1-(NN[tt]/KK)))}
print (NN)
plot(1:(ttMax+1),NN, xlab = "time", ylab = "N", type = "b", col="blue")
}
discreLogisticFun(NO=80, RR=0.5, KK=100)
#base in the plot RR and time change the behavior of the model. 


#Mini-exercise 3.3.2
discreLogisticFun(NO=10, RR=0.5, KK=100)
growthRate <- c(-0.3, 0.3, 1.3, 1.9, 2.2, 2.7)
par(mfrow=c(2,3))
for(ii in growthRate){discreLogisticFun(NO=10, RR=ii, KK=100)}




