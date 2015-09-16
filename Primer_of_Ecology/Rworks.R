# This will calculate the mean of 10 random standard normal variables.
mean( rnorm(10) )
'?'(mean)
'?'(rnorm)
options(htmlhelp=TRUE)
#when you do not know the exact name of the R function
help.search("mean")
RSiteSearch("violin")
RSiteSearch("violin", restrict = c("functions"))
a <- 2+3
a
b<-a+a
b
a+a;a+b
Y<-c(8.3, 8.6, 10.7, 10.8, 11, 11, 11.1, 11.2, 11.3, 11.4)
Y
1:4
4:1
-1:3
-(1:3)
seq(from=1,to=3,by=0.2)
seq(1, 3, by=0.2)
seq(1,3,length=7)
rep(1,3)
rep(1:3,2)
rep(1:3, each=2)
sum(Y)
mean(Y)
max(Y)
lenght(Y)
length(Y)
summary(Y)
Names<-c("Sarah", "Yunluan")
Names
Names<-C("Sarah", "Yunluan")
b<-c(TRUE, FALSE)
b
class(Y)
class(b)
Y>10
y>10
Y>mean(Y)
Y==11
#A test of "not equal to"
Y!=11
a<-1:3
a
b<-4:6
a+b
a*b
a/b
a+1
a*2
1/a
a*1:2
a*c(1,2,1)
1:4*1:2
1:4*c(1,2,1,2)
Y[1]
Y[1:3]
Y>mean(Y)
Y[Y>mean(Y)]
a<-c(5,3,6,NA)
a
is.na(a)
!is.na(a)
a[!is,na(a)]
a[!is.na(a)]
na.exclude(a)
mean(a)
mean(a,na.rm=TRUE)
d<-na.exclude(a)
meand(d)
mean(d)
matrix(letters[1:4],ncol=2)
M<-matrix(1:4,nrow=2)
M
M2<-matrix(1:4, nrow =2, byrow=TRUE)
M2
I<-diag(1,nrow=2)
I
Minv<-solve(M)
M%*%Minv
M[1,2]
M[1,1:2]
M[,2]
M[,]
dat<-data.frame(species=c("S.altissima", "S.rugosa", "E.graminifolia", "A. pilosus"), treatment=factor(c("Control", "Water", "Control", "Water")), height=c(1.1, 0.8, 0.9, 1), width=c(1,1.7,0.6,0.2))
dat
dat[2,]
dat[3,4]
dat[,2]=="Water"
dat[dat[,2]=="Water"]
dat[dat[,2]=="Water",]
subset(dat,treatment=="Water")
