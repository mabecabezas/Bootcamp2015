#First exercise#####################

for (ii in 1:9){
  cat("\n");
  if(ii==9){
    cat("*\n")}
}



#Second exercise##################### 

for(ii in 1:10){
  ii="*"
  cat (ii, sep="","&") 
}



#Third exercise#####################

###dogs
#by hand
#initial value = 10, value at start of iteraction 11, value at the end 15
#first iteration: initial value = 10, final value of the loop = 11
#second iteration: initial value = 11, final value of the loop = 12
#thirtd iteration: initial value = 12, final value of the loop = 13
#fourd iteration: initial value = 13, final value of the loop = 14
#fifth iteration: initial value = 14, final value of the loop = 15

###meatloaf
#by hand
#initial value = 0, value at start of iteraction -4, value at the end -30
#first iteration: initial value = 0, final value of the loop = -4
#second iteration: initial value = -4, final value of the loop = -9
#thirtd iteration: initial value = -9, final value of the loop = -15
#fourd iteration: initial value = -15, final value of the loop = -22
#fifth iteration: initial value = -22, final value of the loop = -30

###bubbles
#by hand
#initial value = 12, value at start of iteraction -1, value at the end -4
#first iteration: initial value = 12, final value of the loop = -1
#second iteration: initial value = -1, final value of the loop = -2
#thirtd iteration: initial value = -2, final value of the loop = -3
#fourd iteration: initial value = -3, final value of the loop = -4



#Fourth exercise#####################

years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 4 == 0){      #years multiples of 4
    cat(years[ii], 'Hooray, congressional and presidential elections!', sep = '\t', fill = T)
  }
  else if(years[ii] == 2018){
      cat(years[ii], "Hooray, congressional elections!", sep = '\t', fill=T)
    }
  else if(years[ii] == 2015){
  cat(years[ii], " ", sep = '\t', fill=T)
  }
  else if(years[ii] == 2021){
    cat(years[ii], " ", sep = '\t', fill=T)
  }
}

#Second method

years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 4 == 0){      
    cat(years[ii], 'Hooray, congressional and presidential elections!', sep = '\t', fill = T)
  }
  else if(years[ii] %% 2 == 0){     
    cat(years[ii], "Hooray, congressional elections!", sep = '\t', fill=T)
  }
  else if(years[ii] %% 1 == 0){
    cat(years[ii], " ", sep = '\t', fill=T)
  }
}



#Fifth exercise#####################

bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
interestRate <- 0.0125;
compounded = rep(NA,length(bankAccounts)) #I initialized the objeto 'compounded' with N/A values
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }



#Sixth exercise#####################

bankAccounts <- c(10, 9.2, 5.6);
interestRate2 <- 0.0525;   
house <- c(4.8, 3.8, 5.7);
food<- c(3.5, 4.3, 5.0);
fun <- c(7.8, 2.1, 10.5);
income <- c(21, 21, 21);
expeses <- house + food + fun

for (jj in 1:5){
  for (ii in 1:length(bankAccounts)){
    bankAccounts[ii] <- bankAccounts[ii] - expeses[ii] + income[ii]
  bankAccounts[ii] <- bankAccounts[ii] + interestRate2*bankAccounts[ii]
}
}



#Seventh exercise#####################

bankAccounts <- c(10, 9.2, 5.6);
interestRate2 <- 0.0525;
house <- c(4.8, 3.8, 5.7); 
food<- c(3.5, 4.3, 5.0); 
fun <- c(7.8, 2.1, 10.5); 
expenses <- house + food + fun 
income <- c(21, 21, 21);

for (years in 2015:2020){
  for (i in 1:length(bankAccounts)){
    bankAccounts[i] <- bankAccounts[i] - expenses [i] + income [i]
  if(years %% 2 == 1){ #odd years (%% 2 == 0 means even years)
    if( i==1 | i ==3){ # students 1 and 3
      bankAccounts[i] <- bankAccounts[i] + 5
    }
  }
  bankAccounts[i] <- bankAccounts[i] + bankAccounts[i]*interestRate2
  }
}



#Eighth exercise#####################

counter <- 0
total <- 0
while(counter <= 17){
  total <- total + counter
  counter <- counter + 1
}
total == sum(1:17)



#Ninth exercise#####################

numbers = function(x){ # I created the fundtion numbers 
if (x <= -1){
  print("small");
}
if (x>-1 & x<1){
  print("medium");
}
if (x >= 1){
  print("big")
}
}
numbers(-6) #it works!!!
numbers(0)
numbers(7)

#Second method

numbers = function(x){ # I created the function numbers 
  if (x <= -1){
    print("small");
  }
  else if (x>-1 & x<1){
    print("medium");
  }
  else {
    print("big")
  }
}
numbers(-6) #it works!!!
numbers(0)
numbers(7)