lm_grad <- function(X,Y,delta,epsilon,maxitr) {
  beta<-rep(0,5)
  n <- length(Y)
  hist <- rep(0, maxitr)
  d = ncol(X)
  beta_new <- beta
  itr=1
  
  # repeat until an interrupt occurs  
  repeat{
    # update iteration
    itr <- itr + 1    

    # gradient descent
    hist[itr]  <- compute_cost(X,Y,beta)
    
    # calculate for all rows in X    
    for(j in 1:d)
    {
      beta_new[j] = beta[j] + (delta / n) * sum((Y - (X%*%beta)) * X[,j]);
    }
    
    # update beta
    beta <- beta_new
    
    # keep history of beta in a list as it converges
    hist[itr]  <- compute_cost(X,Y,beta)
    
    # check for criteria to stop the loop of learning
    if(abs(beta_new - beta) > epsilon || itr > maxitr ){
      break
    }
  }
  # create two different lists for final coefficients as beta and historical values as hist
  results<-list("beta" = beta, "history" = hist)
  return(results)
}

# this function calculates gradient descent as cost function
compute_cost<-function(X,Y,beta){
  n <- length(Y)
  J <- sum((Y-X%*%beta)^2)/(2*n)
  return(J)
}

# ____________________________ Q1 ____________________________

# library to include
library(alr4)

# data preparation 
x_cols = c('Ht','Wt','LBM','BMI','SSF')
X = scale(as.matrix(ais[,x_cols]))
Y = scale(as.vector(ais[,"Bfat"]))

# estimated values after tuning 
delta <- 0.4
maxitr <- 10000
epsilon <-0.0001

# call learning
result <- lm_grad(X,Y,delta,epsilon,maxitr)
beta <- result$beta

# print coefficients
beta

# print historical values when it convergences
# FOUNDED: [1]  0.07270597  1.92976432 -2.20179377  0.03777011  0.20102739
hist <- result$history
plot(hist)