library(AER)
library(mvtnorm)
set.seed(8787)
corr <- matrix(c(1,0.999,0.999,1), byrow = 1, nrow = 2)
choice <- function(complier, z, x1, x2, x3, e){
  if (complier == 1){
    if (0.5*z - 1.2*x1 + 3.9*x2 - 0.4*x3 + e > 1){
      return(1)
    }
    else{
      return(0)
    }
  }
  else{
    if (-0.5*z - 1.2*x1 + 3.9*x2 - 0.4*x3 + e > 1){
      return(1)
    }
    else{
      return(0)
    }
  }
}
outcome <- function(d, x1, x2, x3, e){
  if (complier == 1){
    return(100*d + 0.29*x1 - 0.45*x2 - 0.12*x3 + e)
  }
  else{
    return(-100*d + 0.29*x1 - 0.45*x2 - 0.12*x3 + e)
  }
}
sample_size <- 5000
ive_for_different_complier_proportions <- c()
for (j in 1:1000){
  z <- rnorm(sample_size)
  x1 <- rnorm(sample_size)
  x2 <- rnorm(sample_size)
  x3 <- rnorm(sample_size)
  d <- c()
  y <- c()
  complier_proportion <- 0.001*j
  disturbance <- rmvnorm(sample_size, mean = c(0,0), sigma = corr)
  for (i in 1:sample_size){
    complier <- rbinom(1,1,complier_proportion)
    d[i] <- choice(complier, z[i], x1[i], x2[i], x3[i], disturbance[i,1])
    y[i] <- outcome(d[i], x1[i], x2[i], x3[i], disturbance[i,2])
  }
  ive_for_different_complier_proportions[j] <-
    as.numeric(coef(ivreg(y ~ d + x1 + x2 + x3 | z + x1 + x2 + x3))[2])
}

plot(seq(0,1,length = 1000),ive_for_different_complier_proportions, type = "l",
     xlab = "Proportion of Complier", ylab = "Estimated LATE", 
     main = "Complier and Defier in Instrumental Variable Analysis")

plot(seq(0,0.3,length = 300),ive_for_different_complier_proportions[1:300], type = "l",
     xlab = "Proportion of Complier", ylab = "Estimated LATE", 
     main = "Complier and Defier in Instrumental Variable Analysis")

plot(seq(0.7,1,length = 300),ive_for_different_complier_proportions[701:1000], type = "l",
     xlab = "Proportion of Complier", ylab = "Estimated LATE", 
     main = "Complier and Defier in Instrumental Variable Analysis")

