#clear environment
rm(list=ls())

#read data, set column names, code outcomes as factors
logit_data <- read.csv("logit_data.csv", header = FALSE)
colnames(logit_data) <- c("A","study_hours")

#assignment of initial variables
y <- logit_data$A
x <- logit_data$study_hours
alpha <- 0.001
b_last <- matrix(ncol=2,nrow=1,data=0)
colnames(b_last) <- c("b0","b1")
b <- matrix(ncol=2,nrow=1,data=0)
colnames(b_last) <- c("b0","b1")
err <- 100
eps <- 10^-4
b_history <- matrix(ncol=2, data = 0)
colnames(b_history) <- c("b0","b1")
grad <- matrix(ncol=1,nrow=2)

#calculation of grad descent
while(err > eps) {
  grad[1,] <- sum(y-exp(b_last[,1]+b_last[,2]*x)/(1+exp(b_last[,1]+b_last[,2]*x)))
  grad[2,] <- sum(x*y-x*exp(b_last[,1]+b_last[,2]*x)/(1+exp(b_last[,1]+b_last[,2]*x)))
  b[,1] = b_last[,1] + alpha*grad[1,]
  b[,2] = b_last[,2] + alpha*grad[2,]
  err_temp <- abs(b - b_last)
  err <- sqrt(err_temp[,1]^2+err_temp[,2]^2)
  b_last <- b
  b_history <- rbind(b_history, b_last)
}

#print final betas
cat("b_0 =",b_last[,1],"\n")
cat("b_1 =",b_last[,2],"\n")

#plot betas
plot(b_history[,1], xlab = "Iterations", ylab = "beta_0")
plot(b_history[,2], xlab = "Iterations", ylab = "beta_1")

#calculate probability of NOT getting an A (y=0)
#if studying 5 hours per week
b_0 <- b_last[,1]
b_1 <- b_last[,2]
x_input <- 5
prob_a <- exp(b_0+b_1*x_input)/(1+exp(b_0+b_1*x_input))
prob_not_a <- 1-prob_a
cat("A student who studies 5 hours per week has a probability of not getting an A of",prob_not_a,"\n")

