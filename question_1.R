#clear environment
rm(list=ls())

#predicting Direction using logistic regression
data <- read.csv("Weekly.csv")
data$Direction <- as.factor(data$Direction)
logit_fit <- glm(data = data, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(logit_fit)
Direction_hat_logit <- rep("Down", nrow(data))
Direction_hat_logit_prob <- predict(logit_fit, type = "response")
Direction_hat_logit[which(Direction_hat_logit_prob >= 0.5)] <- "Up"
Direction_hat_logit <- as.factor(Direction_hat_logit)

#confusion matrix method from book
table(Direction_hat_logit,data$Direction)
mean(Direction_hat_logit == data$Direction)

#alt confusion matrix method w/ calculated performance metrics
install.packages("caret")
library(caret)
confusionMatrix(data = as.factor(Direction_hat_logit), reference = data$Direction)

#predicting Direction using LDA
lda_fit <- MASS::lda(data = data, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume)
lda_pred <- predict(lda_fit, data)
Direction_hat_lda <- lda_pred$class

#confusion matrix method from book - lda
table(Direction_hat_lda,data$Direction)
mean(Direction_hat_lda == data$Direction)

#second change for git testing

#alt confusion matrix method w/ calculated performance metrics - lda
confusionMatrix(data = as.factor(Direction_hat_lda), reference = data$Direction)