library(tidyverse)
library(Rlab)
library(devtools)
library(splitstackshape)
library(MASS)
library(LaplacesDemon)
library(rstan)
library(rstanarm)
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/metrop.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/data.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/likelihood.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/prior_posterior_proposal.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/TailoredBayes.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/weights_construction.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/CV.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/posterior_predictive.R")
source("/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/NB_treated.R")
source('/Users/shenjia/Desktop/Mphil dissertation/Ridge_TB/R/Data_Simulation.R')

library(tidyverse)
library(caret)
library(GGally)
library(ggplot2)
library(corrplot)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
install.packages('rstanarm')
library(rstanarm)
options(mc.cores = 1)
library(loo)
install.packages('projpred')
library(projpred)
library(Rlab)
library(bayesreg)
library(devtools)

q <- 1
x1<- runif(5000)
x2 <- runif(5000)
d <- q * x2/(x1 + x2 * q)

y<- rbern(5000,d)

data <- data.frame(y  = y, x1 = x1, x2 = x2)
set.seed(42)
data_split <- stratified(data, group = "y", size = 0.2, bothSets = T)
data_design <- data_split$SAMP1
data_train <- data_split$SAMP2
#
#
pre_fit <- glm(y ~ x1 + x2, family = binomial("logit"),data =  data_design)
pred <- predict(pre_fit, newdata = data_train, type = "response")
fit_tailor <- metrop_tailor(factor(y) ~ x1 + x2 , data = data_train, lambda = 0, pi_u = pred, t = 0.1,user_prior_density = ridge_prior)
fit_2d <- bayesreg(factor(y) ~ ., model = "logistic", prior = "ridge", data = data,n.samples = 5000)

table_lambda  <- cbind(exp(fit_tailor$chain[,4]),t(fit_2d$tau2 ^ (-1/2)))

table_lambda <- cbind(table_lambda, t(fit_2d$sigma2 ^(-1/2)))
table_lambda <- as.data.frame(table_lambda)
colnames(table_lambda) <- c('tb','tau2','sigma2')
ggplot(table_lambda)+
    geom_density(aes(tb),color = 'red')
    # geom_density(aes(tau2),color = 'blue')+
    # geom_density(aes(sigma2), color = 'green')+
        # labs( x= 'value', y = 'density')

t_01 <- NULL
for (lambda in seq(3,30,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train, 0, 0, lambda, 0.1, 1000, 1000, 5000,pretrain = pre_fit)
    t_01 <-append(t_01,mean(fit_result$errors[,4]))
}

t_03 <- NULL
for (lambda in seq(3,30,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train, 0, 0, lambda, 0.3, 1000, 1000, 5000,pretrain = pre_fit)
    t_03 <-append(t_03,mean(fit_result$errors[,4]))
}

t_05 <- NULL
for (lambda in seq(3,30,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train, 0, 0, lambda, 0.5, 1000, 1000, 5000,pretrain = pre_fit)
    t_05 <-append(t_05,mean(fit_result$errors[,4]))
}
simulate_D1



data_split <- stratified(simulate_D1, group = "y", size = 0.2, bothSets = T)
data_design_d1 <- data_split$SAMP1
data_train_d1<- data_split$SAMP2

pre_fit_glm <- glm(y ~ ., family = binomial("logit"),data =  data_design_d1)
pred_glm <- predict(pre_fit_glm, newdata = data_train_d1 ,type = "response")



# T = 0.1

pre_fit_01 <- bayesreg(y ~ ., model = "logistic", prior = "ridge", data = simulate_D1,n.samples = 5000)
pred_fit <- predict(pre_fit_glm,data_train_d1,type = 'response')
pi_u <- NULL
for (i in 1: length(pred_fit)){
    pi_u<- append(pi_u,pred_fit[[i]])
}

# result <- CV_function_loss_ADAPT_nb(5, data_train_d1, 0, 0, 3, 0.1, 1000, 1000, 5000,pretrain = pre_fit)

t_01_d1 <- NULL
for (lambda in seq(3,15,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train_d1, 0, 0, lambda, 0.1, 1000, 1000, 5000,pretrain = pre_fit_glm)
    t_01_d1 <-append(t_01_d1,mean(fit_result$errors[,4]))
    print('loop')
}
t_01_d1
fit_tailor_d1 <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 6,  pi_u = pred_fit , t = 0.1,user_prior_density = ridge_prior)
fit_tailor_d1_SB <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 0,  pi_u = pred_fit , t = 0.1,user_prior_density = ridge_prior)

burnin <- 1000
warm_up <- 1000
predict_d1 <- probs.Solon(fit_tailor_d1$chain[-c(1:(burnin + warm_up)),], simulate_D1_t[,-101], fun = mean)
predict_d_SB <- probs.Solon(fit_tailor_d1_SB$chain[-c(1:(burnin + warm_up)),], simulate_D1_t[,-101], fun = mean)
NB_01_TB <- net_benefit_treated(predict_d1, obs_y = simulate_D1_t[,101], risk_threshold = 0.1)
NB_01_SB <- net_benefit_treated(predict_d_SB, obs_y = simulate_D1_t[,101], risk_threshold = 0.1)



predict_01_BR <-predict(pre_fit_01,simulate_D1_t[,-61],type = 'response')
NB_01_BR <- net_benefit_treated(predict_01_BR, obs_y = simulate_D1_t[,61], risk_threshold = 0.1)


predict_01_SB <- probs.Solon(fit_tailor_d1_SB$chain[-c(1:2000),], simulate_D1_t[,-61], fun = mean)
NB_01_SB <- net_benefit_treated(predict_01_SB, obs_y = simulate_D1_t[,61], risk_threshold = 0.1)


#  t = 0.3

t_03_d1 <- NULL
for (lambda in seq(18,24,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train_d1, 0, 0, lambda, 0.3, 1000, 1000, 5000,pretrain = pre_fit_glm)
    t_03_d1 <-append(t_03_d1,mean(fit_result$errors[,4]))
}


fit_tailor_d3 <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 15,  pi_u = pred_fit , t = 0.3,user_prior_density = ridge_prior)
fit_tailor_d3_SB <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 0,  pi_u = pred_fit , t = 0.3,user_prior_density = ridge_prior)
pre_fit_03 <- bayesreg(y ~ ., model = "logistic", prior = "ridge", data = simulate_D1,n.samples = 5000)

burnin <- 1000
warm_up <- 1000
predict_d3 <- probs.Solon(fit_tailor_d3$chain[-c(1:(burnin + warm_up)),], simulate_D1_t[,-101], fun = mean)
predict_d3_SB <- probs.Solon(fit_tailor_d3_SB$chain[-c(1:(burnin + warm_up)),], simulate_D1_t[,-61], fun = mean)
NB_03_TB <- net_benefit_treated(predict_d3, obs_y = simulate_D1_t[,101], risk_threshold = 0.3)
NB_03_SB <- net_benefit_treated(predict_d3_SB, obs_y = simulate_D1_t[,61], risk_threshold = 0.3)

for (i in 1: 100) {
    print(quantile(fit_tailor_d3$chain[,i+1], c(0.05,0.95)))
}

# lambda = 28

t_05_d1 <- NULL
for (lambda in seq(3,30,3)){
    fit_result <- CV_function_loss_ADAPT_nb(5, data_train_d1, 0, 0, lambda, 0.5, 1000, 1000, 5000,pretrain = pre_fit_glm)
    t_05_d1 <-append(t_05_d1,mean(fit_result$errors[,4]))
}

fit_tailor_d5 <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 3,  pi_u = pred_fit , t = 0.5,user_prior_density = ridge_prior)
fit_tailor_d5_SB <- metrop_tailor(y ~ . , data = data_train_d1, lambda = 0,  pi_u = pred_fit , t = 0.5,user_prior_density = ridge_prior)

plot(density(exp(fit_tailor_d3_SB$chain[,102])),col = 'red')
plot(density(exp(fit_tailor_d3$chain[,102])))











fit_result <- CV_function_loss_ADAPT_nb(5, data, 0, 0, 1, 0.1, 1000, 1000, 5000)






rv <- bayesreg(factor(y) ~ x1 + x2, model = "logistic", prior = "ridge", data = data_train,n.samples = 5000)




plot(density(fit_tailor$chain[2001:5000,3]))
lines(density(rv$beta[2,1001:5000]), col = 'red')





plot(fit_tailor$schain)
traceplot(fit_tailor$chain)
mode(fit_tailor$chain)


data_linear <- data.frame(fit_tailor$chain)
data_linear$slope <- -data_linear$X2 / data_linear$X3
data_linear$intercept <- (log(3/7)-data_linear$X1) / data_linear$X3

new_data <- data_linear[,4:5]
x1 <- cbind(c(0.25,1),c(0.75,1))

result <- as.matrix(new_data) %*% (x1)
new_data$x1 <- 0.5
new_data$x2 <- new_data$slope * new_data$x1 + new_data$intercept
result$x2 <- result


mean_slope <- (quantile(result[,2],probs = 0.5)[['50%']] - quantile(result[,1],probs = 0.5)[['50%']]) / 0.5
mean_intercept <- quantile(result[,1],probs = 0.5)[['50%']] - mean_slope*0.25

lower_slope <- (quantile(result[,2],probs = 0.025)[['2.5%']] - quantile(result[,1],probs = 0.025)[['2.5%']]) / 0.5
lower_intercept <- quantile(result[,1],probs = 0.025)[['2.5%']] - lower_slope*0.25

upper_slope <- (quantile(result[,2],probs = 0.975)[['97.5%']] - quantile(result[,1],probs = 0.975)[['97.5%']]) / 0.5
upper_intercept <- quantile(result[,1],probs = 0.975)[['97.5%']] - upper_slope*0.25

myline<- geom_abline(slope = mean_slope ,intercept = mean_intercept,color= 'red',size = 1 )
myline_5<- geom_abline(slope = lower_slope ,intercept = lower_intercept,color= 'red',size = 1 )
myline_95<- geom_abline(slope = upper_slope ,intercept = upper_intercept,color= 'red',size = 1 )

baseline_09<- geom_abline(slope = 9, intercept = 0, color = 'black', size = 1)
baseline_01<- geom_abline(slope = 1/9, intercept = 0, color = 'black', size = 1)
baseline_05<- geom_abline(slope = 1, intercept = 0, color = 'black', size = 1)
baseline_07<- geom_abline(slope = 7/3, intercept = 0, color = 'black', size = 1)
baseline_03<- geom_abline(slope = 3/7, intercept = 0, color = 'black', size = 1)

myplot <- qplot(0,0)

myplotXscale <- scale_x_continuous(limits = c(0,1))
myplotYscale <- scale_y_continuous(limits = c(0,1))

myplotXlabel <- xlab('x1')
myplotYlabel <- ylab('x2')
myplotTitle <- ggtitle('My Graph Title')

myplot <- myplot + myplotXscale + myplotYscale + myplotXlabel + myplotYlabel + myplotTitle

myplot <- myplot + myline + myline_5 + myline_95
myplot <- myplot + baseline_09 +baseline_01+baseline_05+baseline_07+baseline_03
print(myplot)


























