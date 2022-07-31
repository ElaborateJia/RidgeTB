# function to create probabilistic predictions
# param: mcmc chain
# x: the covariates
# fun: how to summarise the posterior predictive distribution
probs.Solon <- function(param, x, fun) {
    # prob1 <- expit(param[,1] + as.matrix(x) %*% t(param[,-1]))
    y <- ncol(param)
    d<- c(-1,-y)

    # print(dim(t(param[,d])))
    # print(dim(x))
    # as.matrix(sapply(SFI, as.numeric))
    prob1 <- expit(param[,1] +  as.matrix(sapply(x, as.numeric)) %*% t(param[,d]))
    print(dim(prob1))
    prob1[is.nan(prob1)] <- 0
    predictions_logit <- apply(prob1, MAR = 1, fun)
    predictions_logit
}
install.packages('spate')
library(spate)
plot(fit_tailor_d1_SB$chain[40001:100000,1])
lines(fit_tailor_d1_SBv2$chain[40001:100000,1])

trace.plot(fit_tailor_d1_SB$chain[40001:100000,1])
