#
# # Function to perform cross validation for the Hand model
#
# ########################################### logistic Hand model - train and predict
# TrainModelAndMakePredictions_Hand <- function(data.training, data.validation, startvalue, probabilities, lambda, c, sd, n.mcmc, burnIn, ...){
#
#     run.mcmc <- run.metropolis.MCMC(startvalue, probabilities, x = data.training[,-1], y = data.training$y, lambda, c, sd, n.mcmc, ...)
#
#     predictions.logit <- predict.logit.Solon(run.mcmc$chain[-c(1:burnIn),], data.validation[,-1], c, median)
#
#     err_logit <- mean(predictions.logit != data.validation$y)
#
#     fpr_logit <- mean(predictions.logit == 1 & data.validation$y == 0)
#     fnr_logit <- mean(predictions.logit == 0 & data.validation$y == 1)
#
#     err_logit_weight <- mean((c * (predictions.logit == 1 & data.validation$y == 0)) +
#                                  (1 - c) * (predictions.logit == 0 & data.validation$y == 1))
#
#     return(list(errors = c(err_logit, err_logit_weight, fpr_logit, fnr_logit),
#                 mcmc = run.mcmc))
# }
#
# ########################################### Cross-validation
# library(caret) # needed for stratified sampling
# CV_function_loss <- function(nfolds, data, startvalue, probabilities, lambda, c, sd, burnIn = 500, n.mcmc = 1000, ...){
#
#     n_folds <- nfolds
#
#     folds_i <- createFolds(factor(data$y), k = n_folds, list = FALSE)
#     #folds_i <- sample(rep(1:n_folds, length.out = nrow(data)))
#     train_predict <- matrix(NA, nrow = n_folds, ncol = 4)
#     mcmc <- list()
#
#     for(k in 1:n_folds){
#         test_i <- which(folds_i == k)
#         train_data <- data[-test_i, ]
#         test_data <- data[test_i, ]
#         train_model <- TrainModelAndMakePredictions_Hand(train_data, test_data, startvalue, probabilities[-test_i], lambda,
#                                                          c, sd, n.mcmc, burnIn)
#         train_predict[k,] <- train_model$errors
#         mcmc[[k]] <- train_model$mcmc
#
#     }
#     out <- list(errors = train_predict,
#                 mcmc = mcmc)
#
#     return(out)
# }
#
# ########################################### Adaptive MCMC
# TrainModelAndMakePredictions_Hand_ADAPT <- function(data.training, data.validation, startvalue, probabilities, lambda, c, warm_up,
#                                                     burnin, n.mcmc, ...){
#
#     run.mcmc <- run_metropolis_MCMC_adapt_Hand(startvalue, probabilities, x = data.training[,-1], y = data.training$y, lambda, c, warm_up,
#                                                burnin, n.mcmc, ...)
#
#     predictions.logit <- predict.logit.Solon(run.mcmc$chain[-c(1:(burnin + warm_up)),], data.validation[,-1], c, median)
#
#     err_logit <- mean(predictions.logit != data.validation$y)
#
#     fpr_logit <- mean(predictions.logit == 1 & data.validation$y == 0)
#     fnr_logit <- mean(predictions.logit == 0 & data.validation$y == 1)
#
#     err_logit_weight <- mean((c * (predictions.logit == 1 & data.validation$y == 0)) +
#                                  (1 - c) * (predictions.logit == 0 & data.validation$y == 1))
#
#     return(list(errors = c(err_logit, err_logit_weight, fpr_logit, fnr_logit),
#                 mcmc = run.mcmc))
# }
#
# ########################################### Adaptive CV
# library(caret) # needed for stratified sampling
# CV_function_loss_ADAPT <- function(nfolds, data, startvalue, probabilities, lambda, c, warm_up, burnin, n.mcmc, ...){
#
#     n_folds <- nfolds
#
#     folds_i <- createFolds(factor(data$y), k = n_folds, list = FALSE)
#     #folds_i <- sample(rep(1:n_folds, length.out = nrow(data)))
#     train_predict <- matrix(NA, nrow = n_folds, ncol = 4)
#     mcmc <- list()
#
#     for(k in 1:n_folds){
#         test_i <- which(folds_i == k)
#         train_data <- data[-test_i, ]
#         test_data <- data[test_i, ]
#         train_model <- TrainModelAndMakePredictions_Hand_ADAPT(train_data, test_data, startvalue, probabilities[-test_i], lambda,
#                                                                c, warm_up, burnin, n.mcmc)
#         train_predict[k,] <- train_model$errors
#         mcmc[[k]] <- train_model$mcmc
#
#     }
#     out <- list(errors = train_predict,
#                 mcmc = mcmc)
#
#     return(out)
# }


########################################### Adaptive MCMC - NET BENEFIT

#need to replace run_metropolis_MCMC_adapt_Hand with metrop_tailor() and
#change some of the input arguments to make it work

#TrainModelAndMakePredictions_Hand_ADAPT_nb() does 3 things:
# runs mcmc on data.training - run_metropolis_MCMC_adapt_Hand()
# creates predictions on data.validation - probs.Solon()
# calculates net benenit on data.validation - net_benefit_treated()
TrainModelAndMakePredictions_Hand_ADAPT_nb <- function(data.training, data.validation, startvalue, probabilities, lambda, c, warm_up,
                                                       burnin, n.mcmc, epsilon, ridge_prior,pretrain,...){
    # run mcmc - need to replace with metrop_tailor()
    # run.mcmc <- run_metropolis_MCMC_adapt_Hand(startvalue, probabilities, x = data.training[,-1], y = data.training$y, lambda, c, warm_up,
    #                                            burnin, n.mcmc, epsilon, ...)
    ridge_prior <- function(param){

        lambda <-dhalfcauchy(exp(param[length(param)]),1,log = TRUE)
        betas <- sum(dnorm(param[1:(length(param)-1)],mean = 0,sd = (1/exp(param[length(param)])) ^ (1/2),log = TRUE)) +lambda
        return(betas)
    }
    # data_split <- stratified(data.training, group = "y", size = 0.2, bothSets = T)
    # data_design <- data_split$SAMP1
    # data_train <- data_split$SAMP2
    # fit <- glm(y ~ . , family = binomial("logit"),data =  data_design)
    pred <- predict(pretrain, newdata = data.training, type = "response")

    run.mcmc <- metrop_tailor(y ~. , data = data.training, lambda = lambda, pi_u = pred  , t= c,
                              user_prior_density = ridge_prior, n_mcmc = n.mcmc, warm_up = warm_up,burn_in =  burnin)

    pred_probs <- probs.Solon(run.mcmc$chain[-c(1:(burnin + warm_up)),], data.validation[,-61], fun = mean)

    #predictions.logit <- predict.logit.Solon(run.mcmc$chain[-c(1:(burnin + warm_up)),], data.validation[,-1], c, median)

    net_benefit <- net_benefit_treated(pred_probs, obs_y = data.validation[,61], risk_threshold = c)

    # err_logit <- mean(predictions.logit != data.validation$y)

    #fpr_logit <- mean(predictions.logit == 1 & data.validation$y == 0)
    #fnr_logit <- mean(predictions.logit == 0 & data.validation$y == 1)

    #err_logit_weight <- mean((c * (predictions.logit == 1 & data.validation$y == 0)) +
    #                         (1 - c) * (predictions.logit == 0 & data.validation$y == 1))

    return(list(net_benefit = net_benefit,
                mcmc = run.mcmc))
}

########################################### Adaptive CV - NET BENEFIT
library(caret) # needed for stratified sampling
# performs cross-validation. It uses the TrainModelAndMakePredictions_Hand_ADAPT_nb() function.
# some arguments need to change to be compatible with metrop_tailor()

CV_function_loss_ADAPT_nb <- function(nfolds, data, startvalue, probabilities, lambda, c, warm_up, burnin, n.mcmc,pretrain, ...){

    n_folds <- nfolds

    folds_i <- createFolds(factor(data$y), k = n_folds, list = FALSE)
    #folds_i <- sample(rep(1:n_folds, length.out = nrow(data)))
    train_predict <- matrix(NA, nrow = n_folds, ncol = 9)
    mcmc <- list()

    for(k in 1:n_folds){
        test_i <- which(folds_i == k)
        train_data <- data[-test_i, ]
        test_data <- data[test_i, ]
        train_model <- TrainModelAndMakePredictions_Hand_ADAPT_nb(train_data, test_data, startvalue, probabilities[-test_i], lambda,
                                                                  c, warm_up, burnin, n.mcmc, epsilon,ridge_prior,pretrain)
        train_predict[k,] <- as.matrix(train_model$net_benefit)
        mcmc[[k]] <- train_model$mcmc
        print(k)

    }

    names <- c("threshold", "TPR", "FPR", "NB", "sNB", "rho", "NB_none", "NB_all", "sNB_all")
    colnames(train_predict) <- names

    out <- list(errors = train_predict,
                mcmc = mcmc)

    return(out)
}



#train_predict <- matrix(NA, nrow = 5, ncol = 9)

#train_predict[1,] <- as.matrix(tr$net_benefit)
#names <- c("threshold", "TPR", "FPR", "NB", "sNB", "rho", "NB_none", "NB_all", "sNB_all")
#colnames(train_predict) <- names
