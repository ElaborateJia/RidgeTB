# get array element number
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID")
task_id <- as.numeric(task_id_string)
task_id = 30


# load all necessary packages
library(tidyverse)
library(Rlab)
library(devtools)
library(splitstackshape)
library(MASS)
library(LaplacesDemon)
library(rstan)
library(rstanarm)
library(bayesreg)


# load additional functions/scripts
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

#########################################################
############################## combinations #############
#########################################################
# combinations of lambdas and thresholds to use
lambdas <- seq(3, 30, by = 3)
thresholds <- c(0.1, 0.3, 0.5)

combinations <- expand.grid(thresholds, lambdas)

#########################################################
############################## load data ################
#########################################################
# data_design
load(file = 'design_data.Rdata') # .Rdata object
# data_train
load(file = 'train_data.Rdata') # .Rdata object

# estimate pi_u(x)
pre_fit_glm <- glm(y ~ ., family = binomial("logit"), data = data_design_d1)
pred_glm <- predict(pre_fit_glm, newdata = data_train_d1 ,type = "response" )


# CV code
n_folds <- 5

#add CV code

# run mcmc at each fold
fit_result <- CV_function_loss_ADAPT_nb(n_folds, data_train_d1, lambda = combinations$Var2[task_id], c = combinations$Var1[task_id],
                                       n.mcmc = 100000,warm_up = 20000, burnin=20000,pretrain = pre_fit_glm)

#run_mcmc <- metrop_tailor(lambda = combinations$Var2[task_id], t = combinations$Var1[task_id], pi_u = preds, ... )

# evaluate NB


# CV output - data.frame
fit_result

# need to add t and lambda used
fit_result$lambdas <- combinations$Var2[task_id]
fit_result$threshold <- combinations$Var1[task_id]


output_file <- paste0("output_sim_", task_id, ".Rdata")

# Set the working directory to a subfolder within the current working directory



save(fit_result, file = output_file)



