#'
#'Metropolis algorithm
#'
#' @description
#' Random walk Metropolis algorithm for Tailored Bayesian inference.
#'
#' @param formula An object of class 'formula'. The details of model specification are given under ‘Details’.
#'
#' @param data Data frame.
#'
#' @param lambda The discounting factor. A scalar greater or equal to zero. see 'Details'.
#'
#' @param pi_u The unweighted probability of the outcome. A vector of equal size as the datapoints.
#' It must range between zero and one.
#'
#' @param t The target threshold. A scalar between zero and one.
#'
#' @param distance_measure The function to calculate the distance between pi_u and t. Possible values "squared"
#' or "absolute". Default is "squared", see 'Details.
#'
#' @param epsilon The insensitivity threshold. A scalar greater or equal to zero. Default is zero, see 'Details'.
#'
#' @param h_function A function object. Custom weighting function. Default is NULL.
#' Details section and the examples below for more information.
#'
#' @param initial A real vector, the initial state of the Markov chain. If left unspecified a vector of zeros is used.
#'
#' @param warm_up The number of warm-up iterations for the sampler.
#'
#' @param burn_in The number of burn-in iterations for the sampler.
#'
#' @param n_mcmc The number of Metropolis iterations for the sampler.
#'
#' @param tune Scalar to achieve optimal acceptance rate. Default is 2.38;
#' \insertCite{@see Chapter 4, @brooks2011handbook}{TailoredBayes}. Make sure that the acceptance rate is satisfactory
#' (typically between 0.20 and 0.5) before using the posterior sample for inference.
#'
#' @param iter_adapt The number of iterations between warm-up and burn-in that the proposal is tuned. Default is 200.
#'
#' @param user_prior_density If non-NULL, the prior (log)density up to a constant of proportionality.
#' This must be a function defined in R. See 'Details'.
#'
#' @param prior_mean The mean for the default Gaussian prior. Default is 0.
#'
#' @param prior_sd The standard deviation for the default Gaussian prior. Default is 1000.
#'
#' @param proposal_function The proposal function. The default is \eqn{k}-variate Gaussian,
#' where \eqn{k} is the length of the parameter vector. Future implementation will support more proposals.
#'
#' @param ... Optional args.
#'
#'
#' @details
#'
#' \subsection{The Model}{
#'   A typical predictor has the form response ~ terms where response is a binary
#'   response vector (denoted \eqn{y}), and terms is a series of terms (denoted \eqn{x})
#'   which specifies a linear predictor for response.
#'
#'   \code{metrop_tailor()} simulates from the posterior distribution of a tailored logistic regression model
#'   using a random walk Metropolis algorithm. The objective loss takes the following form:
#'
#'   \deqn{\ell(y_i, x^T_i \beta) = -(\pi_w(x_i;\beta))^{y_i} (1 - \pi_w(x_i;\beta))^{1-y_i},}
#'
#'   where \eqn{\pi_w(x_i;\beta) = (\exp \{ {x^T_i \beta} \}/ 1 + \exp\{ {x^T_i \beta}\})^{w_i}},
#'   \eqn{w_i = \exp - \{ \lambda h(pi_u, t, \epsilon) \} } and \eqn{i = 1, \ldots, n} (the sample size).
#'   The function \eqn{h()} is family  of \eqn{\epsilon}-insensitive functions, allowing customization using
#'   the \code{distance_measure} and \code{epsilon} arguments. The default function \eqn{h()} is
#'   the squared error with \eqn{\epsilon = 0}, i.e
#'
#'   \eqn{h(pi_u, t, \epsilon) := (pi_u - t)^2}.
#'
#'   The user can specify custom \eqn{h()}, see below. By default, we assume a multivariate Normal prior
#'   on \eqn{\beta}:
#'
#'   \deqn{\beta \sim \mathcal{N}(0, I\sigma)}{\beta \sim \mathcal{N}(0, I\sigma),}
#'
#'   with \eqn{\sigma = 1000}.
#'   }
#'
#' \subsection{Custom \code{h_function} and \code{user_prior_density}}{
#'  Both use the function factory pattern \insertCite{@see Chapter 10, @wickham_advanced}{TailoredBayes}.
#'  This is to avoid using either global variables or `\dots` arguments.
#'  See examples below and \href{https://cran.r-project.org/web/packages/mcmc/vignettes/demo.pdf}{Geyer, 2020 Appendix A},
#'  and the package vignette.
#'  }
#'
#' @return A list object where the first element \code{chain} is of class "mcmc". This element can be summarized by
#' functions provided by the \href{https://cran.r-project.org/web/packages/coda/coda.pdf}{coda} package,
#' or similar ones, such as \href{https://cran.r-project.org/web/packages/ggmcmc/ggmcmc.pdf)}{ggmcmc}.
#'
#'
#' @examples
#' data(out)
#' set_lambda <- 0
#' p <- rep(1, nrow(out))
#' threshold <- 0.3
#'
#' ## default independent normal prior
#' posterior <- metrop_tailor(y ~ x1 + x2, data = out, lambda = set_lambda, pi_u = p, t = threshold)
#'
#' plot(posterior$chain)
#' summary(posterior$chain)
#'
#' ## user-defined independent Cauchy prior
#' logpriorfun <- function(beta){
#'     sum(dcauchy(beta, log = TRUE))
#'     }
#'
#' posterior <- metrop_tailor(y ~ x1 + x2, data = out, lambda = set_lambda, pi_u = p, t = threshold,
#'                           user_prior_density = logpriorfun)
#'
#' plot(posterior$chain)
#' summary(posterior$chain)
#'
#' ## user-defined independent Cauchy prior with additional args
#' logpriorfun <- function(location, scale) function(beta){
#'    sum(dcauchy(beta, location, scale, log = TRUE))
#'    }
#'
#' posterior <- metrop_tailor(y ~ x1 + x2, data = out, lambda = set_lambda, pi_u = p, t = threshold,
#'                           user_prior_density = logpriorfun(location = 0, scale = 5))
#' plot(posterior$chain)
#' summary(posterior$chain)
#'
#' ## user-defined h_function
#' my_h_function <- function(indicator) function(pi_u, t){
#' # the second function() should always have two args, pi_u and t
#'    if(indicator == 1){
#'        (pi_u - t)^2 # squared error
#'    } else {
#'        abs(pi_u - t) # absolute error
#'    }
#'}
#'
#'posterior <- metrop_tailor(y ~ x1 + x2, data = out, lambda = set_lambda, pi_u = p, t = threshold,
#'                           h_function = my_h_function(indicator = 2))
#'
#' plot(posterior$chain)
#' summary(posterior$chain)
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @references
#' \insertAllCited{}
#'
#'
# mcmc sampling ---------------------------
# tune 2.38
metrop_tailor <- function(formula, data, lambda, pi_u, t, distance_measure, epsilon, h_function = NULL,
                          initial, warm_up = 20000, burn_in = 20000, n_mcmc = 100000,
                          tune = 2.38, iter_adapt = 200, user_prior_density = NULL, prior_mean = 0, prior_sd = 1000,
                          proposal_function = proposal_fun, ...) {

    #===================
    # Basic checks
    #===================

    if (missing(distance_measure)) {
        distance_measure = "squared"
        } else {
            distance_measure = distance_measure
            }

    if (missing(epsilon)) {
        epsilon = 0
        } else {
            epsilon = epsilon
            }

    if(!is.null(h_function)){
        distance_measure = "NULL"
        epsilon = "NULL"
        }

    if(!is.null(h_function) & !is.function(h_function)){
        stop("The h_function argument must be a function")
        }

    if(!is.null(h_function)){
        h_function <- do.call(h_function, list(pi_u, t))
    }

    if(!is.null(user_prior_density) & !is.function(user_prior_density)){
        stop("The user_prior_density argument must be a function")
    }

    if(is.null(user_prior_density)){
        prior_fun <- prior_default(prior_mean, prior_sd)
    } else {
        prior_fun <- user_prior_density
    }

    #===================
    # Form starting parameters
    #===================
    mf <- model.frame(formula = formula, data = data)
    x <- model.matrix(attr(mf, "terms"), data = mf)
    # print(dim(x))
    if (missing(initial)) {
        initial = rep(0, ncol(x) +1)
        } else if (length(initial) != ncol(x) + 1 ){
            stop("initial values must be the same length as the unknown parameters")
            }

    #===================
    # Sampling
    #===================
    chain = array(dim = c(warm_up + burn_in + n_mcmc + 1, length(initial)))
    # schain = array(dim = c(warm_up + burn_in + n_mcmc + 1, 1))

    chain[1,] = c(initial)
    # schain[1,] = 0
    # print(chain[1,])
    # print(prior_fun)
    loglik.old <- posterior(formula = formula, data = data, param = chain[1, ],lambda = lambda,
                            pi_u = pi_u, t = t, distance_measure = distance_measure, epsilon = epsilon,
                            h_function = h_function, prior_fun = prior_fun, ...)

    logliks <- NULL

    p <- length(initial)
    print(loglik.old)
    sigma = 0.1 * diag(p)
    # alpha = 0.1

        for (i in 1:warm_up) {
            # proposal = unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[1])
            # hyperparameter =  unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[2])
            proposal = proposal_function(chain[i,],sigma)
            # print(mode(proposal))
            # print(proposal)
            # print(hyperparameter)
            loglik.new <- posterior(formula = formula, data = data, param = proposal, lambda = lambda,
                                    pi_u = pi_u, t = t, distance_measure = distance_measure, epsilon = epsilon,
                                    h_function = h_function, prior_fun = prior_fun, ...)

            ratio = exp(loglik.new - loglik.old)
            # print(ratio)

            if (runif(1) < ratio) {
                chain[i + 1,] = proposal
                # schain[i+1,] = hyperparameter
                # accept <- accept + 1
                logliks <- c(logliks, loglik.new)
                loglik.old <- loglik.new
            } else {
                chain[i + 1,] = chain[i,]
                # schain[i+1,] = schain[i,]
                logliks <- c(logliks, loglik.old)
            }
        }
        print('Warm_up finished')
        for (i in (warm_up + 1):(warm_up + burn_in)) {
            if (i %% iter_adapt == 0) {
                sigma = (((tune) ^ 2) / p) * cov(chain[seq(i - warm_up, i - 1, length.out = iter_adapt),],
                                                              use = "complete.obs") + (1e-06 * diag(p)) #1e-04*diag(p)
            }
            # proposal = unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[1])
            # hyperparameter =  unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[2])
            proposal = proposal_function(chain[i,],sigma)


            loglik.new <- posterior(formula = formula, data = data, param = proposal,lambda = lambda,
                                    pi_u = pi_u, t = t, distance_measure = distance_measure, epsilon = epsilon,
                                    h_function = h_function, prior_fun = prior_fun, ...)

            ratio = exp(loglik.new - loglik.old)

            # why judge ratio to na
            if (is.na(ratio)) {
                chain[i + 1,] = proposal
                # schain[i+1,] = hyperparameter
            } else {
                if (runif(1) < ratio) {
                    chain[i + 1,] = proposal
                    # schain[i+1,] = hyperparameter
                    # accept <- accept + 1
                    logliks <- c(logliks, loglik.new)
                    loglik.old <- loglik.new
                } else {
                    chain[i + 1,] = chain[i,]
                    # schain[i+1,] = schain[i,]
                    logliks <- c(logliks, loglik.old)
                }
            }
        }
        print('Burning finished')
        accept <- 0

        for (i in (warm_up + burn_in + 1):(warm_up + burn_in + n_mcmc)) {
            # proposal = unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[1])
            # hyperparameter =  unlist(proposal_function(chain[i, ], schain[i,], sigma,alpha)[2])
            proposal = proposal_function(chain[i,],sigma)


            loglik.new <- posterior(formula = formula, data = data, param = proposal, lambda = lambda,
                                    pi_u = pi_u, t = t, distance_measure = distance_measure, epsilon = epsilon,
                                    h_function = h_function, prior_fun = prior_fun, ...)

            ratio = exp(loglik.new - loglik.old)

            if (is.na(ratio)) {
                chain[i + 1,] = proposal
                # schain[i+1,] = hyperparameter
            } else {
                if (runif(1) < ratio) {
                    chain[i + 1,] = proposal
                    # schain[i+1,] = hyperparameter
                    accept <- accept + 1
                    logliks <- c(logliks, loglik.new)
                    loglik.old <- loglik.new
                } else {
                    chain[i + 1,] = chain[i,]
                    # schain[i+1,] = schain[i,]
                    logliks <- c(logliks, loglik.old)
                }
            }
        }
        print('Training finished')
        chain <- chain[-(1: (warm_up + burn_in + 1)), ]
        # schain <- schain[-(1: (warm_up + burn_in + 1)), ]

        out <- list(chain = coda::mcmc(chain),
                    # schain = coda::mcmc(schain),
             accept = accept / n_mcmc,
             accept2 = accept,
             logliks = logliks,
             Sigma = sigma,
             data = data,
             lambda = lambda,
             pi_u = pi_u,
             t = t,
             distance_measure = distance_measure,
             epsilon = epsilon,
             h_function = h_function,
             initial = initial,
             tune = tune,
             iter_adapt = iter_adapt,
             formula = formula,
             prior = prior_fun,
             prior_mean = prior_mean,
             prior_sd = prior_sd,
             call = match.call(),
             additional_args = ...)

        return(out)

    }









