
# log-likelihood ---------------------------
like <- function(formula, data, param, lambda, pi_u, t, distance_measure, epsilon, h_function = NULL) {

    if (is.null(h_function)) {
        w <- weights(lambda, pi_u, t, distance_measure, epsilon)
    } else {
        w <- weights(lambda, pi_u, t, distance_measure, epsilon, h_function)
    }

    #w <- weights(lambda, pi_u, t, distance_measure, epsilon)

    # probs OLD
    # prob <- (as.numeric(expit(param[1] + as.matrix(x) %*% param[-1])))

    # probs
    mf <- model.frame(formula = formula, data = data)
    x <- model.matrix(attr(mf, "terms"), data = mf)
    # print(x)
    # print(param[1:100])
    y <- model.response(mf)

    ## y \in {0, 1} error checking
    if (!all(y == 0 | y == 1)){
        stop("Outcome must be coded 0 (non event) and 1 (event)")
    }
    n <- length(param) -1
    prob <- as.numeric(expit(x %*% param[1:n]))
    # print(sum(w[y == 1]*log(prob[y == 1])) + sum(w[y == 0]*log(1 - prob[y == 0])))
    # likelihood
    return (sum(w[y == 1]*log(prob[y == 1])) + sum(w[y == 0]*log(1 - prob[y == 0])))
}

#x <- rnorm(10)
#y <- rbinom(10, size = 1, prob = 0.5)
#dat <- data.frame(y = y, x = x)
#lambda = 1
#p <- runif(length(x), 0, 1)
#t <- c(0.3)
#param <- c(1, 1)
#like(y ~ x, dat, param, lambda, p, t)

# ### likelihood
# loss_logit_Hand <- function(param, weights, x, y, lambda, c) {
#
#     # weights
#     w <- exp(-lambda * (weights - c) ^ 2)
#
#     # weighted probs
#     prob1 <- (as.numeric(expit(param[1] + as.matrix(x) %*% param[-1])))
#
#     # likelihood
#     sum(w[y == 1]*log(prob1[y == 1])) + sum(w[y == 0]*log(1 - prob1[y == 0]))
# }
#
# loss_logit_abs <- function(param, weights, x, y, lambda, c) {
#
#     # weights
#     w <- exp(-lambda * abs(weights - c))
#
#     # weighted probs
#     prob1 <- (as.numeric(expit(param[1] + as.matrix(x) %*% param[-1])))
#
#     # likelihood
#     sum(w[y == 1]*log(prob1[y == 1])) + sum(w[y == 0]*log(1 - prob1[y == 0]))
# }
#
#
# loss_logit_epsilon_insensitive <- function(param, weights, x, y, lambda, c, epsilon) {
#
#     dist <- abs(weights - c)
#
#     w <- ifelse(dist <= epsilon, 1,
#                 # weights
#                 exp(-lambda * abs(weights - c) - epsilon)
#     )
#
#     # weighted probs
#     prob1 <- (as.numeric(expit(param[1] + as.matrix(x) %*% param[-1])))
#
#     # likelihood
#     sum(w[y == 1]*log(prob1[y == 1])) + sum(w[y == 0]*log(1 - prob1[y == 0]))
# }
