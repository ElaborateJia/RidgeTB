

# h function ---------------------------
h_function_default <- function(pi_u, t, distance_measure, epsilon){

    if (!all(dplyr::between(pi_u, 0, 1))) stop("pi_u must be between 0 and 1")

    if (!dplyr::between(t, 0, 1)) stop("t must be between 0 and 1")

    if (missing(distance_measure)) {
        # message("squared loss is used as distance measure")
        dist <- (pi_u - t) ^ 2
        distance_measure = "squared"
    } else if (distance_measure == "squared") {
        dist <- (pi_u - t) ^ 2
    } else if (distance_measure == "absolute"){
        dist <- abs(pi_u - t)
    } else {
        stop("only \"squared\" and \"absolute\" are supported as distance measures")
    }

    if (missing(epsilon)) {
        #message("epsilon = 0 is used as default")
        epsilon = 0
    }

    if (epsilon < 0) {
        stop("epsilon must be greater or equal to zero")
    } #else if (missing(epsilon)) {
    #        # message("epsilon = 0 is used as default")
    #        epsilon = 0
    #        }

    h <- ifelse(dist <= epsilon, 0, (dist - epsilon))

    out <- list(value = h,
                pi_u = pi_u,
                t = t,
                distance_measure = distance_measure,
                epsilon = epsilon)

    return(out)
}

#p <- seq(0.1, 0.9, 0.01)
#t <- c(0.3)
#h1 <- h_function.default(p, t, epsilon = 0.02)
#plot(p, h1$value)

# weigths construction ---------------------------
weights <- function(lambda, pi_u, t, distance_measure, epsilon, h_function = NULL){

    if (missing(lambda)) message("lambda is set to zero")

    if (lambda < 0) stop("lambda must be greater or equal to zero")

    if (is.null(h_function)) {
        h_function <- h_function_default(pi_u, t, distance_measure, epsilon)
        w <- exp(-lambda * h_function$value)
    } else {
        #h_function <- do.call(h_function, list(pi_u, t, ...))
        w <- exp(-lambda * h_function)
    }

    # get output from h_function.default

    return(w)
}

#w1 <- weights(1, p, t)
#w1
#my_h_fun <- runif(10, 0, 1)
#w2 <- weights(1, h_function = my_h_fun)
#w2

#sum(w1 == w2)
