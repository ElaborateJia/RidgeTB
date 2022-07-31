
#' @title TailoredBayes: A package for targeted Bayesian inference for binary outcomes
#'
#' @description
#' The package provides functions for tailored Bayesian inference when different classification errors incur
#' different penalties. See vignette for an introduction.
#'
#' @section TailoredBayes functions:
#' The main function is \code{metrop_tailor()} which uses random walk MCMC algorithm to sample from the posterior.
#' It returns a list object where the first element, \code{chain}, is of class "mcmc". This element can be summarized by
#' functions provided by the coda package or similar ones. See function doc for details.
#'
#' The \code{ESS_t()} function calculates the effective sample size for tailoring, i.e., the sample size
#' inferences will be based on.
#'
#' @docType package
#' @name TailoredBayes
NULL
