library(BBmisc)
library(checkmate)
library(magrittr)
source(here::here("R", "cmaesr-utils.R"))
source("des.R")
source("cma-esr-csa.R")

#' DES-CMA-ES-CSA

des_cma_esr_csa <- function(
  par,
  fn,
  ...,
  lower = -100,
  upper = 100,
  control = list()
) {
  lb <- lower
  ub <- upper
  n <- length(par)
  budget <- 10000 * n
  lambda <- getCMAESParameter(control, "lambda", 4 * n)
  assertInt(lambda, lower = 4)
  mu <- getCMAESParameter(control, "mu", floor(lambda / 2))
  assertInt(mu)

  des_result <- des(par = par, fn = fn, ..., lower = lb, upper = ub, control = list(
    budget = 0.5 * budget,
    lambda = lambda,
    mu = mu
  ))

  C <- cov(t(des_result$diagnostic$pop))
  e <- eigen(C, symmetric = TRUE)
  b <- e$vectors
  d <- diag(sqrt(e$values), length(e$values))

  return(cma_esr_csa(
    par = des_result$par,
    fn = fn,
    lower = lb,
    upper = ub,
    control = list(
      budget = 0.5 * budget,
      B_matrix = b,
      D_matrix = d
    )
  ))
}
