library(BBmisc)
library(checkmate)
library(magrittr)
library(Matrix)
source(here::here("R", "cmaesr-utils.R"))
source(here::here("R", "des.R"))
source(here::here("R", "cma-esr-ppmf.R"))

#' DES-CMA-ES-PPMF

des_cma_esr_ppmf <- function(
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

  b <- matrix()
  d <- matrix()
  C <- cov(t(des_result$diagnostic$pop))
  e <- eigen(C, symmetric = TRUE)
  if (any(e$values) < 0) {
    C_corr = Matrix::nearPD(C)
    e_corr = eigen(C_corr, symmetrix = TRUE)
    b = e_corr$vectors
    d <- diag(sqrt(e_corr$values), length(e_corr$values))
  } else {
    b <- e$vectors
    d <- diag(sqrt(e$values), length(e$values))
  }

  return(cma_esr_ppmf(
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
