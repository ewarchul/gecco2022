hybrid_factory = function(
  des,
  cmaes
) {
  return(\(par, fn, ..., lower, upper, control) {
    lb <- lower; ub <- upper; n <- length(par)
    budget <- 10000 * n

    lambda <- getCMAESParameter(control, "lambda", 4 * n)
    assertInt(lambda, lower = 4)

    mu <- getCMAESParameter(control, "mu", floor(lambda / 2))
    assertInt(mu)

    des_result <- des(
      par = par,
      fn = fn,
      ...,
      lower = lb,
      upper = ub,
      control = list(budget = 0.5 * budget, lambda = lambda, mu = mu)
    )

    b <- matrix(); d <- matrix()
    C <- cov(t(des_result$diagnostic$pop))
    e <- eigen(C, symmetric = TRUE)

    if (any(is.nan(sqrt(e$values)))) {
      C_corr = Matrix::nearPD(C)$mat
      e_corr = eigen(C_corr, symmetric = TRUE)
      b = e_corr$vectors
      d <- diag(sqrt(e_corr$values), length(e_corr$values))
    } else {
      b <- e$vectors
      d <- diag(sqrt(e$values), length(e$values))
    }

    return(cmaes(
      par = des_result$par,
      fn = fn,
      lower = lb,
      upper = ub,
      control = list(budget = 0.5 * budget, B_matrix = b, D_matrix = d)
    ))
    }
  )
}
