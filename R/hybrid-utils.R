
hybrid_factory <- function(des,
                           cmaes, point_type = "best.param", transfer_cov = FALSE) {
  return(function(par, fn, ..., lower, upper, control) {
    lb <- lower
    ub <- upper
    n <- length(par)

    budget <- getCMAESParameter(
      control,
      "budget",
      if (n == 10) { 2 * 10^5 } else if (n == 20) { 10^6 } else { n * 10^4 }
    )
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

    b <- matrix()
    d <- matrix()
    C <- cov(t(des_result$diagnostic$pop))
    e <- eigen(C, symmetric = TRUE)
    if (transfer_cov) {
      if (any(is.nan(sqrt(e$values)))) {
        C_corr <- Matrix::nearPD(C)$mat
        e_corr <- eigen(C_corr, symmetric = TRUE)
        b <- e_corr$vectors
        d <- diag(sqrt(e_corr$values), length(e_corr$values))
      } else {
        b <- e$vectors
        d <- diag(sqrt(e$values), length(e$values))
      }
    } else {
      b <- diag(n)
      d <- diag(n)
    }

    cmaes_result <- cmaes(
      par = des_result[[point_type]],
      fn = fn,
      lower = lb,
      upper = ub,
      control = list(budget = 0.5 * budget, B_matrix = b, D_matrix = d)
    )

    best_param <- if (fn(des_result$best.param) < fn(cmaes_result$best.param)) {
      des_result$best.param
    } else {
      cmaes_result$best.param
    }
    best_fitness <- min(des_result$best.fitness, cmaes_result$best.fitness)

    best_val_log <- (list(bestVal = matrix(sort(decreasing = TRUE, x = c(
      des_result$diagnostic$bestVal,
      cmaes_result$diagnostic$bestVal
    )))))

    return(list(
      best.param = best_param,
      best.fitness = best_fitness,
      value = best_fitness,
      n.evals = des_result$n.evals + cmaes_result$n.evals,
      label = "hybrid",
      diagnostic = best_val_log,
      message = cmaes_result$message,
      classes = "hybrid_result",
      des_result = des_result,
      cmaes_result = cmaes_result
    ))
})}

