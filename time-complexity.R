library(cecs)
source("./R/des-cma-esr-csa-emean-no-cov.R")
print("start")

alg = des_cma_esr_csa_emean_no_cov

results = list()
for (D in c(10, 20)) {
  startTimeT0 = base::Sys.time()
  x = 0.55
  for (i in 1:200000) {
    x = x + x;
    x = x / 2;
    x = x * x;
    x = sqrt(x);
    x = log(x);
    x = exp(x);
    x = x / (x + 2);
  }
  endTimeT0 = base::Sys.time()
  T0 = endTimeT0 - startTimeT0
  
  startTimeT1 = base::Sys.time()
  alg(
    par = runif(D, -100, 100),
    lower = -100, upper = 100,
    fn = function(x) { cecs::cec2022(1, x) }
  )
  endTimeT1 = base::Sys.time()
  T1 = endTimeT1 - startTimeT1

  vecT2 = c()
  for (k in 1:5) {
    startTimeT2 = base::Sys.time()
    alg(
      par = runif(D, -100, 100),
      lower = -100, upper = 100,
      fn = function(x) { cecs::cec2022(1, x) }
    )
    endTimeT2 = base::Sys.time()
    T2 = endTimeT2 - startTimeT2
    vecT2 = c(vecT2, T2)
  }
  results[[D]] = list(
    T0 = T0,
    T1 = T1,
    vecT2 = vecT2
  )
}
