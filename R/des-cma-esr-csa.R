library(BBmisc)
library(checkmate)
library(magrittr)
source(here::here("R", "cmaesr-utils.R"))
source(here::here("R", "hybrid-utils.R"))
source(here::here("R", "des.R"))
source(here::here("R", "cma-esr-csa.R"))

#' DES-CMA-ES-CSA

des_cma_esr_csa_best_no_cov <- hybrid_factory(
  des,
  cma_esr_csa
)

des_cma_esr_csa_amean_no_cov <- hybrid_factory(
  des,
  cma_esr_csa,
  "last_amean.param"
)
des_cma_esr_csa_emean_no_cov <- hybrid_factory(
  des,
  cma_esr_csa,
  "last_emean.param"
)

des_cma_esr_csa_best_cov <- hybrid_factory(
  des,
  cma_esr_csa,
  "best.param",
  TRUE
)
des_cma_esr_csa_amean_cov <- hybrid_factory(
  des,
  cma_esr_csa,
  "last_amean.param",
  TRUE
)
des_cma_esr_csa_emean_cov <- hybrid_factory(
  des,
  cma_esr_csa,
  "last_emean.param",
  TRUE
)


