library(BBmisc)
library(checkmate)
library(magrittr)
library(Matrix)
source(here::here("R", "cmaesr-utils.R"))
source(here::here("R", "hybrid-utils.R"))
source(here::here("R", "des.R"))
source(here::here("R", "cma-esr-ppmf.R"))

#' DES-CMA-ES-PPMF

des_cma_esr_ppmf_best_no_cov <- hybrid_factory(
  des,
  cma_esr_ppmf
)

des_cma_esr_ppmf_amean_no_cov <- hybrid_factory(
  des,
  cma_esr_ppmf,
  "last_amean.param"
)
des_cma_esr_ppmf_emean_no_cov <- hybrid_factory(
  des,
  cma_esr_ppmf,
  "last_emean.param"
)

des_cma_esr_ppmf_best_cov <- hybrid_factory(
  des,
  cma_esr_ppmf,
  "best.param",
  TRUE
)
des_cma_esr_ppmf_amean_cov <- hybrid_factory(
  des,
  cma_esr_ppmf,
  "last_amean.param",
  TRUE
)
des_cma_esr_ppmf_emean_cov <- hybrid_factory(
  des,
  cma_esr_ppmf,
  "last_emean.param",
  TRUE
)

