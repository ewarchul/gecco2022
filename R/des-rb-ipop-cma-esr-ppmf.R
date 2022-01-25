library(BBmisc)
library(checkmate)
library(magrittr)
source(here::here("R", "cmaesr-utils.R"))
source(here::here("R", "hybrid-utils.R"))
source(here::here("R", "des.R"))
source(here::here("R", "rb-ipop-cma-esr-ppmf.R"))

#' DES-RB-IPOP-CMA-ES-PPMF

des_rb_ipop_cma_esr_ppmf <- hybrid_factory(des, rb_ipop_cma_esr_ppmf)

