library(BBmisc)
library(checkmate)
library(magrittr)
library(Matrix)
source(here::here("R", "cmaesr-utils.R"))
source(here::here("R", "hybrid-utils.R"))
source(here::here("R", "des.R"))
source(here::here("R", "cma-esr-ppmf.R"))

#' DES-CMA-ES-PPMF

des_cma_esr_ppmf <- hybrid_factory(des, cma_esr_ppmf)
