#!/usr/bin/env Rscript
library(tidyverse)
library(magrittr)
library(cecb)
args <- commandArgs(trailingOnly = TRUE)

algs <- c(
  "csa",
  "ppmf",
#  "des",
  "csa-best-cov",
  "nl-shade-rsp"
  #  "csa-best-no-cov",
  # "ppmf-best-cov",
  # "ppmf-best-no-cov",
  # "csa-amean-cov",
  # "csa-amean-no-cov",
  # "ppmf-amean-cov",
  # "ppmf-amean-no-cov",
  # "csa-emean-cov",
  # "csa-emean-no-cov",
  # "ppmf-emean-cov",
  # "ppmf-emean-no-cov"
)

get_config <- \(algs, dim, fns) {
  list(
    idpaths = purrr::map_chr(algs, function(x) {
      paste0("./data/final/", x)
    }),
    config = list(dim = dim, probnums = fns, reps = 30)
  )
}

config <- get_config(algs, 10, c(1:5, 7:12))

pplots <- \() { 
  cecb::cec_problem_grid(
    type = "m",
    cec = 22,
    filepaths = config$idpaths,
    config = config$config
  )
}
cpplots <- \() {
  cecb::cec_class_grid(
    type = "m",
    cec = 22,
    filepaths = config$idpaths,
    dim = config$config$dim,
    rep = config$config$rep
  )
}
