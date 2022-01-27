library(tidyverse)
library(magrittr)


get_config = function(algs, dim, fns) {
  list(
    idpaths = purrr::map_chr(algs, function(x) {
      paste0("./data/cec22/", x)
    }),
    config = list(dim = dim, probnums = fns, reps = 30)
  )
}
