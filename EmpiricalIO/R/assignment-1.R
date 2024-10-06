
library(tidyverse)
library(evd)

set.seed(1)

df <- tibble(i = rep(1:1000, each = 2),
             k = rep(1:2, 1000),
             x = rep(0:1, 1000),
             e = revd(2000))
