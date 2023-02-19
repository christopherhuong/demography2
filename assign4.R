
library(haven)
library(tidyverse)



dat <- read_xpt('LLCP2021.XPT_')


save(dat, file = "BRFSS.RData")

load('BRFSS.RData')

