
library(tidyverse)
library(ggplot2)

library(haven)
dat2 <- read_xpt('LLCP2020.XPT_')

save(dat2, file = "BRFSS2020.RData")
