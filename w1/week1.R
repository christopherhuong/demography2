library(tidyverse)
library(readxl)
library(car)
library(janitor)
library(ggplot2)
library(stargazer)
library(ggpubr)
library(ggrepel)



dat <- read_excel("Junkins Data.xlsx")



dat <- dat %>%
  mutate(south = if_else(region == 3, 1L, 0L),
         northeast = if_else(region == 1, 1L, 0L),
         midwest = if_else(region == 2, 1L, 0L),
         west = if_else(region == 4, 1L, 0L))


sum(dat$south, dat$midwest, dat$northeast, dat$west)



dat <- dat %>%
  mutate(relcons2 = relcons^2,
         relcons3 = relcons^3,
         relconsln = log(relcons),
         relconsrec = relcons^-1)






# plot --------------------------------------------------------------------



plot(dat$relcons,dat$t_ageFM)

cor.test(dat$relcons,dat$t_ageFM)



# more models ------------------------------------------------------------------



mod1 <-lm(t_ageFM ~ relcons, dat)

summary(mod1)

mod2 <- lm(t_ageFM ~ relcons + relcons2, dat)

summary(mod2)

mod3 <-lm(t_ageFM~relcons + relcons2 + relcons3, dat)

summary(mod3)

mod4 <-lm(t_ageFM~relconsln, dat)

summary(mod4)

mod5 <-lm(t_ageFM~relconsrec, dat)

summary(mod5)


# model comparisons -------------------------------------------------------



BIC(mod1, mod2, mod3, mod4, mod5)
anova(mod1, mod2, mod3, mod4, mod5)


c(mod1,mod2,mod3,mod4,mod5) %>%
summary()$adj.r.squared





# mediation test ----------------------------------------------------------


summary(lm(t_ageFM~northeast, data = dat))
mod_med <-lm(t_ageFM~northeast + relcons, dat)

summary(mod_med)
# coefficient went from -0.04911 to -0.04375



# moderation test ---------------------------------------------------------

mod_moder <-lm(t_ageFM~northeast + relcons + (northeast*relcons), dat)

summary(mod_moder)

# non significant northeast:relcons interaction term



# pretty table ------------------------------------------------------------


mod_a <- lm(t_ageFM~northeast, dat)
mod_b <- lm (t_ageFM~northeast + relcons, dat)




stargazer(mod_a, mod_b,type="text", 
          column.labels = c("Model 1", "Model 2"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 





















