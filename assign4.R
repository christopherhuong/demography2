
library(haven)
library(tidyverse)


# dat <- read_xpt('LLCP2021.XPT_')
# 
# save(dat, file = "BRFSS.RData")

load('BRFSS.RData')



d <- dat %>%
  select("ADDEPEV3", "_AGEG5YR", "_SEX", "_RACE")


colnames(d) <- c("DEPR", "AGE", "SEX", "RACE")


d <- d %>%
  mutate(DEPR = case_when(DEPR == 1 ~ 1, # 1 = yes depr
                          DEPR == 2 ~ 0, # 0 = no depr
                          DEPR == 7 ~ NA_real_,
                          DEPR == 9 ~ NA_real_))



d[, 2][d[, 2] == 14] <- NA #assign 14 to NA in age column


d$SEX <- factor(d$SEX, labels = c("M", "F"))


d <- d %>%
  mutate(RACE = case_when(RACE == 1 ~ "White", 
                          RACE == 2 ~ "Black",
                          RACE == 3 ~ "Native",
                          RACE == 4 ~ "Asian",
                          RACE == 5 ~ "PI",
                          RACE == 6 ~ NA_character_,
                          RACE == 7 ~ NA_character_,
                          RACE == 8 ~ "Hispanic",
                          RACE == 9 ~ NA_character_
  ))

d$RACE <- as.factor(d$RACE)



mod <- glm(DEPR ~ RACE + SEX + AGE,
            family = "binomial",
            data = d)

summary(mod)













# part2 -------------------------------------------------------------------




df <- dat %>%
  select("ADDEPEV3", "_AGEG5YR", "_SEX", 
  "ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN",
  "ACEDIVRC", "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH",
  "ACETTHEM", "ACEHVSEX"
  
    
  )


colnames(df) <- c("DEPR", "AGE", "SEX",
  "ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN",
  "ACEDIVRC", "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH",
  "ACETTHEM", "ACEHVSEX"
  
  )


#Input NA for coded missing

df <- df %>%
  mutate(DEPR = case_when(DEPR == 1 ~ 1, # 1 = yes depr
                          DEPR == 2 ~ 0, # 0 = no depr
                          DEPR == 7 ~ NA_real_,
                          DEPR == 9 ~ NA_real_))



df[, 2][df[, 2] == 14] <- NA #assign 14 to NA in age column


df$SEX <- factor(df$SEX, labels = c("M", "F"))

df[, 4:14][df[, 4:14] == 7] <- NA
df[, 4:14][df[, 4:14] == 8] <- NA
df[, 4:14][df[, 4:14] == 9] <- NA


# ACEDEPRS - ACEDIVRC: yes=1, no=2
#convert to yes=1, no=0
df[, 4:8][df[, 4:8] == 2] <- 0 
# ACEHURT1 - ACEHVSEX: never=1, once=2, morethanonce=3
#convert to never=0, once=.5, morethanonce=1
df[, 9:14][df[, 9:14] == 1] <- 0
df[, 9:14][df[, 9:14] == 2] <- .5
df[, 9:14][df[, 9:14] == 3] <- 1


# 7 = don't know/not sure, 9 = refused. convert all to missing

df <- na.omit(df)
sum(is.na(df))
#remove all NA, 51299 rows left, 11.7% of original sample





df <- df%>%
  mutate(ACE = ACEDEPRS + ACEDRINK + ACEDRUGS + ACEPRISN +
              ACEDIVRC + ACEPUNCH + ACEHURT1 + ACESWEAR + 
              ACETOUCH + ACETTHEM + ACEHVSEX)



#total ACE score



cols <- c(4:16)
df <- df %>% mutate_at(cols, factor)


mod1 <- glm(DEPR ~ ACE, data = df)
summary(mod1)

mod1 <- glm(DEPR ~ )

















