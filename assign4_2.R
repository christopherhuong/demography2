
library(tidyverse)
library(ggplot2)

load('BRFSS2020.RData')





df2 <- dat2 %>%
  select("ADDEPEV3", "_AGEG5YR", "_SEX", "_STATE",
         "ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN",
         "ACEDIVRC", "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH",
         "ACETTHEM", "ACEHVSEX",  "_TOTINDA", "MENTHLTH", "_EDUCAG", 
         "_RACE", "_BMI5CAT", "_INCOMG1"
         
         
  )



colnames(df2) <- c("DEPR", "AGE", "SEX", "STATE",
                  "ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN",
                  "ACEDIVRC", "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH",
                  "ACETTHEM", "ACEHVSEX", "PA", "MH_DAYS", "EDU",
                  "RACE", "BMI", "INCOME"
                  
)

# DEPR
# Question: (Ever told) (you had) a depressive disorder 
# (including depression, major depression, dysthymia, or minor depression)? 


# PA
# Question: Adults who reported doing physical activity or exercise 
# during the past 30 days other than their regular job 





#Input NA for coded missing

df <- df %>%
  mutate(DEPR = case_when(DEPR == 1 ~ 1, # 1 = yes depr
                          DEPR == 2 ~ 0, # 0 = no depr
                          DEPR == 7 ~ NA_real_,
                          DEPR == 9 ~ NA_real_))


df <- df %>%
  mutate(PA = case_when(PA == 1 ~ 1, # 1 = yes PA
                        PA == 2 ~ 0, # 0 = no PA
                        PA == 9 ~ NA_real_))


df[, 17][df[, 17] == 88] <- 0 #assign 88 to 0 in MH_DAYS column
df[, 17][df[, 17] == 77] <- NA
df[, 17][df[, 17] == 99] <- NA 




df <- df %>%
  mutate(EDU = case_when(EDU == 1 ~ "NO_HS",
                         EDU == 2 ~ "HS",
                         EDU == 3 ~ "ATTENDCOL",
                         EDU == 4 ~ "GRADCOL",
                         EDU == 9 ~ NA_character_))


df <- df %>%
  mutate(RACE = case_when(RACE == 1 ~ "WHITE",
                          RACE == 2 ~ "BLACK",
                          RACE == 3 ~ "NATIVE",
                          RACE == 4 ~ "ASIAN",
                          RACE == 5 ~ "ISLANDER",
                          RACE == 6 ~ "OTHER",
                          RACE == 7 ~ "MULTI",
                          RACE == 8 ~ "HISPANIC",
                          RACE == 9 ~ NA_character_))

df <- df %>%
  mutate(BMI = case_when(BMI == 1 ~ "UNDERW",
                         BMI == 2 ~ "NORMW",
                         BMI == 3 ~ "OVERW",
                         BMI == 4 ~ "OBESE"))


df <- df %>%
  mutate(INCOME = case_when(INCOME == 1 ~ "0-15K",
                            INCOME == 2 ~ "15-25K",
                            INCOME == 3 ~ "25-35K",
                            INCOME == 4 ~ "35-50K",
                            INCOME == 5 ~ "50-100K",
                            INCOME == 6 ~ "100-200K",
                            INCOME == 7 ~ "200K+",
                            INCOME == 9 ~ NA_character_))









