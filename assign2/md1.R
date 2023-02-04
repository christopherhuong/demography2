
# Week 2 PCA analysis of HRC policy variables
# 2023-01-28
# Kara Joyner
# Kara.Joyner@utsa.edu
# Including libraries
library(readxl)
library(car)
## Loading required package: carData
library(janitor)
## 
## Attaching package: 'janitor'
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
library(ggplot2)
library(stargazer)
## 
## Please cite as:
##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer
library(ggpubr)
library(ggrepel)
library(tidyverse)
## ── Attaching packages
## ───────────────────────────────────────
## tidyverse 1.3.2 ──
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ✔ purrr   1.0.1      
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ✖ dplyr::recode() masks car::recode()
## ✖ purrr::some()   masks car::some()
library(usmap)
library(FactoMineR)
# Reading in the Excel file
Policy <- read_excel("State Data for PC.xlsx")

head(Policy)
## # A tibble: 6 × 41
##   STATEAB  fips State   regio…¹ divis…²    SO    GI  SOGI LGBPlus SS_Un…³ SS_Mar
##   <chr>   <dbl> <chr>   <chr>   <chr>   <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
## 1 AL          1 Alabama 3       6       -2     -6.5 -8.5     9.15  0.0129 0.0085
## 2 AK          2 Alaska  4       9        9.75   9.5 19.2    12.1   0.0102 0.009 
## 3 AZ          4 Arizona 4       8        5.75  -3.5  2.25   11.9   0.0206 0.0147
## 4 AR          5 Arkans… 3       7       -0.5   -5   -5.5     9.11  0.0118 0.0064
## 5 CA          6 Califo… 4       9       20     21.8 41.8    12.4   0.0216 0.0155
## 6 CO          8 Colora… 4       8       20.2   21.2 41.5    13.0   0.0177 0.0119
## # … with 30 more variables: SS_Cohab <dbl>, SS_Unions_Cohab <dbl>,
## #   abortcounties <dbl>, abortwomen <dbl>, biden <dbl>, trump <dbl>,
## #   Climate12 <dbl>, Climate9908 <dbl>, rank12 <dbl>, rank9908 <dbl>,
## #   relcons2010 <dbl>, HRC2021 <chr>, HRC2021D <chr>, LGBTWill <dbl>,
## #   ADOPTYR <dbl>, HATEYR <dbl>, EMPLOYYR <dbl>, HOUSEYR <dbl>, PUBYR <dbl>,
## #   MARYR <dbl>, BA2021 <dbl>, Employment <dbl>, Education <dbl>,
## #   Gender_Marker <dbl>, Housing <dbl>, Anticonversion_Therapy <dbl>, …
#changing this so that it is not a tibble which caused me trouble

Policy <- as.data.frame(Policy)
glimpse(Policy)
head(Policy)
##   STATEAB fips      State regionname divisionname    SO    GI  SOGI LGBPlus
## 1      AL    1    Alabama          3            6 -2.00 -6.50 -8.50    9.15
## 2      AK    2     Alaska          4            9  9.75  9.50 19.25   12.06
## 3      AZ    4    Arizona          4            8  5.75 -3.50  2.25   11.89
## 4      AR    5   Arkansas          3            7 -0.50 -5.00 -5.50    9.11
## 5      CA    6 California          4            9 20.00 21.75 41.75   12.40
## 6      CO    8   Colorado          4            8 20.25 21.25 41.50   12.99
##   SS_Union SS_Mar SS_Cohab SS_Unions_Cohab abortcounties abortwomen biden trump
## 1   0.0129 0.0085   0.0565          0.4015            93         59 36.57 62.03
## 2   0.0102 0.0090   0.0179          0.2490            87         33 42.77 52.83
## 3   0.0206 0.0147   0.0527          0.3953            80         18 49.36 49.06
## 4   0.0118 0.0064   0.0527          0.5209            99         86 34.78 62.40
## 5   0.0216 0.0155   0.0593          0.3814            38          3 63.48 34.32
## 6   0.0177 0.0119   0.0537          0.4192            77         26 55.40 41.90
##   Climate12 Climate9908 rank12 rank9908 relcons2010 HRC2021 HRC2021D LGBTWill
## 1        46          44     44       47       42.76      HP     HPBE      3.1
## 2        57          56     29       25       18.75      SE    SEWIE      3.7
## 3        58          58     28       19       18.08      HP     HPBE      4.5
## 4        48          44     41       47       39.93      HP     HPBE      3.3
## 5        70          64      9        6       11.45     WIE    SEWIE      5.3
## 6        65          61     16       12       14.78     WIE    SEWIE      4.6
##   ADOPTYR HATEYR EMPLOYYR HOUSEYR PUBYR MARYR BA2021 Employment Education
## 1       4     -5       -5      -5    -5     5 0.2743          0         0
## 2       4     -5       -5      -5    -5     6 0.3279          2         1
## 3       4     17       -5      -5    -5     6 0.3243          2         0
## 4       4     -5        5       5     5     5 0.2527          0         0
## 5      16     21       28      21    15    15 0.3619          2         2
## 6       4     15       13      12    12     7 0.4442          2         2
##   Gender_Marker Housing Anticonversion_Therapy Hate_Crimes
## 1             0       0                      0           0
## 2             2       2                      0           0
## 3             1       0                      0           1
## 4             1       0                      0           0
## 5             2       2                      2           2
## 6             2       2                      2           2
##   Public_Accommodations School_Anti_Bullying Trans_Healthcare
## 1                     0                    0                0
## 2                     2                    0                1
## 3                     0                    0                0
## 4                     0                    2                0
## 5                     2                    2                2
## 6                     2                    2                2
#DC always messes up patterns

Policy <-filter(Policy, STATEAB!="DC")
# PCA
# We use the prcomp function to do the pca. We specify the variables we want in our index and the name of the data. We also specify R to z-score the data Center=T removes the mean, scale=T divides by the standard deviation for each variable), and retx=T calculates the PC scores for each individual for each component.

Policy.pc <- prcomp(Policy[,c("Employment","Education","Gender_Marker","Housing",   "Anticonversion_Therapy","Hate_Crimes","Public_Accommodations","School_Anti_Bullying","Trans_Healthcare")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("Employment", "Education", "Gender_Marker", 
##     "Housing", "Anticonversion_Therapy", "Hate_Crimes", "Public_Accommodations", 
##     "School_Anti_Bullying", "Trans_Healthcare")], center = TRUE, 
##     scale. = TRUE, retx = T, rownames = Policy[, 1]) :
##  extra argument 'rownames' will be disregarded
# Provides important summary information like how much variation is explained by each component

summary(Policy.pc)
## Importance of components:
##                           PC1     PC2     PC3     PC4     PC5     PC6    PC7
## Standard deviation     2.5004 0.88378 0.75431 0.68321 0.58870 0.50891 0.3923
## Proportion of Variance 0.6946 0.08679 0.06322 0.05186 0.03851 0.02878 0.0171
## Cumulative Proportion  0.6946 0.78143 0.84465 0.89652 0.93503 0.96380 0.9809
##                           PC8    PC9
## Standard deviation     0.3313 0.2492
## Proportion of Variance 0.0122 0.0069
## Cumulative Proportion  0.9931 1.0000
# The only components worth looking at are those that have SDs > 1

Policy.pc$rotation
##                              PC1         PC2         PC3           PC4
## Employment             0.3324435  0.53370737 -0.23575639  1.333351e-05
## Education              0.3302341 -0.25190872 -0.24323551 -4.099014e-01
## Gender_Marker          0.3031525  0.07666669  0.74927027  3.471691e-01
## Housing                0.3527371  0.46002874 -0.11592319 -8.315454e-02
## Anticonversion_Therapy 0.3515331 -0.24388655 -0.07082355  4.124903e-01
## Hate_Crimes            0.3264284 -0.16402210 -0.40821892  5.627135e-01
## Public_Accommodations  0.3643842  0.24534639  0.11919355 -3.071882e-01
## School_Anti_Bullying   0.3088723 -0.46749434 -0.07979846 -2.272973e-01
## Trans_Healthcare       0.3252582 -0.26280128  0.34376989 -2.677990e-01
##                                PC5         PC6          PC7         PC8
## Employment             -0.06928227  0.15341640 -0.009778581 -0.55784175
## Education               0.33218168 -0.65697564 -0.086435195 -0.21576043
## Gender_Marker          -0.10512689 -0.32340118 -0.258426064 -0.18456515
## Housing                -0.08608655  0.07764595  0.156714871  0.09497738
## Anticonversion_Therapy -0.04428614 -0.12504818  0.773441781  0.07891855
## Hate_Crimes             0.23569641  0.15181083 -0.512968716  0.20010071
## Public_Accommodations  -0.06332011 -0.06339816 -0.074256070  0.72325082
## School_Anti_Bullying   -0.73889818  0.19745046 -0.163699185 -0.11871954
## Trans_Healthcare        0.50882092  0.59375047  0.086193246 -0.12538147
##                                PC9
## Employment             -0.45764287
## Education               0.06572320
## Gender_Marker           0.06164547
## Housing                 0.77238408
## Anticonversion_Therapy -0.14050247
## Hate_Crimes             0.03841248
## Public_Accommodations  -0.40227247
## School_Anti_Bullying    0.04654182
## Trans_Healthcare        0.02569790
# The variables under PC1 have roughly the same loadings (eigenvectors), suggesting a single component.

biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


# More variation in the first PC than in the second

# Consistent with the loadings, the lines are roughly the same length in terms of how far it extends on the x-axis

#the closeness of the lines to each other tells you some policies are more aligned.

#where states fall on the x axis is important. Places like North Dakota are imbetween in terms of policy


#Screeplot

screeplot(Policy.pc, type = "l", main = "Scree Plot")
abline(h=1)


#these are eigenvalues; we can confirm this by using the formula for eigenvalues

Policy.pc$sdev^2
## [1] 6.25183702 0.78106829 0.56897875 0.46677220 0.34657072 0.25898694 0.15388106
## [8] 0.10978913 0.06211589
#also we can use a different function to obtain the eigenvalues

Policy.play <- PCA(Policy[,c("Employment","Education","Gender_Marker","Housing",    "Anticonversion_Therapy","Hate_Crimes","Public_Accommodations","School_Anti_Bullying","Trans_Healthcare")], scale.unit=T,graph=F)

eigenvalues<-Policy.play$eig
head(eigenvalues[,1:2])
##        eigenvalue percentage of variance
## comp 1  6.2518370              69.464856
## comp 2  0.7810683               8.678537
## comp 3  0.5689788               6.321986
## comp 4  0.4667722               5.186358
## comp 5  0.3465707               3.850786
## comp 6  0.2589869               2.877633
#then I plot the first 2 components

hist(Policy.pc$x[,1])


#almost bimodal distribution

hist(Policy.pc$x[,2])


#note the zero correlation, indicating PC1 and PC2 are orthogonal

cor(Policy.pc$x[,1],Policy.pc$x[,2])
## [1] -4.680993e-15
#now putting scores in separate file so that I can merge them with my original data set

scores<-data.frame(Policy.pc$x)
Policy<-cbind(Policy,scores)

#now imposing HRC's grouping on a biplot as in the Youtube video

ggplot(Policy, aes(PC1, PC2, col = Species, fill = HRC2021)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
## Too few points to calculate an ellipse
## Warning in MASS::cov.trob(data[, vars]): Probable convergence failure


ggplot(Policy, aes(PC1, PC2, col = Species, fill = HRC2021D)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")


#can explore better groupings next week

#correlations of original variables with PC scores

round(cor(Policy[,c("Employment","Education","Gender_Marker","Housing", "Anticonversion_Therapy","Hate_Crimes","Public_Accommodations","School_Anti_Bullying","Trans_Healthcare","PC1", "PC2")]), 3)
##                        Employment Education Gender_Marker Housing
## Employment                  1.000     0.591         0.561   0.918
## Education                   0.591     1.000         0.492   0.645
## Gender_Marker               0.561     0.492         1.000   0.625
## Housing                     0.918     0.645         0.625   1.000
## Anticonversion_Therapy      0.633     0.708         0.667   0.688
## Hate_Crimes                 0.653     0.659         0.521   0.654
## Public_Accommodations       0.810     0.732         0.701   0.883
## School_Anti_Bullying        0.490     0.671         0.506   0.550
## Trans_Healthcare            0.539     0.686         0.635   0.609
## PC1                         0.831     0.826         0.758   0.882
## PC2                         0.472    -0.223         0.068   0.407
##                        Anticonversion_Therapy Hate_Crimes Public_Accommodations
## Employment                              0.633       0.653                 0.810
## Education                               0.708       0.659                 0.732
## Gender_Marker                           0.667       0.521                 0.701
## Housing                                 0.688       0.654                 0.883
## Anticonversion_Therapy                  1.000       0.805                 0.694
## Hate_Crimes                             0.805       1.000                 0.617
## Public_Accommodations                   0.694       0.617                 1.000
## School_Anti_Bullying                    0.711       0.607                 0.645
## Trans_Healthcare                        0.681       0.603                 0.720
## PC1                                     0.879       0.816                 0.911
## PC2                                    -0.216      -0.145                 0.217
##                        School_Anti_Bullying Trans_Healthcare   PC1    PC2
## Employment                            0.490            0.539 0.831  0.472
## Education                             0.671            0.686 0.826 -0.223
## Gender_Marker                         0.506            0.635 0.758  0.068
## Housing                               0.550            0.609 0.882  0.407
## Anticonversion_Therapy                0.711            0.681 0.879 -0.216
## Hate_Crimes                           0.607            0.603 0.816 -0.145
## Public_Accommodations                 0.645            0.720 0.911  0.217
## School_Anti_Bullying                  1.000            0.636 0.772 -0.413
## Trans_Healthcare                      0.636            1.000 0.813 -0.232
## PC1                                   0.772            0.813 1.000  0.000
## PC2                                  -0.413           -0.232 0.000  1.000
#now I want to see this in a map

# Get centroids

centroid_labels <- usmapdata::centroid_labels("states")

Policy$fips <- sprintf("%02d", as.numeric(as.character(Policy$fips)))

# Join data to centroids
data_labels <- merge(centroid_labels, Policy, by = "fips")

#producing map of PC1 values for States

plot_usmap(data = Policy, values = "PC1") +
  labs(title="Policy Index", 
       subtitle="PC1", 
       caption = "Source: Wendy 2021 Data") +
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "white", high ="darkred", 
                        name = "Value on Index",label = scales::comma) + 
  theme(legend.position = "right") +
  guides(fill = "none") +
  geom_text(data = data_labels, ggplot2::aes(
    x = x, y = y,
    label = scales::number(PC1, scale = 1, accuracy = .1)), color = "black")


#producing map of values for Index created by MAP for the same time period

plot_usmap(data = Policy, values = "SOGI") +
  labs(title="Policy Index Based on MAP Data", 
       subtitle="SOGI Index", 
       caption = "Source: Wendy 2021 Data") +
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "white", high ="darkred", 
                        name = "Value on Index",label = scales::comma) + 
  theme(legend.position = "right") +
  guides(fill = "none") +
  geom_text(data = data_labels, ggplot2::aes(
    x = x, y = y,
    label = scales::number(SOGI, scale = 1, accuracy = .1)), color = "black")


#now let's see correlation with MAP Index

ggplot(data =Policy, aes(x = PC1, y = SOGI)) + geom_point() +
  stat_smooth(method = "lm", size = 1) +
  stat_regline_equation(aes(label = ..rr.label..)) +
  geom_text_repel(aes(PC1, SOGI, label = STATEAB), size = 6)+guides(color=F)+
  theme_bw(base_size = 18) +
  labs(title="Correlation between Two Different Indexes", 
       subtitle="2020 Period", 
       caption="Source: Wendy Data") +  
  xlab(label="PC1 Index Based on HRC Data") +
  ylab(label="SOGI Index Based on MAP Data") 
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
## of ggplot2 3.3.4.
## Warning: The dot-dot notation (`..rr.label..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(rr.label)` instead.
## `geom_smooth()` using formula = 'y ~ x'
## Warning: ggrepel: 14 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps


#out of curiosity PC2

#producing map of PC2 values for States

#once again it does not make much sense

plot_usmap(data = Policy, values = "PC2") +
  labs(title="Policy Index", 
       subtitle="PC2", 
       caption = "Source: Wendy 2021 Data") +
  theme(panel.background = element_rect(colour = "black"))+
  scale_fill_continuous(low = "white", high ="darkred", 
                        name = "Value on Index",label = scales::comma) + 
  theme(legend.position = "right") +
  guides(fill = "none") +
  geom_text(data = data_labels, ggplot2::aes(
    x = x, y = y,
    label = scales::number(PC2, scale = 1, accuracy = .1)), color = "black")


#looking at states highest in terms of employment

#head(Policy[order(-Policy$Employment),])

```

