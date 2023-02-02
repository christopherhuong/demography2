Week 2 PCA Variables That Capture Structural Heterosexism
2023-01-28
Kara Joyner
Kara.Joyner@utsa.edu
Including libraries
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
Reading in the Excel file
Policy <- read_excel("C:/Users/aon632/OneDrive - University of Texas at San Antonio/Desktop/Stats 2/State Data for PC.xlsx")

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
#changing this so that it is not a tibble

Policy <- as.data.frame(Policy)

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
PCA
# all possible variables

#STATEAB    fips    State   regionname  divisionname    SO  GI  SOGI    LGBPlus SS_Union    SS_Mar  SS_Cohab    SS_Unions_Cohab abortcounties   abortwomen  biden   trump   Climate12   Climate9908 rank12  rank9908    relcons2010 HRC2021 HRC2021D    LGBTWill    ADOPTYR HATEYR  EMPLOYYR    HOUSEYR PUBYR   MARYR   BA2021  Employment  Education   Gender_Marker   Housing Anticonversion_Therapy  Hate_Crimes Public_Accommodations   School_Anti_Bullying    Trans_Healthcare

#We use the prcomp function to do the pca, we specify our variables we want in our index using a formula, the name of the data, we also specify R to z-score the data (center=T removes the mean, scale=T divides by the standard deviation for each variable), and the retx=T will calculate the PC scores for each individual for each component.

#creating alternative variable so that same valence

#alternatively we could multiple the original variable by -1

Policy$liberalrel<-100-Policy$relcons2010

#universe of variables usually examined

Policy.pc <- prcomp(Policy[,c("SO","GI","LGBPlus","LGBTWill","SS_Mar","SS_Cohab","Climate12","biden","liberalrel")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SO", "GI", "LGBPlus", "LGBTWill", 
##     "SS_Mar", "SS_Cohab", "Climate12", "biden", "liberalrel")], 
##     center = TRUE, scale. = TRUE, retx = T, rownames = Policy[, 
##         1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1    PC2     PC3     PC4    PC5     PC6     PC7
## Standard deviation     2.3971 1.1548 0.85251 0.64772 0.5572 0.47518 0.38571
## Proportion of Variance 0.6384 0.1482 0.08075 0.04662 0.0345 0.02509 0.01653
## Cumulative Proportion  0.6384 0.7866 0.86738 0.91400 0.9485 0.97358 0.99011
##                            PC8     PC9
## Standard deviation     0.22206 0.19916
## Proportion of Variance 0.00548 0.00441
## Cumulative Proportion  0.99559 1.00000
Policy.pc$rotation
##                   PC1          PC2         PC3         PC4         PC5
## SO         0.37647752 -0.158548231  0.18998797 -0.40221437  0.03214457
## GI         0.38202762 -0.124327364  0.00817053 -0.49585790  0.09886456
## LGBPlus    0.27600963  0.339712675  0.68265629 -0.02283017 -0.33482877
## LGBTWill   0.36005316  0.248601287  0.14133086  0.37330244 -0.14145921
## SS_Mar     0.34497272  0.206304156 -0.05862410  0.52071094  0.50627000
## SS_Cohab   0.07875676  0.718222753 -0.54614538 -0.26246359 -0.28900811
## Climate12  0.38635466 -0.186909012 -0.22411932 -0.06314601  0.07419176
## biden      0.38136482 -0.009879456 -0.20929382 -0.04653623  0.27968674
## liberalrel 0.29424841 -0.434465135 -0.28652899  0.32602541 -0.65841280
##                   PC6         PC7          PC8         PC9
## SO         -0.3799543  0.18772011  0.427115364 -0.52171871
## GI         -0.1973521  0.16836258 -0.334122759  0.63557042
## LGBPlus     0.1226858 -0.45634162 -0.009995033  0.07977144
## LGBTWill    0.2044134  0.74450844 -0.165026391 -0.07686292
## SS_Mar     -0.4919272 -0.24110015  0.021207164  0.08351676
## SS_Cohab   -0.1343900 -0.05768793  0.042940452 -0.06350906
## Climate12   0.2637821 -0.29264792 -0.604576567 -0.48511564
## biden       0.6319719 -0.10046029  0.532028559  0.19425224
## liberalrel -0.1755784 -0.12916057  0.166553286  0.16585798
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


#looks like there could be two components but SS cohab appears off by itself so let's see what happens when we remove it

Policy.pc <- prcomp(Policy[,c("SO","GI","LGBPlus","LGBTWill","SS_Mar","Climate12","biden","liberalrel")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SO", "GI", "LGBPlus", "LGBTWill", 
##     "SS_Mar", "Climate12", "biden", "liberalrel")], center = TRUE, 
##     scale. = TRUE, retx = T, rownames = Policy[, 1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
## Standard deviation     2.3910 0.9899 0.69062 0.59146 0.48286 0.38795 0.22569
## Proportion of Variance 0.7146 0.1225 0.05962 0.04373 0.02914 0.01881 0.00637
## Cumulative Proportion  0.7146 0.8371 0.89669 0.94042 0.96957 0.98838 0.99475
##                            PC8
## Standard deviation     0.20499
## Proportion of Variance 0.00525
## Cumulative Proportion  1.00000
#good call

Policy.pc$rotation
##                  PC1         PC2         PC3         PC4         PC5
## SO         0.3798196 -0.06257663  0.47939447 -0.09882633  0.37022356
## GI         0.3840127 -0.12616998  0.39637539 -0.29539814  0.15771984
## LGBPlus    0.2748720  0.66099770  0.34674041  0.36405780 -0.14454369
## LGBTWill   0.3584707  0.33578929 -0.29547835  0.23785641 -0.22764598
## SS_Mar     0.3434765  0.21758399 -0.59960514 -0.16629232  0.61570821
## Climate12  0.3886888 -0.27402688 -0.04750644 -0.13726528 -0.24218923
## biden      0.3816344 -0.08956571 -0.18758436 -0.37111544 -0.56852669
## liberalrel 0.2991916 -0.54775665 -0.09252052  0.72776231  0.06609526
##                    PC6         PC7         PC8
## SO         -0.19477482 -0.51812307 -0.41083007
## GI         -0.20018054  0.46662692  0.55637522
## LGBPlus     0.45382457  0.03399238  0.08219064
## LGBTWill   -0.72869340  0.12003642 -0.13279935
## SS_Mar      0.24968708 -0.01240887  0.07685986
## Climate12   0.30522796  0.48923906 -0.60129618
## biden       0.14511187 -0.49835250  0.28422412
## liberalrel  0.09205709 -0.10187993  0.22141911
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


#variation by component one improves. Let's see what happens if we remove LGBTPlus

Policy.pc <- prcomp(Policy[,c("SO","GI","LGBTWill","SS_Mar","Climate12","biden","liberalrel")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SO", "GI", "LGBTWill", "SS_Mar", "Climate12", 
##     "biden", "liberalrel")], center = TRUE, scale. = TRUE, retx = T, 
##     rownames = Policy[, 1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1     PC2     PC3     PC4     PC5     PC6     PC7
## Standard deviation     2.3094 0.82278 0.64672 0.51058 0.46422 0.22665 0.20977
## Proportion of Variance 0.7619 0.09671 0.05975 0.03724 0.03079 0.00734 0.00629
## Cumulative Proportion  0.7619 0.85860 0.91835 0.95559 0.98638 0.99371 1.00000
#good call

Policy.pc$rotation
##                  PC1         PC2          PC3         PC4         PC5
## SO         0.3916013 -0.14864168  0.491953909 -0.41130209  0.09233656
## GI         0.3996998 -0.15470348  0.494582393 -0.08696808  0.05508155
## LGBTWill   0.3598494  0.49780685 -0.184386215 -0.33064973 -0.67299198
## SS_Mar     0.3521584  0.53905571 -0.283643016 -0.09356415  0.70035681
## Climate12  0.4106829 -0.20118954 -0.057302592  0.40387882  0.05504140
## biden      0.3991398  0.07752753  0.009093797  0.68178160 -0.20462630
## liberalrel 0.3246666 -0.60752944 -0.628929000 -0.27791977 -0.01098379
##                    PC6         PC7
## SO         -0.54752712 -0.32412044
## GI          0.53886514  0.52030962
## LGBTWill    0.14022073 -0.08239650
## SS_Mar     -0.00548928  0.07527275
## Climate12   0.40360356 -0.67715228
## biden      -0.46875619  0.32888263
## liberalrel -0.08670769  0.21229617
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


#even more variation improved so let see what happens if we remove the religion variable

Policy.pc <- prcomp(Policy[,c("SO","GI","LGBTWill","SS_Mar","Climate12","biden")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SO", "GI", "LGBTWill", "SS_Mar", "Climate12", 
##     "biden")], center = TRUE, scale. = TRUE, retx = T, rownames = Policy[, 
##     1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1     PC2     PC3     PC4     PC5     PC6
## Standard deviation     2.1962 0.74796 0.53099 0.46425 0.26393 0.22360
## Proportion of Variance 0.8039 0.09324 0.04699 0.03592 0.01161 0.00833
## Cumulative Proportion  0.8039 0.89714 0.94414 0.98006 0.99167 1.00000
#good call

Policy.pc$rotation
##                 PC1         PC2         PC3         PC4         PC5         PC6
## SO        0.4120450 -0.38349177  0.52166836 -0.09583873 -0.00371497 -0.63388977
## GI        0.4217606 -0.40982274  0.24370889 -0.05209921 -0.23736987  0.73192247
## LGBTWill  0.3864741  0.52761043  0.30031045  0.66721521  0.17587870  0.07726029
## SS_Mar    0.3791061  0.59734332  0.04822082 -0.70200205 -0.05790984  0.03120716
## Climate12 0.4247614 -0.22283649 -0.47522228 -0.05249206  0.73537742  0.02345392
## biden     0.4228841 -0.01146642 -0.59170962  0.21762856 -0.60710373 -0.23447844
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


#but most studies just measure same-sex coresidential unions

Policy.pc <- prcomp(Policy[,c("SO","GI","LGBTWill","SS_Union","Climate12","biden")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SO", "GI", "LGBTWill", "SS_Union", 
##     "Climate12", "biden")], center = TRUE, scale. = TRUE, retx = T, 
##     rownames = Policy[, 1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6
## Standard deviation     2.1801 0.8079 0.53064 0.43906 0.26508 0.22266
## Proportion of Variance 0.7922 0.1088 0.04693 0.03213 0.01171 0.00826
## Cumulative Proportion  0.7922 0.9010 0.94790 0.98003 0.99174 1.00000
#slightly worse

Policy.pc$rotation
##                 PC1         PC2          PC3         PC4         PC5
## SO        0.4115617 -0.39077132  0.506215530 -0.12303733  0.02106794
## GI        0.4239403 -0.37732091  0.230486561 -0.19356753 -0.27145263
## LGBTWill  0.3928063  0.46038196  0.364393588  0.67644650  0.15823120
## SS_Union  0.3636957  0.66672655 -0.000306554 -0.64795523 -0.02913710
## Climate12 0.4268302 -0.21909838 -0.479820396 -0.02299647  0.73194886
## biden     0.4267742 -0.02113688 -0.572373470  0.26351231 -0.60351834
##                   PC6
## SO         0.63724292
## GI        -0.71667606
## LGBTWill  -0.13546965
## SS_Union   0.05006055
## Climate12 -0.05750061
## biden      0.23692295
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


# what if we combine the two policy indicators

Policy.pc <- prcomp(Policy[,c("SOGI","LGBTWill","SS_Mar","Climate12","biden")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SOGI", "LGBTWill", "SS_Mar", "Climate12", 
##     "biden")], center = TRUE, scale. = TRUE, retx = T, rownames = Policy[, 
##     1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                           PC1     PC2     PC3     PC4     PC5
## Standard deviation     2.0123 0.70100 0.46888 0.41856 0.25312
## Proportion of Variance 0.8099 0.09828 0.04397 0.03504 0.01281
## Cumulative Proportion  0.8099 0.90818 0.95215 0.98719 1.00000
#slightly better

Policy.pc$rotation
##                 PC1        PC2         PC3         PC4         PC5
## SOGI      0.4469987  0.4333049 -0.16286497  0.70511245  0.29787658
## LGBTWill  0.4307085 -0.5246153 -0.71127341 -0.03962408 -0.17829476
## SS_Mar    0.4248580 -0.5804914  0.65816404  0.21351245  0.06130195
## Climate12 0.4640905  0.4007505  0.18274430 -0.18293255 -0.74643103
## biden     0.4677586  0.1986313  0.03146026 -0.64976494  0.56441413
biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


# what if we remove Biden

Policy.pc <- prcomp(Policy[,c("SOGI","LGBTWill","SS_Mar","Climate12")], center = TRUE,scale. = TRUE,retx=T,rownames=Policy[,1])
## Warning: In prcomp.default(Policy[, c("SOGI", "LGBTWill", "SS_Mar", "Climate12")], 
##     center = TRUE, scale. = TRUE, retx = T, rownames = Policy[, 
##         1]) :
##  extra argument 'rownames' will be disregarded
summary(Policy.pc)
## Importance of components:
##                          PC1    PC2     PC3     PC4
## Standard deviation     1.788 0.6877 0.46879 0.33343
## Proportion of Variance 0.799 0.1182 0.05494 0.02779
## Cumulative Proportion  0.799 0.9173 0.97221 1.00000
#slightly worse but maybe better theoretically

Policy.pc$rotation
##                 PC1        PC2        PC3        PC4
## SOGI      0.5044807 -0.5301845  0.1361379  0.6677350
## LGBTWill  0.4929394  0.4826219  0.7113810 -0.1342534
## SS_Mar    0.4892166  0.5323787 -0.6645832  0.1885981
## Climate12 0.5130078 -0.4500594 -0.1836649 -0.7074862
#really nice how close the values are together

biplot(Policy.pc, scale = 0,xlabs=Policy[,1])


#suggests behavior could be distinct from pol


#now putting scores in separate file so that I can merge them with my original data set

scores<-data.frame(Policy.pc$x)
Policy<-cbind(Policy,scores)


#now I want a pretty map

# Get centroids

centroid_labels <- usmapdata::centroid_labels("states")

Policy$fips <- sprintf("%02d", as.numeric(as.character(Policy$fips)))

# Join data to centroids
data_labels <- merge(centroid_labels, Policy, by = "fips")

#producing map of PC1 values for States

plot_usmap(data = Policy, values = "PC1") +
  labs(title="Structural Heterosexism Index", 
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


#producing map of PC2 values for States

plot_usmap(data = Policy, values = "PC2") +
  labs(title="Structural Heterosexism Index", 
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


#this is intriguing







#now let's run some regression models


model <-lm(PC1~BA2021 + relcons2010 + trump + abortwomen, Policy)

summary(model)
## 
## Call:
## lm(formula = PC1 ~ BA2021 + relcons2010 + trump + abortwomen, 
##     data = Policy)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2299 -0.4937 -0.1052  0.4405  1.5919 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.149360   1.645115   4.954 1.07e-05 ***
## BA2021      -2.540996   2.811419  -0.904   0.3709    
## relcons2010 -0.022434   0.009260  -2.423   0.0195 *  
## trump       -0.124881   0.019629  -6.362 9.04e-08 ***
## abortwomen  -0.012655   0.005756  -2.199   0.0331 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6753 on 45 degrees of freedom
## Multiple R-squared:  0.869,  Adjusted R-squared:  0.8573 
## F-statistic: 74.61 on 4 and 45 DF,  p-value: < 2.2e-16
```