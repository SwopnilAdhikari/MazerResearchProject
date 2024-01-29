---
title: "DataFromOtherCodeAC"
output: html_document
date: "2023-11-17"
---

```r
library(Matrix)
library(dplyr)
library(ggplot2)
library(jtools)
library(lme4)
library(lmerTest)
library(lubridate)
library(readr)
library(tidyverse)
library(visreg)

AC_Data_2022 <- read_csv("~/MazerResearchProject/Data/AC_Data_2022.csv")
```

```
## Rows: 319 Columns: 23
## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (6): Population, Generation, Donor, Recipient, FFD, LFD
## dbl (16): Field_Year, Block, Transect, Sequence, Plant_ID, Total_Closed_Fruits, Total_Fruits, Tot_Seed_Num_ClosedFt, Mean_Ind_Seed_M...
## lgl  (1): Left_Or_Right
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
AC_Data_2022 <- AC_Data_2022 %>% mutate(FFD = yday(mdy(FFD)), LFD = yday(mdy(LFD)), Flowering_Duration = LFD-FFD) 

AC_Data_2022$Flowering_Duration - AC_Data_2022$fl_duration
```

```
##   [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [44]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [87]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [130]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [173]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [216]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [259]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [302]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
```

```r
min(AC_Data_2022$Total_Fruits, na.rm=TRUE) 
```

```
## [1] 0
```

```r
min(AC_Data_2022$Mean_Seeds_per_Fruit, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(AC_Data_2022$Lifetime_Fecundity, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(AC_Data_2022$Stem_Biomass, na.rm=TRUE)
```

```
## [1] 1.35
```

```r
min(AC_Data_2022$Leaf_Area_mm2, na.rm=TRUE)
```

```
## [1] 4.585
```

```r
AC_Data_2022 <- AC_Data_2022 %>% mutate(Log_Total_Fruits=log(Total_Fruits+1), Log_Mean_Seeds_per_Fruit=log(Mean_Seeds_per_Fruit+1), Log_Lifetime_Fecundity=log(Lifetime_Fecundity+1), Log_Stem_Biomass=log(Stem_Biomass), Log_Leaf_Area_mm2=log(Leaf_Area_mm2))

ACTransect1 <- subset(AC_Data_2022, Transect == 1)
ACTransect2 <- subset(AC_Data_2022, Transect == 2)
ACTransect3 <- subset(AC_Data_2022, Transect == 3)
ACTransect4 <- subset(AC_Data_2022, Transect == 4)
ACTransect5 <- subset(AC_Data_2022, Transect == 5)
ACTransect6 <- subset(AC_Data_2022, Transect == 6)
ACTransect7 <- subset(AC_Data_2022, Transect == 7)
ACTransect8 <- subset(AC_Data_2022, Transect == 8)
ACTransect9 <- subset(AC_Data_2022, Transect == 9)


center_scale <- function(x) {
  scale(x, scale = FALSE)
}
```


```r
PopulationV1 <- ACTransect1$Population # Creates a vector of Population as charACter variables
Field_YearV1 <- ACTransect1$Field_Year
GenerationV1 <- ACTransect1$Generation
BlockV1 <- ACTransect1$Block
TransectV1 <- ACTransect1$Transect
SequenceV1 <- ACTransect1$Sequence
DonorV1 <- ACTransect1$Donor
RecipientV1 <- ACTransect1$Recipient
FFDV1 <- ACTransect1$FFD
LFDV1 <- ACTransect1$LFD

# All of these vectors include all of the values in the original data set (AC_Data_2022)

# Remove values of these variables in ACTransect1 (the data frame that contains the variables for which we need to get rid of the outliers)

ACTransect1$Population <-NULL
ACTransect1$Field_Year <- NULL
ACTransect1$Generation <- NULL
ACTransect1$Block <- NULL
ACTransect1$Transect <- NULL
ACTransect1$Sequence <- NULL
ACTransect1$Donor <- NULL
ACTransect1$Recipient <- NULL
ACTransect1$FFD <- NULL
ACTransect1$LFD <- NULL


# z-scores are calculated and the for loop goes tACough eACh element of the z-score array (eACh row and column), looking for values above 3, which implies that the value is an outlier.

# The index (row i, column j) will be removed from the original ACTransect1 data frame.

z_scores <- as.data.frame(sapply(ACTransect1, function(ACTransect1) (abs(ACTransect1-mean(ACTransect1, na.rm = TRUE))/sd(ACTransect1, na.rm = TRUE))))

class(z_scores) # Shows that z_scores is a data_frame
```

```
## [1] "data.frame"
```

```r
View(z_scores) # EACh value is a z_score

# The for loop is as follows.  For eACh index (row=i, column=j) in the z_score data frame that is > 3, it replACes the value for that index in Transect1 with an "NA"

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect1[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect1[i,j] = NA
      }
    }
  }

ACTransect1 <- ACTransect1 %>% mutate(Population = PopulationV1 , Field_Year = Field_YearV1, Generation = GenerationV1, Block = BlockV1, Transect = TransectV1, Sequence = SequenceV1, Donor = DonorV1, Recipient = RecipientV1, FFD = FFDV1, LFD = LFDV1, .before = Left_Or_Right, )

print(ACTransect1$Sequence)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
```

```r
################################################################################################
PopulationV2 <- ACTransect2$Population # Creates a vector of Population as charACter variables
Field_YearV2 <- ACTransect2$Field_Year
GenerationV2 <- ACTransect2$Generation
BlockV2 <- ACTransect2$Block
TransectV2 <- ACTransect2$Transect
SequenceV2 <- ACTransect2$Sequence
DonorV2 <- ACTransect2$Donor
RecipientV2 <- ACTransect2$Recipient
FFDV2 <- ACTransect2$FFD
LFDV2 <- ACTransect2$LFD


# Remove values of these variables in ACTransect2 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect2$Population <-NULL
ACTransect2$Field_Year <- NULL
ACTransect2$Generation <- NULL
ACTransect2$Block <- NULL
ACTransect2$Transect <- NULL
ACTransect2$Sequence <- NULL
ACTransect2$Donor <- NULL
ACTransect2$Recipient <- NULL
ACTransect2$FFD <- NULL
ACTransect2$LFD <- NULL


# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original ACTransect2 data frame
z_scores <- as.data.frame(sapply(ACTransect2, function(ACTransect2) (abs(ACTransect2-mean(ACTransect2, na.rm = TRUE))/sd(ACTransect2, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect2[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect2[i,j] = NA
      }
    }
  }

ACTransect2 <- ACTransect2 %>% mutate(Population = PopulationV2 , Field_Year = Field_YearV2, Generation = GenerationV2, Block = BlockV2, Transect = TransectV2, Sequence = SequenceV2, Donor = DonorV2, Recipient = RecipientV2, FFD = FFDV2, LFD = LFDV2, .before = Left_Or_Right)


View(ACTransect2)


PopulationV3 <- ACTransect3$Population # Creates a vector of Population as charACter variables
Field_YearV3 <- ACTransect3$Field_Year
GenerationV3 <- ACTransect3$Generation
BlockV3 <- ACTransect3$Block
TransectV3 <- ACTransect3$Transect
SequenceV3 <- ACTransect3$Sequence
DonorV3 <- ACTransect3$Donor
RecipientV3 <- ACTransect3$Recipient
FFDV3 <- ACTransect3$FFD
LFDV3 <- ACTransect3$LFD


# Remove values of these variables in ACTransect3 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect3$Population <-NULL
ACTransect3$Field_Year <- NULL
ACTransect3$Generation <- NULL
ACTransect3$Block <- NULL
ACTransect3$Transect <- NULL
ACTransect3$Sequence <- NULL
ACTransect3$Donor <- NULL
ACTransect3$Recipient <- NULL
ACTransect3$FFD <- NULL
ACTransect3$LFD <- NULL



# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original ACTransect3 data frame
z_scores <- as.data.frame(sapply(ACTransect3, function(ACTransect3) (abs(ACTransect3-mean(ACTransect3, na.rm = TRUE))/sd(ACTransect3, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect3[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect3[i,j] = NA
      }
    }
  }

ACTransect3 <- ACTransect3 %>% mutate(Population = PopulationV3 , Field_Year = Field_YearV3, Generation = GenerationV3, Block = BlockV3, Transect = TransectV3, Sequence = SequenceV3, Donor = DonorV3, Recipient = RecipientV3, FFD = FFDV3, LFD = LFDV3, .before = Left_Or_Right)


names(ACTransect3)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV4 <- ACTransect4$Population # Creates a vector of Population as charACter variables
Field_YearV4 <- ACTransect4$Field_Year
GenerationV4 <- ACTransect4$Generation
BlockV4 <- ACTransect4$Block
TransectV4 <- ACTransect4$Transect
SequenceV4 <- ACTransect4$Sequence
DonorV4 <- ACTransect4$Donor
RecipientV4 <- ACTransect4$Recipient
FFDV4 <- ACTransect4$FFD
LFDV4 <- ACTransect4$LFD


# Remove values of these variables in ACTransect4 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect4$Population <-NULL
ACTransect4$Field_Year <- NULL
ACTransect4$Generation <- NULL
ACTransect4$Block <- NULL
ACTransect4$Transect <- NULL
ACTransect4$Sequence <- NULL
ACTransect4$Donor <- NULL
ACTransect4$Recipient <- NULL
ACTransect4$FFD <- NULL
ACTransect4$LFD <- NULL


# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 4 which implies the value is an outlier and removes that index from the original ACTransect4 data frame
z_scores <- as.data.frame(sapply(ACTransect4, function(ACTransect4) (abs(ACTransect4-mean(ACTransect4, na.rm = TRUE))/sd(ACTransect4, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect4[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect4[i,j] = NA
      }
    }
  }

ACTransect4 <- ACTransect4 %>% mutate(Population = PopulationV4 , Field_Year = Field_YearV4, Generation = GenerationV4, Block = BlockV4, Transect = TransectV4, Sequence = SequenceV4, Donor = DonorV4, Recipient = RecipientV4, FFD = FFDV4, LFD = LFDV4, .before = Left_Or_Right)

names(ACTransect4)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV5 <- ACTransect5$Population # Creates a vector of Population as charACter variables
Field_YearV5 <- ACTransect5$Field_Year
GenerationV5 <- ACTransect5$Generation
BlockV5 <- ACTransect5$Block
TransectV5 <- ACTransect5$Transect
SequenceV5 <- ACTransect5$Sequence
DonorV5 <- ACTransect5$Donor
RecipientV5 <- ACTransect5$Recipient
FFDV5 <- ACTransect5$FFD
LFDV5 <- ACTransect5$LFD


# Remove values of these variables in ACTransect5 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect5$Population <-NULL
ACTransect5$Field_Year <- NULL
ACTransect5$Generation <- NULL
ACTransect5$Block <- NULL
ACTransect5$Transect <- NULL
ACTransect5$Sequence <- NULL
ACTransect5$Donor <- NULL
ACTransect5$Recipient <- NULL
ACTransect5$FFD <- NULL
ACTransect5$LFD <- NULL


# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 5 which implies the value is an outlier and removes that index from the original ACTransect5 data frame
z_scores <- as.data.frame(sapply(ACTransect5, function(ACTransect5) (abs(ACTransect5-mean(ACTransect5, na.rm = TRUE))/sd(ACTransect5, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect5[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect5[i,j] = NA
      }
    }
  }

ACTransect5 <- ACTransect5 %>% mutate(Population = PopulationV5 , Field_Year = Field_YearV5, Generation = GenerationV5, Block = BlockV5, Transect = TransectV5, Sequence = SequenceV5, Donor = DonorV5, Recipient = RecipientV5,FFD = FFDV5, LFD = LFDV5, .before = Left_Or_Right)

names(ACTransect5)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV6 <- ACTransect6$Population # Creates a vector of Population as charACter variables
Field_YearV6 <- ACTransect6$Field_Year
GenerationV6 <- ACTransect6$Generation
BlockV6 <- ACTransect6$Block
TransectV6 <- ACTransect6$Transect
SequenceV6 <- ACTransect6$Sequence
DonorV6 <- ACTransect6$Donor
RecipientV6 <- ACTransect6$Recipient
FFDV6 <- ACTransect6$FFD
LFDV6 <- ACTransect6$LFD


# Remove values of these variables in ACTransect6 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect6$Population <-NULL
ACTransect6$Field_Year <- NULL
ACTransect6$Generation <- NULL
ACTransect6$Block <- NULL
ACTransect6$Transect <- NULL
ACTransect6$Sequence <- NULL
ACTransect6$Donor <- NULL
ACTransect6$Recipient <- NULL
ACTransect6$FFD <- NULL
ACTransect6$LFD <- NULL


# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 6 which implies the value is an outlier and removes that index from the original ACTransect6 data frame
z_scores <- as.data.frame(sapply(ACTransect6, function(ACTransect6) (abs(ACTransect6-mean(ACTransect6, na.rm = TRUE))/sd(ACTransect6, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect6[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect6[i,j] = NA
      }
    }
  }

ACTransect6 <- ACTransect6 %>% mutate(Population = PopulationV6 , Field_Year = Field_YearV6, Generation = GenerationV6, Block = BlockV6, Transect = TransectV6, Sequence = SequenceV6, Donor = DonorV6, Recipient = RecipientV6, FFD = FFDV6, LFD = LFDV6, .before = Left_Or_Right)


names(ACTransect6)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV7 <- ACTransect7$Population # Creates a vector of Population as charACter variables
Field_YearV7 <- ACTransect7$Field_Year
GenerationV7 <- ACTransect7$Generation
BlockV7 <- ACTransect7$Block
TransectV7 <- ACTransect7$Transect
SequenceV7 <- ACTransect7$Sequence
DonorV7 <- ACTransect7$Donor
RecipientV7 <- ACTransect7$Recipient
FFDV7 <- ACTransect7$FFD
LFDV7 <- ACTransect7$LFD


# Remove values of these variables in ACTransect7 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect7$Population <-NULL
ACTransect7$Field_Year <- NULL
ACTransect7$Generation <- NULL
ACTransect7$Block <- NULL
ACTransect7$Transect <- NULL
ACTransect7$Sequence <- NULL
ACTransect7$Donor <- NULL
ACTransect7$Recipient <- NULL
ACTransect7$FFD <- NULL
ACTransect7$LFD <- NULL

# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 7 which implies the value is an outlier and removes that index from the original ACTransect1 data frame
z_scores <- as.data.frame(sapply(ACTransect7, function(ACTransect7) (abs(ACTransect7-mean(ACTransect7, na.rm = TRUE))/sd(ACTransect7, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect7[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect7[i,j] = NA
      }
    }
  }

ACTransect7 <- ACTransect7 %>% mutate(Population = PopulationV7 , Field_Year = Field_YearV7, Generation = GenerationV7, Block = BlockV7, Transect = TransectV7, Sequence = SequenceV7, Donor = DonorV7, Recipient = RecipientV7, FFD = FFDV7, LFD = LFDV7, .before = Left_Or_Right)


names(ACTransect7)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV8 <- ACTransect8$Population # Creates a vector of Population as charACter variables
Field_YearV8 <- ACTransect8$Field_Year
GenerationV8 <- ACTransect8$Generation
BlockV8 <- ACTransect8$Block
TransectV8 <- ACTransect8$Transect
SequenceV8 <- ACTransect8$Sequence
DonorV8 <- ACTransect8$Donor
RecipientV8 <- ACTransect8$Recipient
FFDV8 <- ACTransect8$FFD
LFDV8 <- ACTransect8$LFD


# Remove values of these variables in ACTransect8 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect8$Population <-NULL
ACTransect8$Field_Year <- NULL
ACTransect8$Generation <- NULL
ACTransect8$Block <- NULL
ACTransect8$Transect <- NULL
ACTransect8$Sequence <- NULL
ACTransect8$Donor <- NULL
ACTransect8$Recipient <- NULL
ACTransect8$FFD <- NULL
ACTransect8$LFD <- NULL



# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 8 which implies the value is an outlier and removes that index from the original ACTransect data frame
z_scores <- as.data.frame(sapply(ACTransect8, function(ACTransect8) (abs(ACTransect8-mean(ACTransect8, na.rm = TRUE))/sd(ACTransect8, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect8[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect8[i,j] = NA
      }
    }
  }

ACTransect8 <- ACTransect8 %>% mutate(Population = PopulationV8 , Field_Year = Field_YearV8, Generation = GenerationV8, Block = BlockV8, Transect = TransectV8, Sequence = SequenceV8, Donor = DonorV8, Recipient = RecipientV8, FFD = FFDV8, LFD = LFDV8, .before = Left_Or_Right)


names(ACTransect8)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
PopulationV9 <- ACTransect9$Population # Creates a vector of Population as charACter variables
Field_YearV9 <- ACTransect9$Field_Year
GenerationV9 <- ACTransect9$Generation
BlockV9 <- ACTransect9$Block
TransectV9 <- ACTransect9$Transect
SequenceV9 <- ACTransect9$Sequence
DonorV9 <- ACTransect9$Donor
RecipientV9 <- ACTransect9$Recipient
FFDV9 <- ACTransect9$FFD
LFDV9 <- ACTransect9$LFD


# Remove values of these variables in ACTransect9 (the data frame that contains the variables for which we want to get rid of the outliers)
ACTransect9$Population <-NULL
ACTransect9$Field_Year <- NULL
ACTransect9$Generation <- NULL
ACTransect9$Block <- NULL
ACTransect9$Transect <- NULL
ACTransect9$Sequence <- NULL
ACTransect9$Donor <- NULL
ACTransect9$Recipient <- NULL
ACTransect9$FFD <- NULL
ACTransect9$LFD <- NULL



# z-scores are calculated and the for loop goes tACough eACh element of the z-score array, looking for values above 9 which implies the value is an outlier and removes that index from the original ACTransect9 data frame
z_scores <- as.data.frame(sapply(ACTransect9, function(ACTransect9) (abs(ACTransect9-mean(ACTransect9, na.rm = TRUE))/sd(ACTransect9, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      ACTransect9[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      ACTransect9[i,j] = NA
      }
    }
  }

ACTransect9 <- ACTransect9 %>% mutate(Population = PopulationV9 , Field_Year = Field_YearV9, Generation = GenerationV9, Block = BlockV9, Transect = TransectV9, Sequence = SequenceV9, Donor = DonorV9, Recipient = RecipientV9, FFD = FFDV9, LFD = LFDV9, .before = Left_Or_Right)


names(ACTransect9)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```


```r
# Mean-centering the data frame for Transect 1

names(ACTransect1)
```

```
##  [1] "Population"               "Field_Year"               "Generation"               "Block"                   
##  [5] "Transect"                 "Sequence"                 "Donor"                    "Recipient"               
##  [9] "FFD"                      "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"    "Mean_Ind_Seed_Mass_mg"   
## [17] "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"       "Stem_Biomass"             "Corolla_Diameter"        
## [21] "Corolla_Area"             "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"   "Log_Stem_Biomass"        
## [29] "Log_Leaf_Area_mm2"
```

```r
# MC = mean-centered

ACTransect1 <- ACTransect1 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

View(ACTransect1)

# Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 6.914286
```

```r
min(ACTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 28
```

```r
mean(ACTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -3.043185e-16
```

```r
min(ACTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -6.914286
```

```r
max(ACTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 21.08571
```

```r
 ACTransect2 <- ACTransect2 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

 # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 7.828571
```

```r
min(ACTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 24
```

```r
mean(ACTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 1.779083e-16
```

```r
min(ACTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -7.828571
```

```r
max(ACTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 16.17143
```

```r
 ACTransect3 <- ACTransect3 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))
 
  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 19.52941
```

```r
min(ACTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 34
```

```r
mean(ACTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -1.671483e-15
```

```r
min(ACTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -19.52941
```

```r
max(ACTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 14.47059
```

```r
ACTransect4 <- ACTransect4 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 24.65714
```

```r
min(ACTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 45
```

```r
mean(ACTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -1.21728e-15
```

```r
min(ACTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -24.65714
```

```r
max(ACTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 20.34286
```

```r
ACTransect5 <- ACTransect5 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 23.02778
```

```r
min(ACTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 34
```

```r
mean(ACTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -7.886728e-16
```

```r
min(ACTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -23.02778
```

```r
max(ACTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 10.97222
```

```r
ACTransect6 <- ACTransect6 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 253.6636
```

```r
min(ACTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 153.16
```

```r
max(ACTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 375.646
```

```r
mean(ACTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -2.032476e-15
```

```r
min(ACTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -100.5036
```

```r
max(ACTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 121.9824
```

```r
ACTransect7 <- ACTransect7 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 288.275
```

```r
min(ACTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 223.047
```

```r
max(ACTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 420.859
```

```r
mean(ACTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 1.894847e-14
```

```r
min(ACTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -65.228
```

```r
max(ACTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 132.584
```

```r
ACTransect8 <- ACTransect8 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

  # Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(ACTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 20.00344
```

```r
min(ACTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 16.2
```

```r
max(ACTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 23.918
```

```r
mean(ACTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] 9.210097e-16
```

```r
min(ACTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] -3.803444
```

```r
max(ACTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] 3.914556
```

```r
ACTransect9 <- ACTransect9 %>% mutate(
  Total_Closed_Fruits_MC = center_scale(Total_Closed_Fruits),
  Log_Total_Fruits_MC = center_scale(Log_Total_Fruits),
  Mean_Ind_Seed_Mass_mg_MC = center_scale(Mean_Ind_Seed_Mass_mg),
  Log_Mean_Seeds_per_Fruit_MC = center_scale(Log_Mean_Seeds_per_Fruit),
  Log_Lifetime_Fecundity_MC = center_scale(Log_Lifetime_Fecundity),
  Log_Stem_Biomass_MC = center_scale(Log_Stem_Biomass),
  Corolla_Diameter_MC = center_scale(Corolla_Diameter),
  Corolla_Area_MC = center_scale(Corolla_Area),
  Log_Leaf_Area_mm2_MC = center_scale(Log_Leaf_Area_mm2),
  FFD_MC = center_scale(FFD),
  LFD_MC = center_scale(LFD),
  Flowering_Duration_MC = center_scale(Flowering_Duration))

View(ACTransect9)

# Let's take a look at the mean, minimum and maximum of the number of closed fruits for both the raw values and the mean-centered values, for Transect 9.


mean(ACTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 5.823529
```

```r
min(ACTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(ACTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 28
```

```r
mean(ACTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 2.351571e-16
```

```r
min(ACTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -5.823529
```

```r
max(ACTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 10.17647
```


```r
Recipients <- c(ACTransect1$Recipient, ACTransect2$Recipient, ACTransect3$Recipient, ACTransect4$Recipient, ACTransect5$Recipient, ACTransect6$Recipient, ACTransect7$Recipient, ACTransect8$Recipient, ACTransect9$Recipient)

# Now, modify this vector so that eACh recipient occurs only once

Recipients <- unique(Recipients) # Names of maternal ID's, without repetition (n=107)

str(Recipients)
```

```
##  chr [1:107] "AC_200" "AC_191" "AC_107" "AC_109" "AC_110" "AC_048" "AC_190" "AC_074" "AC_130" "AC_070" "AC_181" "AC_046" "AC_131" ...
```

```r
length(Recipients)
```

```
## [1] 107
```

```r
# Create a vector that includes ONLY the variables that we want in the final unified dataframe that contains the data for all of the transects.  We're going to include the raw, untransformed values for eACh trait and the mean-centered values (some of which are based on log-transformed values)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "FFD_MC", "LFD_MC", "Flowering_Duration_MC")

# Combine transects to get mean-centered population data for easy averaging of same maternal IDs

AC_MeanCentered_AllTransects <- rbind(ACTransect1,ACTransect2,ACTransect3,ACTransect4,ACTransect5,ACTransect6,ACTransect7,ACTransect8, ACTransect9) # This contains 41 variables

AC_MeanCentered_AllTransects <- AC_MeanCentered_AllTransects[Variables] # Includes only the variables in the "Variables" vector = 35 variables

# Now, let's summarize the data, using the means of the rows representing a given recipient (= Maternal ID) and averaging eACh maternal ID's values while ignoring NA values. For recipients for which a trait has  values that are ALL NA, this will return a "NaN" for that recipient and trait.

# this code creates a new data frame with the MEAN VALUES for EACH RECIPIENT and TRAIT

# In other words, these are the means for the mean-centered trait values of eACh maternal genotype

AC_Avg_MC_Population_ByRecip <- AC_MeanCentered_AllTransects %>%
  group_by(Recipient) %>% #uses the list of unique recipients, without repeated values
  summarise(AMC_Log_Stem_Biomass = mean(Log_Stem_Biomass_MC, na.rm=TRUE), 
            AMC_Corolla_Diameter = mean(Corolla_Diameter_MC, na.rm=TRUE), 
            AMC_Corolla_Area = mean(Corolla_Area_MC, na.rm=TRUE), 
            AMC_Log_Lifetime_Fecundity = mean(Log_Lifetime_Fecundity_MC, na.rm=TRUE), 
            AMC_Log_Total_Fruits = mean(Log_Total_Fruits_MC, na.rm=TRUE), 
            AMC_Log_Mean_Seeds_per_Fruit = mean(Log_Mean_Seeds_per_Fruit_MC, na.rm=TRUE),
            AMC_Mean_Ind_Seed_Mass_mg = mean(Mean_Ind_Seed_Mass_mg_MC, na.rm=TRUE),
            AMC_Log_Leaf_Area_mm2 = mean(Log_Leaf_Area_mm2_MC, na.rm=TRUE),
            AMC_FFD = mean(FFD_MC, na.rm=TRUE), 
            AMC_LFD = mean(LFD_MC, na.rm=TRUE),
            AMC_Flowering_Duration = mean(Flowering_Duration_MC, na.rm=TRUE))

View(AC_Avg_MC_Population_ByRecip)

Donors <- c(ACTransect1$Donor, ACTransect2$Donor, ACTransect3$Donor, ACTransect4$Donor, ACTransect5$Donor, ACTransect6$Donor, ACTransect7$Donor, ACTransect8$Donor, ACTransect9$Donor)

Donors <- unique(Donors) # Names of Paternal ID's, without repetition (n=40)

str(Donors)
```

```
##  chr [1:40] "AC_150" "AC_102" "AC_177" "AC_094" "AC_186" "AC_015" "AC_096" "AC_107" "AC_207" "AC_054" "AC_052" "AC_064" "AC_068" ...
```

```r
length(Donors)
```

```
## [1] 40
```

```r
# Create a vector that includes only the variables that we want in the final unified dataframe that contains the data for all of the transects.  We're going to include the raw, untransformed values for eACh trait and the mean-centered values (some of which are based on log-transformed values)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "Flowering_Duration_MC")

# Combine transects bACk together to get mean-centered population data for easy averaging of same paternal IDs

AC_MC_Population <- rbind(ACTransect1,ACTransect2,ACTransect3,ACTransect4,ACTransect5,ACTransect6,ACTransect7,ACTransect8, ACTransect9)

AC_MC_Population <- AC_MC_Population[Variables] # Includes only the variables in the "Variables" vector.

# This summarizes the data, using the means of the rows representing a given recipient (= Maternal ID) and averaging eACh maternal ID's values while ignoring NA values. For recipients for which a trait has  values that are ALL NA, this will return a "NaN" for that recipient and trait.

# this code creates a new data frame with the MEAN VALUES for EACH RECIPIENT and TRAIT

# In other words, these are the means for the mean-centered trait values of eACh maternal genotype

AC_Avg_MC_Population_ByDonor <- AC_MC_Population %>%
  group_by(Donor) %>% #uses the list of unique recipients, without repeated values
  summarise(AMC_Log_Stem_Biomass = mean(Log_Stem_Biomass_MC, na.rm=TRUE), 
            AMC_Corolla_Diameter = mean(Corolla_Diameter_MC, na.rm=TRUE), 
            AMC_Corolla_Area = mean(Corolla_Area_MC, na.rm=TRUE), 
            AMC_Log_Lifetime_Fecundity = mean(Log_Lifetime_Fecundity_MC, na.rm=TRUE), 
            AMC_Log_Total_Fruits = mean(Log_Total_Fruits_MC, na.rm=TRUE), 
            AMC_Log_Mean_Seeds_per_Fruit = mean(Log_Mean_Seeds_per_Fruit_MC, na.rm=TRUE),
            AMC_Mean_Ind_Seed_Mass_mg = mean(Mean_Ind_Seed_Mass_mg_MC, na.rm=TRUE),
            AMC_Log_Leaf_Area_mm2 = mean(Log_Leaf_Area_mm2_MC, na.rm=TRUE),
            Mean_FFD = mean(FFD, na.rm=TRUE), #not mean-centered
            Mean_LFD = mean(LFD, na.rm=TRUE), #not mean-centered
            AMC_Flowering_Duration = mean(Flowering_Duration_MC, na.rm=TRUE))

View(AC_Avg_MC_Population_ByDonor)
```


```r
# Deal with NaN values, replacing NaN with "NA"

for(i in 1:nrow(AC_Avg_MC_Population_ByRecip)){
  for(j in 1:ncol(AC_Avg_MC_Population_ByRecip)){
    if(AC_Avg_MC_Population_ByRecip[i,j] == "NaN"){
      AC_Avg_MC_Population_ByRecip[i,j] <- NA
    }
  }
}

for(i in 1:nrow(AC_Avg_MC_Population_ByDonor)){
  for(j in 1:ncol(AC_Avg_MC_Population_ByDonor)){
    if(AC_Avg_MC_Population_ByDonor[i,j] == "NaN"){
      AC_Avg_MC_Population_ByDonor[i,j] <- NA
    }
  }
}
```
