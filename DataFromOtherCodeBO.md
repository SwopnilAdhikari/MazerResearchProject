---
title: "DataFromOtherCodeBO"
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

BO_Data_2022 <- read_csv("~/MazerResearchProject/Data/BO_Data_2022.csv")
```

```
## Rows: 349 Columns: 23
## ── Column specification ─────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (6): Population, Generation, Donor, Recipient, FFD, LFD
## dbl (16): Field_Year, Block, Transect, Sequence, Plant_ID, Total_Closed_Fruits, Total_Fruits,...
## lgl  (1): Left_Or_Right
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
BO_Data_2022 <- BO_Data_2022 %>% mutate(FFD = yday(mdy(FFD)), LFD = yday(mdy(LFD)), Flowering_Duration = LFD-FFD) 

BO_Data_2022$Flowering_Duration - BO_Data_2022$fl_duration
```

```
##   [1]  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [31]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [61]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0
##  [91]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [121]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [151]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [181]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [211]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [241]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [271]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [301]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0
## [331]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
```

```r
min(BO_Data_2022$Total_Fruits, na.rm=TRUE) 
```

```
## [1] 0
```

```r
min(BO_Data_2022$Mean_Seeds_per_Fruit, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BO_Data_2022$Lifetime_Fecundity, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BO_Data_2022$Stem_Biomass, na.rm=TRUE)
```

```
## [1] 1.47
```

```r
min(BO_Data_2022$Leaf_Area_mm2, na.rm=TRUE)
```

```
## [1] 5.156
```

```r
BO_Data_2022 <- BO_Data_2022 %>% mutate(Log_Total_Fruits=log(Total_Fruits+1), Log_Mean_Seeds_per_Fruit=log(Mean_Seeds_per_Fruit+1), Log_Lifetime_Fecundity=log(Lifetime_Fecundity+1), Log_Stem_Biomass=log(Stem_Biomass), Log_Leaf_Area_mm2=log(Leaf_Area_mm2))

BOTransect1 <- subset(BO_Data_2022, Transect == 1)
BOTransect2 <- subset(BO_Data_2022, Transect == 2)
BOTransect3 <- subset(BO_Data_2022, Transect == 3)
BOTransect4 <- subset(BO_Data_2022, Transect == 4)
BOTransect5 <- subset(BO_Data_2022, Transect == 5)
BOTransect6 <- subset(BO_Data_2022, Transect == 6)
BOTransect7 <- subset(BO_Data_2022, Transect == 7)
BOTransect8 <- subset(BO_Data_2022, Transect == 8)
BOTransect9 <- subset(BO_Data_2022, Transect == 9)
BOTransect10 <- subset(BO_Data_2022, Transect == 10)
BOTransect11 <- subset(BO_Data_2022, Transect == 11)
BOTransect12 <- subset(BO_Data_2022, Transect == 12)

center_scale <- function(x) {
  scale(x, scale = FALSE)
}
```


```r
PopulationV1 <- BOTransect1$Population # Creates a vector of Population as charBOter variables
Field_YearV1 <- BOTransect1$Field_Year
GenerationV1 <- BOTransect1$Generation
BlockV1 <- BOTransect1$Block
TransectV1 <- BOTransect1$Transect
SequenceV1 <- BOTransect1$Sequence
DonorV1 <- BOTransect1$Donor
RecipientV1 <- BOTransect1$Recipient
FFDV1 <- BOTransect1$FFD
LFDV1 <- BOTransect1$LFD

# All of these vectors include all of the values in the original data set (BO_Data_2022)

# Remove values of these variables in BOTransect1 (the data frame that contains the variables for which we need to get rid of the outliers)

BOTransect1$Population <-NULL
BOTransect1$Field_Year <- NULL
BOTransect1$Generation <- NULL
BOTransect1$Block <- NULL
BOTransect1$Transect <- NULL
BOTransect1$Sequence <- NULL
BOTransect1$Donor <- NULL
BOTransect1$Recipient <- NULL
BOTransect1$FFD <- NULL
BOTransect1$LFD <- NULL


# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array (eBOh row and column), looking for values above 3, which implies that the value is an outlier.

# The index (row i, column j) will be removed from the original BOTransect1 data frame.

z_scores <- as.data.frame(sapply(BOTransect1, function(BOTransect1) (abs(BOTransect1-mean(BOTransect1, na.rm = TRUE))/sd(BOTransect1, na.rm = TRUE))))

class(z_scores) # Shows that z_scores is a data_frame
```

```
## [1] "data.frame"
```

```r
View(z_scores) # EBOh value is a z_score

# The for loop is as follows.  For eBOh index (row=i, column=j) in the z_score data frame that is > 3, it replBOes the value for that index in Transect1 with an "NA"

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect1[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect1[i,j] = NA
      }
    }
  }

BOTransect1 <- BOTransect1 %>% mutate(Population = PopulationV1 , Field_Year = Field_YearV1, Generation = GenerationV1, Block = BlockV1, Transect = TransectV1, Sequence = SequenceV1, Donor = DonorV1, Recipient = RecipientV1, FFD = FFDV1, LFD = LFDV1, .before = Left_Or_Right, )

print(BOTransect1$Sequence)
```

```
##  [1]  1  2  3  4  5  6  7  9 10 11 12 13 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
## [32] 35 37 38 40 42 44
```

```r
################################################################################################
PopulationV2 <- BOTransect2$Population # Creates a vector of Population as charBOter variables
Field_YearV2 <- BOTransect2$Field_Year
GenerationV2 <- BOTransect2$Generation
BlockV2 <- BOTransect2$Block
TransectV2 <- BOTransect2$Transect
SequenceV2 <- BOTransect2$Sequence
DonorV2 <- BOTransect2$Donor
RecipientV2 <- BOTransect2$Recipient
FFDV2 <- BOTransect2$FFD
LFDV2 <- BOTransect2$LFD


# Remove values of these variables in BOTransect2 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect2$Population <-NULL
BOTransect2$Field_Year <- NULL
BOTransect2$Generation <- NULL
BOTransect2$Block <- NULL
BOTransect2$Transect <- NULL
BOTransect2$Sequence <- NULL
BOTransect2$Donor <- NULL
BOTransect2$Recipient <- NULL
BOTransect2$FFD <- NULL
BOTransect2$LFD <- NULL


# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original BOTransect2 data frame
z_scores <- as.data.frame(sapply(BOTransect2, function(BOTransect2) (abs(BOTransect2-mean(BOTransect2, na.rm = TRUE))/sd(BOTransect2, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect2[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect2[i,j] = NA
      }
    }
  }

BOTransect2 <- BOTransect2 %>% mutate(Population = PopulationV2 , Field_Year = Field_YearV2, Generation = GenerationV2, Block = BlockV2, Transect = TransectV2, Sequence = SequenceV2, Donor = DonorV2, Recipient = RecipientV2, FFD = FFDV2, LFD = LFDV2, .before = Left_Or_Right)


View(BOTransect2)


PopulationV3 <- BOTransect3$Population # Creates a vector of Population as charBOter variables
Field_YearV3 <- BOTransect3$Field_Year
GenerationV3 <- BOTransect3$Generation
BlockV3 <- BOTransect3$Block
TransectV3 <- BOTransect3$Transect
SequenceV3 <- BOTransect3$Sequence
DonorV3 <- BOTransect3$Donor
RecipientV3 <- BOTransect3$Recipient
FFDV3 <- BOTransect3$FFD
LFDV3 <- BOTransect3$LFD


# Remove values of these variables in BOTransect3 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect3$Population <-NULL
BOTransect3$Field_Year <- NULL
BOTransect3$Generation <- NULL
BOTransect3$Block <- NULL
BOTransect3$Transect <- NULL
BOTransect3$Sequence <- NULL
BOTransect3$Donor <- NULL
BOTransect3$Recipient <- NULL
BOTransect3$FFD <- NULL
BOTransect3$LFD <- NULL



# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original BOTransect3 data frame
z_scores <- as.data.frame(sapply(BOTransect3, function(BOTransect3) (abs(BOTransect3-mean(BOTransect3, na.rm = TRUE))/sd(BOTransect3, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect3[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect3[i,j] = NA
      }
    }
  }

BOTransect3 <- BOTransect3 %>% mutate(Population = PopulationV3 , Field_Year = Field_YearV3, Generation = GenerationV3, Block = BlockV3, Transect = TransectV3, Sequence = SequenceV3, Donor = DonorV3, Recipient = RecipientV3, FFD = FFDV3, LFD = LFDV3, .before = Left_Or_Right)


names(BOTransect3)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV4 <- BOTransect4$Population # Creates a vector of Population as charBOter variables
Field_YearV4 <- BOTransect4$Field_Year
GenerationV4 <- BOTransect4$Generation
BlockV4 <- BOTransect4$Block
TransectV4 <- BOTransect4$Transect
SequenceV4 <- BOTransect4$Sequence
DonorV4 <- BOTransect4$Donor
RecipientV4 <- BOTransect4$Recipient
FFDV4 <- BOTransect4$FFD
LFDV4 <- BOTransect4$LFD


# Remove values of these variables in BOTransect4 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect4$Population <-NULL
BOTransect4$Field_Year <- NULL
BOTransect4$Generation <- NULL
BOTransect4$Block <- NULL
BOTransect4$Transect <- NULL
BOTransect4$Sequence <- NULL
BOTransect4$Donor <- NULL
BOTransect4$Recipient <- NULL
BOTransect4$FFD <- NULL
BOTransect4$LFD <- NULL


# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 4 which implies the value is an outlier and removes that index from the original BOTransect4 data frame
z_scores <- as.data.frame(sapply(BOTransect4, function(BOTransect4) (abs(BOTransect4-mean(BOTransect4, na.rm = TRUE))/sd(BOTransect4, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect4[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect4[i,j] = NA
      }
    }
  }

BOTransect4 <- BOTransect4 %>% mutate(Population = PopulationV4 , Field_Year = Field_YearV4, Generation = GenerationV4, Block = BlockV4, Transect = TransectV4, Sequence = SequenceV4, Donor = DonorV4, Recipient = RecipientV4, FFD = FFDV4, LFD = LFDV4, .before = Left_Or_Right)

names(BOTransect4)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV5 <- BOTransect5$Population # Creates a vector of Population as charBOter variables
Field_YearV5 <- BOTransect5$Field_Year
GenerationV5 <- BOTransect5$Generation
BlockV5 <- BOTransect5$Block
TransectV5 <- BOTransect5$Transect
SequenceV5 <- BOTransect5$Sequence
DonorV5 <- BOTransect5$Donor
RecipientV5 <- BOTransect5$Recipient
FFDV5 <- BOTransect5$FFD
LFDV5 <- BOTransect5$LFD


# Remove values of these variables in BOTransect5 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect5$Population <-NULL
BOTransect5$Field_Year <- NULL
BOTransect5$Generation <- NULL
BOTransect5$Block <- NULL
BOTransect5$Transect <- NULL
BOTransect5$Sequence <- NULL
BOTransect5$Donor <- NULL
BOTransect5$Recipient <- NULL
BOTransect5$FFD <- NULL
BOTransect5$LFD <- NULL


# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 5 which implies the value is an outlier and removes that index from the original BOTransect5 data frame
z_scores <- as.data.frame(sapply(BOTransect5, function(BOTransect5) (abs(BOTransect5-mean(BOTransect5, na.rm = TRUE))/sd(BOTransect5, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect5[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect5[i,j] = NA
      }
    }
  }

BOTransect5 <- BOTransect5 %>% mutate(Population = PopulationV5 , Field_Year = Field_YearV5, Generation = GenerationV5, Block = BlockV5, Transect = TransectV5, Sequence = SequenceV5, Donor = DonorV5, Recipient = RecipientV5,FFD = FFDV5, LFD = LFDV5, .before = Left_Or_Right)

names(BOTransect5)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV6 <- BOTransect6$Population # Creates a vector of Population as charBOter variables
Field_YearV6 <- BOTransect6$Field_Year
GenerationV6 <- BOTransect6$Generation
BlockV6 <- BOTransect6$Block
TransectV6 <- BOTransect6$Transect
SequenceV6 <- BOTransect6$Sequence
DonorV6 <- BOTransect6$Donor
RecipientV6 <- BOTransect6$Recipient
FFDV6 <- BOTransect6$FFD
LFDV6 <- BOTransect6$LFD


# Remove values of these variables in BOTransect6 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect6$Population <-NULL
BOTransect6$Field_Year <- NULL
BOTransect6$Generation <- NULL
BOTransect6$Block <- NULL
BOTransect6$Transect <- NULL
BOTransect6$Sequence <- NULL
BOTransect6$Donor <- NULL
BOTransect6$Recipient <- NULL
BOTransect6$FFD <- NULL
BOTransect6$LFD <- NULL


# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 6 which implies the value is an outlier and removes that index from the original BOTransect6 data frame
z_scores <- as.data.frame(sapply(BOTransect6, function(BOTransect6) (abs(BOTransect6-mean(BOTransect6, na.rm = TRUE))/sd(BOTransect6, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect6[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect6[i,j] = NA
      }
    }
  }

BOTransect6 <- BOTransect6 %>% mutate(Population = PopulationV6 , Field_Year = Field_YearV6, Generation = GenerationV6, Block = BlockV6, Transect = TransectV6, Sequence = SequenceV6, Donor = DonorV6, Recipient = RecipientV6, FFD = FFDV6, LFD = LFDV6, .before = Left_Or_Right)


names(BOTransect6)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV7 <- BOTransect7$Population # Creates a vector of Population as charBOter variables
Field_YearV7 <- BOTransect7$Field_Year
GenerationV7 <- BOTransect7$Generation
BlockV7 <- BOTransect7$Block
TransectV7 <- BOTransect7$Transect
SequenceV7 <- BOTransect7$Sequence
DonorV7 <- BOTransect7$Donor
RecipientV7 <- BOTransect7$Recipient
FFDV7 <- BOTransect7$FFD
LFDV7 <- BOTransect7$LFD


# Remove values of these variables in BOTransect7 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect7$Population <-NULL
BOTransect7$Field_Year <- NULL
BOTransect7$Generation <- NULL
BOTransect7$Block <- NULL
BOTransect7$Transect <- NULL
BOTransect7$Sequence <- NULL
BOTransect7$Donor <- NULL
BOTransect7$Recipient <- NULL
BOTransect7$FFD <- NULL
BOTransect7$LFD <- NULL

# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 7 which implies the value is an outlier and removes that index from the original BOTransect1 data frame
z_scores <- as.data.frame(sapply(BOTransect7, function(BOTransect7) (abs(BOTransect7-mean(BOTransect7, na.rm = TRUE))/sd(BOTransect7, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect7[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect7[i,j] = NA
      }
    }
  }

BOTransect7 <- BOTransect7 %>% mutate(Population = PopulationV7 , Field_Year = Field_YearV7, Generation = GenerationV7, Block = BlockV7, Transect = TransectV7, Sequence = SequenceV7, Donor = DonorV7, Recipient = RecipientV7, FFD = FFDV7, LFD = LFDV7, .before = Left_Or_Right)


names(BOTransect7)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV8 <- BOTransect8$Population # Creates a vector of Population as charBOter variables
Field_YearV8 <- BOTransect8$Field_Year
GenerationV8 <- BOTransect8$Generation
BlockV8 <- BOTransect8$Block
TransectV8 <- BOTransect8$Transect
SequenceV8 <- BOTransect8$Sequence
DonorV8 <- BOTransect8$Donor
RecipientV8 <- BOTransect8$Recipient
FFDV8 <- BOTransect8$FFD
LFDV8 <- BOTransect8$LFD


# Remove values of these variables in BOTransect8 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect8$Population <-NULL
BOTransect8$Field_Year <- NULL
BOTransect8$Generation <- NULL
BOTransect8$Block <- NULL
BOTransect8$Transect <- NULL
BOTransect8$Sequence <- NULL
BOTransect8$Donor <- NULL
BOTransect8$Recipient <- NULL
BOTransect8$FFD <- NULL
BOTransect8$LFD <- NULL



# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 8 which implies the value is an outlier and removes that index from the original BOTransect data frame
z_scores <- as.data.frame(sapply(BOTransect8, function(BOTransect8) (abs(BOTransect8-mean(BOTransect8, na.rm = TRUE))/sd(BOTransect8, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect8[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect8[i,j] = NA
      }
    }
  }

BOTransect8 <- BOTransect8 %>% mutate(Population = PopulationV8 , Field_Year = Field_YearV8, Generation = GenerationV8, Block = BlockV8, Transect = TransectV8, Sequence = SequenceV8, Donor = DonorV8, Recipient = RecipientV8, FFD = FFDV8, LFD = LFDV8, .before = Left_Or_Right)


names(BOTransect8)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV9 <- BOTransect9$Population # Creates a vector of Population as charBOter variables
Field_YearV9 <- BOTransect9$Field_Year
GenerationV9 <- BOTransect9$Generation
BlockV9 <- BOTransect9$Block
TransectV9 <- BOTransect9$Transect
SequenceV9 <- BOTransect9$Sequence
DonorV9 <- BOTransect9$Donor
RecipientV9 <- BOTransect9$Recipient
FFDV9 <- BOTransect9$FFD
LFDV9 <- BOTransect9$LFD


# Remove values of these variables in BOTransect9 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect9$Population <-NULL
BOTransect9$Field_Year <- NULL
BOTransect9$Generation <- NULL
BOTransect9$Block <- NULL
BOTransect9$Transect <- NULL
BOTransect9$Sequence <- NULL
BOTransect9$Donor <- NULL
BOTransect9$Recipient <- NULL
BOTransect9$FFD <- NULL
BOTransect9$LFD <- NULL



# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 9 which implies the value is an outlier and removes that index from the original BOTransect9 data frame
z_scores <- as.data.frame(sapply(BOTransect9, function(BOTransect9) (abs(BOTransect9-mean(BOTransect9, na.rm = TRUE))/sd(BOTransect9, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect9[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect9[i,j] = NA
      }
    }
  }

BOTransect9 <- BOTransect9 %>% mutate(Population = PopulationV9 , Field_Year = Field_YearV9, Generation = GenerationV9, Block = BlockV9, Transect = TransectV9, Sequence = SequenceV9, Donor = DonorV9, Recipient = RecipientV9, FFD = FFDV9, LFD = LFDV9, .before = Left_Or_Right)


names(BOTransect9)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
####
PopulationV10 <- BOTransect10$Population # Creates a vector of Population as charBOter variables
Field_YearV10 <- BOTransect10$Field_Year
GenerationV10 <- BOTransect10$Generation
BlockV10 <- BOTransect10$Block
TransectV10 <- BOTransect10$Transect
SequenceV10 <- BOTransect10$Sequence
DonorV10 <- BOTransect10$Donor
RecipientV10 <- BOTransect10$Recipient
FFDV10 <- BOTransect10$FFD
LFDV10 <- BOTransect10$LFD


# Remove values of these variables in BOTransect9 (the data frame that contains the variables for which we want to get rid of the outliers)
BOTransect10$Population <-NULL
BOTransect10$Field_Year <- NULL
BOTransect10$Generation <- NULL
BOTransect10$Block <- NULL
BOTransect10$Transect <- NULL
BOTransect10$Sequence <- NULL
BOTransect10$Donor <- NULL
BOTransect10$Recipient <- NULL
BOTransect10$FFD <- NULL
BOTransect10$LFD <- NULL



# z-scores are calculated and the for loop goes tBOough eBOh element of the z-score array, looking for values above 9 which implies the value is an outlier and removes that index from the original BOTransect9 data frame
z_scores <- as.data.frame(sapply(BOTransect10, function(BOTransect10) (abs(BOTransect10-mean(BOTransect10, na.rm = TRUE))/sd(BOTransect10, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect10[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect10[i,j] = NA
      }
    }
  }

BOTransect10 <- BOTransect10 %>% mutate(Population = PopulationV10 , Field_Year = Field_YearV10, Generation = GenerationV10, Block = BlockV10, Transect = TransectV10, Sequence = SequenceV10, Donor = DonorV10, Recipient = RecipientV10, FFD = FFDV10, LFD = LFDV10, .before = Left_Or_Right)


names(BOTransect10)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
#########################################
PopulationV11<- BOTransect11$Population 
Field_YearV11<- BOTransect11$Field_Year
GenerationV11<- BOTransect11$Generation
BlockV11<- BOTransect11$Block
TransectV11<- BOTransect11$Transect
SequenceV11<- BOTransect11$Sequence
DonorV11<- BOTransect11$Donor
RecipientV11<- BOTransect11$Recipient
FFDV11<- BOTransect11$FFD
LFDV11<- BOTransect11$LFD

BOTransect11$Population <-NULL
BOTransect11$Field_Year <- NULL
BOTransect11$Generation <- NULL
BOTransect11$Block <- NULL
BOTransect11$Transect <- NULL
BOTransect11$Sequence <- NULL
BOTransect11$Donor <- NULL
BOTransect11$Recipient <- NULL
BOTransect11$FFD <- NULL
BOTransect11$LFD <- NULL


z_scores <- as.data.frame(sapply(BOTransect11, function(BOTransect11) (abs(BOTransect11-mean(BOTransect11, na.rm = TRUE))/sd(BOTransect11, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect11[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect11[i,j] = NA
      }
    }
  }

BOTransect11<- BOTransect11%>% mutate(Population = PopulationV11, Field_Year = Field_YearV11, Generation = GenerationV11, Block = BlockV11, Transect = TransectV11, Sequence = SequenceV11, Donor = DonorV11, Recipient = RecipientV11, FFD = FFDV11, LFD = LFDV11, .before = Left_Or_Right)


names(BOTransect11)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
#########################################
PopulationV12<- BOTransect12$Population 
Field_YearV12<- BOTransect12$Field_Year
GenerationV12<- BOTransect12$Generation
BlockV12<- BOTransect12$Block
TransectV12<- BOTransect12$Transect
SequenceV12<- BOTransect12$Sequence
DonorV12<- BOTransect12$Donor
RecipientV12<- BOTransect12$Recipient
FFDV12<- BOTransect12$FFD
LFDV12<- BOTransect12$LFD

BOTransect12$Population <-NULL
BOTransect12$Field_Year <- NULL
BOTransect12$Generation <- NULL
BOTransect12$Block <- NULL
BOTransect12$Transect <- NULL
BOTransect12$Sequence <- NULL
BOTransect12$Donor <- NULL
BOTransect12$Recipient <- NULL
BOTransect12$FFD <- NULL
BOTransect12$LFD <- NULL


z_scores <- as.data.frame(sapply(BOTransect12, function(BOTransect12) (abs(BOTransect12-mean(BOTransect12, na.rm = TRUE))/sd(BOTransect12, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BOTransect12[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BOTransect12[i,j] = NA
      }
    }
  }

BOTransect12<- BOTransect12%>% mutate(Population = PopulationV12, Field_Year = Field_YearV12, Generation = GenerationV12, Block = BlockV12, Transect = TransectV12, Sequence = SequenceV12, Donor = DonorV12, Recipient = RecipientV12, FFD = FFDV12, LFD = LFDV12, .before = Left_Or_Right)


names(BOTransect12)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```


```r
names(BOTransect1)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
BOTransect1 <- BOTransect1 %>% mutate(
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

View(BOTransect1)


mean(BOTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 1.371429
```

```r
min(BOTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 7
```

```r
mean(BOTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -3.799354e-17
```

```r
min(BOTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -1.371429
```

```r
max(BOTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 5.628571
```

```r
 BOTransect2 <- BOTransect2 %>% mutate(
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

mean(BOTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 1.884615
```

```r
min(BOTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 9
```

```r
mean(BOTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BOTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -1.884615
```

```r
max(BOTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 7.115385
```

```r
 BOTransect3 <- BOTransect3 %>% mutate(
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

mean(BOTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 22.95238
```

```r
min(BOTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 48
```

```r
mean(BOTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -1.688464e-16
```

```r
min(BOTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -22.95238
```

```r
max(BOTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 25.04762
```

```r
BOTransect4 <- BOTransect4 %>% mutate(
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

mean(BOTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 20.18182
```

```r
min(BOTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 42
```

```r
mean(BOTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -1.614727e-15
```

```r
min(BOTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -20.18182
```

```r
max(BOTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 21.81818
```

```r
BOTransect5 <- BOTransect5 %>% mutate(
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

mean(BOTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 31.87879
```

```r
min(BOTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 70
```

```r
mean(BOTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BOTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -31.87879
```

```r
max(BOTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 38.12121
```

```r
BOTransect6 <- BOTransect6 %>% mutate(
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

mean(BOTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 343.0485
```

```r
min(BOTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 154.946
```

```r
max(BOTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 570.21
```

```r
mean(BOTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 8.528594e-15
```

```r
min(BOTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -188.1025
```

```r
max(BOTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 227.1615
```

```r
BOTransect7 <- BOTransect7 %>% mutate(
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

mean(BOTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 351.1234
```

```r
min(BOTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 217.855
```

```r
max(BOTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 479.683
```

```r
mean(BOTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -8.881784e-15
```

```r
min(BOTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -133.2684
```

```r
max(BOTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 128.5596
```

```r
BOTransect8 <- BOTransect8 %>% mutate(
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

mean(BOTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 23.86071
```

```r
min(BOTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 16.54
```

```r
max(BOTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 31.418
```

```r
mean(BOTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] -8.358051e-16
```

```r
min(BOTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] -7.320706
```

```r
max(BOTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] 7.557294
```

```r
BOTransect9 <- BOTransect9 %>% mutate(
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

View(BOTransect9)

# Let's take a look at the mean, minimum and maximum of the number of closed fruits for both the raw values and the mean-centered values, for Transect 9.


mean(BOTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 1.592593
```

```r
min(BOTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(BOTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 7
```

```r
mean(BOTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 3.289951e-17
```

```r
min(BOTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -1.592593
```

```r
max(BOTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 3.407407
```

```r
names(BOTransect10)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
BOTransect10 <- BOTransect10 %>% mutate(
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

View(BOTransect10)

names(BOTransect11)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
BOTransect11 <- BOTransect11 %>% mutate(
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

View(BOTransect11)

names(BOTransect12)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num_ClosedFt"   
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
BOTransect12 <- BOTransect12 %>% mutate(
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

View(BOTransect12)
```


```r
Recipients <- c(BOTransect1$Recipient, BOTransect2$Recipient, BOTransect3$Recipient, BOTransect4$Recipient, BOTransect5$Recipient, BOTransect6$Recipient, BOTransect7$Recipient, BOTransect8$Recipient, BOTransect9$Recipient, BOTransect10$Recipient, BOTransect11$Recipient, BOTransect12$Recipient)

# Now, modify this vector so that eBOh recipient occurs only once

Recipients <- unique(Recipients) # Names of maternal ID's, without repetition (n=107)

str(Recipients)
```

```
##  chr [1:131] "BO_130" "BO_166" "BO_019" "BO_066" "BO_137" "BO_045" "BO_048" "BO_099" ...
```

```r
length(Recipients)
```

```
## [1] 131
```

```r
Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "FFD_MC", "LFD_MC", "Flowering_Duration_MC")

# Combine transects to get mean-centered population data for easy averaging of same maternal IDs

BO_MeanCentered_AllTransects <- rbind(BOTransect1,BOTransect2,BOTransect3,BOTransect4,BOTransect5,BOTransect6,BOTransect7,BOTransect8, BOTransect9, BOTransect10, BOTransect11, BOTransect12) # This contains 41 variables

BO_MeanCentered_AllTransects <- BO_MeanCentered_AllTransects[Variables]


BO_Avg_MC_Population_ByRecip <- BO_MeanCentered_AllTransects %>%
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

View(BO_Avg_MC_Population_ByRecip)

Donors <- c(BOTransect1$Donor, BOTransect2$Donor, BOTransect3$Donor, BOTransect4$Donor, BOTransect5$Donor, BOTransect6$Donor, BOTransect7$Donor, BOTransect8$Donor, BOTransect9$Donor, BOTransect10$Donor, BOTransect11$Donor, BOTransect12$Donor)

Donors <- unique(Donors) # Names of Paternal ID's, without repetition (n=40)

str(Donors)
```

```
##  chr [1:44] "BO_201" "BO_192" "BO_005" "BO_070" "BO_038" "BO_170" "BO_001" "BO_033" "BO_065" ...
```

```r
length(Donors)
```

```
## [1] 44
```

```r
Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "Flowering_Duration_MC")



BO_MC_Population <- rbind(BOTransect1,BOTransect2,BOTransect3,BOTransect4,BOTransect5,BOTransect6,BOTransect7,BOTransect8, BOTransect9, BOTransect10, BOTransect11, BOTransect12)

BO_MC_Population <- BO_MC_Population[Variables] # Includes only the variables in the "Variables" vector.


BO_Avg_MC_Population_ByDonor <- BO_MC_Population %>%
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

View(BO_Avg_MC_Population_ByDonor)
```


```r
for(i in 1:nrow(BO_Avg_MC_Population_ByRecip)){
  for(j in 1:ncol(BO_Avg_MC_Population_ByRecip)){
    if(BO_Avg_MC_Population_ByRecip[i,j] == "NaN"){
      BO_Avg_MC_Population_ByRecip[i,j] <- NA
    }
  }
}

for(i in 1:nrow(BO_Avg_MC_Population_ByDonor)){
  for(j in 1:ncol(BO_Avg_MC_Population_ByDonor)){
    if(BO_Avg_MC_Population_ByDonor[i,j] == "NaN"){
      BO_Avg_MC_Population_ByDonor[i,j] <- NA
    }
  }
}
```
