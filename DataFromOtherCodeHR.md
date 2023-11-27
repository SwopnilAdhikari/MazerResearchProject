---
title: "DataFromOtherCodeHR"
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

HR_Data_2022 <- read_csv("~/MazerResearchProject/Data/HR_Data_2022.csv")
```

```
## Rows: 406 Columns: 23
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
HR_Data_2022 <- HR_Data_2022 %>% mutate(FFD = yday(mdy(FFD)), LFD = yday(mdy(LFD)), Flowering_Duration = LFD-FFD) 

HR_Data_2022$Flowering_Duration - HR_Data_2022$fl_duration
```

```
##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [47] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [93] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [139] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [185] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [231] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [277] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [323] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [369] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

```r
min(HR_Data_2022$Total_Fruits, na.rm=TRUE) 
```

```
## [1] 0
```

```r
min(HR_Data_2022$Mean_Seeds_per_Fruit, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(HR_Data_2022$Lifetime_Fecundity, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(HR_Data_2022$Stem_Biomass, na.rm=TRUE)
```

```
## [1] 2.06
```

```r
min(HR_Data_2022$Leaf_Area_mm2, na.rm=TRUE)
```

```
## [1] 5.882
```

```r
HR_Data_2022 <- HR_Data_2022 %>% mutate(Log_Total_Fruits=log(Total_Fruits+1), Log_Mean_Seeds_per_Fruit=log(Mean_Seeds_per_Fruit+1), Log_Lifetime_Fecundity=log(Lifetime_Fecundity+1), Log_Stem_Biomass=log(Stem_Biomass), Log_Leaf_Area_mm2=log(Leaf_Area_mm2))

HRTransect1 <- subset(HR_Data_2022, Transect == 1)
HRTransect2 <- subset(HR_Data_2022, Transect == 2)
HRTransect3 <- subset(HR_Data_2022, Transect == 3)
HRTransect4 <- subset(HR_Data_2022, Transect == 4)
HRTransect5 <- subset(HR_Data_2022, Transect == 5)
HRTransect6 <- subset(HR_Data_2022, Transect == 6)
HRTransect7 <- subset(HR_Data_2022, Transect == 7)
HRTransect8 <- subset(HR_Data_2022, Transect == 8)
HRTransect9 <- subset(HR_Data_2022, Transect == 9)


center_scale <- function(x) {
  scale(x, scale = FALSE)
}
```


```r
PopulationV1 <- HRTransect1$Population # Creates a vector of Population as charHRter variables
Field_YearV1 <- HRTransect1$Field_Year
GenerationV1 <- HRTransect1$Generation
BlockV1 <- HRTransect1$Block
TransectV1 <- HRTransect1$Transect
SequenceV1 <- HRTransect1$Sequence
DonorV1 <- HRTransect1$Donor
RecipientV1 <- HRTransect1$Recipient
FFDV1 <- HRTransect1$FFD
LFDV1 <- HRTransect1$LFD

# All of these vectors include all of the values in the original data set (HR_Data_2022)

# Remove values of these variables in HRTransect1 (the data frame that contains the variables for which we need to get rid of the outliers)

HRTransect1$Population <-NULL
HRTransect1$Field_Year <- NULL
HRTransect1$Generation <- NULL
HRTransect1$Block <- NULL
HRTransect1$Transect <- NULL
HRTransect1$Sequence <- NULL
HRTransect1$Donor <- NULL
HRTransect1$Recipient <- NULL
HRTransect1$FFD <- NULL
HRTransect1$LFD <- NULL


# z-scores are calculated and the for loop goes through eHRh element of the z-score array (eHRh row and column), looking for values above 3, which implies that the value is an outlier.

# The index (row i, column j) will be removed from the original HRTransect1 data frame.

z_scores <- as.data.frame(sapply(HRTransect1, function(HRTransect1) (abs(HRTransect1-mean(HRTransect1, na.rm = TRUE))/sd(HRTransect1, na.rm = TRUE))))

class(z_scores) # Shows that z_scores is a data_frame
```

```
## [1] "data.frame"
```

```r
View(z_scores) # EHRh value is a z_score

# The for loop is as follows.  For eHRh index (row=i, column=j) in the z_score data frame that is > 3, it replHRes the value for that index in Transect1 with an "NA"

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect1[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect1[i,j] = NA
      }
    }
  }

HRTransect1 <- HRTransect1 %>% mutate(Population = PopulationV1 , Field_Year = Field_YearV1, Generation = GenerationV1, Block = BlockV1, Transect = TransectV1, Sequence = SequenceV1, Donor = DonorV1, Recipient = RecipientV1, FFD = FFDV1, LFD = LFDV1, .before = Left_Or_Right, )

print(HRTransect1$Sequence)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 28 29 30 31 32 33
## [32] 34 35 36 37 38 39 40 41 42 43 44 45 46
```

```r
################################################################################################
PopulationV2 <- HRTransect2$Population # Creates a vector of Population as charHRter variables
Field_YearV2 <- HRTransect2$Field_Year
GenerationV2 <- HRTransect2$Generation
BlockV2 <- HRTransect2$Block
TransectV2 <- HRTransect2$Transect
SequenceV2 <- HRTransect2$Sequence
DonorV2 <- HRTransect2$Donor
RecipientV2 <- HRTransect2$Recipient
FFDV2 <- HRTransect2$FFD
LFDV2 <- HRTransect2$LFD


# Remove values of these variables in HRTransect2 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect2$Population <-NULL
HRTransect2$Field_Year <- NULL
HRTransect2$Generation <- NULL
HRTransect2$Block <- NULL
HRTransect2$Transect <- NULL
HRTransect2$Sequence <- NULL
HRTransect2$Donor <- NULL
HRTransect2$Recipient <- NULL
HRTransect2$FFD <- NULL
HRTransect2$LFD <- NULL


# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original HRTransect2 data frame
z_scores <- as.data.frame(sapply(HRTransect2, function(HRTransect2) (abs(HRTransect2-mean(HRTransect2, na.rm = TRUE))/sd(HRTransect2, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect2[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect2[i,j] = NA
      }
    }
  }

HRTransect2 <- HRTransect2 %>% mutate(Population = PopulationV2 , Field_Year = Field_YearV2, Generation = GenerationV2, Block = BlockV2, Transect = TransectV2, Sequence = SequenceV2, Donor = DonorV2, Recipient = RecipientV2, FFD = FFDV2, LFD = LFDV2, .before = Left_Or_Right)


View(HRTransect2)


PopulationV3 <- HRTransect3$Population # Creates a vector of Population as charHRter variables
Field_YearV3 <- HRTransect3$Field_Year
GenerationV3 <- HRTransect3$Generation
BlockV3 <- HRTransect3$Block
TransectV3 <- HRTransect3$Transect
SequenceV3 <- HRTransect3$Sequence
DonorV3 <- HRTransect3$Donor
RecipientV3 <- HRTransect3$Recipient
FFDV3 <- HRTransect3$FFD
LFDV3 <- HRTransect3$LFD


# Remove values of these variables in HRTransect3 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect3$Population <-NULL
HRTransect3$Field_Year <- NULL
HRTransect3$Generation <- NULL
HRTransect3$Block <- NULL
HRTransect3$Transect <- NULL
HRTransect3$Sequence <- NULL
HRTransect3$Donor <- NULL
HRTransect3$Recipient <- NULL
HRTransect3$FFD <- NULL
HRTransect3$LFD <- NULL



# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 3 which implies the value is an outlier and removes that index from the original HRTransect3 data frame
z_scores <- as.data.frame(sapply(HRTransect3, function(HRTransect3) (abs(HRTransect3-mean(HRTransect3, na.rm = TRUE))/sd(HRTransect3, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect3[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect3[i,j] = NA
      }
    }
  }

HRTransect3 <- HRTransect3 %>% mutate(Population = PopulationV3 , Field_Year = Field_YearV3, Generation = GenerationV3, Block = BlockV3, Transect = TransectV3, Sequence = SequenceV3, Donor = DonorV3, Recipient = RecipientV3, FFD = FFDV3, LFD = LFDV3, .before = Left_Or_Right)


names(HRTransect3)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV4 <- HRTransect4$Population # Creates a vector of Population as charHRter variables
Field_YearV4 <- HRTransect4$Field_Year
GenerationV4 <- HRTransect4$Generation
BlockV4 <- HRTransect4$Block
TransectV4 <- HRTransect4$Transect
SequenceV4 <- HRTransect4$Sequence
DonorV4 <- HRTransect4$Donor
RecipientV4 <- HRTransect4$Recipient
FFDV4 <- HRTransect4$FFD
LFDV4 <- HRTransect4$LFD


# Remove values of these variables in HRTransect4 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect4$Population <-NULL
HRTransect4$Field_Year <- NULL
HRTransect4$Generation <- NULL
HRTransect4$Block <- NULL
HRTransect4$Transect <- NULL
HRTransect4$Sequence <- NULL
HRTransect4$Donor <- NULL
HRTransect4$Recipient <- NULL
HRTransect4$FFD <- NULL
HRTransect4$LFD <- NULL


# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 4 which implies the value is an outlier and removes that index from the original HRTransect4 data frame
z_scores <- as.data.frame(sapply(HRTransect4, function(HRTransect4) (abs(HRTransect4-mean(HRTransect4, na.rm = TRUE))/sd(HRTransect4, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect4[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect4[i,j] = NA
      }
    }
  }

HRTransect4 <- HRTransect4 %>% mutate(Population = PopulationV4 , Field_Year = Field_YearV4, Generation = GenerationV4, Block = BlockV4, Transect = TransectV4, Sequence = SequenceV4, Donor = DonorV4, Recipient = RecipientV4, FFD = FFDV4, LFD = LFDV4, .before = Left_Or_Right)

names(HRTransect4)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV5 <- HRTransect5$Population # Creates a vector of Population as charHRter variables
Field_YearV5 <- HRTransect5$Field_Year
GenerationV5 <- HRTransect5$Generation
BlockV5 <- HRTransect5$Block
TransectV5 <- HRTransect5$Transect
SequenceV5 <- HRTransect5$Sequence
DonorV5 <- HRTransect5$Donor
RecipientV5 <- HRTransect5$Recipient
FFDV5 <- HRTransect5$FFD
LFDV5 <- HRTransect5$LFD


# Remove values of these variables in HRTransect5 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect5$Population <-NULL
HRTransect5$Field_Year <- NULL
HRTransect5$Generation <- NULL
HRTransect5$Block <- NULL
HRTransect5$Transect <- NULL
HRTransect5$Sequence <- NULL
HRTransect5$Donor <- NULL
HRTransect5$Recipient <- NULL
HRTransect5$FFD <- NULL
HRTransect5$LFD <- NULL


# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 5 which implies the value is an outlier and removes that index from the original HRTransect5 data frame
z_scores <- as.data.frame(sapply(HRTransect5, function(HRTransect5) (abs(HRTransect5-mean(HRTransect5, na.rm = TRUE))/sd(HRTransect5, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect5[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect5[i,j] = NA
      }
    }
  }

HRTransect5 <- HRTransect5 %>% mutate(Population = PopulationV5 , Field_Year = Field_YearV5, Generation = GenerationV5, Block = BlockV5, Transect = TransectV5, Sequence = SequenceV5, Donor = DonorV5, Recipient = RecipientV5,FFD = FFDV5, LFD = LFDV5, .before = Left_Or_Right)

names(HRTransect5)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV6 <- HRTransect6$Population # Creates a vector of Population as charHRter variables
Field_YearV6 <- HRTransect6$Field_Year
GenerationV6 <- HRTransect6$Generation
BlockV6 <- HRTransect6$Block
TransectV6 <- HRTransect6$Transect
SequenceV6 <- HRTransect6$Sequence
DonorV6 <- HRTransect6$Donor
RecipientV6 <- HRTransect6$Recipient
FFDV6 <- HRTransect6$FFD
LFDV6 <- HRTransect6$LFD


# Remove values of these variables in HRTransect6 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect6$Population <-NULL
HRTransect6$Field_Year <- NULL
HRTransect6$Generation <- NULL
HRTransect6$Block <- NULL
HRTransect6$Transect <- NULL
HRTransect6$Sequence <- NULL
HRTransect6$Donor <- NULL
HRTransect6$Recipient <- NULL
HRTransect6$FFD <- NULL
HRTransect6$LFD <- NULL


# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 6 which implies the value is an outlier and removes that index from the original HRTransect6 data frame
z_scores <- as.data.frame(sapply(HRTransect6, function(HRTransect6) (abs(HRTransect6-mean(HRTransect6, na.rm = TRUE))/sd(HRTransect6, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect6[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect6[i,j] = NA
      }
    }
  }

HRTransect6 <- HRTransect6 %>% mutate(Population = PopulationV6 , Field_Year = Field_YearV6, Generation = GenerationV6, Block = BlockV6, Transect = TransectV6, Sequence = SequenceV6, Donor = DonorV6, Recipient = RecipientV6, FFD = FFDV6, LFD = LFDV6, .before = Left_Or_Right)


names(HRTransect6)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV7 <- HRTransect7$Population # Creates a vector of Population as charHRter variables
Field_YearV7 <- HRTransect7$Field_Year
GenerationV7 <- HRTransect7$Generation
BlockV7 <- HRTransect7$Block
TransectV7 <- HRTransect7$Transect
SequenceV7 <- HRTransect7$Sequence
DonorV7 <- HRTransect7$Donor
RecipientV7 <- HRTransect7$Recipient
FFDV7 <- HRTransect7$FFD
LFDV7 <- HRTransect7$LFD


# Remove values of these variables in HRTransect7 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect7$Population <-NULL
HRTransect7$Field_Year <- NULL
HRTransect7$Generation <- NULL
HRTransect7$Block <- NULL
HRTransect7$Transect <- NULL
HRTransect7$Sequence <- NULL
HRTransect7$Donor <- NULL
HRTransect7$Recipient <- NULL
HRTransect7$FFD <- NULL
HRTransect7$LFD <- NULL

# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 7 which implies the value is an outlier and removes that index from the original HRTransect1 data frame
z_scores <- as.data.frame(sapply(HRTransect7, function(HRTransect7) (abs(HRTransect7-mean(HRTransect7, na.rm = TRUE))/sd(HRTransect7, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect7[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect7[i,j] = NA
      }
    }
  }

HRTransect7 <- HRTransect7 %>% mutate(Population = PopulationV7 , Field_Year = Field_YearV7, Generation = GenerationV7, Block = BlockV7, Transect = TransectV7, Sequence = SequenceV7, Donor = DonorV7, Recipient = RecipientV7, FFD = FFDV7, LFD = LFDV7, .before = Left_Or_Right)


names(HRTransect7)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV8 <- HRTransect8$Population # Creates a vector of Population as charHRter variables
Field_YearV8 <- HRTransect8$Field_Year
GenerationV8 <- HRTransect8$Generation
BlockV8 <- HRTransect8$Block
TransectV8 <- HRTransect8$Transect
SequenceV8 <- HRTransect8$Sequence
DonorV8 <- HRTransect8$Donor
RecipientV8 <- HRTransect8$Recipient
FFDV8 <- HRTransect8$FFD
LFDV8 <- HRTransect8$LFD


# Remove values of these variables in HRTransect8 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect8$Population <-NULL
HRTransect8$Field_Year <- NULL
HRTransect8$Generation <- NULL
HRTransect8$Block <- NULL
HRTransect8$Transect <- NULL
HRTransect8$Sequence <- NULL
HRTransect8$Donor <- NULL
HRTransect8$Recipient <- NULL
HRTransect8$FFD <- NULL
HRTransect8$LFD <- NULL



# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 8 which implies the value is an outlier and removes that index from the original HRTransect data frame
z_scores <- as.data.frame(sapply(HRTransect8, function(HRTransect8) (abs(HRTransect8-mean(HRTransect8, na.rm = TRUE))/sd(HRTransect8, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect8[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect8[i,j] = NA
      }
    }
  }

HRTransect8 <- HRTransect8 %>% mutate(Population = PopulationV8 , Field_Year = Field_YearV8, Generation = GenerationV8, Block = BlockV8, Transect = TransectV8, Sequence = SequenceV8, Donor = DonorV8, Recipient = RecipientV8, FFD = FFDV8, LFD = LFDV8, .before = Left_Or_Right)


names(HRTransect8)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
PopulationV9 <- HRTransect9$Population # Creates a vector of Population as charHRter variables
Field_YearV9 <- HRTransect9$Field_Year
GenerationV9 <- HRTransect9$Generation
BlockV9 <- HRTransect9$Block
TransectV9 <- HRTransect9$Transect
SequenceV9 <- HRTransect9$Sequence
DonorV9 <- HRTransect9$Donor
RecipientV9 <- HRTransect9$Recipient
FFDV9 <- HRTransect9$FFD
LFDV9 <- HRTransect9$LFD


# Remove values of these variables in HRTransect9 (the data frame that contains the variables for which we want to get rid of the outliers)
HRTransect9$Population <-NULL
HRTransect9$Field_Year <- NULL
HRTransect9$Generation <- NULL
HRTransect9$Block <- NULL
HRTransect9$Transect <- NULL
HRTransect9$Sequence <- NULL
HRTransect9$Donor <- NULL
HRTransect9$Recipient <- NULL
HRTransect9$FFD <- NULL
HRTransect9$LFD <- NULL



# z-scores are calculated and the for loop goes through eHRh element of the z-score array, looking for values above 9 which implies the value is an outlier and removes that index from the original HRTransect9 data frame
z_scores <- as.data.frame(sapply(HRTransect9, function(HRTransect9) (abs(HRTransect9-mean(HRTransect9, na.rm = TRUE))/sd(HRTransect9, na.rm = TRUE))))
for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      HRTransect9[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      HRTransect9[i,j] = NA
      }
    }
  }

HRTransect9 <- HRTransect9 %>% mutate(Population = PopulationV9 , Field_Year = Field_YearV9, Generation = GenerationV9, Block = BlockV9, Transect = TransectV9, Sequence = SequenceV9, Donor = DonorV9, Recipient = RecipientV9, FFD = FFDV9, LFD = LFDV9, .before = Left_Or_Right)


names(HRTransect9)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```


```r
# Mean-centering the data frame for Transect 1

names(HRTransect1)
```

```
##  [1] "Population"               "Field_Year"               "Generation"              
##  [4] "Block"                    "Transect"                 "Sequence"                
##  [7] "Donor"                    "Recipient"                "FFD"                     
## [10] "LFD"                      "Left_Or_Right"            "Plant_ID"                
## [13] "Total_Closed_Fruits"      "Total_Fruits"             "Tot_Seed_Num(ClosedFt)"  
## [16] "Mean_Ind_Seed_Mass_mg"    "Mean_Seeds_per_Fruit"     "Lifetime_Fecundity"      
## [19] "Stem_Biomass"             "Corolla_Diameter"         "Corolla_Area"            
## [22] "Leaf_Area_mm2"            "fl_duration"              "Flowering_Duration"      
## [25] "Log_Total_Fruits"         "Log_Mean_Seeds_per_Fruit" "Log_Lifetime_Fecundity"  
## [28] "Log_Stem_Biomass"         "Log_Leaf_Area_mm2"
```

```r
# MC = mean-centered

HRTransect1 <- HRTransect1 %>% mutate(
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

View(HRTransect1)

# Check the mean, min, and max for one of the variables to make sure that they make sense.  Must add "na.rm=TRUE".

mean(HRTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 10.70455
```

```r
min(HRTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 28
```

```r
mean(HRTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -3.230306e-16
```

```r
min(HRTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -10.70455
```

```r
max(HRTransect1$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 17.29545
```

```r
 HRTransect2 <- HRTransect2 %>% mutate(
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

mean(HRTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 9.866667
```

```r
min(HRTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect2$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 33
```

```r
mean(HRTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -4.743071e-16
```

```r
min(HRTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -9.866667
```

```r
max(HRTransect2$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 23.13333
```

```r
 HRTransect3 <- HRTransect3 %>% mutate(
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

mean(HRTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 27.02222
```

```r
min(HRTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect3$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 48
```

```r
mean(HRTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 7.896847e-17
```

```r
min(HRTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -27.02222
```

```r
max(HRTransect3$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 20.97778
```

```r
HRTransect4 <- HRTransect4 %>% mutate(
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

mean(HRTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 24.41304
```

```r
min(HRTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect4$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 54
```

```r
mean(HRTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -1.390216e-15
```

```r
min(HRTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -24.41304
```

```r
max(HRTransect4$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 29.58696
```

```r
HRTransect5 <- HRTransect5 %>% mutate(
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

mean(HRTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 22.22222
```

```r
min(HRTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect5$Flowering_Duration, na.rm=TRUE)
```

```
## [1] 48
```

```r
mean(HRTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 7.891064e-16
```

```r
min(HRTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] -22.22222
```

```r
max(HRTransect5$Flowering_Duration_MC, na.rm=TRUE)
```

```
## [1] 25.77778
```

```r
HRTransect6 <- HRTransect6 %>% mutate(
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

mean(HRTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 310.7955
```

```r
min(HRTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 95.902
```

```r
max(HRTransect6$Corolla_Area, na.rm=TRUE)
```

```
## [1] 468.014
```

```r
mean(HRTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -1.492226e-14
```

```r
min(HRTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -214.8935
```

```r
max(HRTransect6$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 157.2185
```

```r
HRTransect7 <- HRTransect7 %>% mutate(
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

mean(HRTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 320.9119
```

```r
min(HRTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 185.362
```

```r
max(HRTransect7$Corolla_Area, na.rm=TRUE)
```

```
## [1] 486.078
```

```r
mean(HRTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -2.094268e-14
```

```r
min(HRTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] -135.5499
```

```r
max(HRTransect7$Corolla_Area_MC, na.rm=TRUE)
```

```
## [1] 165.1661
```

```r
HRTransect8 <- HRTransect8 %>% mutate(
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

mean(HRTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 21.50477
```

```r
min(HRTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 12.51
```

```r
max(HRTransect8$Corolla_Diameter, na.rm=TRUE)
```

```
## [1] 27.064
```

```r
mean(HRTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] -4.553649e-17
```

```r
min(HRTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] -8.994769
```

```r
max(HRTransect8$Corolla_Diameter_MC, na.rm=TRUE)
```

```
## [1] 5.559231
```

```r
HRTransect9 <- HRTransect9 %>% mutate(
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

View(HRTransect9)

# Let's take a look at the mean, minimum and maximum of the number of closed fruits for both the raw values and the mean-centered values, for Transect 9.


mean(HRTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 13.11111
```

```r
min(HRTransect9$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 0
```

```r
max(HRTransect1$Total_Closed_Fruits, na.rm=TRUE)
```

```
## [1] 28
```

```r
mean(HRTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 3.944376e-16
```

```r
min(HRTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] -13.11111
```

```r
max(HRTransect9$Total_Closed_Fruits_MC, na.rm=TRUE)
```

```
## [1] 26.88889
```


```r
Recipients <- c(HRTransect1$Recipient, HRTransect2$Recipient, HRTransect3$Recipient, HRTransect4$Recipient, HRTransect5$Recipient, HRTransect6$Recipient, HRTransect7$Recipient, HRTransect8$Recipient, HRTransect9$Recipient)

# Now, modify this vector so that eHRh recipient occurs only once

Recipients <- unique(Recipients) # Names of maternal ID's, without repetition (n=107)

str(Recipients)
```

```
##  chr [1:137] "HR_058" "HR_183" "HR_176" "HR_146" "HR_177" "HR_115" "HR_036" "HR_033" ...
```

```r
length(Recipients)
```

```
## [1] 137
```

```r
# Create a vector that includes ONLY the variables that we want in the final unified dataframe that contains the data for all of the transects.  We're going to include the raw, untransformed values for eHRh trait and the mean-centered values (some of which are based on log-transformed values)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "FFD_MC", "LFD_MC", "Flowering_Duration_MC")

# Combine transects to get mean-centered population data for easy averaging of same maternal IDs

HR_MeanCentered_AllTransects <- rbind(HRTransect1,HRTransect2,HRTransect3,HRTransect4,HRTransect5,HRTransect6,HRTransect7,HRTransect8, HRTransect9) # This contains 41 variables

HR_MeanCentered_AllTransects <- HR_MeanCentered_AllTransects[Variables] # Includes only the variables in the "Variables" vector = 35 variables

# Now, let's summarize the data, using the means of the rows representing a given recipient (= Maternal ID) and averaging eHRh maternal ID's values while ignoring NA values. For recipients for which a trait has  values that are ALL NA, this will return a "NaN" for that recipient and trait.

# this code creates a new data frame with the MEAN VALUES for EHRH RECIPIENT and TRAIT

# In other words, these are the means for the mean-centered trait values of eHRh maternal genotype

HR_Avg_MC_Population_ByRecip <- HR_MeanCentered_AllTransects %>%
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

View(HR_Avg_MC_Population_ByRecip)

Donors <- c(HRTransect1$Donor, HRTransect2$Donor, HRTransect3$Donor, HRTransect4$Donor, HRTransect5$Donor, HRTransect6$Donor, HRTransect7$Donor, HRTransect8$Donor, HRTransect9$Donor)

Donors <- unique(Donors) # Names of Paternal ID's, without repetition (n=40)

str(Donors)
```

```
##  chr [1:46] "HR_008" "HR_196" "HR_085" "HR_006" "HR_108" "HR_074" "HR_180" "HR_160" "HR_070" ...
```

```r
length(Donors)
```

```
## [1] 46
```

```r
# Create a vector that includes only the variables that we want in the final unified dataframe that contains the data for all of the transects.  We're going to include the raw, untransformed values for eHRh trait and the mean-centered values (some of which are based on log-transformed values)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Transect", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "Flowering_Duration_MC")

# Combine transects bHRk together to get mean-centered population data for easy averaging of same paternal IDs

HR_MC_Population <- rbind(HRTransect1,HRTransect2,HRTransect3,HRTransect4,HRTransect5,HRTransect6,HRTransect7,HRTransect8, HRTransect9)

HR_MC_Population <- HR_MC_Population[Variables] # Includes only the variables in the "Variables" vector.

# This summarizes the data, using the means of the rows representing a given recipient (= Maternal ID) and averaging eHRh maternal ID's values while ignoring NA values. For recipients for which a trait has  values that are ALL NA, this will return a "NaN" for that recipient and trait.

# this code creates a new data frame with the MEAN VALUES for EHRH RECIPIENT and TRAIT

# In other words, these are the means for the mean-centered trait values of eHRh maternal genotype

HR_Avg_MC_Population_ByDonor <- HR_MC_Population %>%
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

View(HR_Avg_MC_Population_ByDonor)
```


```r
# Deal with NaN values, replHRing NaN with "NA"

for(i in 1:nrow(HR_Avg_MC_Population_ByRecip)){
  for(j in 1:ncol(HR_Avg_MC_Population_ByRecip)){
    if(HR_Avg_MC_Population_ByRecip[i,j] == "NaN"){
      HR_Avg_MC_Population_ByRecip[i,j] <- NA
    }
  }
}

for(i in 1:nrow(HR_Avg_MC_Population_ByDonor)){
  for(j in 1:ncol(HR_Avg_MC_Population_ByDonor)){
    if(HR_Avg_MC_Population_ByDonor[i,j] == "NaN"){
      HR_Avg_MC_Population_ByDonor[i,j] <- NA
    }
  }
}
```
