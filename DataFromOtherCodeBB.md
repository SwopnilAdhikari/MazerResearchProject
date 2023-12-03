---
title: "DataFromOtherCodeBB"
output: html_document
date: "2023-11-21"
---


```r
install.packages("jtools")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("lme4")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("lmerTest")
```

```
## Error in install.packages : Updating loaded packages
```

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

BB_Data_2022 <- read_csv("~/MazerResearchProject/Data/BB_Data_2022.csv")
```

```
## Rows: 369 Columns: 23
## ── Column specification ────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (6): Population, Generation, Donor, Recipient, FFD, LFD
## dbl (16): Field_Year, Block, Location, Sequence, Plant_ID, Total_Closed_Fruits, Total_Fruits...
## lgl  (1): Left_Or_Right
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
view(BB_Data_2022)
```


```r
BB_Data_2022 <- BB_Data_2022 %>% mutate(FFD = yday(mdy(FFD)), LFD = yday(mdy(LFD)), Flowering_Duration = LFD-FFD) 

BB_Data_2022$Flowering_Duration - BB_Data_2022$fl_duration
```

```
##   [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [31]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##  [61]  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0
##  [91]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [121]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0 NA NA  0
## [151]  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0
## [181]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [211]  0  0  0  0  0  0  0  0  0 NA NA  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0
## [241]  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA NA  0  0  0  0 NA  0
## [271]  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [301]  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0  0  0  0  0  0
## [331]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0
## [361]  0  0  0  0  0  0  0  0  0
```

```r
min(BB_Data_2022$Total_Fruits, na.rm=TRUE) 
```

```
## [1] 0
```

```r
min(BB_Data_2022$Mean_Seeds_per_Fruit, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BB_Data_2022$Lifetime_Fecundity, na.rm=TRUE)
```

```
## [1] 0
```

```r
min(BB_Data_2022$Stem_Biomass, na.rm=TRUE)
```

```
## [1] 1.93
```

```r
min(BB_Data_2022$Leaf_Area_mm2, na.rm=TRUE)
```

```
## [1] 9.523
```

```r
BB_Data_2022 <- BB_Data_2022 %>% mutate(Log_Total_Fruits=log(Total_Fruits+1), Log_Mean_Seeds_per_Fruit=log(Mean_Seeds_per_Fruit+1), Log_Lifetime_Fecundity=log(Lifetime_Fecundity+1), Log_Stem_Biomass=log(Stem_Biomass), Log_Leaf_Area_mm2=log(Leaf_Area_mm2))

BBBlock1 <- subset(BB_Data_2022, Block == 1)
BBBlock2 <- subset(BB_Data_2022, Block == 2)
BBBlock3 <- subset(BB_Data_2022, Block == 3)
```


```r
PopulationV1 <- BBBlock1$Population 
Field_YearV1 <- BBBlock1$Field_Year
GenerationV1 <- BBBlock1$Generation
BlockV1 <- BBBlock1$Block
SequenceV1 <- BBBlock1$Sequence
DonorV1 <- BBBlock1$Donor
RecipientV1 <- BBBlock1$Recipient
FFDV1 <- BBBlock1$FFD
LFDV1 <- BBBlock1$LFD

BBBlock1$Population <-NULL
BBBlock1$Field_Year <- NULL
BBBlock1$Generation <- NULL
BBBlock1$Block <- NULL
BBBlock1$Sequence <- NULL
BBBlock1$Donor <- NULL
BBBlock1$Recipient <- NULL
BBBlock1$FFD <- NULL
BBBlock1$LFD <- NULL

z_scores <- as.data.frame(sapply(BBBlock1, function(BBBlock1) (abs(BBBlock1-mean(BBBlock1, na.rm = TRUE))/sd(BBBlock1, na.rm = TRUE))))

class(z_scores)
```

```
## [1] "data.frame"
```

```r
View(z_scores)

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BBBlock1[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BBBlock1[i,j] = NA
      }
    }
  }

BBBlock1 <- BBBlock1 %>% mutate(Population = PopulationV1 , Field_Year = Field_YearV1, Generation = GenerationV1, Block = BlockV1, Sequence = SequenceV1, Donor = DonorV1, Recipient = RecipientV1, FFD = FFDV1, LFD = LFDV1, .before = Left_Or_Right, )

print(BBBlock1$Sequence)
```

```
##   [1]   1   2   3   4   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23
##  [23]  24  25  26  27  28  29  30  31  32  33  35  36  37  38  39  40  41  42  43  44  45  46
##  [45]  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
##  [67]  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
##  [89]  91  92  93  94  95  97  98  99 100 101 102 103 104 106 107 108 109 111 112 113 114 115
## [111] 116 117 118 119 120 122 123 124 125 127 128 129 131 134 135 136 137 138 139 140 141 142
## [133] 143 144 145 146 147
```

```r
PopulationV2 <- BBBlock2$Population 
Field_YearV2 <- BBBlock2$Field_Year
GenerationV2 <- BBBlock2$Generation
BlockV2 <- BBBlock2$Block
SequenceV2 <- BBBlock2$Sequence
DonorV2 <- BBBlock2$Donor
RecipientV2 <- BBBlock2$Recipient
FFDV2 <- BBBlock2$FFD
LFDV2 <- BBBlock2$LFD

BBBlock2$Population <-NULL
BBBlock2$Field_Year <- NULL
BBBlock2$Generation <- NULL
BBBlock2$Block <- NULL
BBBlock2$Sequence <- NULL
BBBlock2$Donor <- NULL
BBBlock2$Recipient <- NULL
BBBlock2$FFD <- NULL
BBBlock2$LFD <- NULL

z_scores <- as.data.frame(sapply(BBBlock2, function(BBBlock2) (abs(BBBlock2-mean(BBBlock2, na.rm = TRUE))/sd(BBBlock2, na.rm = TRUE))))

class(z_scores)
```

```
## [1] "data.frame"
```

```r
View(z_scores)

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BBBlock2[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BBBlock2[i,j] = NA
      }
    }
  }

BBBlock2 <- BBBlock2 %>% mutate(Population = PopulationV2 , Field_Year = Field_YearV2, Generation = GenerationV2, Block = BlockV2, Sequence = SequenceV2, Donor = DonorV2, Recipient = RecipientV2, FFD = FFDV2, LFD = LFDV2, .before = Left_Or_Right, )

print(BBBlock2$Sequence)
```

```
##   [1] 148 151 152 153 154 155 156 157 158 159 161 162 163 164 166 167 168 170 173 174 178 181
##  [23] 183 185 186 187 188 189 190 191 192 193 194 195 197 199 200 202 203 204 205 206 207 208
##  [45] 210 211 212 213 214 215 216 218 219 220 223 224 225 226 227 228 230 231 232 233 234 235
##  [67] 236 237 238 239 240 241 242 243 244 253 254 255 257 260 262 263 267 269 271 272 274 275
##  [89] 276 277 278 279 280 283 285 287 288 289 290 291 292 293 294
```

```r
PopulationV3 <- BBBlock3$Population 
Field_YearV3 <- BBBlock3$Field_Year
GenerationV3 <- BBBlock3$Generation
BlockV3 <- BBBlock3$Block
SequenceV3 <- BBBlock3$Sequence
DonorV3 <- BBBlock3$Donor
RecipientV3 <- BBBlock3$Recipient
FFDV3 <- BBBlock3$FFD
LFDV3 <- BBBlock3$LFD

BBBlock3$Population <-NULL
BBBlock3$Field_Year <- NULL
BBBlock3$Generation <- NULL
BBBlock3$Block <- NULL
BBBlock3$Sequence <- NULL
BBBlock3$Donor <- NULL
BBBlock3$Recipient <- NULL
BBBlock3$FFD <- NULL
BBBlock3$LFD <- NULL

z_scores <- as.data.frame(sapply(BBBlock3, function(BBBlock3) (abs(BBBlock3-mean(BBBlock3, na.rm = TRUE))/sd(BBBlock3, na.rm = TRUE))))

class(z_scores)
```

```
## [1] "data.frame"
```

```r
View(z_scores)

for(i in 1:nrow(z_scores)){
  for(j in 1:ncol(z_scores)){
    if(is.na(z_scores[i,j])){
      BBBlock3[i,j] = NA
    }
      else if(z_scores[i,j] > 3){
      BBBlock3[i,j] = NA
      }
    }
  }

BBBlock3 <- BBBlock3 %>% mutate(Population = PopulationV3 , Field_Year = Field_YearV3, Generation = GenerationV3, Block = BlockV3, Sequence = SequenceV3, Donor = DonorV3, Recipient = RecipientV3, FFD = FFDV3, LFD = LFDV3, .before = Left_Or_Right, )

print(BBBlock3$Sequence)
```

```
##   [1] 295 296 300 302 303 304 305 306 307 308 309 310 311 313 314 315 316 317 318 320 321 323
##  [23] 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345
##  [45] 346 347 348 349 350 351 352 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368
##  [67] 369 371 372 373 374 375 376 377 379 380 381 382 383 384 385 386 388 389 390 391 393 394
##  [89] 395 396 397 398 399 400 401 402 403 404 405 406 409 410 411 412 413 414 415 416 417 420
## [111] 421 423 424 425 426 428 429 430 431 432 433 434 435 436 437 438 439 440 441
```


```r
names(BBBlock1)
```

```
##  [1] "Location"                 "Population"               "Field_Year"              
##  [4] "Generation"               "Block"                    "Sequence"                
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
BBBlock1 <- BBBlock1 %>% mutate(
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

View(BBBlock1)

 BBBlock2 <- BBBlock2 %>% mutate(
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
 
  BBBlock3 <- BBBlock3 %>% mutate(
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
```


```r
Recipients <- c(BBBlock1$Recipient, BBBlock2$Recipient, BBBlock3$Recipient)
Recipients <- unique(Recipients)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "FFD_MC", "LFD_MC", "Flowering_Duration_MC")

BB_MeanCentered_AllBlocks <- rbind(BBBlock1,BBBlock2,BBBlock3)

BB_MeanCentered_AllBlocks <- BB_MeanCentered_AllBlocks[Variables]
BB_Avg_MC_Population_ByRecip <- BB_MeanCentered_AllBlocks %>%
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

View(BB_Avg_MC_Population_ByRecip)

Donors <- c(BBBlock1$Donor, BBBlock2$Donor, BBBlock3$Donor)
Donors <- unique(Donors)

Variables <- c("Population", "Field_Year", "Generation", "Block", "Sequence", "Plant_ID", "Donor", "Recipient", "FFD", "LFD", "Total_Fruits", "Mean_Ind_Seed_Mass_mg", "Mean_Seeds_per_Fruit", "Lifetime_Fecundity", "Stem_Biomass", "Corolla_Diameter", "Corolla_Area", "Leaf_Area_mm2", "Flowering_Duration", "Log_Total_Fruits", "Log_Mean_Seeds_per_Fruit", "Log_Lifetime_Fecundity", "Log_Stem_Biomass", "Log_Total_Fruits_MC", "Mean_Ind_Seed_Mass_mg_MC", "Log_Mean_Seeds_per_Fruit_MC", "Log_Lifetime_Fecundity_MC", "Log_Stem_Biomass_MC", "Corolla_Diameter_MC", "Corolla_Area_MC", "Log_Leaf_Area_mm2_MC", "FFD_MC", "LFD_MC", "Flowering_Duration_MC")

BB_MC_Population <- rbind(BBBlock1,BBBlock2,BBBlock3)

BB_MC_Population <- BB_MC_Population[Variables]

BB_Avg_MC_Population_ByDonor <- BB_MC_Population %>%
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

View(BB_Avg_MC_Population_ByDonor)
```
