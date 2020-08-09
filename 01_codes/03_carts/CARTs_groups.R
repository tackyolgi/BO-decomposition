library(DescTools)
library(tidyverse)
library(modeest)

# H group
# Sector: Manufacturing, Region: transdanubian, property: foreign majority (1,2), education: primary and lower secondary (1-2)

# L group:
# Region: central, Sector: not-manufacturing, firm size: 1-2, education: higher secondary and tertiary (3-4), no foreign property (4)
# Size 1,2



setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2016.RData")

source("C:/Users/tacky/OneDrive - Corvinus University of Budapest/Bertarifa kutatas/bertarifa/codes/CARTs_csoportok_fun03.R")


csak_nok <- subset(minta$train, minta$train$nem == "0")
pivot <- kimutatas(csak_nok)

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results")
write.csv(pivot, "pivot_2016.csv")


level_data_H1 <- subset(minta$train, minta$train$nem =="0" & 
                         minta$train$exp_kib_class == 2 &
                         minta$train$kshreg_3 == 2 &
                         (minta$train$kra_ordered == 1 | minta$train$kra_ordered ==2) &
                         (minta$train$iskveg4_ordered == 1 | minta$train$iskveg4_ordered ==2)
                       )

level_data_L3 <- subset(minta$train, minta$train$nem == "0" & 
                         minta$train$exp_kib_class != 2 &
                         minta$train$kshreg_3 == 1 &
                         minta$train$kra_ordered == 4 &
                         (minta$train$vallmeret == 1 | minta$train$vallmeret==2) &
                         (minta$train$iskveg4_ordered == 3 | minta$train$iskveg4_ordered ==4)
                       )


pivot_H1 <- kimutatas(level_data_H1)

pivot_L1 <- kimutatas(level_data_L3)


write.csv(pivot_H1, "pivot_H_2016.csv")
write.csv(pivot_L1, "pivot_L_2016.csv")
