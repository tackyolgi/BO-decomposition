# we deleted firms with fewer than 20 employees
library(haven)

 data_08 <- read_dta("./data/tarifa2008_b2.dta")
 data_09 <- read_dta("./data/tarifa2009_b2.dta")
 data_10 <- read_dta("./data/tarifa2010_b2.dta")
 data_11 <- read_dta("./data/tarifa2011_b2.dta")
 data_12 <- read_dta("./data/tarifa2012_b2.dta")
 data_13 <- read_dta("./data/tarifa2013_b2.dta")
 data_14 <- read_dta("./data/tarifa2014_b2.dta")
 data_15 <- read_dta("./data/tarifa2015_b2_A.dta")
 data_16 <- read_dta("./data/tarifa2016_b2_A.dta")
 

 write.csv(data_08, "./data/tarifa2008.csv")
 write.csv(data_09, "./data/tarifa2009.csv")
 write.csv(data_10, "./data/tarifa2010.csv")
 write.csv(data_11, "./data/tarifa2011.csv")
 write.csv(data_12, "./data/tarifa2012.csv")
 write.csv(data_13, "./data/tarifa2013.csv")
 write.csv(data_14, "./data/tarifa2014.csv")
 write.csv(data_15, "./data/tarifa2015.csv")
 write.csv(data_16, "./data/tarifa2016.csv")

 data_08 <- read.csv("./data/tarifa2008.csv")
 data_09 <- read.csv("./data/tarifa2009.csv")
 data_10 <- read.csv("./data/tarifa2010.csv")
 data_11 <- read.csv("./data/tarifa2011.csv")
 data_12 <- read.csv("./data/tarifa2012.csv")
 data_13 <- read.csv("./data/tarifa2013.csv")
 data_14 <- read.csv("./data/tarifa2014.csv")
 data_15 <- read.csv("./data/tarifa2015.csv")
 data_16 <- read.csv("./data/tarifa2016.csv")
# private sector and full time employees
tarifred_08 <- subset(data_08, fforma==1 & atip==4)
tarifred_09 <- subset(data_09, fforma==1 & atip==4)
tarifred_10 <- subset(data_10, fforma==1 & atip==4)
tarifred_11 <- subset(data_11, fforma==1 & atip==4)
tarifred_12 <- subset(data_12, fforma==1 & atip==4)
tarifred_13 <- subset(data_13, fforma==1 & atip==4)
tarifred_14 <- subset(data_14, fforma==1 & atip==4)
tarifred_15 <- subset(data_15, fforma==1 & atip==4)
tarifred_16 <- subset(data_16, fforma==1 & atip==4)

# age squared
tarifred_08$kor2 <- tarifred_08$kor^2
tarifred_09$kor2 <- tarifred_09$kor^2
tarifred_10$kor2 <- tarifred_10$kor^2
tarifred_11$kor2 <- tarifred_11$kor^2
tarifred_12$kor2 <- tarifred_12$kor^2
tarifred_13$kor2 <- tarifred_13$kor^2
tarifred_14$kor2 <- tarifred_14$kor^2
tarifred_15$kor2 <- tarifred_15$kor^2
tarifred_16$kor2 <- tarifred_16$kor^2

# female distribution in 2 digit isco
source('./01_codes/01_data loading/female_percent_in_feor.R', encoding = 'UTF-8')
library(dplyr)

tarifred_08 <- get_female_percent_in_feor97(data_08, tarifred_08)
tarifred_09 <- get_female_percent_in_feor97(data_09, tarifred_09)
tarifred_10 <- get_female_percent_in_feor97(data_10, tarifred_10)
tarifred_11 <- get_female_percent_in_feor08(data_11, tarifred_11)
tarifred_12 <- get_female_percent_in_feor08(data_12, tarifred_12)
tarifred_13 <- get_female_percent_in_feor08(data_13, tarifred_13)
tarifred_14 <- get_female_percent_in_feor08(data_14, tarifred_14)
tarifred_15 <- get_female_percent_in_feor08(data_15, tarifred_15)
tarifred_16 <- get_female_percent_in_feor08(data_16, tarifred_16)

# excluding NA-s
source("./01_codes/01_data loading/torles.R")
library(stringr)

tarifred_08 <- delete_missing_obs(tarifred_08)
tarifred_09 <- delete_missing_obs(tarifred_09)
tarifred_10 <- delete_missing_obs(tarifred_10)
tarifred_11 <- delete_missing_obs(tarifred_11)
tarifred_12 <- delete_missing_obs(tarifred_12)
tarifred_13 <- delete_missing_obs(tarifred_13)
tarifred_14 <- delete_missing_obs(tarifred_14)
tarifred_15 <- delete_missing_obs(tarifred_15)
tarifred_16 <- delete_missing_obs(tarifred_16)

# factors
source("./01_codes/01_data loading/faktorizalas_ordered.R")
tarifred_08 <- get_factors(tarifred_08)
tarifred_09 <- get_factors(tarifred_09)
tarifred_10 <- get_factors(tarifred_10)
tarifred_11 <- get_factors(tarifred_11)
tarifred_12 <- get_factors(tarifred_12)
tarifred_13 <- get_factors(tarifred_13)
tarifred_14 <- get_factors(tarifred_14)
tarifred_15 <- get_factors(tarifred_15)
tarifred_16 <- get_factors(tarifred_16)


obs_num <- 50000
source("./01_codes/02_estimations/becslesek.R", encoding = 'UTF-8')

minta_08 <- get_sample(tarifred_08, obs_num)
saveRDS(minta_08, "./data/minta_08.RData")
saveRDS(tarifred_08, "./data/tarifred_08.RData")

minta_09 <- get_sample(tarifred_09, obs_num)
saveRDS(minta_09, "./data/minta_09.RData")
saveRDS(tarifred_09, "./data/tarifred_09.RData")

minta_10 <- get_sample(tarifred_10, obs_num)
saveRDS(minta_10, "./data/minta_10.RData")
saveRDS(tarifred_10, "./data/tarifred_10.RData")

minta_11 <- get_sample(tarifred_11, obs_num)
saveRDS(minta_11, "./data/minta_11.RData")
saveRDS(tarifred_11, "./data/tarifred_11.RData")

minta_12 <- get_sample(tarifred_12, obs_num)
saveRDS(minta_12, "./data/minta_12.RData")
saveRDS(tarifred_12, "./data/tarifred_12.RData")

minta_13 <- get_sample(tarifred_13, obs_num)
saveRDS(minta_13, "./data/minta_13.RData")
saveRDS(tarifred_13, "./data/tarifred_13.RData")

minta_14 <- get_sample(tarifred_14, obs_num)
saveRDS(minta_14, "./data/minta_14.RData")
saveRDS(tarifred_14, "./data/tarifred_14.RData")

minta_15 <- get_sample(tarifred_15, obs_num)
saveRDS(minta_15, "./data/minta_15.RData")
saveRDS(tarifred_15, "./data/tarifred_15.RData")

minta_16 <- get_sample(tarifred_16, obs_num)
saveRDS(minta_16, "./data/minta_16.RData")
saveRDS(tarifred_16, "./data/tarifred_16.RData")



