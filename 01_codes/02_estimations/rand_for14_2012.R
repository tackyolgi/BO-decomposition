# I run RF and Ols estimations with this code for every year separately

library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(oaxaca)
library(randomForestSRC)
library(stringi)

minta <- readRDS("./data/minta_12.RData")

# parameters
# random forest
num_tree <- 1000     # ntree
num_vars <- 5      # mtry
num_nodes <- 5    # nodesize

source('./01_codes/02_estimations/becslesek.R', encoding = 'UTF-8')


# estimations
set.seed(mean(minta$train$ev))
B_regr_ordered <- get_B_oax_regr_ordered(minta$train) # rerun after RF models in 2015

B_RF_female_ordered <- get_B_oax_RF_female_ordered(minta$train, num_vars, num_nodes, num_tree)
B_RF_male_ordered <- get_B_oax_RF_male_ordered(minta$train, num_vars, num_nodes, num_tree)

saveRDS(B_regr_ordered, file="./models/B_regr_ordered_2012.RData")
saveRDS(B_RF_female_ordered, file="./models/B_RF_female_ordered_2012.RData")
saveRDS(B_RF_male_ordered, file="./models/B_RF_male_ordered_2012.RData")

# unexplaind part
# RF
# RF predict
minta$train$pred_B_RF_subgr_ordered[minta$train$nem==0] <- predict.rfsrc(B_RF_female_ordered, subset(minta$train, nem==0))$predicted
minta$train$pred_B_RF_subgr_ordered[minta$train$nem==1] <- predict.rfsrc(B_RF_male_ordered, subset(minta$train, nem==1))$predicted
minta$test$pred_B_RF_subgr_ordered[minta$test$nem==0] <- predict.rfsrc(B_RF_female_ordered, subset(minta$test, nem==0))$predicted
minta$test$pred_B_RF_subgr_ordered[minta$test$nem==1] <- predict.rfsrc(B_RF_male_ordered, subset(minta$test, nem==1))$predicted

minta$train$pred_B_RF_subgr_ordered_male <- predict.rfsrc(B_RF_male_ordered, minta$train)$predicted
minta$test$pred_B_RF_subgr_ordered_male <- predict.rfsrc(B_RF_male_ordered, minta$test)$predicted

# differencies (pm(M)-pr(M)+pr(F)-pf(F)) male model is the reference model
# diff in RF 
minta$train$pred_diff_B_RF_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_RF_subgr_ordered- 
                                                                     minta$train$pred_B_RF_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_RF_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_RF_subgr_ordered_male
                                                                   -minta$train$pred_B_RF_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_RF_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_RF_subgr_ordered
                                                                 - minta$test$pred_B_RF_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_RF_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_RF_subgr_ordered_male
                                                                 -minta$test$pred_B_RF_subgr_ordered,
                                                                 minta$test$nem==0)

# basic information
# train
# raw difference
raw <- mean(subset(minta$train, nem==1)$lnker)-mean(subset(minta$train, nem==0)$lnker)

# explained part
expl <- mean(subset(minta$train$pred_B_RF_subgr_ordered_male, minta$train$nem==1))-
  mean(subset(minta$train$pred_B_RF_subgr_ordered_male, minta$train$nem==0))


# unexplained part
unexpl <- mean(subset(minta$train$pred_B_RF_subgr_ordered_male, minta$train$nem==0))-
  mean(subset(minta$train$pred_B_RF_subgr_ordered,minta$train$nem==0))

# results
results_RF <- as.data.frame(rbind(results_RF, cbind( ev=mean(minta$train$ev), nyers=round(raw,4), 
                             magyarazott=round(expl,4), nem_magyarazott=round(unexpl,4), 
                                                     kulonbseg=round(raw-expl-unexpl,4))))

# test
# raw difference
raw <- mean(subset(minta$test, nem==1)$lnker)-mean(subset(minta$test, nem==0)$lnker)

# explained
expl <- mean(subset(minta$test$pred_B_RF_subgr_ordered_male, minta$test$nem==1))-
  mean(subset(minta$test$pred_B_RF_subgr_ordered_male, minta$test$nem==0))


# unexplained
unexpl <- mean(subset(minta$test$pred_B_RF_subgr_ordered_male, minta$test$nem==0))-
  mean(subset(minta$test$pred_B_RF_subgr_ordered,minta$test$nem==0))

# results
results_RF <- as.data.frame(rbind(results_RF, (cbind( ev=mean(minta$test$ev), nyers=round(raw,4), 
                                                      magyarazott=round(expl,4), nem_magyarazott=round(unexpl,4), 
                                                      kulonbseg=round(raw-expl-unexpl,4)))))

 
# save
saveRDS(minta, file="./data/minta_2012.RData")


# size categorization
minta$train$vallmeret <- minta$train$letszam_bv1
minta$test$vallmeret <- minta$test$letszam_bv1

minta$train$vallmeret[minta$train$letszam_bv1>=20 & minta$train$letszam_bv1<50] <-1
minta$train$vallmeret[minta$train$letszam_bv1>=50 & minta$train$letszam_bv1<150] <-2
minta$train$vallmeret[minta$train$letszam_bv1>=150 & minta$train$letszam_bv1<500] <-3
minta$train$vallmeret[minta$train$letszam_bv1>=500] <-4

minta$test$vallmeret[minta$test$letszam_bv1>=20 & minta$test$letszam_bv1<50] <-1
minta$test$vallmeret[minta$test$letszam_bv1>=50 & minta$test$letszam_bv1<150] <-2
minta$test$vallmeret[minta$test$letszam_bv1>=150 & minta$test$letszam_bv1<500] <-3
minta$test$vallmeret[minta$test$letszam_bv1>=500] <-4

minta$train$vallmeret <- factor(minta$train$vallmeret, ordered=TRUE)
minta$test$vallmeret <- factor(minta$test$vallmeret, ordered=TRUE)


# age categorization
minta$train$kor_csop[minta$train$kor<25]<-1
minta$train$kor_csop[minta$train$kor>24 & minta$train$kor<35]<-2
minta$train$kor_csop[minta$train$kor>34 & minta$train$kor<45]<-3
minta$train$kor_csop[minta$train$kor>44 & minta$train$kor<55]<-4
minta$train$kor_csop[minta$train$kor>54]<-5

minta$test$kor_csop[minta$test$kor<25]<-1
minta$test$kor_csop[minta$test$kor>24 & minta$test$kor<35]<-2
minta$test$kor_csop[minta$test$kor>34 & minta$test$kor<45]<-3
minta$test$kor_csop[minta$test$kor>44 & minta$test$kor<55]<-4
minta$test$kor_csop[minta$test$kor>54]<-5

minta$train$kor_csop <- factor(minta$train$kor_csop, ordered=TRUE)
minta$test$kor_csop <- factor(minta$test$kor_csop, ordered=TRUE)

# education
minta$train$iskveg4_ordered <- minta$train$iskveg5
minta$test$iskveg4_ordered <- minta$test$iskveg5
minta$train$iskveg4_ordered[minta$train$iskveg5==1] <- 2 
minta$test$iskveg4_ordered[minta$test$iskveg5==1] <- 2 

minta$train$iskveg4_ordered[minta$train$iskveg4_ordered==2] <- 1 
minta$test$iskveg4_ordered[minta$test$iskveg4_ordered==2] <- 1 
minta$train$iskveg4_ordered[minta$train$iskveg4_ordered==3] <- 2 
minta$test$iskveg4_ordered[minta$test$iskveg4_ordered==3] <- 2 
minta$train$iskveg4_ordered[minta$train$iskveg4_ordered==4] <- 3 
minta$test$iskveg4_ordered[minta$test$iskveg4_ordered==4] <- 3
minta$train$iskveg4_ordered[minta$train$iskveg4_ordered==5] <- 4 
minta$test$iskveg4_ordered[minta$test$iskveg4_ordered==5] <- 4



minta$train$iskveg4_ordered <- factor(minta$train$iskveg4_ordered, ordered=TRUE)
minta$test$iskveg4_ordered <- factor(minta$test$iskveg4_ordered, ordered=TRUE)


# tenure categorization
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                         minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                         minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                           minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                           minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75)] <- 4

minta$train$szolgho_csop <- factor(minta$train$szolgho_csop, ordered=TRUE)
minta$test$szolgho_csop <- factor(minta$test$szolgho_csop, ordered=TRUE)

# collective dummy
minta$train$kollszerz <- 0
minta$train$kollszerz[minta$train$kol == 1 | minta$train$ksz == 1 |minta$train$kag == 1 ] <- 1

minta$test$kollszerz <- 0
minta$test$kollszerz[minta$test$kol == 1 | minta$test$ksz == 1 |minta$test$kag == 1 ] <- 1

minta$train$kollszerz <- factor(minta$train$kollszerz, ordered= FALSE)
minta$test$kollszerz <- factor(minta$test$kollszerz, ordered= FALSE)

# isco
minta$train$feor_1 <- substr(minta$train$feor_2, 1,1)
minta$test$feor_1 <- substr(minta$test$feor_2, 1,1)

minta$train$feor_1 <- factor(minta$train$feor_1, ordered=FALSE)
minta$test$feor_1 <- factor(minta$test$feor_1, ordered=FALSE)


# export/import clusters
exp_kib <-read.csv("./data/exp_kib.csv", sep=";")
names(exp_kib)<-c("ag1", "exp_kib", "exp_kib_class")

minta$train <- merge(minta$train, exp_kib, by=c("ag1", "ag1"))
minta$test <- merge(minta$test, exp_kib, by=c("ag1", "ag1"))
minta$train$exp_kib_class <- factor(minta$train$exp_kib_class, ordered=FALSE)
minta$test$exp_kib_class <- factor(minta$test$exp_kib_class, ordered=FALSE)


# regio categorization
minta$train$kshreg_3[minta$train$kshreg==1] <- 1
minta$train$kshreg_3[minta$train$kshreg==2 | minta$train$kshreg==3 |minta$train$kshreg==4 ] <- 2
minta$train$kshreg_3[minta$train$kshreg==5 | minta$train$kshreg==6 |minta$train$kshreg==7 ] <- 3

minta$test$kshreg_3[minta$test$kshreg==1] <- 1
minta$test$kshreg_3[minta$test$kshreg==2 | minta$test$kshreg==3 |minta$test$kshreg==4 ] <- 2
minta$test$kshreg_3[minta$test$kshreg==5 | minta$test$kshreg==6 |minta$test$kshreg==7 ] <- 3

minta$train$kshreg_3 <- factor(minta$train$kshreg_3, ordered=FALSE)
minta$test$kshreg_3 <- factor(minta$test$kshreg_3, ordered=FALSE)


saveRDS(minta, file="./data/minta_2012.RData")

