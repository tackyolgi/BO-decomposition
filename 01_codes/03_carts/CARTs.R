# CARTs

library(rpart)
library(rpart.plot)
library(treeClust)

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2016.RData")

cp_value <- 0.00005
min_bucket <- 50
depth_value <- 4

# female
set.seed(mean(minta$train$ev))
CART_train_B_RF_female_ordered <- rpart(pred_diff_B_RF_ordered~iskveg4_ordered  + kor_csop +szolgho_csop
                                        +vallmeret+exp_kib_class+kollszerz+
                                         ara_ordered+ kra_ordered+   kshreg_3 + ttip
                                        ,
                                        data=subset(minta$train, nem==0),
                                        control = rpart.control(cp=cp_value ,  minbucket=min_bucket 
                                                    
                                                                            #, maxdepth = depth_value
                                        ), model=TRUE)
CART_train_B_RF_female_ordered
printcp(CART_train_B_RF_female_ordered)
plotcp(CART_train_B_RF_female_ordered)

cp_value <- 0.001 
minbuck_value <- 100

CART_train_B_RF_female_ordered <- rpart(pred_diff_B_RF_ordered~iskveg4_ordered  + kor_csop +szolgho_csop
                                             +vallmeret+exp_kib_class+kollszerz+
                                               ara_ordered+ kra_ordered+   kshreg_3 + ttip
                                             ,
                                             data=subset(minta$train, nem==0),
                                             control = rpart.control(cp=cp_value ,  minbucket=minbuck_value 
                                                                    # , maxdepth = depth_value
                                             ), model=TRUE)
CART_train_B_RF_female_ordered
rpart.plot(CART_train_B_RF_female_ordered, type=4, extra=101,
           tweak = 3.5,  compress=FALSE,  fallen.leaves =FALSE, varlen=10)

path.rpart(CART_train_B_RF_female_ordered, nodes=names(table(leaf.numbers(CART_train_B_RF_female_ordered))))


