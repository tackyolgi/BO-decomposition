# CARTs

library(rpart)
library(rpart.plot)
library(treeClust)

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp")
minta <- readRDS("./data/minta_2012.RData")

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

rel_error_rate <- CART_train_B_RF_female_ordered$cptable[, 2:3]
rel_error_rate[,1] <- as.numeric(rel_error_rate[, 1])
rel_error_rate[,2] <- as.numeric(rel_error_rate[, 2])
rel_error_rate <- as.data.frame(rel_error_rate)
names(rel_error_rate) <- c("nsplits", "rel_error")

jpeg(as.character("./results/figures/rerfemale2012.jpg"), 
     width = 1200, height=1200, res=300)
ggplot(rel_error_rate, aes(y=rel_error, x=nsplits))+
  geom_line()+ labs(x="Number of splits", y="Relative error")+
  theme(axis.title.x = element_text(size=12))
dev.off()

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

cart_path <- path.rpart(CART_train_B_RF_female_ordered, nodes=names(table(leaf.numbers(CART_train_B_RF_female_ordered))))

saveRDS(CART_train_B_RF_female_ordered, 
        "./models/CART_2012.RData")