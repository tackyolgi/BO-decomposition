library(tidyverse)

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results")

minta_08<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_08.RData")
minta_09<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_09.RData")
minta_10<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_10.RData")
minta_11<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_11.RData")
minta_12<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_12.RData")
minta_13<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_13.RData")
minta_14<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_14.RData")
minta_15<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_15.RData")
minta_16<-readRDS("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note 2/data/tarifred_16.RData")

desc_stat <- matrix(NA, 10, 4)
desc_stat[1, ] <- c("Year", "No_of_obs", "Average_wage_gap", " Female_ratio_in_full_dataset") 

desc_stat[2:10, 1] <- seq(2008,2016)

desc_stat[2:10, 2] <- c(nrow(minta_08), nrow(minta_09), nrow(minta_10), nrow(minta_11),
                        nrow(minta_12), nrow(minta_13), nrow(minta_14), nrow(minta_15),
                        nrow(minta_16))

desc_stat[2, 3] <- round(mean(subset(minta_08, minta_08$nem==1)$lnker)-
                            mean(subset(minta_08, minta_08$nem==0)$lnker), 4)
desc_stat[3, 3] <- round(mean(subset(minta_09, minta_09$nem==1)$lnker)-
                           mean(subset(minta_09, minta_09$nem==0)$lnker), 4)
desc_stat[4, 3] <- round(mean(subset(minta_10, minta_10$nem==1)$lnker)-
                           mean(subset(minta_10, minta_10$nem==0)$lnker), 4)
desc_stat[5, 3] <- round(mean(subset(minta_11, minta_11$nem==1)$lnker)-
                           mean(subset(minta_11, minta_11$nem==0)$lnker), 4)
desc_stat[6, 3] <- round(mean(subset(minta_12, minta_12$nem==1)$lnker)-
                           mean(subset(minta_12, minta_12$nem==0)$lnker), 4)
desc_stat[7, 3] <- round(mean(subset(minta_13, minta_13$nem==1)$lnker)-
                           mean(subset(minta_13, minta_13$nem==0)$lnker), 4)
desc_stat[8, 3] <- round(mean(subset(minta_14, minta_14$nem==1)$lnker)-
                           mean(subset(minta_14, minta_14$nem==0)$lnker), 4)
desc_stat[9, 3] <- round(mean(subset(minta_15, minta_15$nem==1)$lnker)-
                           mean(subset(minta_15, minta_15$nem==0)$lnker), 4)
desc_stat[10, 3] <- round(mean(subset(minta_16, minta_16$nem==1)$lnker)-
                           mean(subset(minta_16, minta_16$nem==0)$lnker), 4)

desc_stat[2, 4] <- round(nrow(subset(minta_08, minta_08$nem==0))/nrow(minta_08)*100, 2)
desc_stat[3, 4] <- round(nrow(subset(minta_09, minta_09$nem==0))/nrow(minta_09)*100, 2)
desc_stat[4, 4] <- round(nrow(subset(minta_10, minta_10$nem==0))/nrow(minta_10)*100, 2)
desc_stat[5, 4] <- round(nrow(subset(minta_11, minta_11$nem==0))/nrow(minta_11)*100, 2)
desc_stat[6, 4] <- round(nrow(subset(minta_12, minta_12$nem==0))/nrow(minta_12)*100, 2)
desc_stat[7, 4] <- round(nrow(subset(minta_13, minta_13$nem==0))/nrow(minta_13)*100, 2)
desc_stat[8, 4] <- round(nrow(subset(minta_14, minta_14$nem==0))/nrow(minta_14)*100, 2)
desc_stat[9, 4] <- round(nrow(subset(minta_15, minta_15$nem==0))/nrow(minta_15)*100, 2)
desc_stat[10, 4] <- round(nrow(subset(minta_16, minta_16$nem==0))/nrow(minta_16)*100, 2)

desc_stat <- as.data.frame(desc_stat)
names(desc_stat) <- desc_stat[1, ]
desc_stat <- desc_stat[-1, ]

ggplot(desc_stat, aes(x=Year, y=as.numeric(Average_wage_gap), group=1))+ 
  geom_path()+
  labs(x="", y="")+ylim(0, 0.2)

write.csv(desc_stat, "desc_stat.csv")
