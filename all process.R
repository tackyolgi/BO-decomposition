# dataloading...

source('./01_codes/02_estimations/rand_for14_2008.R')
source('./01_codes/02_estimations/rand_for14_2009.R')
source('./01_codes/02_estimations/rand_for14_2010.R')
source('./01_codes/02_estimations/rand_for14_2011.R')
source('./01_codes/02_estimations/rand_for14_2012.R')
source('./01_codes/02_estimations/rand_for14_2013.R')
source('./01_codes/02_estimations/rand_for14_2014.R')
source('./01_codes/02_estimations/rand_for14_2015.R') 
# it is needed to rerun manually in 2015 because ols oaxaca decomposition fails
source('./01_codes/02_estimations/rand_for14_2016.R')

names(results_RF) <- c("year", "raw", "composition_effect",
                    "wage_structure_effect", "bias")

results_RF <- cbind(results_RF, dataset=rep(c("training", "test"), 9))

# mse - figure 1
source("./01_codes/02_estimations/mse.R")

mse$Database <- factor(mse$Database, levels=c("Training sample",
                                              "Test sample"))
library(tidyverse)

tiff(as.character("./results/figures/fig1.tiff"), 
     width = 1750, height=1111, res=300)
ggplot(subset(mse, mse$Gender=="Females"), aes(x=Year, y=as.numeric(MSE), fill=Method))+
  geom_bar(stat = "identity", width = 0.5)+
  facet_grid(Database~Method)+labs(x="", y="")+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size=8),
        strip.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))
dev.off()

# table 3 and table 4 
results_RF_training <- subset(results_RF, results_RF$dataset=="training")
results_RF_test <- subset(results_RF, results_RF$dataset=="test")

write.csv(results_RF_training, "./results/results_RF_training.csv")
write.csv(results_RF_test, "./results/results_RF_test.csv")

# figure 2
results_RF <- mutate(results_RF, composition_percentage=composition_effect/raw*100)
results_RF <- mutate(results_RF, ws_percentage=wage_structure_effect/raw*100)


results_RF_longer <- results_RF %>% pivot_longer(cols=c("composition_percentage", 
                                                        "ws_percentage"), names_to="effects" )

results_RF_longer$year <- as.character(results_RF_longer$year)

results_RF_longer$dataset <- factor(results_RF_longer$dataset,
                                    c("training", "test"))
panel_label <- c(training="A", test="B")

tiff("./results/figures/Fig2.tiff", 
     width = 1900, height=1111, res=300)
ggplot(results_RF_longer, aes(x=year, y=value, fill=effects))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks=seq(-40, 140, 20))+
  facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ 
  theme(legend.position = "bottom")+
  xlab("")+ylab("")+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))
dev.off()

# figure 3
results_RF_longer <- results_RF %>% pivot_longer(cols=c("wage_structure_effect", 
                                                        "raw"), names_to="raw_ws" )

results_RF_longer$dataset <- factor(results_RF_longer$dataset,
                                    c("training", "test"))

tiff("./results/figures/Fig3.tiff",
     width = 1750, height=1111, res=300)
ggplot(results_RF_longer, aes(x=year, y=value, color=raw_ws))+
  geom_line()+
  facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0, .17)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))
dev.off()


# ws effects per variables in 2008, 2012, 2016
# education
source('./01_codes/04_ws effects/diff_osszehasonlitas_iskveg.R')

pivot_iskveg4$dataset <- factor(pivot_iskveg4$dataset,
                                    c("training", "test"))
tiff("./results/figures/Fig4.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_iskveg4, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))
dev.off()

# age
source('./01_codes/04_ws effects/diff_osszehasonlitas_kor.R')

pivot_kor$dataset <- factor(pivot_kor$dataset,
                            c("training", "test"))
tiff("./results/figures/Fig5.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_kor, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))  
dev.off()

# tenureship 
source('./01_codes/04_ws effects/diff_osszehasonlitas_szolgho.R')

pivot_szolgho$dataset <- factor(pivot_szolgho$dataset,
                            c("training", "test"))
tiff("./results/figures/Fig6.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_szolgho, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank()) +
  theme(strip.background = element_rect(fill="white")) 
dev.off()

# foreign ownership
source('./01_codes/04_ws effects/diff_osszehasonlitas_kra_4.R')
  
pivot_kra$dataset <- factor(pivot_kra$dataset,
                              c("training", "test"))
tiff("./results/figures/Fig7.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_kra, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))  
dev.off()


# state ownership
source('./01_codes/04_ws effects/diff_osszehasonlitas_ara_4.R')
  
pivot_ara$dataset <- factor(pivot_ara$dataset,
                                c("training", "test"))
tiff("./results/figures/Fig8.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_ara, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))
dev.off()


# femaleness in occupations
source('./01_codes/04_ws effects/diff_osszehasonlitas_nok_aranya.R')

pivot_nok_aranya$dataset <- factor(pivot_nok_aranya$dataset,
                                c("training", "test"))

tiff("./results/figures/Fig9.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_nok_aranya, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())  +
  theme(strip.background = element_rect(fill="white"))
dev.off()

# occupation - 1 digit
source('./01_codes/04_ws effects/diff_osszehasonlitas_feor.R')

pivot_feor$dataset <- factor(pivot_feor$dataset,
                             c("training", "test"))

tiff("./results/figures/Fig10.tiff", 
     width = 1200, height=1200, res=300)
ggplot(pivot_feor, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))  
dev.off()


# L* and S* characteristics - table 8-10
source("./01_codes/03_carts/CARTs_groups.R")

# leave out some variables
reduced_total_matrix <- total_matrix
reduced_L_group_matrix <- L_group_matrix
reduced_S_group_matrix <- S_group_matrix

reduced_total_matrix<- reduced_total_matrix[-c(6, 10:15),]
reduced_L_group_matrix<- reduced_L_group_matrix[-c(6, 10:15),]
reduced_S_group_matrix<- reduced_S_group_matrix[-c(6, 10:15),]

write.csv(reduced_total_matrix, "./results/reduced_total_matrix.csv")
write.csv(reduced_L_group_matrix, "./results/reduced_L_group_matrix.csv")
write.csv(reduced_S_group_matrix, "./results/reduced_S_group_matrix.csv")


# table 10 about job vacancy and unemployment
library(eurostat)
job_vacancy <- get_eurostat("jvs_a_rate_r2", time_format = "num",
                            filters = list(geo="HU", time=2008:2016, nace_r2="A-S",
                                           sizeclas="TOTAL", unit="AVG_A"))
unemployment <- get_eurostat("une_rt_a", time_format = "num",
                             filters=list(geo="HU",time=2008:2016, 
                                          age="Y15-74", sex="T", unit="PC_ACT"))
unemp_job_vac <- left_join(unemployment, job_vacancy, by=c("time"= "time"))

tiff("./results/figures/Fig11.tiff", 
     width = 1200, height=1200, res=300)
ggplot(unemp_job_vac, aes(x=values.x, y=values.y, label=time))+
  geom_path()+ylim(0,2)+xlim(0,12)+
  geom_text(size=3)+
  xlab("")+ylab("")+
  theme(legend.title = element_blank())
dev.off()

# S1 figure
source('./01_codes/02_estimations/rand_for14_2008_95.R')
source('./01_codes/02_estimations/rand_for14_2009_95.R')
source('./01_codes/02_estimations/rand_for14_2010_95.R')
# rerun ols decomposition manually in 2010 
source('./01_codes/02_estimations/rand_for14_2011_95.R')
source('./01_codes/02_estimations/rand_for14_2012_95.R')
source('./01_codes/02_estimations/rand_for14_2013_95.R')
source('./01_codes/02_estimations/rand_for14_2014_95.R')
source('./01_codes/02_estimations/rand_for14_2015_95.R') 
source('./01_codes/02_estimations/rand_for14_2016_95.R')

names(results_RF) <- c("year", "raw", "composition_effect",
                       "wage_structure_effect", "bias")

results_RF <- cbind(results_RF, dataset=rep(c("training", "test"), 9))

# just in case table 3 and table 4 for reduced dataset
results_RF_training <- subset(results_RF, results_RF$dataset=="training")
results_RF_test <- subset(results_RF, results_RF$dataset=="test")

write.csv(results_RF_training, "./results/results_RF_95_training.csv")
write.csv(results_RF_test, "./results/results_RF_95_test.csv")

# figure 2
results_RF <- mutate(results_RF, composition_percentage=composition_effect/raw*100)
results_RF <- mutate(results_RF, ws_percentage=wage_structure_effect/raw*100)


results_RF_longer <- results_RF %>% pivot_longer(cols=c("composition_percentage", 
                                                        "ws_percentage"), names_to="effects" )

results_RF_longer$year <- as.character(results_RF_longer$year)

results_RF_longer$dataset <- factor(results_RF_longer$dataset,
                                    c("training", "test"))

tiff("./results/figures/S1_fig.tiff", 
     width = 1200, height=1200, res=300)
ggplot(results_RF_longer, aes(x=year, y=value, fill=effects))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks=seq(-40, 140, 20))+
  facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "none")+
  xlab("")+ylab("")+
  theme(strip.background = element_rect(fill="white"))
dev.off()

# S2 - CARTs for ws effect in each year
source("./01_codes/03_carts/CARTs_2008.R")
source("./01_codes/03_carts/CARTs_2009.R")
source("./01_codes/03_carts/CARTs_2010.R")
source("./01_codes/03_carts/CARTs_2011.R")
source("./01_codes/03_carts/CARTs_2012.R")
source("./01_codes/03_carts/CARTs_2013.R")
source("./01_codes/03_carts/CARTs_2014.R")
source("./01_codes/03_carts/CARTs_2015.R")
source("./01_codes/03_carts/CARTs_2016.R")

rmarkdown::render("./01_codes/03_carts/CARTs_2008.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2008.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2009.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2009.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2010.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2010.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2011.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2011.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2012.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2012.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2013.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2013.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2014.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2014.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2015.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2015.doc")
rmarkdown::render("./01_codes/03_carts/CARTs_2016.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2016.doc")

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp")

# further calculations in the excel file

# S? - obb error tables
# 2008
model <- readRDS("./models/B_RF_female_ordered_2008.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2008.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2008.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2008.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2009
model <- readRDS("./models/B_RF_female_ordered_2009.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2009.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2009.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2009.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2010
model <- readRDS("./models/B_RF_female_ordered_2010.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2010.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2010.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2010.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2011
model <- readRDS("./models/B_RF_female_ordered_2011.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2011.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2011.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2011.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2012
model <- readRDS("./models/B_RF_female_ordered_2012.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2012.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2012.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2012.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2013
model <- readRDS("./models/B_RF_female_ordered_2013.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2013.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2013.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2013.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2014
model <- readRDS("./models/B_RF_female_ordered_2014.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2014.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2014.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2014.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2015
model <- readRDS("./models/B_RF_female_ordered_2015.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2015.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2015.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2015.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

# 2016
model <- readRDS("./models/B_RF_female_ordered_2016.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_female_2016.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()

model <- readRDS("./models/B_RF_male_ordered_2016.RData")

error_rate <- as.data.frame(model$err.rate)

tiff(as.character("./results/figures/error_rate_male_2016.tiff"), 
     width = 1200, height=1200, res=300)
ggplot(error_rate, aes(y=model$err.rate, x=seq(1:1000)))+
  geom_line()+ labs(x="", y="")+
  theme(axis.title.x = element_text(size=12))
dev.off()


