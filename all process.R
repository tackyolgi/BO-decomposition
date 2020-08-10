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

# table 3 and table 4 
results_RF_training <- subset(results_RF, results_RF$dataset=="training")
results_RF_test <- subset(results_RF, results_RF$dataset=="test")

write.csv(results_RF_training, "./results/results_RF_training.csv")
write.csv(results_RF_test, "./results/results_RF_test.csv")

# figure 1
results_RF <- mutate(results_RF, composition_percentage=composition_effect/raw*100)
results_RF <- mutate(results_RF, ws_percentage=wage_structure_effect/raw*100)


results_RF_longer <- results_RF %>% pivot_longer(cols=c("composition_percentage", 
                                                        "ws_percentage"), names_to="effects" )

results_RF_longer$year <- as.character(results_RF_longer$year)

results_RF_longer$dataset <- factor(results_RF_longer$dataset,
                                    c("training", "test"))
panel_label <- c(training="A", test="B")

tiff("./results/figures/Fig1.tiff")
ggplot(results_RF_longer, aes(x=year, y=value, fill=effects))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks=seq(-40, 140, 20))+
  facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))
dev.off()

# figure 2
results_RF_longer <- results_RF %>% pivot_longer(cols=c("wage_structure_effect", 
                                                        "raw"), names_to="raw_ws" )

results_RF_longer$dataset <- factor(results_RF_longer$dataset,
                                    c("training", "test"))

tiff("./results/figures/Fig2.tiff")
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
tiff("./results/figures/Fig3.tiff")
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
tiff("./results/figures/Fig4.tiff")
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
tiff("./results/figures/Fig5.tiff")
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
tiff("./results/figures/Fig6.tiff")
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
tiff("./results/figures/Fig7.tiff")
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

tiff("./results/figures/Fig8.tiff")
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

tiff("./results/figures/Fig9.tiff")
ggplot(pivot_feor, aes(x=category, y=as.numeric(RF), group=year, color=year ))+
  geom_path()+facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "bottom")+
  xlab("")+ylab("")+labs(fill="")+ylim(0,0.25)+
  theme(legend.title = element_blank())+
  theme(strip.background = element_rect(fill="white"))  
dev.off()

# CARTs for ws effect in each year
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
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2008.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2009.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2009.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2010.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2010.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2011.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2011.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2012.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2012.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2013.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2013.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2014.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2014.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2015.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2015.docx")
rmarkdown::render("./01_codes/03_carts/CARTs_2016.R", 
                  output_file = "C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp/results/CARTs/CARTs_2016.docx")



setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/BO decomp")

# further calculation in the excel file
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

tiff("./results/figures/S1_fig.tiff")
ggplot(results_RF_longer, aes(x=year, y=value, fill=effects))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks=seq(-40, 140, 20))+
  facet_grid(.~dataset, labeller=labeller(dataset=panel_label))+ theme(legend.position = "none")+
  xlab("")+ylab("")+
  theme(strip.background = element_rect(fill="white"))
dev.off()


