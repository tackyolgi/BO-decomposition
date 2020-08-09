# this script contains the A, B and C estimations 
#    what Vincze mentioned in his e-mail on 5th of Aug
# it contains oaxaca and rf estimations only

# sampling
get_sample <- function(df, obs_num){
  set.seed(mean(df$ev))
  index <- sample(seq_len(nrow(df)), obs_num)
  train <- df[index, ]
  test <- df[-index, ]
  list(train=train, test=test)
}

# oaxaca
get_B_oax_regr_ordered <- function(df) {
  oax_decomp <- oaxaca(formula = lnker~  as.character(iskveg9_ordered)  + 
                         kor +kor2+szolgho+letszam_bv1
                       + as.character(ag1)+ as.character(ksz)+ 
                         as.character(kag)+as.character(kol)+ 
                         as.character(ara_ordered)+ as.character(kra_ordered)
                       +   as.character(kshreg) + as.character(ttip)+
                         as.character(feor_2)
                       | nem
                       , data = df,
                       # Neumark does not include group indicator variable
                       R = 10)
}

get_B_oax_regr_notordered <- function(df) {
  oax_decomp <- oaxaca(formula = lnker~  as.character(iskveg9_ordered)  + 
                         kor +kor2+szolgho+letszam_bv1
                       + as.character(ag1)+ as.character(ksz)+ 
                         as.character(kag)+as.character(kol)+ 
                         as.character(ara_ordered)+ as.character(kra_ordered)
                       +   as.character(kshreg) + as.character(ttip)+
                         as.character(feor_2)
                       | nem
                       , data = df,
                       # Neumark does not include group indicator variable
                       R = 10)
}


# random forest
get_B_oax_RF_R1_ordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ nem+iskveg9_ordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_ordered+ kra_ordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
  
}


get_B_oax_RF_R2_ordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ iskveg9_ordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_ordered+ kra_ordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
  
}


get_B_oax_RF_female_ordered <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_ordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_ordered+ kra_ordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female, block.size = 1)
  
}

get_B_oax_RF_male_ordered <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_ordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_ordered+ kra_ordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male, block.size = 1)
  
}

get_A_oax_RF_R1_notordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ nem+iskveg9_notordered  + kor+szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered+   kshreg + ttip,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
}

get_A_oax_RF_R2_notordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor+szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered+   kshreg + ttip,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
}

get_A_oax_RF_female_notordered <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female, block.size = 1)
  
}

get_A_oax_RF_male_notordered <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male, block.size = 1)
  
}

get_B_oax_RF_R1_notordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ nem+iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
  
}

get_B_oax_RF_R2_notordered <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df, block.size = 1)
  
}


get_B_oax_RF_female_notordered <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female, block.size = 1)
  
}

get_B_oax_RF_male_notordered <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- rfsrc(lnker ~ iskveg9_notordered  + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara_notordered+ kra_notordered
                              +   kshreg + ttip+ feor_2,
                              na.action="na.omit", mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male, block.size = 1)
  
}



