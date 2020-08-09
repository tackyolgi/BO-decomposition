delete_missing_obs <- function(df){
  df$prop <- as.integer(str_c(df$kra, df$ara, sep="")) # foregin-state ownership: 1-1 v. 2-2
  df <- df[!is.na(df$lnker) 
           & !is.na(df$nem)
           & !is.na(df$iskveg9)
           & !is.na(df$kor)
           & !is.na(df$szolgho)
           & !is.na(df$ujbel)
           & !is.na(df$letszam_bv1)
           & !is.na(df$ag1)
           & !is.na(df$feor_2)
           & !is.na(df$ksz)
           & !is.na(df$kag)
           & !is.na(df$kol)
           & !is.na(df$ara)
           & !is.na(df$kra)
           & !is.na(df$kshreg)
           & !is.na(df$ttip)
           & !is.na(df$nok_aranya)
           & df$kshreg!=0
           & df$nok_sample>=20 # few observations
           & df$ferfiak_sample>=20
           & df$ag1!=15 # few observations
           & df$letszam_bv1>=20
           & df$prop!=11
           & df$prop!=22
           & df$prop!=12
           & df$prop!=21
           & df$prop!=13
           & df$prop!=31
           ,]
}