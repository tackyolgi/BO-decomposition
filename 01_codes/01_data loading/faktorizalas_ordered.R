get_factors <- function(df) {
          df$nem <- factor(df$nem)
          df$iskveg9_ordered <- factor(df$iskveg9, ordered=TRUE)
          df$ujbel <- as.factor(df$ujbel)
          df$ag1 <- as.factor(df$ag1)
          df$feor_2 <- as.factor(df$feor_2)
          df$ksz <- as.factor(df$ksz)
          df$kag <- as.factor(df$kag)
          df$kol <- as.factor(df$kol)
          df$ara_ordered <- factor(df$ara, ordered=TRUE)
          df$kra_ordered <- factor(df$kra, ordered=TRUE)
          df$kshreg <- as.factor(df$kshreg)
          df$ttip <- as.factor(df$ttip)
          
          df$iskveg9_notordered <- factor(df$iskveg9, ordered=FALSE)
          df$ara_notordered <- factor(df$ara, ordered=FALSE)
          df$kra_notordered <- factor(df$kra, ordered=FALSE)
          df
}