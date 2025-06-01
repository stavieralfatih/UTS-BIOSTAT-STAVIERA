#TUGAS NO 2
#Soal no 6
set.seed(0001)
pef_ds = sample_frac(pef_clean, 0.85, replace = FALSE)
library(dplyr)
library(tidyverse)
#Soal no 7 : Exclude missing data 
table(pef_ds$pef)
pef_ds <- pef_ds[!is.na(pef_ds$pef) & !is.na(pef_ds$height) & !is.na(pef_ds$age), ]
glimpse(pef_ds)
names(pef_ds)

#Soal no 8 : Bersihkan nilai ekstrem
library(outliers)
chisq.out.test(pef_ds$pef)
max(data_no_outlier$pef)
min(pef_ds$pef)
chisq.out.test(pef_ds$age)
max(pef_ds$age)
plot(pef_ds$age,pef_ds$pef)
chisq.out.test(pef_ds$height)
min(pef_ds$height)
pef_no_outlier <- subset(pef_ds,pef_ds$pef <700 & pef_ds$pef >50 )
pef_age_no_outlier <- subset(pef_no_outlier,pef_no_outlier$age <100 & pef_no_outlier$age >20 )
plot(pef_age_no_outlier$height,pef_age_no_outlier$pef)
PAH_no_outlier <- subset(pef_age_no_outlier, pef_age_no_outlier$height <190 & pef_age_no_outlier$height >120 )
plot(PAH_no_outlier$height,PAH_no_outlier$pef)

#Soal no 9 : akses data w5
library(readr)
data_w5 <- read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")
glimpse(data_w5)
summary(data_w5)

#No 10 : buat data pidlink-asthma-sex
w5_rev = select(data_w5, pidlink, Asthma, sex)

#No 11 : frekuensi asthma & sex
table(w5_rev$Asthma)
table(w5_rev$sex)

#No 12 : pilih no-asthma dan yes-asthma
table(w5_rev$Asthma)
w5_rev = w5_rev%>% filter (Asthma=='Yes-Asthma'|Asthma=='No-Asthma')

#No 13 : cek variabel pidlink
str(pef_ds)
str(w5_rev)
w5_re <- w5_rev %>%
  mutate(pidlink_num = as.integer(pidlink))
w5_rev$pidlink <- w5_rev$pidlink_num
w5_rev$pidlink_num <- NULL

#No.14 : combine dengan pef sbg patokan
pef_w5rev_c_lj = left_join(PAH_no_outlier, w5_rev, by = "pidlink")

#export csv
write.csv(pef_w5rev_c_lj, "TUGASNO2_gabungan_pefw5revisi.csv", row.names=FALSE)
