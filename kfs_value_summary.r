# set working directory
rootDirs=c("C:/Users/lkaiser/Desktop/HCSU/HIecoH/", "D:/projects/HIecoH/HIecoH_P2/HIecoH/")
rootDir=rootDirs[min(which(dir.exists(rootDirs)))]
setwd(rootDir)

kfs_class<-read.csv("infiltration_data/kfs_data/hiecoh_kfs_data.csv")
kfs_class[kfs_class$cover_type=="shrubs_trees","cover_type"]="woody"
kfs_class[kfs_class$cover_type=="forests","cover_type"]="woody"

#View(kfs_class)
library(dplyr)
library(tidyr)

# group by cover_type and calculate summary statistics
summary_kfs_class <- kfs_class %>%
  group_by(cover_type) %>%
  summarize(
    median_Kfs = median(Kfs, na.rm=T),
    mean_Kfs = mean(Kfs, na.rm=T),
    sd_Kfs = sd(Kfs, na.rm=T),
    n=n(),
    ci95_Kfs = 1.96 * sd(Kfs, na.rm=T) / sqrt(n()),
    Upper_ci = mean(Kfs, na.rm=T) + 1.96 * sd(Kfs, na.rm=T) / sqrt(n()),
    Lower_ci = mean(Kfs, na.rm=T) - 1.96 * sd(Kfs, na.rm=T) / sqrt(n()),
    
  ) %>%
  ungroup()

summary_kfs_class

# perform ANOVA
aov_result <- aov(Kfs ~ cover_type, data = kfs_class)

# print ANOVA table
summary(aov_result)

# perform Tukey's test for post-hoc comparison
tukey_result <- TukeyHSD(aov_result)

# print Tukey's test table
tukey_result
