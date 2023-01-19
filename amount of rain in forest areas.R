rm(list = ls()) #remove all past worksheet variables
library(raster)
library(stringr)

###USER CONFIGURATION
rootDirs=c("C:/Users/lkaiser/Desktop/HCSU/HIecoH_outputs/raincheck/", "D:/projects/HIecoH/HIecoH_P2/HIecoH_outputs/raincheck/")
wd=rootDirs[min(which(dir.exists(rootDirs)))]
setwd(wd)

higap_dir="D:/data/higap_data/" #this is where r will create a mirror folder structure with the 

clim_data_dir="D:/data/climate_data/20201123_HRCM_NCAR_projections2/bioclims/baseline_rasters/"
biovars2000 = raster(paste(clim_data_dir, "bio12.tif", sep=""))
HIGAP=raster(paste(higap_dir, "CAH_LandCover.tif", sep=""))

#for comparison, 500m with original class
plot(HIGAP)
# HIGAP_lanai=crop(HIGAP, extent(700000, 730000, 2290000, 2320000)) #xmin, xmax, ymin, ymax
# HIGAP_lanai[HIGAP_lanai>100]=NA
# plot(HIGAP_lanai)

################################
#all wet and mesic forests
###reclass into simple categories
reclass_table=(read.csv(paste0(higap_dir, 'HIGAP_attribute_data.csv'),header=T, stringsAsFactors=F))
reclass_table=reclass_table[,c("Value", "wm_forest")]
HIGAP_reclass=subs(HIGAP,  reclass_table)
HIGAP_reclass2=raster::aggregate(HIGAP_reclass, fact=8, fun=modal)
HIGAP_reclass3=projectRaster(HIGAP_reclass2, biovars2000, method = "ngb")
writeRaster(HIGAP_reclass3, "HIGAP_250m_wm_forests.tif", format="GTiff", overwrite=TRUE, compress="LZW")

###zonal calc
precip=as.data.frame(zonal(biovars2000, HIGAP_reclass3, fun='sum'))
precip_pct=precip[precip$zone==1, "sum"]/sum(precip$sum)*100
area=as.data.frame(zonal(biovars2000>-1, HIGAP_reclass3, fun='sum'))
area_pct=area[area$zone==1, "sum"]/sum(area$sum)*100
#50.87183% of precip
#28.38461% of area

################################
#all NATIVE wet and mesic forests
###reclass into simple categories
reclass_table=(read.csv(paste0(higap_dir, 'HIGAP_attribute_data.csv'),header=T, stringsAsFactors=F))
reclass_table=reclass_table[,c("Value", "wm_n_forest")]
HIGAP_reclass=subs(HIGAP,  reclass_table)
HIGAP_reclass2=raster::aggregate(HIGAP_reclass, fact=8, fun=modal)
HIGAP_reclass3=projectRaster(HIGAP_reclass2, biovars2000, method = "ngb")
writeRaster(HIGAP_reclass3, "HIGAP_250m_wm_nat_forests.tif", format="GTiff", overwrite=TRUE, compress="LZW")

###zonal calc
precip=as.data.frame(zonal(biovars2000, HIGAP_reclass3, fun='sum'))
precip_pct=precip[precip$zone==1, "sum"]/sum(precip$sum)*100
area=as.data.frame(zonal(biovars2000>-1, HIGAP_reclass3, fun='sum'))
area_pct=area[area$zone==1, "sum"]/sum(area$sum)*100
#36.5% of precip
#18.7% of area

################################
#all bare, grass, woody
###reclass into simple categories
reclass_table=(read.csv(paste0(higap_dir, 'HIGAP_attribute_data.csv'),header=T, stringsAsFactors=F))
reclass_table=reclass_table[,c("Value", "three_covers")]
HIGAP_reclass=subs(HIGAP,  reclass_table)
HIGAP_reclass2=raster::aggregate(HIGAP_reclass, fact=8, fun=modal)
HIGAP_reclass3=projectRaster(HIGAP_reclass2, biovars2000, method = "ngb")
writeRaster(HIGAP_reclass3, "HIGAP_250m_three_covers_forests.tif", format="GTiff", overwrite=TRUE, compress="LZW")

###zonal calc
precip=as.data.frame(zonal(biovars2000, HIGAP_reclass3, fun='sum'))
precip$precip_pct=precip[, "sum"]/sum(precip$sum)*100
area=as.data.frame(zonal(biovars2000>-1, HIGAP_reclass3, fun='sum'))
area$area_pct=area[, "sum"]/sum(area$sum)*100
#50.87183% of precip
#28.38461% of area
