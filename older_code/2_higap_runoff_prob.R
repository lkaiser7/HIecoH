# Hawaii Climate Projections for Ecological Models

# Data from Lulin Xue and processed by Yaping Wang
# netcdf data of PDFs for hourly rain rate in bins
# individual class calculations for different landcover types
# based on 10-year simulation period for historical and PGW scenarios 

##### SET UP #####

# set working directory
rootDirs=c("C:/Users/lkaiser/Desktop/HCSU/HIecoH/", "D:/projects/HIecoH/HIecoH_P2/HIecoH/")
rootDir=rootDirs[min(which(dir.exists(rootDirs)))]
#rootDir<-"C:/Users/lkaiser/Desktop/HCSU/HIecoH/"
setwd(rootDir)
# set paths 
dataDir<-paste0(rootDir, "data/")
outDir<-paste0(dirname(rootDir), "/HIecoH_outputs/")

# load packages
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)

# load higap data
# higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_240m.tif"))
# higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_60m_20210308.tif"))
higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_90m_20210308.tif"))
plot(higap)

# higap classes for reference
h_num<-c(0, 1, 2, 3)
h_name<-c("bare_soil", "grass", "shrubs_trees", "forests")
# 0 - bare_soil
# 1 - grass 
# 2 - shrubs_trees 
# 3 - forests
# NA - all else

# set coordinate system to be used with data as needed
LatLon<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
utmSys<-'+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs'

# load location coast shapefile from file path to personal database
loc_coast<-readOGR("data/map_data", "Main_Hawaiian_Islands_simple3")
# convert Islands coastlines to utm coordinates
loc_utm<-spTransform(loc_coast, CRS(utmSys))
plot(loc_utm, add = T)

# remove Niihau from higap data
extent(loc_utm[which(loc_utm$Island == "KA"),])
higap_main<-crop(higap, extent(418686, 946591, 2089683, 2465523))
plot(higap_main)

##### LOAD DATA #####

# kfs data
kfs_class<-read.csv("infiltration_data/kfs_data/hiecoh_kfs_data.csv")
# head(kfs_class); summary(kfs_class$Kfs)
# # remove negative values if any
# kfs_class<-kfs_class[which(kfs_class$Kfs >= 0),]

# pdf points by HIGAP class
pdf_class<-read_csv(paste0(outDir, "AllRasters_coords_pdf_higap.csv"))
# raster reference data
nc_hist_data<-read_csv(paste0(outDir, "AllRasters_coords_historical_pdf.csv"))
nc_pgw_data<-read_csv(paste0(outDir, "AllRasters_coords_future_pdf.csv"))
# merge pdf higap data with historical and pgw (future) data
pdf_hist<-cbind(pdf_class, nc_hist_data[,-1:-3])
pdf_pgw<-cbind(pdf_class, nc_pgw_data[,-1:-3])
# head(pdf_hist, n = c(3, 9))
# head(pdf_pgw, n = c(3, 9))

# # TAKES TOO LONG TO PROCESS PROBABILITIES
# # updated raster reference data at 90 m (allow 2 min to load each file)
# pdf_hist<-read_csv(paste0(outDir, "AllRasters_coords_historical_90m.csv"))
# pdf_pgw<-read_csv(paste0(outDir, "AllRasters_coords_future_90m.csv"))
# head(data.frame(pdf_hist), c(4, 15))
# head(data.frame(pdf_pgw), c(4, 15))

##### DATA PROCESSING #####

# bins divided into 0 to 10^-3 mm/h in the first bin
# then evenly divided into 120 bins from 10^-3 to 10^3
left_flux_breaks = seq(from = 10^-3, to = 10^3, length.out = 120)
dir.create(paste0(outDir, "probabilities/"), showWarnings = F)

# loop through all higap classes
for (h in 1:4){  # set h = 2 for debugging
  # select higap class
  c_num<-h_num[h]
  c_name<-h_name[h]
  
  # kfs by higap class
  kfs<-kfs_class$Kfs[which(kfs_class$cover_type == c_name)]
  # remove any negative values (if any)
  kfs<-kfs[which(kfs >= 0)]
  # hist(kfs)
  
  # create the number of bin classes of mm/hr values
  # flux_breaks = c(0, left_flux_breaks, max(kfs))
  flux_breaks = c(0, left_flux_breaks, 17950)
  mm_n_classes = length(flux_breaks)-1
  
  # characterize probability distribution of Kfs by higap class
  kfs_prob=hist(kfs, breaks=flux_breaks)$counts/sum(hist(kfs, breaks=flux_breaks)$counts, plot=F) 
  # calculate kfs probabilities along the size classes
  flux_breaks_midpoints=hist(kfs, breaks=flux_breaks)$mids
  flux_breaks_midpoints[length(flux_breaks_midpoints)]=flux_breaks[length(flux_breaks)-1]
  # plot(flux_breaks_midpoints, kfs_prob, type="l")
  
  # pdf location data by class
  loc<-pdf_hist[which(pdf_hist$HIGAP == c_num), 1:7]
  # loc<-pdf_hist[which(pdf_hist$HIGAP_veg_type_90m_20210308 == c_num), 1:13]
  # head(loc)
  
  # pdf rainfall data by class
  rf_hist<-pdf_hist[which(pdf_hist$HIGAP == c_num), -1:-7]
  rf_pgw<-pdf_pgw[which(pdf_pgw$HIGAP == c_num), -1:-7]
  # rf_hist<-pdf_hist[which(pdf_hist$HIGAP_veg_type_90m_20210308 == c_num), -1:-13]
  # rf_pgw<-pdf_pgw[which(pdf_pgw$HIGAP_veg_type_90m_20210308 == c_num), -1:-13]
  # head(rf_hist[1:3])
  # head(rf_pgw[1:3])
  
  # loop through each row and calculate runoff probability
  for(r in 1:dim(rf_hist)[1]){  # set r = 1 for debugging
  # for(r in 1:10){  # set r = 1 for debugging
    rf_dist_hist<-as.matrix(rf_hist[r,])
    # hist(rf_dist_hist)
    rf_dist_pgw<-as.matrix(rf_pgw[r,])
    # hist(rf_dist_pgw)
    print(r) # for counting 
    
    # rainfall probabilities along the size classes
    rf_prob_hist=hist(rf_dist_hist, breaks=flux_breaks)$counts/sum(hist(rf_dist_hist, breaks=flux_breaks)$counts, plot=F)
    # plot(flux_breaks_midpoints, rf_prob_hist, type="l")
    rf_prob_pgw=hist(rf_dist_pgw, breaks=flux_breaks)$counts/sum(hist(rf_dist_pgw, breaks=flux_breaks)$counts, plot=F)
    # plot(flux_breaks_midpoints, rf_prob_pgw, type="l")

    # HISTORICAL CALCS
    # create joined data frame of midpoints, kfs, and rainfall
    joined_DF=data.frame(mm=flux_breaks_midpoints, Kfs=kfs_prob, RF_intensity=rf_prob_hist)
    # calculate cumulative sums
    joined_DF$Kfs_CDF=cumsum(joined_DF$Kfs)
    joined_DF$RF_intensity_CDF=cumsum(joined_DF$RF_intensity)
    # calculate cumulative probability of runoff
    joined_DF$Kfs_CDF_inv=1-joined_DF$Kfs_CDF
    # PREVIOUS CALC
    # joined_DF$P_RF_gtr_Kfs=joined_DF$RF_intensity*joined_DF$Kfs_CDF
    joined_DF$P_RF_gtr_Kfs=c(joined_DF$RF_intensity[c(1:length(joined_DF$RF_intensity))-1]*
                               joined_DF$Kfs_CDF[c(2:length(joined_DF$Kfs_CDF))], 0)
    # essentially how many hourly rainfall events are greater than infiltration rates
    hist_prob_runoff=sum(joined_DF$P_RF_gtr_Kfs) 
    # prob_runoff
    
    # PGW (FUTURE) CALCS
    # create joined data frame of midpoints, kfs, and rainfall
    joined_DF2=data.frame(mm=flux_breaks_midpoints, Kfs=kfs_prob, RF_intensity=rf_prob_pgw)
    # calculate cumulative sums
    joined_DF2$Kfs_CDF=cumsum(joined_DF2$Kfs)
    joined_DF2$RF_intensity_CDF=cumsum(joined_DF2$RF_intensity)
    # calculate cumulative probability of runoff
    joined_DF2$Kfs_CDF_inv=1-joined_DF2$Kfs_CDF
    # PREVIOUS CALC
    # joined_DF2$P_RF_gtr_Kfs=joined_DF2$RF_intensity*joined_DF2$Kfs_CDF
    joined_DF2$P_RF_gtr_Kfs=c(joined_DF2$RF_intensity[c(1:length(joined_DF2$RF_intensity))-1]*
                                joined_DF2$Kfs_CDF[c(2:length(joined_DF2$Kfs_CDF))], 0)
    # essentially how many hourly rainfall events are greater than infiltration rates
    pgw_prob_runoff=sum(joined_DF2$P_RF_gtr_Kfs) 
    # pgw_prob_runoff
    
    # create data to be saved
    prob_runoff<-data.frame(loc[r,], HIST_RUNOFF_PROB = hist_prob_runoff, PGW_RUNOFF_PROB = pgw_prob_runoff)
    
    # combine all rows of data
    if(r == 1){
      all_runoff<-prob_runoff
    }else{
      all_runoff<-rbind(all_runoff, prob_runoff)
    }
    
  }

  # save cumulative probability of runoff per higap class
  write_csv(all_runoff, paste0(outDir, "probabilities/", c_name, "_prob_runoff.csv"))
  
  # combine all class runoff probabilities
  if(h == 1){
    all_class_runoff<-all_runoff
  }else{
    all_class_runoff<-rbind(all_class_runoff, all_runoff)
  }
  
}

# save final cumulative probability of runoff
write_csv(all_class_runoff, paste0(outDir, "probabilities/", "all_prob_runoff.csv"))
head(all_class_runoff)

### RASTER REFERENCE ###

# create working copy of data
all_class_runoff2<-all_class_runoff
# all_class_runoff<-read_csv(paste0(outDir, "probabilities/", "all_prob_runoff.csv"))
# head(data.frame(all_class_runoff))

# exctract higap as data frame
# higap_spdf<-as.data.frame(as(higap, "SpatialPixelsDataFrame"))
higap_spdf<-as.data.frame(higap, xy = T)
higap_ex<-extract(higap, cbind(higap_spdf$x, higap_spdf$y), cellnumbers = T)
# head(higap_ex)
higap_spdf$CELLS<-higap_ex[,1]
dim(higap_spdf); head(higap_spdf)
# remove data to free up memory space
rm(higap_ex)

# create an empty raster object to the extent of the points
#rast<-raster(ncols = 240, nrows = 240, ext = extent(higap), crs = crs(higap))
rast<-raster("data/pcp_base.tif") #use past raster created from lulins nc files as template
rast=projectRaster(rast, crs = crs(higap))
# dim() and values() are non-spatial properties
# extent(), crs(), and res() are spatial properties

# set up data frames for rasters
higap_df<-pdf_class[,c(5, 6, 1)]
# create a SpatialPointsDataFrame
coordinates(higap_df) = ~X_UTM+Y_UTM 
# rasterize your irregular points (not gridded)
higap_rast<-rasterize(higap_df, rast, higap_df$...1)
# mean not used to avoid changing/averaging values
# plot(higap_rast)

# extract pdf cells from higap points
pdf_ex<-extract(higap_rast, cbind(higap_spdf$x, higap_spdf$y), cellnumbers = T)
higap_spdf$PDF_CELLS<-pdf_ex[,1]
higap_spdf$PDF_ROWS<-pdf_ex[,2]
# head(higap_spdf, n = 50)
# table(higap_spdf$HIGAP_veg_type_90m_20210308, useNA = 'ifany')
# table(is.na(higap_spdf$PDF_ROWS))
# remove PDF NA values to clip to land and exclude background values
higap_spdf<-higap_spdf[which(higap_spdf$PDF_ROWS != "NA"),]
# remove data to free up memory space
rm(pdf_ex)

# combine data by rows
prob_90m<-merge(higap_spdf, all_class_runoff, by.x = "PDF_ROWS", by.y = "...1", all.x = T)
dim(higap_spdf); dim(prob_90m); head(prob_90m)
# save master data file
write_csv(prob_90m, file = paste0(outDir, "probabilities/", "all_prob_runoff_90m.csv"))

# set up data frames for rasters
rast_df<-prob_90m[,c(2, 3, 4, 13, 14)]
# create a SpatialPointsDataFrame
coordinates(rast_df) = ~x+y

# create an empty raster object to the extent of the points (ncols and nrows of higap)
# rast<-raster(ncols = 240, nrows = 240, ext = extent(higap), crs = crs(higap))
rast<-raster(ncols = 5866, nrows = 4176, ext = extent(higap_main), crs = crs(higap_main))
# dim() and values() are non-spatial properties
# extent(), crs(), and res() are spatial properties

# rasterize your irregular points (not gridded) at 240 m
higap_rast<-rasterize(rast_df, rast, rast_df$HIGAP_veg_type_90m_20210308, fun = mean)
hist_rast<-rasterize(rast_df, rast, rast_df$HIST_RUNOFF_PROB, fun = mean)
pgw_rast<-rasterize(rast_df, rast, rast_df$PGW_RUNOFF_PROB, fun = mean)
# use a mean function here to regularly grid the irregular input points
# higap_main; higap_rast # higap_main-higap_rast should = 0
# plot(pgw_rast-hist_rast, main = "Delta PGW-HIST", col = topo.colors(50))
# plot(loc_utm, add = T)

# # ***CHECK IF NEEDED AT 90 M***
# # NO NEED TO RESAMPLE OR MASK DATA - KEEP ORIGINAL RESOLUTION
# # resample data
# higap_samp<-resample(higap_rast, higap, method = "bilinear")
# # mask data
# higap_mask<-mask(higap_samp, higap)

# save raster outputs
dir.create(paste0("outputs/prob_rasters/"), showWarnings = F)
writeRaster(hist_rast, format = "GTiff", overwrite = TRUE,
            "outputs/prob_rasters/ALL_hist_prob_runoff_90m.tif")
writeRaster(pgw_rast, format = "GTiff", overwrite = TRUE,
            "outputs/prob_rasters/ALL_pgw_prob_runoff_90m.tif")

##### RASTER OUTPUTS #####

# ALL HIGAP CLASSES RASTER OUTPUTS
plot(hist_rast, col = rev(topo.colors(30)), main = "HIST PROB RUNOFF")
# save tiff image file
tiff("outputs/prob_rasters/ALL_hist_prob_runoff_90m_IMG.tif", res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(hist_rast*100, col = rev(topo.colors(30)), main = "HIST PROB (%) RUNOFF")
dev.off()

plot(pgw_rast, col = rev(topo.colors(30)), main = "PGW PROB RUNOFF")
# save tiff image file
tiff("outputs/prob_rasters/ALL_pgw_prob_runoff_90m_IMG.tif", res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(pgw_rast*100, col = rev(topo.colors(30)), main = "PGW PROB (%) RUNOFF")
dev.off()

# DELTA RASTER OUTPUTS
summary(pgw_rast*100)-summary(hist_rast*100)
plot(pgw_rast - hist_rast, col = rev(topo.colors(30)), main = "Delta Change")
# save raster file
writeRaster(pgw_rast - hist_rast, format = "GTiff", overwrite = TRUE,
            "outputs/prob_rasters/ALL_pgw-hist_prob_runoff_delta_90m.tif")
# save tiff image file
tiff("outputs/prob_rasters/ALL_pgw-hist_prob_runoff_delta_90m_IMG.tif", res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(pgw_rast*100 - hist_rast*100, col = rev(topo.colors(30)), main = "PGW - HIST Delta (%) Change")
dev.off()
# plot(hist_rast*100 - pgw_rast*100, col = rev(topo.colors(30)), main = "Delta (%) Change")

dir.create(paste0("outputs/prob_rasters/higap_class/"), showWarnings = F)
# create function to run raster creation per higap class 
class_rast<-function(class_nm, class_name){
  # select data for class
  class_df<-prob_90m[which(prob_90m$HIGAP_veg_type_90m_20210308 == class_nm),]
  
  # set up data frames for rasters
  class_hist<-class_df[,c(2, 3, 13)]
  class_pgw<-class_df[,c(2, 3, 14)]
  
  # create a SpatialPointsDataFrame
  coordinates(class_hist) = ~x+y 
  coordinates(class_pgw) = ~x+y
  
  # rasterize your irregular points (not gridded)
  r_class_hist<-rasterize(class_hist, rast, class_hist$HIST_RUNOFF_PROB, fun = mean)
  r_class_pgw<-rasterize(class_pgw, rast, class_pgw$PGW_RUNOFF_PROB, fun = mean)
 
  # # NO NEED TO RESAMPLE OR MASK DATA - KEEP 90 M RESOLUTION FROM RAST
  
  # save raster outputs
  writeRaster(r_class_hist, format = "GTiff", overwrite = TRUE,
              paste0("outputs/prob_rasters/higap_class/", 
                     class_name, "_hist_prob_runoff_90m.tif"))
  writeRaster(r_class_pgw, format = "GTiff", overwrite = TRUE,
              paste0("outputs/prob_rasters/higap_class/", 
                     class_name, "_pgw_prob_runoff_90m.tif"))
  
  return(plot(r_class_pgw*100 - r_class_hist*100, # zlim = c(0, 3), 
              col = rev(topo.colors(30)), main = paste(class_name, "Delta (%) Change")))
}

# run function for each higap class
class_rast(0, "bare_soil")
class_rast(1, "grass")
class_rast(2, "shrubs_trees")
class_rast(3, "forests")

##### END #####
