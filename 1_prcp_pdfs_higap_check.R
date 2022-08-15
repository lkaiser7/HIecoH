# Hawaii Climate Projections for Ecological Models

# Data from Lulin Xue and processed by Yaping Wang
# netcdf data of PDFs for hourly rain rate in bins
# historical and pgw (future) nc data extraction #

# USGS HIGAP landcover data available at 90 m
# extracts higap class values for pdf points #

##### SET UP #####

# set working directory
rootDirs=c("C:/Users/lkaiser/Desktop/HCSU/HIecoH/", "D:/projects/HIecoH/HIecoH_P2/HIecoH/")
rootDir=rootDirs[min(which(dir.exists(rootDirs)))]
setwd(rootDir)
#rootDir<-"C:/Users/lkaiser/Desktop/HCSU/HIecoH/"setwd(rootDir)
# set paths 
dataDir<-paste0(rootDir, "data/")
outDir<-paste0(dirname(rootDir), "/HIecoH_outputs/")
# create output folder path
dir.create(outDir, showWarnings = FALSE)

# load packages
library(readr)
library(ncdf4)
library(raster)
library(rgdal)

# set coordinate system to be used with data as needed
LatLon<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
utmSys<-'+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs'

# load higap data
# higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_240m.tif"))
# higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_60m_20210308.tif"))
higap_raster<-raster(paste0(dataDir, "higap/HIGAP_veg_type_90m_20210308.tif"))
higap_raster[higap_raster==3]=2
higap_raster=higap_raster+1
#plot(higap_raster)
# 0 - bare
# 1 - grass 
# 2 - shrub 
# 3 - forest
# NA - all else
# higap data also available at 900m if needed

# load location coast shapefile from file path to personal database
loc_coast<-readOGR("data/map_data", "Main_Hawaiian_Islands_simple3")
# convert Islands coastlines to utm coordinates
loc_utm<-spTransform(loc_coast, CRS(utmSys))
plot(loc_utm, add = T)

##### DATA PROCESSING #####

#######################################################
#process nc files that have pdf data for each land pixel
# select hourly precipitation pdf grids
pdf_grids<-list.files(paste0(dataDir, "hourly_pcp_pdf_grid/"), pattern = ".nc")
# head(pdf_grids)
n_pdf<-length(pdf_grids)
n=1
for(n in 1:n_pdf){  # set n = 1 for debugging
  print(n) # for counting
  # open netcdf file
  nc<-nc_open(paste0(dataDir, "hourly_pcp_pdf_grid/", pdf_grids[n]), write = TRUE)
  
  # coordinates
  xlat<-ncvar_get(nc, "xlat")
  xlong<-ncvar_get(nc, "xlong")
  ll_coords<-data.frame(xlong, xlat)
  
  # data values
  hist_array<-ncvar_get(nc, "hist_pdf")
  pgw_array<-ncvar_get(nc, "pgw_pdf")
  
  # combine coordinate and array data
  nc_hist<-c(xlong, xlat, hist_array) # historical
  nc_pgw<-c(xlong, xlat, pgw_array)   # future pgw
  
  # build datasets
  if(n == 1){
    nc_hist_data<-nc_hist
    nc_pgw_data<-nc_pgw
    ll_array<-ll_coords
  }else{
    nc_hist_data<-rbind(nc_hist_data, nc_hist)
    nc_pgw_data<-rbind(nc_pgw_data, nc_pgw)
    ll_array<-rbind(ll_array, ll_coords)
  }
  
  # close nc file connection
  nc_close(nc)
}

# label columns
colnames(nc_hist_data)<-c("LONG", "LAT", 1:121)
colnames(nc_pgw_data)<-c("LONG", "LAT", 1:121)
head(nc_hist_data); head(nc_pgw_data)
# save data table  
write.csv(nc_hist_data, file = paste0(outDir, "AllRasters_coords_historical_pdf.csv"))
write.csv(nc_pgw_data, file = paste0(outDir, "AllRasters_coords_future_pdf.csv"))

# # OUTPUT CHECK
# nc_hist_data<-read.csv(paste0(outDir, "AllRasters_coords_historical_pdf.csv"))
# nc_pgw_data<-read.csv(paste0(outDir, "AllRasters_coords_future_pdf.csv"))
# head(nc_hist_data)
# head(nc_pgw_data)

######################################################################
#NOW EXTRACT HIGAP VALUES FOR SAME POINTS #####

# label columns
colnames(ll_array)<-c("LONG", "LAT")
# # save data table  
# write.csv(ll_array, file = paste0(outDir, "AllRasters_coords_pdf_LL.csv"))
head(ll_array)
# create copy of data
ll_array2<-ll_array

# convert points
ll_pts<-SpatialPoints(cbind(ll_array2$LONG, ll_array2$LAT), proj4string = CRS(LatLon))
ll_utm<-spTransform(ll_pts, CRS(utmSys))

# create data frame
utm_pts<-as.data.frame(ll_utm)
# label columns and save points
colnames(utm_pts)<-c("LONG", "LAT")
# # save data table 
# write.csv(utm_pts, file = paste0(outDir, "AllRasters_coords_pdf_UTM.csv"))

# extract higap values at UTM points
higap_pts<-extract(higap_raster, cbind(utm_pts$LONG, utm_pts$LAT), cellnumbers = T)
table(higap_pts[,2], useNA = "ifany")

# create final data set to save
pdf_higap<-data.frame(CELL = higap_pts[,1], LONG = ll_array$LONG, 
                      LAT = ll_array$LAT, X_UTM = utm_pts$LONG, 
                      Y_UTM = utm_pts$LAT,HIGAP = higap_pts[,2])
head(pdf_higap)

# save final data set
write.csv(pdf_higap, file = paste0(outDir, "AllRasters_coords_pdf_higap.csv"))

####################################################
#NOW CREATE A RASTER BASED ON THE POINT DATA

#FIRST LOAD THE OUTPUTS FROM STEP ABOVE
# create pdf higap raster
pdf_higap<-read.csv(paste0(outDir, "AllRasters_coords_pdf_higap.csv"))
head(pdf_higap)

# pdf historical and pgw (future) data
nc_hist_data<-read.csv(paste0(outDir, "AllRasters_coords_historical_pdf.csv"))
nc_pgw_data<-read.csv(paste0(outDir, "AllRasters_coords_future_pdf.csv"))
# merge pdf higap data with historical and pgw (future) data
pdf_hist<-cbind(pdf_higap, nc_hist_data[,-1:-3])
pdf_pgw<-cbind(pdf_higap, nc_pgw_data[,-1:-3])
# head(pdf_hist, n = c(3, 9))
# head(pdf_pgw, n = c(3, 9))

# #GET HIGHER RESOLUTIO HIGAP (90M) AND TURN INTO DATA FRAME
# # extract higap as data frame
# # higap_spdf<-as.data.frame(as(higap, "SpatialPixelsDataFrame"))
# higap_spdf<-as.data.frame(higap, xy = T)
# higap_ex<-extract(higap, cbind(higap_spdf$x, higap_spdf$y), cellnumbers = T)
# head(higap_ex)
# higap_spdf$CELLS<-higap_ex[,1]
# dim(higap_spdf); head(higap_spdf)
# # remove data to free up memory space
# rm(higap_ex)

##################
#now calc runoff prob
# higap classes for reference
# h_num<-c(0, 1, 2, 3)
h_num<-c(1, 2, 3)
h_name<-c("bare_soil", "grass", "woody")

kfs_class<-read.csv("infiltration_data/kfs_data/hiecoh_kfs_data.csv")
kfs_class[kfs_class$cover_type=="shrubs_trees","cover_type"]="woody"
kfs_class[kfs_class$cover_type=="forests","cover_type"]="woody"


left_flux_breaks = seq(from = 10^-3, to = 10^3, length.out = 120)
dir.create(paste0(outDir, "probabilities/"), showWarnings = F)

pdf_hist_output_df<-pdf_hist[, 1:7]
pdf_pgw_output_df<-pdf_pgw[, 1:7]

# loop through all higap classes
h=1
for (h in 1:3){  # set h = 2 for debugging
  cat(c_name, "\n") # for counting 
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
  loc<-pdf_hist[, 1:7]
  # head(loc)
  
  # pdf rainfall data by class
  rf_hist<-pdf_hist[which(pdf_hist$HIGAP == c_num), -1:-7]
  rf_pgw<-pdf_pgw[which(pdf_pgw$HIGAP == c_num), -1:-7]
  
  rf_hist<-pdf_hist[, -1:-7]
  rf_pgw<-pdf_pgw[, -1:-7]
  

  hist_prob_vec=c()
  pgw_prob_vec=c()
  # loop through each row and calculate runoff probability
  r=1
  for(r in 1:dim(rf_hist)[1]){  # set r = 1 for debugging
    cat(r, " ") # for counting 
    # for(r in 1:10){  # set r = 1 for debugging
    rf_dist_hist<-as.matrix(rf_hist[r,])
    # hist(rf_dist_hist)
    rf_dist_pgw<-as.matrix(rf_pgw[r,])
    # hist(rf_dist_pgw)
    
    # rainfall probabilities along the size classes
    rf_prob_hist=hist(rf_dist_hist, breaks=flux_breaks, plot=F)$counts/sum(hist(rf_dist_hist, breaks=flux_breaks, plot=F)$counts)
    # plot(flux_breaks_midpoints, rf_prob_hist, type="l")
    rf_prob_pgw=hist(rf_dist_pgw, breaks=flux_breaks, plot=F)$counts/sum(hist(rf_dist_pgw, breaks=flux_breaks, plot=F)$counts)
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
    
    hist_prob_vec=c(hist_prob_vec, hist_prob_runoff)
    pgw_prob_vec=c(pgw_prob_vec, pgw_prob_runoff)
    
    # # create data to be saved
    # prob_runoff<-data.frame(loc[r,], HIST_RUNOFF_PROB = hist_prob_runoff, PGW_RUNOFF_PROB = pgw_prob_runoff)
    # 
    # # combine all rows of data
    # if(r == 1){
    #   all_runoff<-prob_runoff
    # }else{
    #   all_runoff<-rbind(all_runoff, prob_runoff)
    # }
    
  }
  pdf_hist_output_df=cbind(pdf_hist_output_df, hist_prob_vec)
  names(pdf_hist_output_df)[length(names(pdf_hist_output_df))]=paste0("prob_",c_num)
  #names(pdf_hist_output_df)
  
  pdf_pgw_output_df=cbind(pdf_pgw_output_df, pgw_prob_vec)
  names(pdf_pgw_output_df)[length(names(pdf_pgw_output_df))]=paste0("prob_",c_num)
  
}
#View(pdf_hist_output_df)
#View(pdf_pgw_output_df)


##########################################
#now create rasters at coarse scale for each cover type
rast<-raster("data/pcp_base.tif") #use past raster created from lulins nc files as template
rast=projectRaster(rast, crs = crs(higap))

df_names=c("pdf_hist_output_df", "pdf_pgw_output_df")
df_name=df_names[1]
h=1
for (df_name in df_names){
  if (df_name == df_names[1]){
    scen="hist"
  }else{
    scen="pgw"
  }
  cat(scen, "\n") # for counting 
  pdf_spdf=get(df_name)
  # set up data frames for rasters
  # create a SpatialPointsDataFrame
  coordinates(pdf_spdf) = ~X_UTM+Y_UTM 
  for (h in h_num){  # set h = 2 for debugging
    cat(h, "\n") # for counting 
    # rasterize your irregular points (not gridded)
    pdf_rast<-raster::rasterize(pdf_spdf, rast, pdf_spdf@data[,paste0("prob_", h)])
    # plot(pdf_rast)
    assign(paste0(scen, "_", h), pdf_rast)
  }
}
hist_stack=stack(hist_1, hist_2, hist_3)
pgw_stack=stack(pgw_1, pgw_2, pgw_3)
delta_stack=pgw_stack-hist_stack
plot(hist_stack)
plot(pgw_stack)

##########################################
#now project to same fine resolution as higap 90m map
#higap_raster<-raster(paste0(dataDir, "higap/HIGAP_veg_type_90m_20210308.tif"))

hist_stack_hr=resample(hist_stack, higap_raster, method="ngb")
hist_stack_hr=projectRaster(hist_stack_hr, higap_raster, method="ngb")

pgw_stack_hr=resample(pgw_stack, higap_raster, method="ngb")
pgw_stack_hr=projectRaster(pgw_stack_hr, higap_raster, method="ngb")
delta_stack_hr=pgw_stack_hr-hist_stack_hr
# plot(hist_stack_hr)
# plot(pgw_stack_hr)
# plot(delta_stack_hr)

##########################################
#now use fine scale higap map to create map
hist_scen_raster=stackSelect(hist_stack_hr, higap_raster)
pgw_scen_raster=stackSelect(pgw_stack_hr, higap_raster)
delta_scen_raster=pgw_scen_raster-hist_scen_raster

plot(hist_scen_raster)
plot(pgw_scen_raster)
plot(delta_scen_raster)

##########################################
#write outputs
out_dir="outputs/revised_raster_outputs/"
dir.create(paste0(out_dir), showWarnings = F)
writeRaster(hist_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "hist_stack_hr.tif"), compress="LZW")
writeRaster(pgw_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "pgw_stack_hr.tif"), compress="LZW")
writeRaster(delta_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "delta_stack_hr.tif"), compress="LZW")

writeRaster(hist_stack, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "hist_stack.tif"), compress="LZW")
writeRaster(pgw_stack, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "pgw_stack.tif"), compress="LZW")
writeRaster(delta_stack, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "delta_stack.tif"), compress="LZW")

writeRaster(hist_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "hist_scen_raster.tif"), compress="LZW")
writeRaster(pgw_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "pgw_scen_raster.tif"), compress="LZW")
writeRaster(delta_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(out_dir, "delta_scen_raster.tif"), compress="LZW")
