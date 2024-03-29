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

not_too_young=raster(paste0(dataDir, "young_substrate_pioneer.tif"))
not_too_young=!not_too_young
not_too_young[not_too_young==0]=NA
not_too_young=resample(not_too_young, higap_raster, method="ngb")

#plot(not_too_young)

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

#LOAD THE OUTPUTS FROM STEP ABOVE (if you don't want to run things from scratch)
pdf_higap<-read.csv(paste0(outDir, "AllRasters_coords_pdf_higap.csv"))
#head(pdf_higap)
nc_hist_data<-read.csv(paste0(outDir, "AllRasters_coords_historical_pdf.csv"))
nc_pgw_data<-read.csv(paste0(outDir, "AllRasters_coords_future_pdf.csv"))
# merge pdf higap data with historical and pgw (future) data
pdf_hist<-cbind(pdf_higap, nc_hist_data[,-1:-3])
pdf_pgw<-cbind(pdf_higap, nc_pgw_data[,-1:-3])
# head(pdf_hist, n = c(3, 9))
# head(pdf_pgw, n = c(3, 9))

##############################################
#now calc runoff prob for each 
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

# loop through all higap classes and calculate runoff probabilities for each point
h=1
for (h in 1:3){  # set h = 2 for debugging
  # select higap class
  c_num<-h_num[h]
  c_name<-h_name[h]
  cat(c_name, "\n") # for counting 
  
  
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
    #midpoints=hist(rf_dist_hist, breaks=flux_breaks, plot=F)$mids
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
    #View(joined_DF)
    #tmp_DF=joined_DF[c(1:23),]
    #plot(tmp_DF$mm, tmp_DF$Kfs, xlab="Baresoil infiltration capacity (mm/hr)", ylab="Density", type="l", col="red", lwd=5)
    #plot(tmp_DF$mm, tmp_DF$RF_intensity, xlab="Rainfall intensity (mm/hr)", ylab="Density", type="l", col="blue", lwd=5)
    
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


#################################################################
#now create rasters at coarse scale for each cover type
rast<-raster("data/pcp_base.tif") #use past raster created from lulins nc files as template
rast=projectRaster(rast, crs = crs(higap_raster))

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
#make low resolution scenario rasters
#first aggregate higap raster, so it matches the stacks created above
higap_aggr_raster=raster::aggregate(higap_raster, fact=16, fun="modal", na.rm=T)
higap_aggr_raster=projectRaster(higap_aggr_raster, hist_stack, method="ngb")

not_too_young_aggr_raster=raster::aggregate(not_too_young, fact=16, fun="modal", na.rm=T)
not_too_young_aggr_raster=projectRaster(not_too_young_aggr_raster, hist_stack, method="ngb")

#plot(higap_aggr_raster)

#now use higap raster as an index to pick values from the stacks above
hist_scen_raster_aggr=stackSelect(hist_stack, higap_aggr_raster)
pgw_scen_raster_aggr=stackSelect(pgw_stack, higap_aggr_raster)
delta_scen_raster_aggr=pgw_scen_raster_aggr-hist_scen_raster_aggr

hist_scen_raster_aggr=hist_scen_raster_aggr*not_too_young_aggr_raster
pgw_scen_raster_aggr=pgw_scen_raster_aggr*not_too_young_aggr_raster
delta_scen_raster_aggr=delta_scen_raster_aggr*not_too_young_aggr_raster

plot(hist_scen_raster_aggr)
plot(pgw_scen_raster_aggr)
plot(delta_scen_raster_aggr)

##########################################
#now project stack into fine resolution spatial scale (same as higap 90m map)
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
hist_scen_raster=stackSelect(hist_stack_hr, higap_raster)*not_too_young
pgw_scen_raster=stackSelect(pgw_stack_hr, higap_raster)*not_too_young
delta_scen_raster=pgw_scen_raster-hist_scen_raster*not_too_young
delta_scen_raster_prop=(pgw_scen_raster-hist_scen_raster)/hist_scen_raster


library(viridis)
plot(hist_scen_raster, col=viridis(100), axes=F)
plot(loc_utm, add = T)
plot(pgw_scen_raster, col=viridis(100), axes=F)
plot(loc_utm, add = T)
plot(delta_scen_raster)
plot(loc_utm, add = T)
plot(delta_scen_raster_prop)
plot(loc_utm, add = T)


##########################################
#write outputs
out_dir="revised_raster_outputs/"
dir.create(paste0(outDir, out_dir), showWarnings = F)
writeRaster(hist_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "hist_stack_hr.tif"), compress="LZW")
writeRaster(pgw_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "pgw_stack_hr.tif"), compress="LZW")
writeRaster(delta_stack_hr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "delta_stack_hr.tif"), compress="LZW")

writeRaster(hist_stack, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "hist_stack.tif"), compress="LZW")
writeRaster(pgw_stack, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "pgw_stack.tif"), compress="LZW")
writeRaster(delta_stack, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "delta_stack.tif"), compress="LZW")

writeRaster(hist_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "hist_scen_raster.tif"), compress="LZW")
writeRaster(pgw_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "pgw_scen_raster.tif"), compress="LZW")
writeRaster(delta_scen_raster, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "delta_scen_raster.tif"), compress="LZW")
writeRaster(delta_scen_raster_prop, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "delta_scen_raster_prop.tif"), compress="LZW")


writeRaster(hist_scen_raster_aggr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "hist_scen_raster_aggr.tif"), compress="LZW")
writeRaster(pgw_scen_raster_aggr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "pgw_scen_raster_aggr.tif"), compress="LZW")
writeRaster(delta_scen_raster_aggr, format = "GTiff", overwrite = TRUE,
            paste0(outDir, out_dir, "delta_scen_raster_aggr.tif"), compress="LZW")

tif_name=paste0(outDir, out_dir, "hist_scen_raster_fig.tif")
tiff(tif_name, res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(hist_scen_raster, col=viridis(100), axes=F)
plot(loc_utm, add = T)
dev.off()

tif_name=paste0(outDir, out_dir, "pgw_scen_raster_fig.tif")
tiff(tif_name, res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(pgw_scen_raster, col=viridis(100), axes=F)
plot(loc_utm, add = T)
dev.off()

tif_name=paste0(outDir, out_dir, "delta_scen_raster_fig.tif")
min_val=floor(cellStats(delta_scen_raster, min, na.rm=T)*100)/100
max_val=ceiling(cellStats(delta_scen_raster, max, na.rm=T)*100)/100
step_sz=0.0025
neg_vals=seq(min_val, -step_sz, step_sz)
pos_vals=seq(step_sz, max_val, step_sz)
breakpoints <- c(neg_vals, 0, pos_vals)
colors <- c(rev(RColorBrewer::brewer.pal(length(neg_vals), "Reds")), "white", RColorBrewer::brewer.pal(length(pos_vals), "Blues"))

tiff(tif_name, res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(delta_scen_raster, breaks = breakpoints, col = colors, axes=F)
plot(loc_utm, add = T)
dev.off()

tif_name=paste0(outDir, out_dir, "delta_scen_raster_prop_fig.tif")
min_val=floor(cellStats(delta_scen_raster_prop, min, na.rm=T)*100)/100
max_val=ceiling(cellStats(delta_scen_raster_prop, max, na.rm=T)*100)/100
neg_vals=seq(min_val, -0.005, 0.005)
pos_vals=seq(0.005, max_val, 0.005)
breakpoints <- c(neg_vals, 0, pos_vals)
colors <- c(rev(RColorBrewer::brewer.pal(length(neg_vals), "Reds")), "white", RColorBrewer::brewer.pal(length(pos_vals), "Blues"))
#plot(delta_scen_raster_prop, breaks = breakpoints, col = colors)

tiff(tif_name, res = 300, units = "in", 
     pointsize = 12, width = 10, height = 8, compression = "lzw")
plot(delta_scen_raster_prop, breaks = breakpoints, col = colors, axes=F)
plot(loc_utm, add = T)
dev.off()

###################################################
#calculate some summaries
#hist
x=zonal(hist_scen_raster_aggr, higap_aggr_raster, fun="mean")[,2]
y=zonal(hist_scen_raster_aggr, higap_aggr_raster, fun="sd")[,2]
z=zonal(hist_scen_raster_aggr, higap_aggr_raster, fun="count")[,2]

x2=zonal(pgw_scen_raster_aggr, higap_aggr_raster, fun="mean")[,2]
y2=zonal(pgw_scen_raster_aggr, higap_aggr_raster, fun="sd")[,2]
z2=zonal(pgw_scen_raster_aggr, higap_aggr_raster, fun="count")[,2]

cover_results=data.frame(hist_mean=x, hist_sd=y, hist_count=z,
                              pgw_mean=x2, pgw_sd=y2, pgw_count=z2)
write.csv(cover_results, paste0(outDir, "runoff_probabilities.csv"), row.names = F)
jnk=round(cover_results$hist_mean/cover_results$hist_mean[3], 2)
dput(jnk) #c(3.34, 1.75, 1)
#now make histograms

i=1
for (i in h_num){
  c_name=h_name[i]
  mask=higap_aggr_raster==i
  mask[mask==0]=NA
  tmp_raster=hist_scen_raster_aggr*mask
  tmp=tmp_raster[]
  tmp=tmp[!is.na(tmp)]
  #plot(tmp_raster)
  vals=hist(tmp, breaks=seq(0, 1, 0.005), plot=F)$density
  vals_DF=data.frame(cover=c_name, x=seq(0, 1, 0.005)[-1], y=vals)
  if (i==1){
    combo_vals_DF=vals_DF
  }else{
    combo_vals_DF=rbind(combo_vals_DF, vals_DF)
  }
}
#View(combo_vals_DF)
library(ggplot2)
ggplot(combo_vals_DF,aes(x=x,y=y,fill=cover)) + 
        geom_bar(stat="identity",position = "dodge", alpha=.3)

#hist_cover_results$ci=hist_cover_results$sd*qnorm(0.975)/sqrt(hist_cover_results$count)
#hist_cover_results$ci_high=hist_cover_results$mean+hist_cover_results$ci
#hist_cover_results$ci_low=hist_cover_results$mean-hist_cover_results$ci

