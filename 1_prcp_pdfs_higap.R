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
higap<-raster(paste0(dataDir, "higap/HIGAP_veg_type_90m_20210308.tif"))
plot(higap)
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

##### HIGAP VALUES #####

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
higap_pts<-extract(higap, cbind(utm_pts$LONG, utm_pts$LAT), cellnumbers = T)
table(higap_pts[,2], useNA = "ifany")

# create final data set to save
pdf_higap<-data.frame(CELL = higap_pts[,1], LONG = ll_array$LONG, 
                      LAT = ll_array$LAT, X_UTM = utm_pts$LONG, 
                      Y_UTM = utm_pts$LAT,HIGAP = higap_pts[,2])
head(pdf_higap)

# save final data set
write.csv(pdf_higap, file = paste0(outDir, "AllRasters_coords_pdf_higap.csv"))

### RASTER REFERENCE ###

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

# exctract higap as data frame
# higap_spdf<-as.data.frame(as(higap, "SpatialPixelsDataFrame"))
higap_spdf<-as.data.frame(higap, xy = T)
higap_ex<-extract(higap, cbind(higap_spdf$x, higap_spdf$y), cellnumbers = T)
head(higap_ex)
higap_spdf$CELLS<-higap_ex[,1]
dim(higap_spdf); head(higap_spdf)
# remove data to free up memory space
rm(higap_ex)

# create an empty raster object to the extent of the points
#rast<-raster(ncols = 240, nrows = 240, ext = extent(higap), crs = crs(higap)) #LF: why 240?
rast<-raster("data/pcp_base.tif") #use past raster created from lulins nc files as template
rast=projectRaster(rast, crs = crs(higap))
# rast<-raster(ncols = 90, nrows = 90, ext = extent(higap), crs = crs(higap))
# dim() and values() are non-spatial properties
# extent(), crs(), and res() are spatial properties

# set up data frames for rasters
higap_df<-pdf_higap[,c(5, 6, 1)]
# create a SpatialPointsDataFrame
coordinates(higap_df) = ~X_UTM+Y_UTM 
# rasterize your irregular points (not gridded)
higap_rast<-rasterize(higap_df, rast, higap_df$X)
# mean not used to avoid changing/averaging values
plot(higap_rast)

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
hist_90m<-merge(higap_spdf, pdf_hist, by.x = "PDF_ROWS", by.y = "X", all.x = T)
dim(higap_spdf); dim(hist_90m)
pgw_90m<-merge(higap_spdf, pdf_pgw, by.x = "PDF_ROWS", by.y = "X", all.x = T)
dim(higap_spdf); dim(pgw_90m)

# save master data files (large files takes time)
write_csv(hist_90m, file = paste0(outDir, "AllRasters_coords_historical_90m.csv"))
write_csv(pgw_90m, file = paste0(outDir, "AllRasters_coords_future_90m.csv"))

##### END #####
