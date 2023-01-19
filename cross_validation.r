wd="D:/projects/HIecoH/HIecoH_P2/HIecoH_comparison/"
setwd(wd)

runoff_ratios=read.csv("WB_basins/table_8_mean_runoff_ratios_v2.csv")
#View(runoff_ratios)

library(raster)
file1=shapefile("WB_basins/WB_basins_Hawaii.shp")
file2=shapefile("WB_basins/WB_basins_Ka_Oa_Ma.shp")
file3=shapefile("WB_basins/WB_basins_Molokai.shp")

names(file1@data)[1]="ID"
names(file2@data)[2]="ID"
names(file3@data)[1]="ID"
file1@data$file="Hawaii"
file2@data$file="Ka_Oa_Ma"
file3@data$file="Molokai"

file1@data=file1@data[,c("file", "ID")]
file2@data=file2@data[,c("file", "ID")]
file3@data=file3@data[,c("file", "ID")]

all_basins=file1
all_basins=raster::union(all_basins, file2)
all_basins=raster::union(all_basins, file3)
#all_basins=spTransform(all_basins,  CRS("+proj=longlat +datum=WGS84"))

historical_prob_raster=raster("D:/projects/HIecoH/HIecoH_P2/HIecoH_outputs/revised_raster_outputs/hist_scen_raster.tif")

basin_runoff=raster::extract(historical_prob_raster, all_basins, fun=mean, na.rm=T)
#View(basin_runoff)

all_basins$p_excess_rain=basin_runoff[,1]
names(all_basins@data)=c("file", "ID", "p_excess_rain")
#View(all_basins@data)

##########
historical_prob_stack=stack("D:/projects/HIecoH/HIecoH_P2/HIecoH_outputs/revised_raster_outputs/hist_stack_hr.tif")
names(historical_prob_stack)=c("bare", "grass", "woody")

MAP=raster("D:/data/climate_data/20201123_HRCM_NCAR_projections2/bioclims/baseline_rasters/bio12.tif")
MAP=projectRaster(MAP, historical_prob_stack)
basin_mean_MAP=raster::extract(MAP, all_basins, fun=mean, na.rm=T)
basin_mean_MAP=basin_mean_MAP[,1]

basin_runoff_by_cover=raster::extract(historical_prob_stack, all_basins, fun=mean, na.rm=T)
#View(basin_runoff_by_cover)
all_basins_cv=all_basins
all_basins_cv=cbind(all_basins_cv, basin_runoff_by_cover, basin_mean_MAP)
names(all_basins_cv@data)[ncol(all_basins_cv@data)]="MAP"

all_basins_cv_DF=all_basins_cv@data
#View(all_basins_cv_DF)
RR_and_PER_DF=merge(runoff_ratios, all_basins_cv_DF, by="ID")
RR_and_PER_DF$avg_PER=apply(RR_and_PER_DF[,c("bare", "grass", "woody")], 1, mean)
#View(RR_and_PER_DF)

plot(RR_and_PER_DF$annual_avg, RR_and_PER_DF$p_excess_rain)

plot(RR_and_PER_DF$annual_avg, RR_and_PER_DF$bare, xlab="Runoff ratio", ylab="Excess rainfall probability")
cor(RR_and_PER_DF$annual_avg, RR_and_PER_DF$bare)

plot(RR_and_PER_DF$annual_avg, RR_and_PER_DF$grass, xlab="Runoff ratio", ylab="Excess rainfall probability")
cor(RR_and_PER_DF$annual_avg, RR_and_PER_DF$grass)

plot(RR_and_PER_DF$annual_avg, RR_and_PER_DF$woody, xlab="Runoff ratio", ylab="Excess rainfall probability")
cor(RR_and_PER_DF$annual_avg, RR_and_PER_DF$woody)
#correlation better for bare soil! worse for woody

plot(RR_and_PER_DF$annual_avg, RR_and_PER_DF$avg_PER, xlab="Runoff ratio", ylab="Excess rainfall probability")
cor(RR_and_PER_DF$annual_avg, RR_and_PER_DF$avg_PER)

plot(RR_and_PER_DF$MAP, RR_and_PER_DF$annual_avg, xlab="MAP", ylab="Runoff ratio")
cor(RR_and_PER_DF$MAP, RR_and_PER_DF$annual_avg)

plot(RR_and_PER_DF$MAP, RR_and_PER_DF$avg_PER, xlab="MAP", ylab="Excess rainfall percent probability")
cor(RR_and_PER_DF$MAP, RR_and_PER_DF$avg_PER)

plot(RR_and_PER_DF$MAP, RR_and_PER_DF$bare, xlab="MAP", ylab="Excess rainfall percent probability")
cor(RR_and_PER_DF$MAP, RR_and_PER_DF$bare)

plot(RR_and_PER_DF$MAP, RR_and_PER_DF$p_excess_rain, xlab="MAP", ylab="Excess rainfall percent probability")
cor(RR_and_PER_DF$MAP, RR_and_PER_DF$p_excess_rain)

# all_basins$p_excess_rain=basin_runoff
# names(all_basins)=c("file", "ID", "p_excess_rain")
