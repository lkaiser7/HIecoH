Use updated HIGAP files from Lucas (as of 02/05/21)

Maps redone and aggregated to 240 and 900m resolution

reclass_table=(read.csv('HIGAP_attribute_data.csv',header=T, stringsAsFactors=F))
reclass_table=reclass_table[,c("Value", "maui_veg_type")]
HIGAP_reclass=subs(HIGAP,  reclass_table)
HIGAP_reclass_900=raster::aggregate(HIGAP_reclass, fact=30, fun=modal)
writeRaster(HIGAP_reclass_900, "HIGAP_veg_type_900m_20210205.tif", format="GTiff", overwrite=TRUE, compress="LZW")

###################################################

USE "HIGAP_veg_type.tif" FILE FROM LUCAS

0 - bare
1 - grass 
2 - shrub 
3 - forest
NA - all else

###################################################

OTHER PROCESSED "HIGAP_coarse_class.tif" DATA FILES
Land Cover Data (Jacobi et al. 2017)

Reclassified for infiltration data values
Original data at 30m resolution
Coarser resolution at 500m

0 - agriculture/developed (NOT USED)
1 - bare soil
2 - grassland
3 - dry shrubland/forests
4 - mesic/wet shrubland/forests