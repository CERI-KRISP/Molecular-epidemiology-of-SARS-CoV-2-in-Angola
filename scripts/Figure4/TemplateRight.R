library(raster)
library(seraphim) # optional
#setwd("")

#This script generates the .asc raster file needed for to plot the phylogeography maps in Fig4 by specifying the coordinates of the borders or the required geographical section
#Just change the values of x_min, x_max, y_min and y_max to generate a different raster .asc file

elevation = raster("phylogeographies/C.16/visualization/elevation_marta.tiff")

plot(elevation) # "raster" package
plotRaster(elevation) # "seraphim" package

x_min = 10; x_max =40; y_min = -35; y_max = 5
study_area = extent(x_min, x_max, y_min, y_max)
elevation_cropped = crop(elevation, study_area) # to crop the template
plot(elevation_cropped)

template = elevation_cropped
template[!is.na(template[])] = 0 # to "empty" the elevation raster
plot(template)
writeRaster(template, "phylogeographies/C.16/visualization/C16_studyarea_round2.asc", overwrite=TRUE)
