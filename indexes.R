library(terra)
library(sf)
#r = rast("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\work lab assistant\\project\\green_houses\\cropped\\full_area_20200418.tif")
r = rast("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\work lab assistant\\project\\green_houses\\cropped\\full_area_2002_02_28.tif")
names(r) = c("blue", "green", "red", "NIR", "swir1", "swir2", "variance", "contrast", "NDVI")

#vegitation indexes
ndvi = r$NDVI #normalized difference vegetation index
savi = 1.5*((r$NIR - r$red)/(r$NIR + r$red + 0.5)) #Soil Adjusted Vegetation Index
evi = 2.5 * ((r$NIR - r$red) / (r$NIR + 6 *r$red - 7.5*r$blue + 1)) #enhanced vegetation index
tdvi = ((r$NIR - r$red)/(sqrt(r$NIR**2 + r$red +0.5)))*1.5 #transformed difference vegetation index

#greenhouse abd other  indexes
#APGI = 100*r$coastal*r$red*((2*(r$nir-r$red-r$swir2))/(2*(r$nir+r$red+r$swir2))) #Advanced Plastic Greenhouse Index
PMLI = ((r$swir1 - r$red) / (r$swir1 + r$red)) #plastic mulched landcover index
vi = ((r$swir1 - r$NIR) / (r$swir1 + r$NIR)) * ((r$NIR - r$red) / (r$NIR + r$red)) #greenhouse vegetable land index 
ndbi = ((r$swir1 - r$NIR) / (r$swir1 + r$NIR)) #Normalized Difference Built-up Index
pgi = (100*((r$blue*(r$NIR - r$red))/1-mean(r$blue+r$green+r$NIR))) #plastic greenhouse index
rpgi = (r$blue/1-mean(r$blue+r$green+r$NIR)) #retrogresive pgi
bsi =  (((r$swir1 - r$red) - (r$NIR - r$blue))/(r$swir1 +r$red) +(r$NIR+r$blue)) #bare soil index

plot(ndvi)
plot(savi)
plot(evi)
plot(tdvi)
plot(PMLI)
plot(vi)
#plot(APGI)
plot(ndbi)
plot(pgi)
plot(rpgi)
plot(bsi)

writeRaster(x = ndvi, filename = "index\\ndvi_l5.tif",
            overwrite = TRUE)
writeRaster(x = savi, filename = "index\\savi_l5.tif",
            overwrite = TRUE)
writeRaster(x = evi, filename = "index\\evi_l5.tif",
            overwrite = TRUE)
writeRaster(x = tdvi, filename = "index\\tdvi_l5.tif",
            overwrite = TRUE)
# writeRaster(x = APGI, filename = "index\\apgi.tif",
#             overwrite = TRUE)
writeRaster(x = PMLI, filename = "index\\pmli_l5.tif",
            overwrite = TRUE)
writeRaster(x = vi, filename = "index\\vi_l5.tif",
            overwrite = TRUE)
writeRaster(x = ndbi, filename = "index\\ndbi_l5.tif",
            overwrite = TRUE)
writeRaster(x = pgi, filename = "index\\pgi_l5.tif",
            overwrite = TRUE)
writeRaster(x = bsi, filename = "index\\bsi_l5.tif",
            overwrite = TRUE)
