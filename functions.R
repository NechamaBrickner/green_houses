CropDatasets <- function(tif_list, study_area) {
  # Read list of TIF files into stack
  # Crop to extent of testing polygons
  # ---------------------------
  # Check whether L5 or L8
  
    if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
      #select wanted bands landsat 5
      tif_list_05 <- tif_list[grep(pattern="LT05_", x=tif_list)]
      tif_list_05 <- tif_list_05[grep(pattern="_SR_", x=tif_list_05)]
      tif_list_05 <- tif_list_05[grep(pattern = "B1|B2|B3|B4|B5|B7",
                                      x = tif_list_05)]  
      tif_stk <- rast(tif_list_05)
    }
    else {
      #select wanted bands landsat 8
      tif_list_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
      tif_list_08 <- tif_list_08[grep(pattern="_SR_", x=tif_list_08)]
      tif_list_08 <- tif_list_08[grep(pattern = "B2|B3|B4|B5|B6|B7",
                                      x = tif_list_08)] #do we need B1 - arisol 
      tif_stk <- rast(tif_list_08)
  }
  
  names(tif_stk) <- c("blue", "green", "red","NIR", "SWIR1", "SWIR2")
  cropped <- terra::crop(tif_stk, study_area)
  cropped = terra::mask(cropped, study_area)# do we want to use mask for the classification area??
  #' Check bounding box of raster stack and study area
  # (st_bbox(study_area))
  # (bbox(cropped))
  return(cropped)
}


AddImageTexture <- function(cropped) {
  # Run glcm() function to create image texture raster
  # Choose one band for glcm, i.e. green
  # Create texture bands from only one spectral band (texture bands from other bands will be almost the same)
  texture <- glcm(raster(cropped$green), #converting the green band to raster format to work in glcm 
                  statistics =  c('variance', 'contrast'),
                  na_opt = "ignore")
  names(texture) <- c("variance","contrast") 
  # -------------------------------
  # Do we want also an NDVI band?
  ndvi <- ((cropped$NIR - cropped$red) / (cropped$NIR + cropped$red))
  names(ndvi) <- "NDVI"
  # -------------------------------
  # Add to "cropped"
  # Convert texture bands back to terra "SpatRaster"
  all_layers <- c(cropped, rast(texture), ndvi)
 
  return(all_layers)
}


# not using supercells
# Perform_Segmentation <- function(all_layers, d, sa) {
#   superpxl <- supercells(all_layers,
#                          k = 2000, compactness = 500, iter = 30)
#   
#   # Create name to save superpixel polygons as geopackage
#   # Get Landsat date from the directory name
#   d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
#   datestr <- unlist(d_split)[4]
#   shpname = paste(sa, datestr, "segments", sep="_")
#   shppath <- file.path(GIS_dir, paste0(shpname, ".gpkg"))
#   st_write(superpxl, shppath, append = FALSE) #save the superpixel as a gpkg file, and overwrite if the file already exists 
# }

LST_band = function(tif_list, study_area) {
  
  if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
    #select thermal band landsat 5 - B6
    LST_05 <- tif_list[grep(pattern="LT05_", x = tif_list)]
    LST_05 <- LST_05[grep(pattern="ST", x = LST_05)]
    LST_05 <- LST_05[grep(pattern = "B6", x = LST_05)]  
    LST <- rast(LST_05)
  }
  else {
    #select thermal band landsat 8 - B10
    LST_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
    LST_08 <- LST_08[grep(pattern="ST", x=LST_08)]
    LST_08 <- LST_08[grep(pattern = "B10", x = LST_08)] 
    LST <- rast(LST_08)
  }
  names(LST) <- "LST"
  cropped <- crop(LST, study_area)
  #rescale factor
  # landsat 8 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1619_Landsat-8-Collection2_Level-2_Science-Product-Guide-v3.pdf
  # landsat 5 https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1618_Landsat-4-7_C2-L2-ScienceProductGuide-v4.pdf
  cropped = cropped*0.00341802+149.0-272.15
 
  return(cropped)
}
