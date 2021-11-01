
# load training data- i need to make
# LoadTrainingData <- function() {
#   #' Load training polygons (from geopackage) and
#   #' raster image bands (geotiff) as terra stack
#   classes_gpkg <- file.path(GIS_dir, "training_classes.gpkg")
#   classes <- st_read(classes_gpkg)
#   return(classes)
# }

study_areas = st_read("area.shp")
tif_dirs_full <- list.dirs(datasets_dir)[-1] #gets all the folders in the dataset_dir without the dataset_dir folder
#tif_dirs <- list.dirs(datasets_dir, full.names = TRUE, recursive = TRUE)



CropDatasets <- function(tif_list, study_area) {
  # Read list of TIF files into stack
  # Crop to extent of testing polygons
  # ---------------------------
  # TODO: 
  # Define separate study areas for each Yishuv, 
  # and do the cropping for each separately
  # ---------------------------
  # Check whether L5 or L8
  
    if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
      #select wanted bands landsat 5
      tif_list_05 <- tif_list[grep(pattern="LT05_", x=tif_list)]
      tif_list_05 <- tif_list_05[grep(pattern="_SR_", x=tif_list_05)]
      tif_list_05 <- tif_list_05[grep(pattern = "B1|B2|B3|B4|B5|B7",
                                      x = tif_list_05)]  
      tif_stk <- rast(tif_list_05)
      #tif_stk <- stack(tif_list)
      names(tif_stk) <- c("blue", "green", "red",
                          "NIR", "SWIR1", "SWIR2")
    }
    else {
      #select wanted bands landsat 8
      tif_list_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
      tif_list_08 <- tif_list_08[grep(pattern="_SR_", x=tif_list_08)]
      tif_list_08 <- tif_list_08[grep(pattern = "B1|B2|B3|B4|B5|B6|B7",
                                      x = tif_list_08)] #do we need B1 - arisol 
      tif_stk <- rast(tif_list_08)
      #tif_stk <- stack(tif_list)
      names(tif_stk) <- c("aerosol", "blue", "green", "red",
                          "NIR", "SWIR1", "SWIR2")
  }
  
  #study_area = st_read("area.shp") # shape i made, can this be outside of the function
  
  cropped <- crop(tif_stk, study_area)
  
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
                  statistics =  c('variance', 'second_moment'),
                  na_opt = "ignore")
  #names() #change the name to shorter??
  # -------------------------------
  # Do we want also an NDVI band?
  ndvi <- (cropped$NIR - cropped$red) / (cropped$NIR + cropped$red)
  names(ndvi) <- "NDVI"
  # -------------------------------
  # Add to "cropped"
  # Convert texture bands back to terra "SpatRaster"
  all_layers <- c(cropped, rast(texture), ndvi)
  #all_layers <- c(rast(cropped), rast(texture), rast(ndvi))
  # Save to cropped_dir, use original directory name for the tif file
 
  #base_tif_dir <-  basename(tif_dirs[2:length(tif_dirs)])
  # base_tif_dir <-  basename(tif_dirs)
  # bands_tif <- paste0(base_tif_dir, ".tif")
  # bands_tif_path = file.path(cropped_dir, bands_tif)
  # writeRaster(all_layers, bands_tif_path, overwrite = TRUE)
  # return(bands_tif_path)
  return(all_layers)
}


Perform_Segmentation <- function(all_layers, d, sa) {
  superpxl <- supercells(all_layers,
                         k = 2000, compactness = 500, iter = 30)
  
  # Create name to save superpixel polygons as geopackage
  # Get Landsat date from the directory name
  d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
  datestr <- unlist(d_split)[4]
  shpname = paste(sa, datestr, "segments", sep="_")
  shppath <- file.path(GIS_dir, paste0(shpname, ".gpkg"))
  st_write(superpxl, shppath, append = FALSE) #save the superpixel as a gpkg file, and overwrite if the file already exists 
}

