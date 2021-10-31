source("./config.R")
source("./functions.R")

#'---------------------------------
#' Start here
#'---------------------------------
t0 = Sys.time()
print(paste(t0, "-- Begin process"))

# not yet cuz i dont have the training data!
# #' Load training polygons as sf object
# classes <- LoadTrainingData()

# Work on each dataset  and study area separately
segmentations <- lapply(study_areas$name, function(sa){
  lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into stack, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      # 
      print(paste("In:", sa, "directory:", d))
      print(paste0(sa,"_", tif_dirs_full, ".tif")) # can be name of raster?
      study_area <- study_areas[study_areas$name == sa,]
      cropped <- CropDatasets(tif_list, study_area)
      crop_all_layers <- AddImageTexture(cropped)
      
      #adding the part that saves the raster to the loopsgement
      #raster_name <- (paste0(sa,"_", tif_dirs_full == d, ".tif"))
       
      #writeRaster(crop_all_layers, raster_name, overwrite = TRUE)
      #writeRaster(all_layers, raster_name, overwrite = TRUE)
      # return(bands_tif_path)
      
      #bands_tif_path <- AddImageTexture(cropped, d)
      segmentation <- Perform_Segmentation(crop_all_layers, d, sa)
     
      return(segmentation)
    }
  })
})

# Get a list of all segmentations,
# one for each combination of date and study area
segmentations <- unlist(segmentations, recursive = FALSE)

#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))
