source("./config.R")
source("./functions.R")
source("./rf_functions.R")

#'---------------------------------
#' Start here
#'---------------------------------
t0 = Sys.time()
print(paste(t0, "-- Begin process"))

# not yet cuz i dont have the training data!
# #' Load training polygons as sf object
# classes <- LoadTrainingData()

# Work on each dataset  and study area separately
crop_rasters <- lapply(study_areas$name, function(sa){
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
      #print(paste0(sa,"_", tif_dirs_full, ".tif")) # can be name of raster?
      study_area <- study_areas[study_areas$name == sa,]
      cropped <- CropDatasets(tif_list, study_area)
      crop_all_layers <- AddImageTexture(cropped)
      
      
      #bands_tif_path <- AddImageTexture(cropped, d)
      #segmentation <- Perform_Segmentation(crop_all_layers, d, sa)
      
      #save the cropped images
      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste(sa, datestr, sep="_")
      rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= crop_all_layers, filename = rastpath, overwrite = TRUE)
     
      return(crop_all_layers)
    }
  })
})

# Get a list of all segmentations,
# one for each combination of date and study area
#segmentations <- unlist(segmentations, recursive = FALSE)

#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))


### random forest 
# create raster for classification
raster1 = load_rast_4_RF(rast_4_RF, rast_shp )
# add texture and index bands
raster1 = addallbands(raster = raster1)
# create training data
TD = create_td(training_data_RF, allbands = raster1)
# delete columns not used in the random forest 
# will change depending on what model we use
## normally has blue but for now has geen
TD1 = TD %>%
  select(-aerosol, -red, -blue, -SWIR2, -dissimilarity, - SAVI)
# run the random forest model
mod = Prepare_RF_Model(training_data = TD1)

# get names of cropped raster's
tif_cropped = list.files(cropped_dir)
tif_cropped = tif_cropped[grep(pattern =".tif$", x=tif_cropped)]

# to add to plots
colors = c("gray", "yellow", "cyan", "dark green", "black", "blue")
x = levels(TD$ground_type)

# loop through the cropped raster's and classify them 

classify_raster = lapply(tif_cropped, function(r){
  print(paste0("cropped\\", r))
  r_1 = rast(paste0("cropped\\", r)) # load raster
  #plot(r_1$green, main = paste(r, "green band"))
  classify_r = ApplyRFModel(all_rast_4_RF = r_1, fit = mod) # classify the raster
  r_split <- strsplit(x=basename(r), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste( r_split, "classified", sep="_")
  rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  plot(classify_r, col = colors, type = "classes", levels = x, main = paste(r, "classified"))
  writeRaster(classify_r, filename = rastpath, overwrite = TRUE ) # save the raster
  return(classify_r)
})







