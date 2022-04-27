source("./config.R")
source("./functions.R")
source("./rf_functions.R")

#'---------------------------------
#' Start here
#'---------------------------------
t0 = Sys.time()
print(paste(t0, "-- Begin process"))

# Load study areas and Landsat tiles
full_area = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                      layer="classification_area")
# Get all Landsat folders in datasets_dir
tif_dirs_full <- list.dirs(datasets_dir)[-1]


#'---------------------------------
#' Crop Landsat to full area
#'---------------------------------
# Work on each Landsat dataset separately
crop_rasters <- lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      # 
      cropped <- CropDatasets(tif_list, full_area)
      crop_all_layers <- AddImageTexture(cropped)
      
      #save the cropped images
      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste("full_area", datestr, sep="_")
      rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= crop_all_layers,
                         filename = rastpath, overwrite = TRUE)
     
      return(crop_all_layers)
    }
})
names(crop_rasters) <- basename(tif_dirs_full) #gives the name of the image by the date...
#'---------------------------------
#' Random Forest classification
#'---------------------------------
# crop_rasters list holds *all* dates with 9 bands each
# Select only 1 for RF


#Prepare RF Model using a single raster stack from the rast_4_RF_list
# the image is from 18_04_2020 
rast_4_RF = crop_rasters$LC08_L2SP_174039_20200418_20200822_02_T1
training_data = CreateTrainingDF(rast_4_RF)

# Prepare the random forest model
set.seed(12)
RFmodel = Prepare_RF_Model(training_data)

# get list of names of cropped raster files
tif_cropped = list.files(cropped_dir, pattern = "tif$",
                         full.names = TRUE)
tif_cropped <- tif_cropped[grep(pattern = "full_area", x = tif_cropped)]  #takes only ... by pattern

#'---------------------------------
#' Run classification
#'---------------------------------
# loop through the cropped raster's and classify them 
classified_rasters = lapply(tif_cropped, function(t){
  # The tif_cropped list already has full path to each file
  r = rast(t)
  r = r[[bands]]
  #plot(r)
  rast_classify = ApplyRFModel(r, fit = RFmodel) # classify the raster
  r_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
  r_split <- unlist(r_split)[1]
  rastname = paste(r_split, "classified", sep="_")
  rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  writeRaster(x = rast_classify, filename = rastpath,
              overwrite = TRUE)
  
  return(rast_classify)
})

PlotClassified(tif_cropped, classified_rasters)


# get list of names of classified raster files
tif_classified = list.files(classified_dir, pattern = "tif$",
                         full.names = TRUE)
tif_classified <- tif_classified[grep(pattern = "classifiedscaled", x = tif_classified)]  #takes only... by pattern
#'---------------------------------
#' albedo band
#'---------------------------------
albedo = lapply(tif_cropped, function(t){
  r = rast(t)
  albedo_b = albedo_band(cropped = r)
  d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
  datestr <- unlist(d_split)[3]
  rastname = paste("full_area_albedo", datestr, sep="_")
  rastpath <- file.path(albedo_dir, paste0(rastname, ".tif"))
  terra::writeRaster(x= albedo_b, filename = rastpath, overwrite = TRUE)
  plot(albedo_b, main = rastname)
  
  return(albedo_b)
})


#'---------------------------------
#' Land surface temperature
#'---------------------------------
LST_crop <- lapply(study_areas$name, function(sa){
  lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into rast, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      
      print(paste("In:", sa, "directory:", d))
      study_area <- study_areas[study_areas$name == sa,]
      LST_b <- LST_band(tif_list, study_area)
      #print(range(LST_b))

      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste("LST", sa, datestr, sep="_")
      rastpath <- file.path(LST_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= LST_b, filename = rastpath, overwrite = TRUE)
      plot(LST_b, main = paste(rastname, "C", sep = " "))
      
      return(LST_b)
    }
  })
})


#'---------------------------------
#' Completed
#'---------------------------------
t2 = Sys.time()
elapsed = round(difftime(t2, t0, units = "mins"),2)
print(paste(t2, "-- End process in", elapsed, "minutes"))



