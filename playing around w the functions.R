raster1 = load_rast_4_RF(rast_4_RF, rast_shp )
raster1 = addallbands(raster = raster1)
TD = create_td(training_data_RF, allbands = raster1)

TD1 = TD %>%
  select(-aerosol, -red, -blue, -SWIR2, -dissimilarity, - SAVI)

mod = Prepare_RF_Model(training_data = TD1)

#### apply model 
raster2 = raster1[[bands]]

raster2_c= ApplyRFModel(all_rast_4_RF = raster2 , fit = mod)

p = study_areas[3,]

p_r =crop(raster1, p)

p_r = p_r[[bands]]

col = c("gray", "yellow", "cyan", "dark green")
x = levels(TD$ground_type)
plot(raster1_c, col =col, type = "classes", levels = x)
plotRGB(p_r, 5,4,3)



LST_band = function(tif_list, study_area) {
  
  if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
    #select thermal band landsat 5 - B6
    LST_05 <- tif_list[grep(pattern="LT05_", x = tif_list)]
    LST_05 <- LST_05[grep(pattern="ST", x = LST_05)]
    LST_05 <- LST_05[grep(pattern = "B6", x = LST_05)]  
    LST <- rast(LST_05)
    names(LST) <- "LST"
  }
  
  else {
    #select thermal band landsat 8 - B10
    LST_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
    LST_08 <- LST_08[grep(pattern="ST", x=LST_08)]
    LST_08 <- LST_08[grep(pattern = "B10", x = LST_08)] 
    LST <- rast(LST_08)
    names(LST) <- "LST"
  }
  
  cropped <- crop(LST, study_area)
  
  return(cropped)
  
}


LST_crop <- lapply(study_areas$name, function(sa){
  lapply(tif_dirs_full, function(d) {
    # Get list of TIF files in each dir
    # Read into rast, and crop
    tif_list = list.files(d, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      # 
      
      print(paste("In:", sa, "directory:", d))
      print(paste0(sa,"_", tif_dirs_full, ".tif")) # can be name of raster?
      study_area <- study_areas[study_areas$name == sa,]
      LST_b <- LST_band(tif_list, study_area)
      print(range(LST_b))
      
      d_split <- strsplit(x=basename(d), split = "_", fixed = TRUE)
      datestr <- unlist(d_split)[4]
      rastname = paste("LST", sa, datestr, sep="_")
      rastpath <- file.path(LST_dir, paste0(rastname, ".tif"))
      terra::writeRaster(x= LST_b, filename = rastpath, overwrite = TRUE)
      
      
      return(LST_b)
    }
  })
})



classify_raster = lapply(crop_rasters, function(r){
  #print(paste0("cropped\\", r))
  r_1 = crop_rasters[[r]][[r]]# load raster
  #r_1 = r[[bands]]
  #plot(r_1)
  classify_r = ApplyRFModel(all_rast_4_RF = crop_rasters, fit = mod) # classify the raster
  # r_split <- strsplit(x=basename(r), split = ".", fixed = TRUE)
  # r_split <- unlist(r_split)[1]
  # rastname = paste( r_split, "classified", sep="_")
  # rastpath <- file.path(classified_dir, paste0(rastname, ".tif"))
  #plot(classify_r, col = colors, type = "classes", levels = x, main = paste(r, "classified"))
  # writeRaster(x = classify_r, filename = rastpath, overwrite = TRUE ) # save the raster
  #return(classify_r)
  #return(r_1)
})

for (i in 1:3)
{
  for (j in 1:13)
  {
    rst = crop_rasters[[i]][[j]]
    rst = rst[[bands]]
    plot(rst)
    classify_rst = ApplyRFModel(r = rst, fit = RFmodel) # classify the raster
    plot(classify_rst)
  }
}


x = levels(training_data$ground_type)

classify_raster_plot = lapply(tif_cropped, function(rst){
  #print(rst)
  r_1 = rast(rst) # load raster
  r_1 = r_1[[bands]]
  #plot(r_1$green, main = paste(rst, "green band"))
  classify_r = ApplyRFModel(r = r_1 , fit = RFmodel) # classify the raster
  # plot(classify_r, col = colors, type = "classes", levels = x, main = paste(rst, "classified"))
  return(classify_r)
})



  

