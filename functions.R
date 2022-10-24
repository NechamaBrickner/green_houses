#functions


#function to downlod landsat images using getspatialdatat library
# doesn't work!
# DownloadLandsat <- function(area, date_ranges_file){
# 
#   #----------------------
#   # This is not working! 
#   #----------------------
# 
#   # Configs for getSpatialData
#   #
#   set_aoi(as_Spatial(area))
#   set_archive(dataset_dir)
#   # Login credentials prepared in advance, and saved to RDS file
#   creds <- readRDS("credentials.rds")
#   login_USGS(username=creds$username, password=creds$passwd)
#   date_ranges <- read.csv("date_ranges.csv")
#   tif_dir_list <- lapply(1:nrow(date_ranges), function(r){
#                        from_date = as.character(date_ranges[r,1])
#                        to_date = as.character(date_ranges[r, 2])
#                        time_range=c(from_date, to_date)
#                        rec8 = getLandsat_records(time_range=time_range,
#                            products = c("landsat_8_c1"),
#                        )
#                        if (length(rec8) != 0) {
#                            getLandsat_data(rec8)
#                        }
#                       rec5 = getLandsat_records(time_range,
#                            products = c("landsat_tm_c2_l1")
#                        )
#                       if (length(rec5) != 0) {
#                           getLandat_data(rec5)
#                       }
#   })
# }


# function to crop the images 
# slects the bands by landsat type (uses tif name)
# create a stack of images using the wanted bands
# gives each band a name - color
# crops and masks the stack using the study area
# scale each band using the scale factor

CropDatasets <- function(tif_list, study_area) {
  # Read list of TIF files into stack
  # Crop to extent of testing polygons
  # ---------------------------
  # Check whether L5 or L8
  
    if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
      #select wanted bands landsat 5
      tif_list_05 <- tif_list[grep(pattern="LT05_", x=tif_list)]
      #tif_list_05 <- tif_list_05[grep(pattern="_SR_", x=tif_list_05)]
      tif_list_05 <- tif_list_05[grep(pattern = "B1|B2|B3|B4|B5|B7",
                                      x = tif_list_05)]  
      tif_stk <- rast(tif_list_05)
    } else {
      #select wanted bands landsat 8
      tif_list_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
      #tif_list_08 <- tif_list_08[grep(pattern="_SR_", x=tif_list_08)]
      # Do not use B1 (aerosol band)
      tif_list_08 <- tif_list_08[grep(pattern = "B2|B3|B4|B5|B6|B7",
                                      x = tif_list_08)]
      tif_stk <- rast(tif_list_08)
   }
    
  names(tif_stk) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
  cropped <- terra::crop(tif_stk, study_area)
  cropped = terra::mask(cropped, study_area) # do we want mask for the classification area so its not so big?
  #' Check bounding box of raster stack and study area
  # (st_bbox(study_area))
  # (bbox(cropped))
  
  #rescale factor for refletacne 
  # landsat 8 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1619_Landsat-8-Collection2_Level-2_Science-Product-Guide-v3.pdf
  # landsat 5 https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1618_Landsat-4-7_C2-L2-ScienceProductGuide-v4.pdf
  cropped = cropped*0.0000275 - 0.2
  return(cropped)
}

# function to add texture and index bands
# gives each new band a name
# create a stack with all the bands (cropped, texture and index)
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
  bsi =  (((cropped$SWIR1 + cropped$red) - (cropped$NIR + cropped$blue))/(cropped$SWIR1 +cropped$red) +(cropped$NIR+cropped$blue)) #bare soil index
  ndbi = ((cropped$SWIR1 - cropped$NIR) / (cropped$SWIR1 + cropped$NIR)) #Normalized Difference Built-up Index
  index = c(ndvi, bsi, ndbi)
  names(index) <- c("NDVI", "BSI", "NDBI")
  #add water index??
  # WI1 <- 4*(cropped$green - cropped$SWIR1) - (0.25*cropped$NIR + 2.75*cropped$SWIR2)
  # names(WI1) <- "WI1"
  # WI2 <- cropped$blue + 2.5*cropped$green - 1.5*(cropped$NIR + cropped$SWIR1) - 0.25*cropped$SWIR2
  # names(WI2) <- "WI2"
  # -------------------------------
  # Add to "cropped"
  # Convert texture bands back to terra "SpatRaster"
  all_layers <- c(cropped, rast(texture), index)
  #all_layers <- c(cropped, rast(texture), ndvi, WI1, WI2)
 
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

# # create LST band 
# # select the correct band 
# # crop to study area
# # scale using the scaling factor
# 
# LST_band = function(tif_list, study_area) {
#   
#   if (length(grep(pattern = "LT05", tif_list, fixed = TRUE)) > 0) {
#     #select thermal band landsat 5 - B6
#     LST_05 <- tif_list[grep(pattern="LT05_", x = tif_list)]
#     LST_05 <- LST_05[grep(pattern="ST", x = LST_05)]
#     LST_05 <- LST_05[grep(pattern = "B6", x = LST_05)]  
#     LST <- rast(LST_05)
#   }
#   else {
#     #select thermal band landsat 8 - B10
#     LST_08 <- tif_list[grep(pattern="LC08_", x=tif_list)]
#     LST_08 <- LST_08[grep(pattern="ST", x=LST_08)]
#     LST_08 <- LST_08[grep(pattern = "B10", x = LST_08)] 
#     LST <- rast(LST_08)
#   }
#   names(LST) <- "LST"
#   cropped <- crop(LST, study_area)
#   #rescale factor for theral band
#   # landsat 8 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1619_Landsat-8-Collection2_Level-2_Science-Product-Guide-v3.pdf
#   # landsat 5 https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1618_Landsat-4-7_C2-L2-ScienceProductGuide-v4.pdf
#   cropped = cropped*0.00341802+149.0-272.15 
#  
#   return(cropped)
# }

# # calculate albedo band 
# albedo_band = function(cropped) {
#   albedo = (cropped$blue*0.356 + cropped$red*0.13 + cropped$NIR*0.373 + cropped$SWIR1*0.085+ cropped$SWIR2*0.072 - 0.0018)/1.016
#   return(albedo)
# }


## probably need to change the number of characters that r being erased
# creates a raster out of a tiff list
rast_cc = function(tif_cc){
  #gets name of pic with out .tif at end and ./croppped/ at begining
  name = substr(tif_cc,1,nchar(tif_cc)-18)
  name = substr(name, 29, nchar(name))
  #take the list of files and turn to multiband raster
  r_tif_cc = rast(tif_cc)
  names(r_tif_cc) = name # give each band the name from name
  #r_tif_cc = paste0(r_tif_cc, yishuv)
  #plot(hazeva_r)
  return(r_tif_cc)
}

# create a table with the frequency of each ground type in every raster
# takes raster list and yishuv
# saves the table
frequency_table = function(tif_cc, yishuv){
  #gets name of pic with out .tif at end and ./croppped/ at begining
  name = substr(tif_cc,1,nchar(tif_cc)-18)
  name = substr(name, 29, nchar(name)) # gets name of raster- yishuv_year
  # df to join with nams of each raster
  num = 1:length(name) #gets number of rasters
  df = data.frame(num, name, years) #makes df with 3 columns years if from begining...
  df$years = as.numeric(df$years)
  #take the list of files and turn to multiband raster
  r_tif_cc = rast(tif_cc)
  names(r_tif_cc) = name # give each band the name from name
  #plot(hazeva_r)
  ft = as.data.frame(freq(r_tif_cc)) # calculate the frequency of each land type per band in a dataframe
  ft = ft %>%
    left_join(df, by = c("layer" = "num"))%>% # join with df of raster names
    group_by(layer) %>%
    mutate(porportion = count/sum(count)*100) %>% #add percentage of each land type
    #separate(name, c("yishuv_name","year"), sep = "_" )%>% ## only works if the name is one word 
    select(name, everything()) 
  #ft$year = as.numeric(ft$year)
  
  ft$value = as.character(ft$value)
  ft["value"][ft["value"] == 1] = "Dark GH"
  ft["value"][ft["value"] == 2] = "Open Ground"
  ft["value"][ft["value"] == 3] = "Light GH"
  ft["value"][ft["value"] == 4] = "Orchard and Vegetation"
  
  table_path <- file.path(output_dir, paste0("frequency_table1_", yishuv, ".csv"))
  write.csv(ft, table_path)
  
  
  return(ft)
}

#functiom that turns a raster to dataframe
raster_to_df = function(rast_to_df) { 
  df = as.data.frame(rast_to_df, xy = TRUE) %>%
    melt(id.vars = c("x", "y")) %>%
    mutate(value_c = value)
  
  df$value_c = as.character(df$value_c)
  df["value_c"][df["value_c"] == 1] = "Dark GH"
  df["value_c"][df["value_c"] == 2] = "Open Ground"
  df["value_c"][df["value_c"] == 3] = "Light GH"
  df["value_c"][df["value_c"] == 4] = "Orchard and Vegetation"
  return(df)
}


#change detection functions

#calculates the change between 2 raster to 16 classes (5-20)
changes_mask <- function(rastA, rastB) {
  num_classes1 <- unique(values(rastA))
  num_classes1 = na.omit(num_classes1)
  num_classes <- length(num_classes1)
  #print(num_classes)
  return(rastA*num_classes + rastB)
}

#loops through a multiband raster and calculates the change and save the rasters
change_detection = function(rast) {
  for (i in 1:(nlyr(rast)-1)) {
    rast1 = rast[[i]]
    rast2 = rast[[(i +1)]]
    CD_1 = changes_mask2(rastA = rast1, rastB = rast2)
    names(CD_1) = paste("CD", names(rast1), names(rast2), sep = "_") 
    plot(CD_1)
    rastname = paste("CD", names(rast1), names(rast2), sep = "_")
    rastpath <- file.path(change_detection_dir, paste0(rastname, ".tif"))
    writeRaster(x = CD_1, filename = rastpath,
                overwrite = TRUE)
    if (i == 1) {
      CD = CD_1
    } else {
      CD = c(CD, CD_1)
    }
  }
  return(CD)
} 

#table with the change classes
change_classes = data.frame(id = 5:20, cover = c("Dark GH", "Dark GH to Open Ground", "Dark GH to Light GH", 
                                                 "Dark GH to Orchard & Vegetation",
                                                 "Open Ground to Dark GH",  "Open Ground", "Open Ground to Light GH", 
                                                 "Open Ground to Orchard & Vegetation","Light GH to Dark GH", 
                                                 "Light GH to Open Ground", "Light GH", "Light GH to Orchard & Vegetation",
                                                 "Orchard & Vegetation to Dark GH", "Orchard & Vegetation to Open Ground", 
                                                 "Orchard & Vegetation to Light GH", "Orchard & Vegetation"))

#turns the Change raster to a dataframe to plot, uses the change class to get classes
raster_2_df_CD = function(rast_2_df){
   df = as.data.frame(rast_2_df, xy = TRUE) %>%
     melt(id.vars = c("x", "y"))%>%
     left_join(change_classes, by = c("value" = "id"))
}



#checks if the pixel changes and sums all the change (
#final is a single band raster with the number of times the pixel changed)  
change_sum = function(rast) {
  for (i in 1:(nlyr(rast)-1)) {
    rast1 = rast[[i]]
    rast2 = rast[[(i +1)]]
    cd1 = change(rastA = rast1, rastB = rast2)
    #plot(cd1)
    if (i == 1) {
      CD = cd1
    } else {
      CD = c(CD, cd1)
    }
  }
  cd_sum = app(CD, sum)
  name_split <- strsplit(x=names(rast1), split = "_", fixed = TRUE)
  name <- unlist(name_split)[1]
  rastname = paste0("CD_sum_", name)
  rastpath <- file.path(change_detection_dir, paste0(rastname, ".tif"))
  writeRaster(x = cd_sum, filename = rastpath,
              overwrite = TRUE)
  plot(cd_sum, main = rastname)
  return(cd_sum)
}


# creating a discrete color palette
color_pal_list = list(
  light24 = c("#FD3216", "#00FE35", "#6A76FC", "#479B55", "#EEA6FB", "#0DF9FF", 
              "#F6F926", "#FF9616", "#FED4C4", "#86CE00", "#DC587D", "#D626FF", 
              "#6E899C", "#00B5F7", "#B68E00", "#C9FBE5", "#FF0092", "#22FFA7", 
              "#E3EE9E", "#FE00CE", "#BC7196", "#7E7DCD", "#FC6955", "#E48F72"),                              
  
  dark24 = c("#2E91E5", "#E15F99", "#1CA71C", "#FB0D0D", "#DA16FF", "#222A2A",
             "#B68100", "#750D86", "#EB663B", "#511CFB", "#00A08B", "#FB00D1",
             "#FC0080", "#B2828D", "#6C7C32", "#778AAE", "#862A16", "#A777F1", 
             "#620042", "#1616A7", "#DA60CA", "#6C4516", "#0D2A63", "#AF0038"), 
  
  alphabet = c("#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", "#1C8356",
               "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F", "#C4451C", "#DEA0FD",
               "#FE00FA", "#325A9B", "#FEAF16", "#F8A19F", "#90AD1C", "#F6222E", 
               "#1CFFCE", "#2ED9FF", "#B10DA1", "#C075A6", "#FC1CBF", "#B00068", 
               "#FBE426", "#FA0087")
)

color_palettes = function(name, n, all_palettes = color_pal_list, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


scale_fill_color_pal = function(name) {
  ggplot2::scale_fill_manual(values = color_palettes(name,
                                                     type = "discrete"))
}
