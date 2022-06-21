rast_4_RF1 = terra::rast("cropped\\full_area_20200418.tif")
#rast_4_RF2 = terra::rast(cropped_dir, "full_area_20200418.tif")
r = rast("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\work lab assistant\\project\\green_houses\\cropped\\full_area_20200418.tif")

#How to mask out the area
classified = rast("output\\classified_full_area\\Classified4\\full_area_20200418_classified.tif")
yishuv = st_read("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\work lab assistant\\project\\landsat segmentation R\\area_new1_b500.shp")
ey = ey[2,] #subset einyahav polygon
plot(ey)
ey = vect(ey) #convert to vect
ey_classified = terra::crop(classified, ey) # crop the classified raster to einyahva poly - gives rectangle..
ey_classified = terra::mask(ey_classified, ey) # mask the classified raster to einyahav poly - gives poly shape
plot(ey_classified)

freq(ey_classified) # claculates the frequency of each class

yishuv = st_read("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\work lab assistant\\project\\landsat segmentation R\\GIS\\mask.shp")
yishuv_ey = vect(yishuv[2, ])
plot(yishuv_ey)

ey_poly_c = rasterize(yishuv_ey, ey_classified) #rasterizes the einyahav polygon
ey_poly_c[ey_poly_c ==1] = -999 # changes the polygon value to -999

mask_ey_c = mask(ey_classified, ey_poly_c, maskvalues = -999) #maskes the area of the yishuv

freq(mask_ey_c)
plot(mask_ey_c)



buffer500 = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                 layer="area_buffer500_detailed")
yishuv_mask = vect(file.path(GIS_dir, "greenhouses.gpkg"),
                   layer="yishuv_mask")
# plot(classified)
# plot(buffer500, add = T)
# plot(yishuv_mask, add = T)

yishuv_mask_r = rasterize(yishuv_mask, classified) #rasterizes the yishuv and othe polygons
yishuv_mask_r[yishuv_mask_r ==1] = -999 # changes the polygon value to -999
classified_mask = mask(classified, yishuv_mask_r, maskvalues = -999) #maskes the area of the yishuv
plot(classified_mask)


#loop to crop the classified images to the study area and mask out the yishuv
crop_classified_rasters <- lapply(buffer500$name, function(sa){
  lapply(tif_classified, function(t) {
    r = rast(t)
    yishuv_mask_r = rasterize(yishuv_mask, r) #rasterizes the yishuv and othe polygons
    yishuv_mask_r[yishuv_mask_r ==1] = -999 # changes the polygon value to -999
    print(paste("In:", sa, "directory:", t))
    study_area <- buffer500[buffer500$name == sa,]
    #crop and mask to yishuv out line
    cropped <- terra::crop(r, study_area)
    masked = terra::mask(r, study_area)
    classified_mask = terra::mask(masked, yishuv_mask_r, maskvalues = -999)#maskes the area of the yishuv, makes the the raster size bigger with NA's
    #save the cropped images
    d_split <- strsplit(x=basename(t), split = "_", fixed = TRUE)
    datestr <- unlist(d_split)[3]
    rastname = paste(sa, datestr,"classified5", sep="_")
    rastpath <- file.path(cropped_dir, paste0(rastname, ".tif"))
    terra::writeRaster(x= classified_mask,
                       filename = rastpath, overwrite = TRUE)
    plot(classified_mask, main = rastname)
    return(classified_mask)
  })
})


tif_crop_classified = list.files(cropped_dir, pattern = "tif$",
                            full.names = TRUE)
tif_crop_classified <- tif_crop_classified[grep(pattern = "classified5", x = tif_crop_classified)]  #takes only... by pattern

frequency_table = lapply(tif_crop_classified, function(t){
  r = rast(t)
  ft = freq(r)
  return(ft)
})

# albedo WORKS GOOD!!!!
l8 = crop_rasters$LC08_L2SP_174039_20200214_20200823_02_T1
albedo = (l8$blue*0.356 + l8$red*0.13 + l8$NIR*0.373 + l8$SWIR1*0.085+ l8$SWIR2*0.072 - 0.0018)/1.016
albedo
plot(albedo)


a = crop(albedo, ey)

#############
from Chamile

library(raster);library(tidyr);library(terra)

## list of classified rasters
tif_crop_classified = list.files(cropped_dir, pattern = "tif$",
                                 full.names = TRUE)
tif_crop_classified <- tif_crop_classified[grep(pattern = "classified", x = tif_crop_classified)]  #takes only... by pattern


## gets the names of each classified raster by yishuv in a list to use to give names in the frequency table
# hazeva_c_c <- tif_crop_classified[grep(pattern = "Hazeva", x = tif_crop_classified)]  #takes only... by pattern
# ein_yahav_c_c <- tif_crop_classified[grep(pattern = "Ein_Yahav", x = tif_crop_classified)]  #takes only... by pattern
# paran_c_c <- tif_crop_classified[grep(pattern = "Paran", x = tif_crop_classified)]  #takes only... by pattern
#
# #gets name of pic with out .tif at end and ./croppped/ at begining
# name_hazeva = substr(hazeva_c_c,1,nchar(hazeva_c_c)-4)
# name_hazeva = substr(name_hazeva, 11, nchar(name_hazeva))
#
# # df to join with nams of each raster
# num = 1:length(name_hazeva)
# names_h = data.frame(num, name_hazeva)
#
# name_ein_yahav = substr(ein_yahav_c_c,1,nchar(ein_yahav_c_c)-4)
# name_ein_yahav = substr(name_ein_yahav, 11, nchar(name_ein_yahav))
#
# # df to join with nams of each raster
# num = 1:length(name_ein_yahav)
# names_ey = data.frame(num, name_ein_yahav)
#
# name_paran = substr(paran_c_c,1,nchar(paran_c_c)-4)
# name_paran = substr(name_paran, 11, nchar(name_paran))
#
# # df to join with nams of each raster
# num = 1:length(name_paran)
# names_h = data.frame(num, name_paran)

# make rasters and name based on the date
H <- rast(H)
names(Hatseva.ocu) =  as.Date(substr(ocu_list[14:26], 89,96), format="%Y%m%d") # name based on position of strings in the original file name

## prop.table which I convert straight into dataframe (because I was preparing the Table to show)
h1 = as.data.frame(prop.table(table(as.vector(H[[1]])))*100)
h2 = as.data.frame(prop.table(table(as.vector(H[[2]])))*100)
h3 = as.data.frame(prop.table(table(as.vector(H[[3]])))*100)

df = as.data.frame(freq(H))
df = df %>%
  group_by(layer) %>%
  mutate(porportion = count/sum(count)*100)







L5 = list(crop_rasters$LT05_L2SP_174039_19900227_20200916_02_T1, crop_rasters$LT05_L2SP_174039_20020228_20211206_02_T1)
names(crop_rasters)[grep(pattern="LT05_", x=names(crop_rasters))]


#new index
coastal = rast(".\\Landsat_datasets\\LC08_L2SP_174039_20200418_20200822_02_T1\\LC08_L2SP_174039_20200418_20200822_02_T1_SR_B1.TIF")
red = rast(".\\Landsat_datasets\\LC08_L2SP_174039_20200418_20200822_02_T1\\LC08_L2SP_174039_20200418_20200822_02_T1_SR_B4.TIF")
nir = rast(".\\Landsat_datasets\\LC08_L2SP_174039_20200418_20200822_02_T1\\LC08_L2SP_174039_20200418_20200822_02_T1_SR_B5.TIF")
swir2 = rast(".\\Landsat_datasets\\LC08_L2SP_174039_20200418_20200822_02_T1\\LC08_L2SP_174039_20200418_20200822_02_T1_SR_B7.TIF")

r = c(coastal, red, nir, swir2)
names(r) = c("coastal", "red", "nir", "swir")
r = r*0.0000275 - 0.2

r = crop(r, full_area)

APGI = 100*r$coastal*r$red*((2*(r$nir-r$red-r$swir))/(2*(r$nir+r$red+r$swir)))
plot(APGI)

index_ey = crop(index, ey)
plot(index_ey)
index_p = crop(index, p)
plot(index_p)
index_h = crop(index, h)
plot(index_h)


r = rast(t)
albedo_b = albedo_band(cropped = r)
d_split <- strsplit(x=basename(t), split = ".", fixed = TRUE)
datestr <- unlist(d_split)[1]
rastname = paste("albedo", datestr, sep="_")
rastpath <- file.path(albedo_dir, paste0(rastname, ".tif"))
terra::writeRaster(x= albedo_b, filename = rastpath, overwrite = TRUE)
plot(albedo_b, main = rastname)




# RF model landsat5

training_data_l5 = st_read(file.path(GIS_dir,"greenhouses.gpkg"),
                           layer="cp_L5")

rast_4_RF_l5 = crop_rasters$LT05_L2SP_174039_20020228_20211206_02_T1
bands_l5 = c("green", "NIR", "SWIR2", "variance" ,"contrast", "NDVI","BSI", "NDBI")

CreateTrainingDF <- function(r, training_data){
  # "tif" is the chosen raster stack to be used *for training*
  # takes training data points from a layer

  training_data = training_data %>%
    filter(Ground_Typ != "Water")
  training_data <- vect(training_data)

  # selects wanted bands to build the model
  train_bands <- r[[bands_l5]]
  extract_points = terra::extract(train_bands, training_data,
                                  method = "simple")

  extract_points$ground_type = factor(training_data$Ground_Typ)
  extract_points = select(extract_points, -ID)


  # count number of incomplete rows and erases them
  cnt_na <- sum(!complete.cases(extract_points))
  if (cnt_na > 0) {
    print(paste("Number of rows with NA:", cnt_na, "(removing...)"))
    extract_points <- extract_points[complete.cases(extract_points),]
  }

  return(extract_points)
}


training_data_L5 = CreateTrainingDF(r = crop_rasters_l5, training_data = training_data_l5)


Prepare_RF_Model_l5 <- function(training_data) {
  # Limit number of variables that will be tried in train()
  # To limit how many variable comgination are tried, We can set either:
  # set tuneGrid (specify values for each parameter), or
  # set tuneLength (specify how many options for all variables)
  # Here is with tuneGrid:
  # these numbers can be changed to get a better modle???
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:5  # How deep each tree
  )
  # THis grid gives a total of 28 combinations of parameters??? not 24

  # Limit how many K-folds and how many retries
  rfControl <- trainControl(                # 10-fold CV, 3 repeats
    method = "repeatedcv",
    number = 10,
    repeats = 4
  )

  # Split train/test
  train_idx <- createDataPartition(
    y = training_data$ground_type,
    p = .75,
    list = FALSE
  )
  train_df <- training_data[train_idx,]
  test_df <- training_data[-train_idx,]

  ## Parallel processing
  ## Use 1/2 of the cores
  # ncores = parallel::detectCores() / 2
  # clust <- makeCluster(ncores)
  # registerDoParallel(clust)
  rfFit <- train(ground_type ~ ., data = train_df,
                 method = "ranger",
                 trControl = rfControl,
                 verbose = TRUE,
                 tuneGrid = rfGrid,
                 preProcess=c("center", "scale"),
                 importance = "permutation"
                 # -----Note:-----
                 # discuss whether to use "impurity" or "permutation"
  )

  # Save model
  model_rds <- file.path(output_dir, "fitted_RF_model_l5.RDS")
  saveRDS(rfFit, model_rds)
  # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])

  # Get and print variable importance
  var_importance <- varImp(rfFit, scale=TRUE)
  cat("\nVariable importance:\n")
  print(var_importance)
  varimp_file <- file.path(output_dir, "variable_importance_l5.png")
  vip <- ggplot(var_importance)
  ggsave(varimp_file, plot = vip)
  #png(varimp_file)
  #plot(var_importance, main="Variable Importance")
  #dev.off()
  # stopCluster(clust)

  # Apply on test data, and show confusion matrix
  rfPred <- predict(rfFit, newdata = test_df)
  con.mat <- confusionMatrix(rfPred, reference = test_df$ground_type)
  cat("\nTest accuracy:\n")
  print(con.mat$overall[c("Accuracy", "Kappa")])
  cat("\nConfusion matrix:")
  print(con.mat$table)
  return(rfFit)
}

set.seed(12)
RFmodel_l5_20 = Prepare_RF_Model_l5(training_data = training_data_L5)

classifiedl5_m20 = terra::predict(object = crop_rasters_l5, model = RFmodel_l5_20,
                              factors = c("Orchard", "Ground", "Light_Green_House",
                                          "Dark_Green_House"),
                              na.rm = TRUE)
writeRaster(x = classifiedl5_m20, filename = "index\\c_l5_m20.tif",
            overwrite = TRUE)

col = c("gray", "yellow", "cyan", "dark green")
x = levels(training_data_L5$ground_type)

plot(classifiedl5, col = col, type = "classes", levels = x)

library(corrplot)

corrplot(cor(training_data_L5[1:9]), method = "number")


#random forest L8

training_data_l8 = st_read(file.path(GIS_dir,"greenhouses.gpkg"),
                           layer="cp2")

#rast_4_RF_l5 = crop_rasters$LT05_L2SP_174039_20020228_20211206_02_T1
bands_l8 = c("green", "NIR","SWIR2", "variance", "NDVI", "BSI", "NDBI")
bands_l8 = c("blue", "NIR","SWIR1", "variance", "NDVI","BSI", "NDBI")
bands_l8 = c("blue", "NIR","SWIR2", "variance", "NDVI","BSI", "NDBI")
bands_l8 = c("green", "NIR","SWIR1", "variance", "NDVI","BSI", "NDBI")

CreateTrainingDF <- function(r, training_data){
  # "tif" is the chosen raster stack to be used *for training*
  # takes training data points from a layer

  training_data = training_data %>%
    filter(Ground_Typ != "Water")
  training_data <- vect(training_data)

  # selects wanted bands to build the model
  train_bands <- r[[bands_l8]]
  extract_points = terra::extract(train_bands, training_data,
                                  method = "simple")

  extract_points$ground_type = factor(training_data$Ground_Typ)
  extract_points = select(extract_points, -ID)


  # count number of incomplete rows and erases them
  cnt_na <- sum(!complete.cases(extract_points))
  if (cnt_na > 0) {
    print(paste("Number of rows with NA:", cnt_na, "(removing...)"))
    extract_points <- extract_points[complete.cases(extract_points),]
  }

  return(extract_points)
}


training_data_L8 = CreateTrainingDF(r = crop_rasters_l8, training_data = training_data_l8)


Prepare_RF_Model_l8 <- function(training_data) {
  # Limit number of variables that will be tried in train()
  # To limit how many variable comgination are tried, We can set either:
  # set tuneGrid (specify values for each parameter), or
  # set tuneLength (specify how many options for all variables)
  # Here is with tuneGrid:
  # these numbers can be changed to get a better modle???
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:5  # How deep each tree
  )
  # THis grid gives a total of 28 combinations of parameters??? not 24

  # Limit how many K-folds and how many retries
  rfControl <- trainControl(                # 10-fold CV, 3 repeats
    method = "repeatedcv",
    number = 10,
    repeats = 4
  )

  # Split train/test
  train_idx <- createDataPartition(
    y = training_data$ground_type,
    p = .75,
    list = FALSE
  )
  train_df <- training_data[train_idx,]
  test_df <- training_data[-train_idx,]

  ## Parallel processing
  ## Use 1/2 of the cores
  # ncores = parallel::detectCores() / 2
  # clust <- makeCluster(ncores)
  # registerDoParallel(clust)
  rfFit <- train(ground_type ~ ., data = train_df,
                 method = "ranger",
                 trControl = rfControl,
                 verbose = TRUE,
                 tuneGrid = rfGrid,
                 preProcess=c("center", "scale"),
                 importance = "permutation"
                 # -----Note:-----
                 # discuss whether to use "impurity" or "permutation"
  )

  # Save model
  model_rds <- file.path(output_dir, "fitted_RF_model_l5.RDS")
  saveRDS(rfFit, model_rds)
  # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])

  # Get and print variable importance
  var_importance <- varImp(rfFit, scale=TRUE)
  cat("\nVariable importance:\n")
  print(var_importance)
  varimp_file <- file.path(output_dir, "variable_importance_l5.png")
  vip <- ggplot(var_importance)
  ggsave(varimp_file, plot = vip)
  #png(varimp_file)
  #plot(var_importance, main="Variable Importance")
  #dev.off()
  # stopCluster(clust)

  # Apply on test data, and show confusion matrix
  rfPred <- predict(rfFit, newdata = test_df)
  con.mat <- confusionMatrix(rfPred, reference = test_df$ground_type)
  cat("\nTest accuracy:\n")
  print(con.mat$overall[c("Accuracy", "Kappa")])
  cat("\nConfusion matrix:")
  print(con.mat$table)
  return(rfFit)
}

set.seed(12)
RFmodel_l8_m10 = Prepare_RF_Model_l8(training_data = training_data_L8)

classifiedl8_m10 = terra::predict(object = crop_rasters_l8, model = RFmodel_l8_m10,
                                  factors = c("Orchard", "Ground", "Light_Green_House",
                                              "Dark_Green_House"),
                                  na.rm = TRUE)
writeRaster(x = classifiedl8_m10, filename = "index\\c_l8_m10.tif",
            overwrite = TRUE)



Prepare_RF_Model_l5 <- function(training_data) {
  # Limit number of variables that will be tried in train()
  # To limit how many variable comgination are tried, We can set either:
  # set tuneGrid (specify values for each parameter), or
  # set tuneLength (specify how many options for all variables)
  # Here is with tuneGrid:
  # these numbers can be changed to get a better modle???
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:5  # How deep each tree
  )
  # THis grid gives a total of 28 combinations of parameters??? not 24

  # Limit how many K-folds and how many retries
  rfControl <- trainControl(                # 10-fold CV, 3 repeats
    method = "repeatedcv",
    number = 10,
    repeats = 4
  )

  # Split train/test
  train_idx <- createDataPartition(
    y = training_data$ground_type,
    p = .75,
    list = FALSE
  )
  train_df <- training_data[train_idx,]
  test_df <- training_data[-train_idx,]

  ## Parallel processing
  ## Use 1/2 of the cores
  # ncores = parallel::detectCores() / 2
  # clust <- makeCluster(ncores)
  # registerDoParallel(clust)
  rfFit <- train(ground_type ~ ., data = train_df,
                 method = "ranger",
                 trControl = rfControl,
                 verbose = TRUE,
                 tuneGrid = rfGrid,
                 preProcess=c("center", "scale"),
                 importance = "permutation"
                 # -----Note:-----
                 # discuss whether to use "impurity" or "permutation"
  )

  # # Save model
  # model_rds <- file.path(output_dir, "fitted_RF_model_l5.RDS")
  # saveRDS(rfFit, model_rds)
  # # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])

  # # Get and print variable importance
  # var_importance <- varImp(rfFit, scale=TRUE)
  # cat("\nVariable importance:\n")
  # print(var_importance)
  # varimp_file <- file.path(output_dir, "variable_importance_l5.png")
  # vip <- ggplot(var_importance)
  # ggsave(varimp_file, plot = vip)
  #png(varimp_file)
  #plot(var_importance, main="Variable Importance")
  #dev.off()
  # stopCluster(clust)

  # Apply on test data, and show confusion matrix
  rfPred <- predict(rfFit, newdata = test_df)
  con.mat <- confusionMatrix(rfPred, reference = test_df$ground_type)
  cat("\nTest accuracy:\n")
  print(con.mat$overall[c("Accuracy", "Kappa")])
  # cat("\nConfusion matrix:")
  # print(con.mat$table)
  c_name = c("m Accuracy", "m Kappa", "t Accuracy", "t Kappa")
  mod = (rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])
  testa = con.mat$overall["Accuracy"]
  testb = con.mat$overall["Kappa"]
  r = c(mod[1,1], mod[1,2], testa, testa)
  table = rbind(c_name, r)
  #t = as.data.frame(rfFit$results[rownames(rfFit$bestTune),], con.mat$overall[c("t Accuracy", "t Kappa")])
  print(table)
  return(rfFit)
}

RFmodel_l5 = Prepare_RF_Model_l5(training_data = training_data_L5)

# x = list()
#
# for (i in 3){
#   RFmodel_l5 = Prepare_RF_Model_l5(training_data = training_data_L5)
#   x = list(RFmodel_l5)
#   return(x)
# }

#####

# trying to make a code based off of crop rasters (run file) that can go 1 more level into
# the folders and do the CropDataset function for each image and then calculate the mean pixel values per band per year
# and then continue with the AddImageTexture function.

#folder layout:

#Landsat datasets
  #1985
    #image1
    #image2
    #image3
  #1990
    #image1
    #image2
    #image3
  #....

# function to get the next level of folders
# folder_list = function(tif_dirs_full) {
#   list.dirs(tif_dirs_full)[-1]
# }
#
#
# #creates a list of lists of landsat folders by year
# tif_dirs_full_year = lapply(tif_dirs_full, folder_list)

#----------------------------------------------------------
# MS: I think the above is unnecessary. The function list.dirs() already returns a list
# Note the parameter "recusrsive = FALSE". If you leave it at TRUE,
# then you'll get the full paths to *all* the image files (not what you want)
tif_dirs_full_year <- list.dirs(tif_dirs_full,
  full.names = TRUE, recursive = FALSE)
# Now just loop thru those year folders and do what you want with the images

 crop_rasters_list <- lapply(tif_dirs_full_year, function(year_folder) {
   image_list <- list.files(path = year_folder,
     pattern = ".tif", full.names = TRUE)
   cropped_year <- CropDatasets(image_list, full_area)
 })

# Just one caveat:
# You probably will want to save the year for each item (cropped raster stack)
# in the list "crop_raster_list"
# You can do this by setting the names for the items in the lists:

years <- list.dirs(datasets_dir,
    full.names = FALSE, # Now we want only the year, NOT the full path
    recursive = FALSE)
year_dirs <- list.dirs(datasets_dir,
                       full.names = TRUE,
                       recursive = FALSE)
names(year_dirs) <- years
# Now the list "cropped_rasters_list" is a "named list"
#----------------------------------------------------------

# folder = tif_dirs_full_year[[1]]
# d = folder[1]

#my attempt to add another lapply to go 1 level in, did not work
# i also subset the original tif_dirs_full to only have the folders of years

crop_rasters <- lapply(1:length(years), function(fidx) {
  year_dir <- year_dirs[[fidx]]
  #folder1 = tif_dirs_full_year[folder]
  year_dataset_list <- lapply(year_dir, function(d){
    #    folder2 = folder1[d]
     #Get list of TIF files in each dir
    year_dataset_list = list.dirs(d, full.names = TRUE, recursive = FALSE)
    
    return(year_dataset_list)
    })
  year_dataset_list <- unlist(year_dataset_list)
  if (length(year_dataset_list) > 0) {
    final_list <- lapply(year_dataset_list, function(d) {
      # Get list of TIF files in each dir
      tif_list = list.files(d, pattern = "TIF$",
                            full.names = TRUE, recursive = TRUE)
      if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
    
      cropped <- CropDatasets(tif_list, full_area)
      # Textures?
      final <- AddImageTexture(cropped)
      return(final)   
    }
  })
  l = sds(final_list)
  final_stack = app(l, mean)
  return(final_stack)
}
})
#}

#? will this give the right name
names(crop_rasters) = years

# code needed to get mean of pixels by band
# l = sds(list())
# meanr = app(l, mean)


#code my sister helped me write using 2 for loops

imagelist = list()
outercounter = 1
for (year in tif_dirs_full_year) {
  #print(year)
  yearlist = list()
  count = 1
  for (image in year) {
    tif_list = list.files(image, pattern = "TIF$",
                          full.names = TRUE, recursive = TRUE)
    if (length(tif_list) > 0) {
      # pass both list of tif files, and containing directory to the cropping function
      # The directory name will be used to name the new, cropped tif file
      #
      cropped <- CropDatasets(tif_list, full_area)

      yearlist[count] = list(cropped)
      count = count +1
      #return(cropped)

    }

  }
  print(yearlist)
  l = sds(yearlist)
  meanr = app(l, mean)
  imagelist[outercounter] = meanr
  #imagelist[outercounter] = list(yearlist)
  outercounter = outercounter +1
}

imagelist
