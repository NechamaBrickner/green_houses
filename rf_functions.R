
# Random Forest functions based of of klils

load_rast_4_RF <- function(rast_4_RF, rast_shp){
  # folder with the tifs to make the list
  # take only the tifs with "_SR_" in name
  # which bands to take
  # create a raster with the wanted bands
  # give the bands names by color/wavelength 
  tif_list_RF = list.files("r_4_classification\\LC08_L2SP_174039_20200418_20200822_02_T1", pattern = "TIF$", full.names = TRUE)
  tif_list_RF <- tif_list_RF[grep(pattern="_SR_", x=tif_list_RF)]
  tif_list_RF <- tif_list_RF[grep(pattern = "B1|B2|B3|B4|B5|B6|B7", x = tif_list_RF)]
  tif_stk_RF <- rast(tif_list_RF)
  names(tif_stk_RF) <- c("aerosol", "blue", "green", "red",
                      "NIR", "SWIR1", "SWIR2") 
  # load shpfile of classification area
  # convert it to a vect object
  rast_shp <- st_read("GIS\\classification_area.shp")
  rast_shp <- vect(rast_shp)
  
  # crop and mask the raster with the shpfile
  crop_rast <- terra::crop(tif_stk_RF, rast_shp)
  mask_rast <- terra::mask(crop_rast, rast_shp)
  
  return(mask_rast)
}


addallbands <- function(raster) {
  # create glcm texture bands to the green band of the raster
    # to use glcm the band raster need to be in raster format and not terra-rast
  # texture bands are variance and second moment
  # give the texture bands names
  # create an NDVI band
  # give it a name
  texture = glcm(raster(raster$green), 
                 statistics = c('variance','contrast','dissimilarity'), 
                 na_opt = "ignore")
  names(texture) <- c("variance", "contrast","dissimilarity") 
  ndvi = (raster$NIR - raster$red)/(raster$NIR + raster$red)
  names(ndvi) = "NDVI"
  savi = 1.5*((raster$NIR - raster$red)/(raster$NIR + raster$red + 0.5))
  names(savi) = "SAVI"
  
  # combine all the bands to 1 raster
    # the texture bands need to be converted to terra-rast format
  allbands <- c(raster, rast(texture), ndvi, savi)
  #allbands <- c(raster, rast(texture), ndvi)
  
 
   return(allbands)
}


create_td <- function(training_data_RF, allbands){
  
  # load training data shpfile
  # convert it to a vect object
  training_data <- st_read("GIS\\classification_points.shp")
  training_data_v = vect(training_data) #convert to vect to us the extract function 
  
  # use extract to give each point the value of the pixel in each band
  train_bands <- allbands[[bands]]# selects wanted bands to build the model
  extract_points = terra::extract(train_bands, training_data_v, method = "simple")
  
  extract_points$ground_type = factor(training_data_v$Ground_Typ)
  extract_points = select(extract_points, -ID)
  
  
  # # get rid of the geometry field in training_data table
  # # join the training data with the extract_points
  # training_data = st_drop_geometry(training_data)
  # training_data = left_join(training_data, extract_points, by = c("Id" = "ID"))
  # 
  # # create a table with the names of classes and the number for each class as a factor
  # class_name = unique(training_data$Ground_Typ)
  # class_factor = 1:length(class_name)
  # class = as.data.frame(cbind(class_name, class_factor))
  # class$class_factor = factor(class$class_factor, labels = class_name)
  # 
  # join the training_data table with the factor data 
  # get rid of columns that aren't needed
  # training_data = full_join(training_data, class, by = c("Ground_Typ" = "class_name"))
  # training_data = select(training_data, -Ground_Typ, -Area)
  
  # count number of incomplete rows and erases them
  cnt_na <- sum(!complete.cases(extract_points))
  if (cnt_na > 0) {
    print(paste("Number of rows with NA:", cnt_na, "(removing...)"))
    extract_points <- extract_points[complete.cases(extract_points),]
  }
  # get rid of rows with NA
  # training_data = na.omit(training_data)

  return(extract_points)
}

set.seed(12)
Prepare_RF_Model <- function(training_data) {
  # Limit number of variables that will be tried in train()
  # To limit how many variable comgination are tried, We can set either:
  # set tuneGrid (specify values for each parameter), or
  # set tuneLength (specify how many options for all variables)
  # Here is with tuneGrid:
  # these numbers can be changed to get a better modle???
  rfGrid <- expand.grid(mtry = 2:5,         # Number of variables at each split
                        splitrule = "gini",  # How to decide when to split
                        min.node.size = 2:4  # How deep each tree
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
  model_rds <- file.path(output_dir, "fitted_RF_model.RDS")
  saveRDS(rfFit, model_rds)
  # Model results:
  cat("\nModel accuracy:\n")
  print(rfFit$results[rownames(rfFit$bestTune),][c("Accuracy", "Kappa")])
  
  # Get and print variable importance
  var_importance <- varImp(rfFit, scale=TRUE)
  cat("\nVariable importance:\n")
  print(var_importance)
  varimp_file <- file.path(output_dir, "variable_importance.png")
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

# #to check
# rf_model2$bestTune
# rf_model2$results

#it always crashes!!
ApplyRFModel <- function(all_rast_4_RF, fit) {
  # Apply model to data.frame of original superpixels
  # sp_predictors <- st_drop_geometry(superpixels) %>% 
  #   subset(select = -c(supercells,x,y))
  raster_predict <- terra::predict(object = all_rast_4_RF, model = fit,
                              factors = c("Water", "Orchard", "Ground", "Light_Green_House", "Dark_Green_House", 
                                          "Solar_Panels"))
  # raster_predict <- factor(FC,
  #              labels = c("FC", "bare_soil", "ring", "rock", "veg", "road"))
  # sp_classified <- cbind(superpixels, FC)
  # sp_classified_file <- file.path(Output_dir, "superpixels_classified.gpkg")
  # st_write(obj = sp_classified, dsn=sp_classified_file,
  #          layer = "superpixels_FC", append = FALSE, delete_layer = TRUE)
  return(raster_predict)
}

#the same thing as above still crashes 
#raster_predict = terra::predict(object = all_rast_4_RF, model = fit, fun = predict)

# #load study area
# area = st_read("GIS/area.shp")
# #pick 1 of the study areasa
# area1 = area[1,]
# area2 = area[2,]
# area3 = area[3,]
# #crop raster to that area1
# ein_yahav = terra::crop(raster1, area1)
# hazeva = terra::crop(raster1, area2)
# paran = terra::crop(raster1, area3)
# #dark green house = gray
# #ground = yellow
# #light green house = cyan
# #orchard = dark green
# #solar panels = black
# #water = blue
# 
 #colors = c("gray", "yellow", "cyan", "dark green", "black", "blue")
# x=levels(TD$ground_type)

# 
# plotRGB(hazeva, 5, 4,3)
# plotRGB(ein_yahav, 5, 4,3)
# plotRGB(paran, 5, 4,3)
#plot(hazeva_c, col = col, type = "classes", levels = x)
#plot(ein_yahav1, col = col, type = "classes", levels = x)
# plot(paran_c, col = col,type = "classes", levels = x)

#plot(hazeva5, col=col,type = "classes", levels = x)






