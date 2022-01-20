# Install and load packages
pkg_list = c(
  #"getSpatialData",  # download Landsat tiles
  "terra", "raster", # read rasters
  "sf",              # vector layers
  "parallel",        # parallel processing
  "exactextractr",   # extract and aggregate raster values per polygon, with clipping
  "remotes",         # to use github to install packages
  "glcm",            # Greylevel co-location matrix
  "caret","ranger",  # ML models
  "dplyr", "tools"
)

# installs packages that are not installed 
installed_packages <- pkg_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg_list[!installed_packages])
}

# Package loading
lapply(pkg_list, library, character.only = TRUE)

# not using supercells
# # install.packages("remotes")
# # add if not installed, install now
# if(!require(supercells)){
#   remotes::install_github("Nowosad/supercells")
#   library("supercells")
# }

# remotes::install_github("Nowosad/supercells")
# library("supercells")

# creates folder Paths
# -------------------------
GIS_dir = "./GIS"
if (!dir.exists(GIS_dir)) {
  dir.create(GIS_dir)
}

#do we need this folder cuz we already have the images
# folder w/ downloaded landsat 
datasets_dir = "./Landsat_datasets"
if (!dir.exists(datasets_dir)) {
  dir.create(datasets_dir)
}

# folder for cropped images
cropped_dir <- "./cropped"
if (!dir.exists(cropped_dir)) {
  dir.create(cropped_dir)
}

# folder for output
output_dir = "./output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
