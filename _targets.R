#================================ Setup ================================

# Clear environment
rm(list = ls(all.names = TRUE))

# libraries needed
libs <- c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4", "tidyr", "dplyr", "readr", "targets", "usethis", "targets", "visNetwork", "tarchetypes")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)

# enter the file path for the highest level folder you're working in 
data_folder <- "C:/Users/natha/Box/_data/_spatial/_erosion-pins/"

# when a file is needed, call the hert() function
# for example; data_frame = read.csv(hert("more_data/measurements_data.csv"))
hert <- function(file) {
  file_path = paste(data_folder, file, sep = "")
  return(file_path)
}

#================================ Targets ================================
# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Set target options:
tar_option_set(
  packages = c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4",
               "tidyr", "dplyr", "readr", "targets", "usethis", "targets",
               "visNetwork", "tarchetypes", "tidyterra") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Target 1: Locate GPS locations based on file path
  tar_target(
    name = find_data,
    "C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv"
  ),
  
  
  # Target 2: Import CSV into R
  tar_target(
    name = points,
    command = get_points(find_data)
  ),
  
  
  # Target 3: Pull DEMs from OpenTopography API. Export as netCDF files
  tar_target(
    cdf_paths,
    pull_dems(points)
  ),
  
  # Target 4: Tracks the actual files on disk, branches over them
  tar_files( # This facilitate branching which is needed for the next step 
    cdfs,
    cdf_paths
  ),
  
  
  # Target 5: plots dems and point data
  tar_target(
    dem_plots,
    plot_dem(cdfs, points),
    pattern = map(cdfs)
  )
  
  
  # Target 6: Prep Erosion pin data
  
  # Target 7: QAQC Erosion pin data
  
  # Target 8: Analyze Erosion pin data
)
  

