#================================ Setup ================================

# Clear environment
rm(list = ls(all.names = TRUE))

# libraries needed
libs <- c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4", "tidyr", "dplyr", "readr", "targets", "usethis", "sf", "targets", "visNetwork", "tarchetypes", "tidyterra", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT", "flextable", "wesanderson")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)


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


#================================ Targets ================================

# Replace the target list below with your own:
list(
  
  #================================ Points & DEMS ================================
  
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
  tar_files( # This facilitates branching which is needed for the next step 
    cdfs,
    cdf_paths
  ),
  
  #================================ Plotting ================================
  
  # Target 1: Plots rasters and point data together, unique plot for each forest
  tar_target(
    indv_plots,
    plot_each(points_mms, cdfs),
    pattern = map(cdfs),
    iteration = "list"
  ),
  
  # Target 2: Put plots in a list
  tar_target(
    all_plots,
    plot_all(indv_plots)
  ),
  
  #================================ Measurements ================================
  
  # Target 4: Pull erosion pin data
  tar_target(
    raw_measurements,
    pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")
  ),
  
  
  # Target 1: Clean up erosion pin data
  tar_target(
    clean_data,
    data_cleanup(raw_measurements)
  ),
  
  
  # Target 2: Difference erosion pins 
  tar_target(
    differenced_pins,
    difference_pins(clean_data)
  ),

  
  # Target 3: QAQC
  tar_target(
    QAQC,
    differenced_QAQC(differenced_pins)
  ),
  
  
  # Target 4: Fitting splines
  tar_target(
    lsplines,
    fit_lspines(differenced_pins),
    format = "file"
  ),
  
  #================================ Combo ================================
  
  # Target 1: Combining point (GPS) data and measurement data
  tar_target(
    points_mms,
    combo(points, differenced_pins)
  ),
  
  
  # Target 2: Extract slope at each point
  tar_target(
    slope_extracted,
    extract_slope(points_mms, cdfs),
    pattern = map(cdfs)
  ),
  
  
  # Target 3: Combine all branches into one dataframe
  tar_target(
    points_mms_slope,
    bind_rows(slope_extracted)
  )
  
  )
  
# tar_visnetwork()

# tar_make()

# tar_meta(fields = error, complete_only = TRUE)

# tar_read(all_plots)
