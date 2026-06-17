#================================ Packages ================================

# Clear environment
rm(list = ls(all.names = TRUE))

# libraries needed
libs <- c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4", "tidyr", "dplyr", "readr", "targets", "usethis", "sf", "targets", "visNetwork", "tarchetypes", "tidyterra", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT", "flextable", "wesanderson", "ggspatial", "extrafont")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
lapply(libs, library, character.only = T)


#================================ Setup ================================

# Set target options:
tar_option_set(
  packages = c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4",
               "tidyr", "dplyr", "readr", "targets", "usethis", "targets",
               "visNetwork", "tarchetypes", "tidyterra") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with functions
tar_source(
    # Contains functions that rely exclusively on erosion pin measurement data.
    "R/measurements-functions.R"
)

tar_source(
    # Contains functions that use spatial data. 
    "R/spatial-functions.R"
)

tar_source(
    # Contains functions that integrate spatial and measurement data. 
    "R/integrated-functions.R"
    )




#================================ Targets ================================

# Replace the target list below with your own:
list(
  ## Pull erosion pin measurement data ====
  tar_target(
    raw_measurements,
    pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")
  ),
  
  ## Clean up erosion pin data ====
  tar_target(
    clean_data,
    data_cleanup(raw_measurements)
  ),
  
  ## Difference erosion pins ====
  tar_target(
    differenced_pins,
    difference_pins(clean_data)
  ),
  
  ## QAQC Measurement data ====
  tar_target(
    QAQC,
    differenced_QAQC(differenced_pins)
  ),
  
  ## Fil lslpines to pins ====
  tar_target(
    lsplines,
    fit_lspines(differenced_pins)
  ),
  
  ## Plot mms over time for each forests ====
  tar_target(
    plot_mmdt,
    plot_mm_dt(lsplines)
  ),
  
  ## Make df for mms over time ====
  tar_target(
    mms_frame,
    frame_mmdt(lsplines)
  ),
  
  # Target 2: Make a nice publication-quality table for mms over time 
  # tar_target(
  #   mms_table,
  #   table_mmdt(mms_df)
  # ),
  
  ## Locate GPS locations based on file path ====
  tar_target(
    name = find_data,
    "C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv"
  ),
  
  ## Import CSV into R ====
  tar_target(
    name = points,
    command = get_points(find_data)
  ),
  
  ## Pull DEMs from OpenTopography API. Export as netCDF files ====
  tar_target(
    cdf_paths,
    pull_dems(points)
  ),
  
  ## Tracks the actual files on disk, branches over them ====
  tar_files( # This facilitates branching which is needed for the next step 
    cdfs,
    cdf_paths
  ),
  
  ## Plots rasters and point data together, unique plot for each forest ====
  tar_target(
    indv_plots,
    plot_each(points_mms, cdfs),
    pattern = map(cdfs),
    iteration = "list"
  ),
  
  ## Put dem/point plots in a list ====
  tar_target(
    all_plots,
    plot_all(indv_plots)
  ),
  
  ## Combine point (GPS) data and measurement data ====
  tar_target(
    points_mms,
    combo(points, differenced_pins)
  ),
  
  ## Extract slope at each point ====
  tar_target(
    slope_extracted,
    extract_slope(points_mms, cdfs),
    pattern = map(cdfs)
  ),
  
  ## Combine all branches into one dataframe ====
  tar_target(
    points_mms_slope,
    bind_rows(slope_extracted)
  )
  
  )
  
# tar_visnetwork()

# tar_make()

# tar_meta(fields = error, complete_only = TRUE)

# tar_read(all_plots)
