#================================ Packages ================================

# Set environment for TERRA
Sys.setenv(PROJ_LIB = "C:/Users/natha/AppData/Local/R/win-library/4.6/terra/proj")

# libraries needed
libs <- c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4", "tidyr", "dplyr", "readr", "targets", "usethis", "sf", "targets", "visNetwork", "tarchetypes", "tidyterra", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT", "flextable", "wesanderson", "ggspatial", "extrafont", "officer", "svglite", "ggspatial", "stringr")

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
  packages = c(libs), # Packages that your targets need for their tasks.
  error = "continue" # tar_make() will continue even if it hits one error
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


#' [WHEN RUNNING FOR THE FIRST TIME]
# Run commented out code below to clear storage:
# tar_destroy(destroy = c("objects")); file.remove(list.files("_plot_outputs", full.names = TRUE))


#================================ Targets ================================

# Replace the target list below with your own:
list(
  
  ## Measurement Functions ====
  ### Pull erosion pin measurement data ====
  tar_target(
    raw_measurements,
    pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/2025_ARB-LR_raw.xlsx", "Sheet1")
  ),
  
  ### Clean up erosion pin data ====
  tar_target(
    clean_data,
    data_cleanup(raw_measurements)
  ),
  
  ### Difference erosion pins ====
  tar_target(
    differenced_pins,
    difference_pins(clean_data)
  ),
  
  ### QAQC Measurement data ====
  tar_target(
    QAQC,
    differenced_QAQC(differenced_pins)
  ),
  
  ### Fil lslpines to pins ====
  tar_target(
    lsplines,
    fit_lspines(differenced_pins),
    format = "file"
  ),
  
  ### Plot mms over time for each forests ====
  tar_target(
    plot_mmdt,
    plot_mm_dt(lsplines)
  ),
  
  ### Compute overall change (mm) by transect ====
  # This is used for plotting
  tar_target(
    transect_overall,
    overall_transect_stats(lsplines)
  ),
  
  ### Compute overall rate of change (mm/day) by slope_pos ====
  tar_target(
    slope_pos_roc,
    roc_slopepos_stats(lsplines)
  ),
  
  ### Pull Baumann et al. 2025 data ====
  tar_target(
    baumann_bd,
    read_excel("C:/Users/natha/Box/_data/_outside_data/Baumenn-et-al_BD-values.xlsx")
  ),
  
  ### Compute back of envelope unit computations using Baumann et al. data ====
  tar_target(
    erosion_totals_units,
    convert_units(slope_pos_roc, baumann_bd),
    format = "file" # Saved as an .xlxs, so need this.
  ),
  
  ### Make flextable erosion totals ====
  tar_target(
    erosion_totals_ft,
    table_erosion_totals(erosion_totals_units)
  ),
  
  ### Make df for mms over time ====
  tar_target(
    mms_frame,
    frame_mmdt(lsplines)
  ),
  
  # Target 2: Make a nice publication-quality table for mms over time 
  # tar_target(
  #   mms_table,
  #   table_mmdt(mms_df)
  # ),
  
  ## Spatial Functions ====
  ### Locate GPS locations based on file path ====
  tar_target(
    find_data,
    "C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv"
  ),
  
  ### Import CSV into R ====
  tar_target(
    points,
    read_csv("C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv",
             show_col_types = FALSE)
  ),
  
  ### Pull DEMs from OpenTopography API. Export as netCDF files ====
  tar_target(
    cdf_paths,
    pull_dems(points)
  ),
  
  ### Tracks the actual files on disk, branches over them ====
  tar_files( # This facilitates branching which is needed for the next step 
    cdfs,
    cdf_paths
  ),
  
  ## Integrated Functions ====
  ### Plots rasters and point data together, unique plot for each forest ====
  tar_target(
    indv_plots,
    plot_each(points_plus_mms, cdfs),
    pattern = map(cdfs),
    iteration = "list"
  ),
  
  ### Put dem/point plots in a list ====
  tar_target(
    all_plots,
    plot_all(indv_plots)
  ),
  
  ### Combine point (GPS) data and measurement data ====
  tar_target(
    points_plus_mms,
    left_join(transect_overall, points, by = c("forest", "transect", "slope_pos"))
  ),
  
  ### Extract slope at each point ====
  tar_target(
    slope_extracted,
    extract_slope(points_plus_mms, cdfs),
    pattern = map(cdfs)
  ),
  
  ### Combine all branches into one dataframe ====
  tar_target(
    points_mms_slope,
    bind_rows(slope_extracted)
  ),
  
  ### Build Figure 2 ====
  tar_target(
    figure2,
    build_fig2(all_plots, plot_mmdt)
  )
  
  )

#================================ Something ================================  

# tar_manifest()

# tar_visnetwork()

# tar_make()

# Clear _targets/objects
# tar_destroy(destroy = c("objects"))

# tar_meta(fields = error, complete_only = TRUE)

# tar_read(all_plots)

# tar_read(plot_mmdt)
