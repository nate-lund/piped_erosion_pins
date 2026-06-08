#================================ Pull point data ================================

get_points = function(path) {
  csv = read_csv(path,
           col_types = cols())
  return(csv)
  }

#' [Test code]
# points = get_points("C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv")

#================================ Pull DEMs ================================

pull_dems = function(file){
  # OpenTopography USGS 1m DEM API: https://portal.opentopography.org/API/usgsdem
  # API key:
  key = "a87bd1c14527afae952eb48288ca7ec3"
  # Coordiante system should be local UTM Zone
  
  # Assign bounding boxes using a lookup table. If more forets are needed, add.
  coords <- tribble(
    ~forest, ~lat, ~lon,
    "ASH", 44.85988792640071, -93.61976309340343,
    "WD", 44.86146495117444, -93.62371266491821,
    "MAG", 44.858404737541555, -93.61694284484315,
    "LRJ", 45.058482385228636, -93.76520775495264,
    "LRW", 45.05234713447312, -93.74745452030763,
    "LRE", 45.05909520030076, -93.73779235869106
  )
  
  forests <- data.frame(forest = unique(file$forest)) %>%
    left_join(coords, by = "forest")
  
  # Build a vector to collect output file paths
  out_paths <- character(nrow(forests))
  
  # For loop to pull DEMs using OpenTopo's API. This will take a few minutes.
  for (i in 1:nrow(forests)) {
    f <- forests$forest[i] # Pull forest name for naming later
    
    # Establish bounding box
    uleft <- forests[i, 2:3] # Pull top left corner
    delta_lat <- 150 / 111320 # Compute lat
    delta_long <- 150 / (111320 * cos(uleft[1] * pi / 180)) # Compute long, correcting for lat
    bright <- c(uleft[1] - delta_lat, uleft[2] + delta_long) # Compute bottom left corner
  
    # Pull elevation data from Opentopo API
    res <- GET(url = "https://portal.opentopography.org/API/usgsdem",
               query = list(datasetName = "USGS1m",
                            south = bright[1],
                            north = uleft[1],
                            west = uleft[2],
                            east = bright[2],
                            outputFormat = "GTiff",
                            API_Key = key))
    
    if (res$status_code != 200) {
      stop("API error for ", f, ": ", content(res, "text"))
    }
    
    
    # Write elevation data to a temporary place then pull into a DEM
    tmp = tempfile(fileext = ".tif")
    writeBin(content(res, "raw"), tmp)
    dem = rast(tmp)
    
    # Reproject to UTM Zone 15N
    dem = project(dem, "EPSG:32615")
    
    # Compute derivatives
    slope  <- terrain(dem, v = "slope", unit = "degrees")
    aspect <- terrain(dem, v = "aspect", unit = "degrees")
    
    
    # Explicitly set the extent (width and height) and the lattiude of the netCDF
    # files we are building.
    lon = xFromCol(dem)
    lat = yFromRow(dem)
    dim_lon = ncdim_def("longitude", "degrees_east", lon)
    dim_lat = ncdim_def("latitude", "degrees_north", lat)
    
    # Create CDF variables for each raster, noting units.
    var_elev = ncvar_def("elevation", "meters", list(dim_lon, dim_lat))
    var_slope = ncvar_def("slope", "degrees", list(dim_lon, dim_lat))
    var_aspect = ncvar_def("aspect", "degrees", list(dim_lon, dim_lat))
    
    # Create the output CDF file, uniquely named for each forest 
    nc_path <- paste0("_topo_outputs/topo-output_", f, ".nc")
    nc <- nc_create(nc_path, list(var_elev, var_slope, var_aspect))
    
    ncvar_put(nc, var_elev, values(dem))
    ncvar_put(nc, var_slope, values(slope))
    ncvar_put(nc, var_aspect, values(aspect))
    nc_close(nc)
    
    # Store fthe file path
    out_paths[i] <- nc_path
    
    
  }
  
  # Return the paths to each DEM
  return(out_paths)
}


#' [Test code]
#paths = pull_dems(points)

#================================ Plot Indiv ================================

#' [Test code]
#points = tar_read(points_mms)

#cdfs = "C:/Users/natha/Documents/_git-projects/piped_erosion_pins/_topo_outputs/topo-output_MAG.nc"


# Define function to plot points and slope raster on same plot. This will be done via 
# branching for each cdf path but using all the points.
plot_each = function(points, cdfs) {
  
  # Extract slope from CDF
  stack = rast(cdfs) # Convert to raster stack
  rast = stack[["slope"]] # Pull slope raster
  
  # Set CRS of raster to UTM 15N (it already is, but we must assign it)
  crs(rast) <- "EPSG:32615"
  
  # Convert p_value to 0.95 signifigance Y/N
  points = points %>%
    mutate(signif = ifelse(p_value <= 0.05, "Y", "N"))
  
  # Convert points dataframe to a usable sf
  pts_sf = st_as_sf(points,
                     coords = c("esrignss_longitude", "esrignss_latitude"),
                     crs = 4326) %>% # Points are recorded in WGS lat long
    st_transform(32615) %>% # Transform to UTM Zone 15N
    st_crop(ext(rast)) # Crop points by DEM extent
  
  # Convert the raster to a dataframe, this is needed for targets reasons
  rast_df = as.data.frame(rast, xy = TRUE)
  
  # Grab forest name for plotting and saving
  forest_name = gsub("topo-output_|\\.nc", "", basename(cdfs))
  
  
  
  # Create plot
  ggplot = ggplot() +
    geom_raster(data = rast_df, aes(x = x, y = y, fill = slope)) +
    geom_sf(data = pts_sf, aes(color = mean, shape = signif), size = 4) +
    
    # Slope/DEM colors
    scale_fill_gradient2(
      low = "grey",
      mid = "beige",
      high = "grey10",
      na.value = "transparent",
      midpoint = 15,
      limits = c(0, 45),
      name = "Slope (°)"
    ) +
  
    # Shape
    scale_shape_manual(values = c("Y" = 17, "N" = 18), name = "Signifigance") +
    
    # Point colors
    scale_color_gradient2(
      high = "darkblue",
      mid = "darkred",
      low = "red",
      midpoint = 0,
      name = "Change (mm)",
      limits = c(-20, 10)  # Fixed scale across all plots
    ) +
    
    ggtitle(forest_name) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.001),
                       name = "Lat.") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001),
                       name = "Long.") +
    
    theme(legend.position = "right",
          panel.grid.major.x = element_line(color = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(family = "sans"),
          panel.border = element_blank(),
          
          strip.background = element_rect(
            fill = "white",   # or any color
            color = "white",  # border color
            linewidth = 0.5
          ),
          
          strip.text = element_text(
            family = "sans",
            #face = "oblique",
            color = "black",
            hjust = 0,   # left
            vjust = 0  # vertical centering
            )
          )
  
  ggplot
    
  # Also saves the plot to the _plots_outputs folder for furture reference
  out_path = paste0("_plot_outputs/plot_", forest_name, ".png")
  ggsave(out_path, ggplot, width = 8, height = 6)
  
  return(ggplot)

}

#================================ Plot All ================================

#' [Test code]
# plots = tar_read(indv_plots)

plot_all = function(plots){
  
  # Strip legends and axis lables from plots
  plots_stipped = lapply(plots, function(p) p + theme(legend.position = "none") +
                           theme(axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title = element_blank()))

  
  # Pull legend from a plot
  legend = get_legend(plots[[4]] + theme(legend.position = "left"))
  
  # Plot together
  plot_panel = plot_grid(plotlist = plots_stipped[c(6, 2, 1, 4, 5, 3)], ncol = 3)
  grid = plot_grid(plot_panel, legend, rel_widths = c(1.2, 0.15))
  
  # Save plot as file
  ggsave("_plot_outputs/all_plot.png", grid, width = 8, height = 6)
  
  return()
}



#================================ Pull measurements ================================

pull_measurements = function(path, sheet){
  xls = read_excel(path, sheet = sheet)
  return(xls)
}


#================================ Data cleanup ================================

#' [Test code]
#data = pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")

data_cleanup = function(data){
  
  # Clean up data through several steps
  data_clean = data %>% 
    mutate(

      # Convert date to days past since 
      date = as.Date(date),
      dayof = as.numeric(date) - 20283 + 195, # convert to days since 1970-01-01, subtract number of days until 2025-01-01
      
      # Create an index column that is a unique numeric ID for each pin
      index = as.numeric(factor((paste(site, forest, transect, slope_pos, pin_ID, sep = "_")))),
      
      # Create a unique id for each measurement
      id = row_number(),
      
      # Add a dt, dmm, and mm column for later
      dt = 0,
      dmm = 0
      ) %>% 
    
    # Remove unneeded columns
    select(id, date, dayof, index, worms, site, forest, transect, slope_pos, pin_ID, mm1_ch, mm2_ch, mm1_bl, mm2_bl, dt, dmm) %>% 
    
    # Arrange data frame by unique pin, in order of date
    arrange(index, date)
    

  }


#================================ Difference mms ================================

#' [Test code]
#data = data_cleanup(pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025"))

difference_pins = function(data){

  # Average the duplicate measurements for each pin.
  data2 = data %>% mutate(
    mma = (mm1_ch + mm2_ch) / 2, # Change since last baseline
    mmb = (mm1_bl + mm2_bl) / 2, # Baseline for next measurement
  ) %>% 
    select(-mm1_ch, -mm2_ch, -mm1_bl, -mm2_bl)
  
  # Number of unique pins
  nindex <- length(unique(data2$index))
  
  # Create a list where each item is a data frame with all the measurements of each pin 
  pin_list <- vector(mode = "list", length = nindex) # Create empty list
  for(i in 1:nindex) { # For loop to split apart the main dataframe
    pin_list[[i]] <- data2 %>% filter(index == i)
  }
  

  # Nested for loops to difference each pin measurement (mma) with the last pin
  # measurement (mmb), getting the change at each pin (dmm) 
  for(i in 1:nindex){ # Outer loop selects each individual pin in the list
    
    for(j in 2:6) { # Inner for loop goes through each row of each df [row, column]
      
      # Subtracts today's day of from last measurement's, dt
      pin_list[[i]][j, "dt"] <- pin_list[[i]][j, "dayof"] - pin_list[[i]][j - 1, "dayof"]
      # Subtracts today's mm from last baseline, dmm
      pin_list[[i]][j, "dmm"] <- pin_list[[i]][j - 1, "mmb"] - pin_list[[i]][j, "mma"] 
    
      } # End inner loop
    
    # Final DF wide edits
    pin_list[[i]] <- pin_list[[i]] %>%
      # Remove rows with NA's in slope_pos column
      drop_na(slope_pos) %>% 
      
      # Calcualte cumulative dmm for each pin
      mutate(mm = cumsum(dmm))
      
    } # End outer loop
  
  #  Rebuild the data frame
  pp_data <- pin_list[[1]] # Start to the for loop
  for(i in 2:nindex) { # for loop to stack dataframes one list at a time
    pp_data <- merge(pp_data, pin_list[[i]], all = TRUE)
  }
  
  return(pp_data)
  
}



#================================ QAQC ================================

#' [Test code]
#data3 = difference_pins(data_cleanup(pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")))
#differenced_stats(data3)

differenced_QAQC = function(data3) {

  # Fit a linear model to very quickly check assumptions on a large scale
  lm = lm(data = data3, mm ~ date)
  
  # Visualize using the performance package
  lm_perform = check_model(lm, check = c("linearity", "homogeneity", "qq", "normality"))
  
  # We can see that the presence of baseline measurements throws things off,
  # so lets try removing them (mms where dt = 0)
  data_nozero = data3 %>% 
    filter(dt != 0)
  
  # Plot histogram alone
  hist_nz = ggplot(data = data_nozero, mapping = aes(x = mm)) +
    geom_histogram(bins = 50)
  
  # Fit a linear model to very quickly check assumptions on a large scale
  lm_nz = lm(data = data_nozero, mm ~ date)
  
  # Visualize using the performance package
  lm_nz_perform = check_model(lm_nz, check = c("linearity", "homogeneity", "qq", "normality"))
  
  
  # Bundle outputs
  perform_list = list(lm_perform, lm_nz_perform)
  
  return(perform_list)
}



#================================ Fitting lsplines ================================

#' [Test code]
#data4 = difference_pins(data_cleanup(pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")))


fit_lspines = function(data4){
  
  # Number of forests in dataset
  forests = unique(data4$forest)
  nforests = length(forests) 
  
  # Create a list where each item is a data frame with all the measurements from
  # one forest and one hillslope position.
  forest_list <- vector(mode = "list", length = nforests * 2) # Create empty list
  
  for(i in 1:nforests) { # For loop to split by forest for BS
    forest_list[[i]] <- data4 %>%
      filter(forest == forests[i] & slope_pos == "BS")
  }
  
  for(i in 1:nforests) { # For loop to split by forest for FS
    forest_list[[i + nforests]] <- data4 %>%
      filter(forest == forests[i] & slope_pos == "FS")
  }
  
  
  # For loop to fit lspline with knots at measurements dates
  for (i in 1:(nforests * 2)){
    # Define knot dates, do not need knots at beginning or end.
    knots <- unique(forest_list[[i]]$date)[2:(length(unique(forest_list[[i]]$dayof))-1)] 
    
    # Fit lspline
    lspline = lm(data = forest_list[[i]], mm ~ lspline(date, knots = knots))
    summary(lspline)
    
    # Create predictions for plotting later
    forest_list[[i]]$predic <- predict(lspline)
    
    ifelse(i == 1,
           ls_data <- forest_list[[i]],
           ls_data <- merge(ls_data, forest_list[[i]], all = TRUE)) # merge data frames
    
    
    # Save model coefficients
    coefs <- tidy(lspline) # pulls estimate, std.error, statistic, p.value
    error <- data.frame(confint(lspline)) %>%   # grab margin of errors
      mutate(m_error = (X97.5.. - X2.5..) / 2)
    coefs$m_error = error$m_error # combine estimates with margin
    
    coefs$forest = unique(forest_list[[i]]$forest) # Add a column denoting forest
    coefs$slope_pos = unique(forest_list[[i]]$slope_pos) # Add a column denoting slope pos
    
    # Add a column for dates, here the date refers to the erosion from the previous
    # measurement period
    coefs$date = unique(forest_list[[i]]$date)
    
    ifelse(i == 1,
           all_coefs <- coefs,
           all_coefs <- merge(all_coefs, coefs, all = TRUE)) # Merge data frames
    
    
  } # End loop
  
  # Write to _stats_output
  write_xlsx(ls_data, "_stats_outputs/ls_data.xlsx")
  write_xlsx(all_coefs, "_stats_outputs/all_coefs.xlsx")
 
  return() 
}


#================================ Combine points and mms ================================

#' [Test code]
#mms = read_excel("_stats_outputs/ls_data.xlsx")

#points = get_points("C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv")


combo = function(points, mms){
  
  # Simplify measurement data to have have only the overall elevation change at each pin
  # and do a basic one-sample t-test to determine significance.
  mms_data = mms %>% 
    group_by(index) %>% 
    filter(date == max(date)) %>% # Select only last measurement
    ungroup() %>% 
    
    # Fit linear models to get mean change of each 'array' (each forest, slope_pos, 
    # transect combo) with summary statistics.
    group_by(forest, slope_pos, transect) %>% 
    summarise(
      mean = as.numeric(tidy(lm(mm - 0 ~ 1))["estimate"]),
      std_error = as.numeric(tidy(lm(mm - 0 ~ 1))["std.error"]),
      p_value = as.numeric(tidy(lm(mm - 0 ~ 1))["p.value"]),
      .groups = "drop"
    ) %>% 
    ungroup()
  
  # Clean up point data to remove extranious columns
  points_data = points %>% 
    select(forest, transect, slope_pos, esrignss_latitude, esrignss_longitude)
  
  points_mms <- left_join(mms_data, points_data, by = c("forest", "transect", "slope_pos"))
  
  return(points_mms)
}


#================================ Pull slope at points ================================

#' [Test code]
#cdfs = "C:/Users/natha/Documents/_git-projects/piped_erosion_pins/_topo_outputs/topo-output_ASH.nc"

#points = tar_read(points_mms)


extract_slope = function(points, cdfs) {

  # Extract slope from CDF
  stack = rast(cdfs) # Convert to raster stack
  rast = stack[["slope"]] # Pull slope raster
  
  # Set CRS of raster to UTM 15N (it already is, but we must assign it)
  crs(rast) <- "EPSG:32615"
  
  # Convert points dataframe to a usable sf
  pts_sf = st_as_sf(points,
                    coords = c("esrignss_longitude", "esrignss_latitude"),
                    crs = 4326) %>% # Points are recorded in WGS lat long
    st_transform(32615) %>%  # Transform to UTM Zone 15N
    st_crop(ext(rast)) # Crop points by DEM extent
  
  # Extract slope values at each point
  extracted = terra::extract(rast, pts_sf)
  
  # Bind slope values back to the points
  pts_sf$slope = extracted$slope
  
  
  return(pts_sf)
}

#================================ Plot dmm/dt ================================

#' [Test code]
# data <- read_excel("_stats_outputs/ls_data.xlsx")

plot_mm_dt = function(data){

# Need to make a index column for plotting
plot_data = data %>% 
  mutate(forest_date = interaction(forest,
                                   date,
                                   slope_pos,
                                   sep = "_"),
         # To ensure propoer ordering for facet_wrap
         forest = factor(forest, levels = c("ASH", "LRW", "LRE", "MAG", "WD", "LRJ")))

ggplot <- ggplot(data = plot_data, mapping = aes(x = date, y = mm)) +
  
  # Plot the lines tracking each pin, color by slope position
  geom_line(aes(group = index,
                color =  slope_pos),
            linetype = 1) +
  
  # Plot boxplots for each forest and slope position
  geom_boxplot(aes(group = forest_date,
                   width = 3,
                   fill = slope_pos)) +
  
  # Plot lspline fits
  geom_line(#data = plot_data,
            aes(x = date,
                y = predic,
                linetype = slope_pos),
            linewidth = 1) +
  
  facet_wrap(~forest, ncol = 3) + 
  
  # Visuals
  scale_color_manual(
    name = "Slope Position",
    values = c(
      "FS" = "royalblue2",
      "BS" = "indianred2"
    )) +
  
  scale_fill_manual(
    name = "Slope Position",
    values = c(
      "FS" = "royalblue2",
      "BS" = "indianred2"
    )) +
  
  scale_linetype_discrete(name = "Slope Position") +
  
  scale_y_continuous( 
    limits = c(-20, 20),
    name = "Adjusted Height (mm)") + # May cut off some outliers, fine for visualization
  scale_x_date(limits = c(ymd("2025-07-10"), ymd("2025-10-01")),
               name = "Date",
               date_breaks = "1 month", date_labels = "%b") + # Remove this section for Par-Sci
  geom_hline(yintercept = 0, linewidth = 0.5) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(color = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "sans"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        
        strip.background = element_rect(
          fill = "white",   # or any color
          color = "white",  # border color
          linewidth = 0.5
        ),
        
        strip.text = element_text(
          family = "sans",
          #face = "oblique",
          color = "black",
          hjust = 0,   # left
          vjust = 0  # vertical centering
          
        )
        
  ) 


ggsave("_plot_outputs/mms_plot.png", ggplot, width = 9, height = 6, dpi = 900)

return(ggplot)
}





#================================ Plot Eros/Slope ================================


# data = as.data.frame(tar_read(points_mms_slope))
# 
# ggplot(data = data, mapping = aes(x = slope, y = mean)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)
#   
# lm = lm(data = data, mean ~ slope)
# summary(lm)


