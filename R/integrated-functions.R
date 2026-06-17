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

