#================================ Plot Indiv ================================

#' [Test code]
# points = tar_read(points_plus_mms); cdfs = "C:/Users/natha/Documents/_git-projects/piped_erosion_pins/_topo_outputs/topo-output_MAG.nc"



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
  
  
  # Establish a bounding box, AKA how much of the DEM we see
  buffer <- 15  # Same units as CRS (m)
  bbox <- sf::st_bbox(pts_sf) # Create rectangular bounding box 
  
  # Center pull the center of the bounding box
  xmid <- mean(c(bbox["xmin"], bbox["xmax"]))
  ymid <- mean(c(bbox["ymin"], bbox["ymax"]))
  
  # Create a distance from the center based on the longer of the two bounding box dimenstions
  half_extent <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]) / 2 + buffer

  
  
  
  
  # Create plot
  ggplot = ggplot() +
    geom_raster(data = rast_df, aes(x = x, y = y, fill = slope)) + 
    geom_sf(data = pts_sf, aes(color = estimate,
                               shape = signif,
                               size = abs(estimate)),
            size = 3.5) +
    
    # Set extent based on bounding box defined above
    coord_sf(
      xlim = c(xmid - half_extent, xmid + half_extent),
      ylim = c(ymid - half_extent, ymid + half_extent),
      expand = FALSE
    ) +
    
    # Add a scale bar
    annotation_scale(
      location = "bl",
      width_hint = 0.35, # fraction of plot width the bar spans
      plot_unit = "m",
      bar_cols = c("grey40", "grey90"),
      style = "bar"
    ) +
    
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
    scale_shape_manual(values = c("Y" = 19, "N" = 10), name = "Signifigant") +
    
    # Point colors
    scale_color_gradientn(
      colours = c("darkmagenta", "red", "orange", "royalblue2"),
      values = scales::rescale(c(-10, -5, 0, 5)),  # must match limits/data range, scaled to 0-1
      limits = c(-10, 5),
      name = "Δ Elevation\n(mm/month)"
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
                                axis.title = element_blank()
                           )
                           )
  
  
  # Pull legend from a plot
  legend = get_legend(plots[[4]] + theme(legend.position = "left"))
  
  # Plot together
  plot_panel = plot_grid(plotlist = plots_stipped[c(6, 2, 1, 4, 5, 3)],
                         ncol = 3)
  grid = plot_grid(NULL, plot_panel, legend,
                   rel_widths = c(0.05, 1.2, 0.15),
                   ncol = 3)
  
  # Save plot as file
  ggsave("_plot_outputs/all_plot.png", grid, width = 9, height = 6)
  
  return(grid)
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


#================================ Figure 2 ================================

#' [Test code]
# space = tar_read(all_plots); time = tar_read(plot_mmdt)

build_fig2 = function(space, time){
  
  fig2 = plot_grid(space, time,
            #rel_width = c(0.9, 1),
            ncol = 1,
            labels = c("A)", "B)"),
            label_size = 14)
  
  ggsave("_plot_outputs/figure_2.png", fig2, bg = "white", width = 9, height = 12)
  
  return(fig2)
  
}

