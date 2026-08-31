#================================ Plot Individal DEMs ================================

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
    geom_sf(data = pts_sf,
            aes(color = estimate,
                shape = signif,
                size = abs(estimate)),
            size = 3.5) +
    
    # Label slope positions
    geom_text_repel(
      data = pts_sf %>% filter(slope_pos == "BS"), # For BS
      aes(label = slope_pos, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      color = "black",
      max.overlaps = Inf,
      point.padding = 0.6,     
      box.padding = 0.3,
      nudge_y = -3,
      nudge_x = -1
    ) +
    
    geom_text_repel(
      data = pts_sf %>% filter(slope_pos == "FS"), # For FS
      aes(label = slope_pos, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      color = "black",
      max.overlaps = Inf,
      point.padding = 0.6,     
      box.padding = 0.3,
      nudge_y = 3,
      nudge_x = 1
    ) +
  
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
      bar_cols = c("black", "white"),
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
          
          plot.title = element_text(face = "italic",
                                    size = 12),
          
          strip.background = element_rect(
            fill = "white",
            color = "white",
            linewidth = 0.5
          ),
          
          strip.text = element_text(
            family = "sans",
            face = "italic",
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

#================================ Combine DEM Plots ================================

#' [Test code]
# plots = tar_read(indv_plots)

plot_all = function(plots){
  
  # Strip legends and axis lables from plots, stores as a lsit
  plots_stipped = lapply(plots, function(p) p + theme(legend.position = "none") +
                           theme(axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title = element_blank()
                           )
                           )
  
  
  # Pull legend from a plot
  legend = get_legend(plots[[4]] + theme(legend.position = "left"))
  
  # Plot worm assemblages together
  panel_ew = plot_grid(plotlist = plots_stipped[c(6, 2, 1)],
                         ncol = 3)
  panel_jw = plot_grid(plotlist = plots_stipped[c(4, 5, 3)],
                       ncol = 3)
  
  # Create worm labels
  ew_label = ggdraw() + draw_label("European Earthworm Dominated",
                                   fontface = "bold",
                                   size = 12,
                                   x = 0.02,
                                   hjust = 0)
  jw_label = ggdraw() + draw_label("Jumping Worm Dominated",
                                   fontface = "bold",
                                   size = 12,
                                   x = 0.02,
                                   hjust = 0)
  
  
  # Plot all together
  plot_panel = plot_grid(ew_label, panel_ew, jw_label, panel_jw,
                   rel_heights = c(0.08, 1.2, 0.08, 1.2),
                   ncol = 1)
  
  
  grid = plot_grid(NULL, plot_panel, legend,
                   rel_widths = c(0.05, 1.2, 0.15),
                   ncol = 3) +
    # Add a white background
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  # Save plot as file
  ggsave("_plot_outputs/all_plot.png", grid, width = 9, height = 6)
  
  return(grid)
}



#================================ Create DF for mms/dt ================================

#' [Test code]
# data = read_xlsx("_stats_outputs/fit_data.xlsx")

frame_mmdt = function(path){
  
  data = read_excel(path)
  
  # Build table of raw data
  table_data <- data %>%
    
    # Summarize to get one estimate for each slope_pos, forest, and date, this
    # does not change any numbers, just collapses rows with repeated estimates
    select(term, forest, slope_pos, estimate, std.error, p.value) %>%  
    distinct() %>% 
    
    # Round estimate values (mm/day)
    mutate(estimate = round(estimate, digits = 3)) %>% 
    mutate(std.error = round(std.error, digits = 3)) %>%
    mutate(p.value = round(p.value, digits = 3)) %>%
    
    # Rename terms to something more recognizable
    mutate(term = case_when(
      term == "lspline(date, knots = knots)1" ~ "slope.1",
      term == "lspline(date, knots = knots)2" ~ "slope.2",
      term == "lspline(date, knots = knots)3" ~ "slope.3",
      term == "lspline(date, knots = knots)4" ~ "slope.4",
      term == "lspline(date, knots = knots)5" ~ "slope.5"
    )) %>% 
    
    pivot_wider(
      names_from = term,
      values_from = c(estimate, std.error, p.value)
    )
  
  return(table_data) 
}



#================================ Flextable for mms/dt ================================

#' [Test code]
# frame = tar_read(mms_frame)

table_mm_dt = function(frame){
  
  # Remove duplicated forest names
  H3_final_table.df <- H3_final_table.df %>%
    mutate(forest = as.character(forest)) %>%
    group_by(forest) %>%
    mutate(forest = ifelse(row_number() == 1, forest, "")) %>%
    ungroup()
  
  
  
  
  
  
  # Add data and format table
  H3_mm_over_time.ft <- flextable(H3_final_table.df,
                                  col_keys = c("forest",
                                               "slope_pos",
                                               "blank",
                                               "estimate_slope.1",
                                               "std.error_slope.1",
                                               #"p.value_slope.1",
                                               "blank1",
                                               "estimate_slope.2",
                                               "std.error_slope.2",
                                               #"p.value_slope.2",
                                               "blank2",
                                               "estimate_slope.3",
                                               "std.error_slope.3",
                                               #"p.value_slope.3",
                                               "blank3",
                                               "estimate_slope.4",
                                               "std.error_slope.4",
                                               #"p.value_slope.4",
                                               "blank4",
                                               "estimate_slope.5",
                                               "std.error_slope.5"
                                               #"p.value_slope.5"))
                                  ))%>% 
    empty_blanks() %>%
    
    font(part = "all", fontname = "Calibri") %>% 
    fontsize(part = "all", size = 11) %>% 
    
    align(align = "center", part = "all") %>% 
    valign(valign = "center", part = "header") %>% 
    
    
    # All
    font(part = "all", fontname = "Calibri") %>% 
    fontsize(part = "all", size = 11) %>% 
    align(align = "right", part = "all") %>%
    
    # Header
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "header") %>% 
    
    width(j = c("forest",
                "slope_pos",
                "estimate_slope.1",
                "std.error_slope.1",
                "estimate_slope.2",
                "std.error_slope.2",
                "estimate_slope.3",
                "std.error_slope.3",
                "estimate_slope.4",
                "std.error_slope.4",
                "estimate_slope.5",
                "std.error_slope.5"), width = 0.7) %>% 
    #width(j = "landcover", width = 3.5) %>% 
    
    line_spacing(space = 1.8, part = "header") %>% 
    
    # Landcover
    #align(align = "left", part = "center", j = "landcover") %>% 
    #align(align = "left", j = "landcover") %>% 
    
    
    # * all significant values
    mk_par(j = "estimate_slope.1",
           i = ~ p.value_slope.1 < 0.05,
           value = as_paragraph(
             estimate_slope.1,
             "*"
           )) %>% 
    mk_par(j = "estimate_slope.2",
           i = ~ p.value_slope.2 < 0.05,
           value = as_paragraph(
             estimate_slope.2,
             "*"
           )) %>% 
    mk_par(j = "estimate_slope.3",
           i = ~ p.value_slope.3 < 0.05,
           value = as_paragraph(
             estimate_slope.3,
             "*"
           )) %>% 
    mk_par(j = "estimate_slope.4",
           i = ~ p.value_slope.4 < 0.05,
           value = as_paragraph(
             estimate_slope.4,
             "*"
           )) %>% 
    mk_par(j = "estimate_slope.5",
           i = ~ p.value_slope.5 < 0.05,
           value = as_paragraph(
             estimate_slope.5,
             "*"
           )) %>% 
    
    # Add +/- to errors
    set_formatter(
      m_error_slope.1  = function(x) ifelse(x != 0, paste0("± ", x, "")),
      m_error_slope.2  = function(x) ifelse(x != 0, paste0("± ", x, "")),
      m_error_slope.3  = function(x) ifelse(x != 0, paste0("± ", x, "")),
      m_error_slope.4  = function(x) ifelse(x != 0, paste0("± ", x, "")),
      m_error_slope.5  = function(x) ifelse(x != 0, paste0("± ", x, ""))
    ) %>% 
    
    
    # Color erosion values
    color(~ estimate_slope.1 < 0, color = "red", j = "estimate_slope.1") %>% 
    color(~ estimate_slope.2 < 0, color = "red", j = "estimate_slope.2") %>% 
    color(~ estimate_slope.3 < 0, color = "red", j = "estimate_slope.3") %>% 
    color(~ estimate_slope.4 < 0, color = "red", j = "estimate_slope.4") %>% 
    color(~ estimate_slope.5 < 0, color = "red", j = "estimate_slope.5") %>% 
    
    add_header_row(values = c("",
                              "Period 1",
                              "Period 2",
                              "Period 3",
                              "Period 4",
                              "Period 5"),
                   colwidths = c(2, # adds up to total number of cols
                                 3,
                                 3,
                                 3,
                                 3,
                                 3)) %>% 
    
    labelizor(
      part = "header", 
      labels = c("forest" = "Site",
                 "slope_pos" = "Slope Position",
                 "estimate_slope.1" = "Erosion (cm/yr)",
                 "std.error_slope.1" = "Std. Error",
                 #"p.value_slope.1" = "p-value",
                 "estimate_slope.2" = "Erosion (cm/yr)",
                 "std.error_slope.2" = "Std. Error",
                 #"p.value_slope.2" = "p-value",
                 "estimate_slope.3" = "Erosion (cm/yr)",
                 "std.error_slope.3" = "Std. Error",
                 #"p.value_slope.3" = "p-value",
                 "estimate_slope.4" = "Erosion (cm/yr)",
                 "std.error_slope.4" = "Std. Error",
                 #"p.value_slope.4" = "p-value",
                 "estimate_slope.5" = "Erosion (cm/yr)",
                 "std.error_slope.5" = "Std. Error"
                 #"p.value_slope.5" = "p-value"
      ))
  
  H3_mm_over_time.ft
  
}


#================================ Flextable for Erosion Totals ================================

#' [Test Code]
# path = tar_read(erosion_totals_units)

table_erosion_totals = function(path){
  
  # Format datatable before building FT
  df = read_excel("_stats_outputs/erosion_totals.xlsx") %>% 
    
    # Round values
    mutate(
      estimate_mm = round(estimate_mm, digits = 2),
      std_error_mm = round(std_error_mm, digits = 2),
      dt = round(dt, digits = 0),
      
      bd_mean = signif(bd_mean, digits = 3),
      bd_se = signif(bd_se, digits = 3),
      
      tons_km2 = signif(tons_km2, digits = 3),
      tons_km2_se = signif(tons_km2_se, digits = 3),
      
      tons_acre = signif(tons_acre, digits = 3),
      tons_acre_se = signif(tons_acre_se, digits = 3)
    ) %>% 
    
    # Change case
    mutate(type = case_when(
      type == "bs" ~ "BS",
      type == "fs" ~ "FS"
    )) %>% 
    
    # Update worms
    mutate(worms = case_when(
      worms == "JW" ~ "Jumping worm",
      worms == "EW" ~ "European worm"
    )) %>% 
    
    
    # Remove duplicated forest names
    mutate(forest = as.character(forest)) %>%
    group_by(forest) %>%
    mutate(forest_vis = ifelse(row_number() == 1, forest, "")) %>%
    ungroup() %>% 
    
    # Order
    mutate(forest = factor(forest, levels = c("ASH", "LRW", "LRE", "MAG", "WD", "LRJ"))) %>% 
    arrange(forest)
  
  
  # Define a border, needed in table
  box_cols <- c("estimate_mm", "tons_km2")
  outline_border <- fp_border(color = "red", width = 1.5)   
  
  # Build flextable
  erosion_units_ft = flextable(df, 
                               col_keys = c("worms",
                                            "forest",
                                            "type",
                                            "blank1",
                                            "estimate",
                                            "std_error",
                                            "p_value",
                                            "blank2",
                                            "dt",
                                            "blank5",
                                            "estimate_mm",
                                            "std_error_mm",
                                            "blank4",
                                            #"worms",
                                            #"bd_mean",
                                            #"bd_se",
                                            "tons_km2",
                                            "tons_km2_se",
                                            "blank3",
                                            "tons_acre",
                                            "tons_acre_se")
  ) %>% 
    empty_blanks() %>%
    bg(bg = "white", part = "all") %>% 
    
    # All
    font(part = "all", fontname = "Calibri") %>% 
    fontsize(part = "all", size = 11) %>% 
    align(align = "right", part = "all") %>%
    
    # Header
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "header") %>% 
    
    width(j = c("worms",
                "forest",
                "type",
                "estimate",
                "std_error",
                "p_value",
                "dt",
                "estimate_mm",
                "std_error_mm",
                #"worms",
                #"bd_mean",
                #"bd_se",
                "tons_km2",
                "tons_km2_se",
                "tons_acre",
                "tons_acre_se"), width = 1) %>% 
    
    line_spacing(space = 1.8, part = "header") %>% 
    
    add_header_row(values = c("",
                              "Statistics (mm/month)",
                              "",
                              "mm/yr",
                              "tonnes/km²/yr",
                              "tons/acre/yr"),
                   colwidths = c(4, # adds up to total number of cols
                                 4,
                                 2,
                                 3,
                                 3,
                                 2)) %>% 
    
    # * all significant values, add units 
    
    mk_par(
      j = "std_error",
      value = as_paragraph(
        as_chunk(formatC(estimate_mm, format = "f", digits = 2))
      )
    ) %>%
    
    mk_par(
      j = "estimate",
      value = as_paragraph(
        as_chunk(formatC(estimate, format = "f", digits = 2)),
        as_chunk(" mm/mo", props = fp_text(color = "grey50", font.size = 8)),
        as_chunk(ifelse(p_value < 0.05, "*", ""))
      )
    ) %>% 
    
    mk_par(
      j = "estimate_mm",
      value = as_paragraph(
        as_chunk(formatC(estimate_mm, format = "f", digits = 2)),
        as_chunk(" mm", props = fp_text(color = "grey50", font.size = 8)),
        as_chunk(ifelse(p_value < 0.05, "*", ""))
      )
    ) %>%
    
    mk_par(
      j = "tons_km2",
      value = as_paragraph(
        as_chunk(formatC(tons_km2, format = "f", digits = 0)),
        as_chunk(" t/km²", props = fp_text(color = "grey50", font.size = 8)),
        as_chunk(ifelse(p_value < 0.05, "*", ""))
      )
    ) %>% 
    
    mk_par(
      j = "tons_acre",
      value = as_paragraph(
        as_chunk(formatC(tons_acre, format = "f", digits = 2)),
        as_chunk(" t/ac", props = fp_text(color = "grey50", font.size = 8)),
        as_chunk(ifelse(p_value < 0.05, "*", ""))
      )
    ) %>%
    
    # Add x to dt
    mk_par(
      j = "dt",
      value = as_paragraph(
        as_chunk(formatC(dt, format = "f", digits = 0)),
        as_chunk(" days", props = fp_text(color = "grey50", font.size = 8))
      )
    ) %>%
    
    # Format p values in scientific notation
    set_formatter(p_value = function(x) formatC(x, format = "e", digits = 2)) %>% 
    
    
    # Zebra striping — every other row
    bg(i = seq(2, nrow(df), by = 2), bg = "grey95", part = "body") %>% 
    
    # Divider line where EW transitions to JW
    hline(i = 6, part = "body", border = fp_border(color = "black", width = 1.5)) %>% 
    
    # Merge duplicate forest names
    merge_v(j = "forest", part = "body") %>% 
    width(j = "forest", width = 0.75) %>% 
    align(j = "forest", align = "center", part = "all") %>% 
    
    
    # Clean up slope positions
    width(j = "type", width = 0.75) %>% 
    align(j = "type", align = "center", part = "all") %>% 
    
    
    # Change widths of SE
    width(j = c("std_error", "tons_km2_se", "tons_acre_se", "p_value", "dt"), width = 0.9) %>% 
    align(j = c("std_error", "tons_km2_se", "tons_acre_se", "p_value", "dt"), align = "center", part = "all") %>% 
    
    
    # Edit width of estimates
    width(j = c("estimate", "tons_km2", "tons_acre"), width = 1.2) %>% 
    
    
    # Create and rotate worms column
    merge_v(j = "worms", part = "body") %>% # Merges worms together 
    rotate(j = "worms", rotation = "btlr", part = "body") %>% # Rotate text
    valign(j = "worms", valign = "center", part = "body") %>%
    align(j = "worms", align = "center", part = "all") %>%
    width(j = "worms", width = 0.3) %>% 
    void(j = "worms", part = "header") %>% 
    
    #Add boxes around key columns
    # left/right sides — second header row only
    border(i = 2, j = box_cols,
           border.left = outline_border,
           border.right = outline_border,
           part = "header") %>%
    # left/right sides — full body
    border(j = box_cols,
           border.left = outline_border,
           border.right = outline_border,
           part = "body") %>%
    # top cap — set on BOTH sides of the row1/row2 boundary
    border(i = 2, j = box_cols,
           border.top = outline_border,
           part = "header") %>%
    # bottom cap
    border(i = nrow(df), j = box_cols,
           border.bottom = outline_border,
           part = "body") %>% 
    
    
    labelizor(
      part = "header", 
      labels = c("forest" = "Forest",
                 "type" = "Pos.",
                 "estimate" = "Estimate",
                 "std_error" = "SE",
                 "p_value" = "p-value",
                 "dt" = "Study Days",
                 "estimate_mm" = "Estimate",
                 "std_error_mm" = "SE",
                 "tons_km2" = "Estimate",
                 "tons_km2_se" = "SE",
                 "tons_acre" = "Estimate",
                 "tons_acre_se" = "SE"
      ))
  
  
  
  path = "_plot_outputs/erosion_totals.svg"
  save_as_image(erosion_units_ft, path = path)
  
  path2 = "_plot_outputs/erosion_totals.png"
  save_as_image(erosion_units_ft, path = path2)
  
  return(path2)
  
}

#================================ Plot dmm/dt ================================

#' [Test code]
# path <- "_stats_outputs/fit_data.xlsx"

plot_mm_dt = function(path){
  
  data = read_excel(path)
  
  # Need to make a index column for plotting
  plot_data = data %>% 
    mutate(forest_date = interaction(forest,
                                     date,
                                     slope_pos,
                                     sep = "_"),
           # To ensure propoer ordering for facet_wrap
           forest = factor(forest, levels = c("ASH", "LRW", "LRE", "MAG", "WD", "LRJ")),
           
           # A Catagorical signifigance column
           signif = ifelse(p.value <= 0.05, "Y", "N"),
           
           worms = case_when(
             worms == "EW" ~ "European Earthworm Dominated",
             worms == "JW" ~ "Jumping Worm Dominated"
             
           )
           
           
    )
  
  
  ggplot <- ggplot(data = plot_data, mapping = aes(x = date, y = mm)) +
    
    
    # Plot reference lines
    geom_hline(yintercept = c(-35, -10, -5, -1, 0, 1, 5, 10, 35), color = "grey80", linewidth = 0.3) + 
    geom_hline(yintercept = 0, linewidth = 0.5) + # Plot line at y = 0
    
    # Plot the lines tracking each pin, color by slope position
    geom_line(aes(group = index,
                  color =  slope_pos),
              linetype = 1,
              alpha = 0.5) +
    
    # Plot boxplots for each forest and slope position
    geom_boxplot(aes(group = forest_date,
                     width = 3,
                     fill = slope_pos
    ),
    alpha = 1) +
    
    # Plot lspline fits
    geom_line(
      aes(x = date,
          y = predic,
          linetype = slope_pos),
      linewidth = 0.8) +
    
    
    # Plot a lm over the whole period for only the BS
    geom_smooth(
      data = plot_data %>% filter(slope_pos == "BS"),
      aes(group = slope_pos,
          linetype = slope_pos),
      method = "lm",
      se = FALSE,
      fullrange = TRUE,
      linewidth = 1.2,
      color = "grey10") +
    
    # Facet by forest using a ggh4x pacakge function
    facet_nested_wrap(
      vars(worms, forest),
      nrow = 2,
      strip.position = "top",
      strip = strip_nested(
        text_x = elem_list_text(face = c("bold", "italic")),
        by_layer_x = TRUE
      )
      ) +
    
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
    
    # Plot some dummy points - this is just to get outliers in the legend 
    geom_point(data = data.frame(date = as.Date(NA), mm = NA_real_),
               aes(x = date, y = mm, shape = "Outliers"),
               color = "black", na.rm = TRUE) +
    
    scale_shape_manual(name = NULL, values = c("Outliers" = 19)) +
    
    scale_linetype_discrete(name = "Slope Position") +
    
    #coord_cartesian(ylim = c(-20, 20)) +   # May cut off some outliers, fine for visualization
    
    scale_y_continuous( 
      name = "log Height (mm)",
      trans = scales::pseudo_log_trans(sigma = 1, base = 10),
      breaks = c(-35, -10, -5, -1, 0, 1, 5, 10, 35)
    ) +
    
    scale_x_date(limits = c(ymd("2025-07-10"), ymd("2025-10-01")),
                 name = "Date",
                 date_breaks = "1 month", date_labels = "%b") +
    
    # Reorder legend
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 1),
      #linetype = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    )+
    
    theme(legend.position = "right",
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
          strip.placement = "outside",
          strip.text = element_text(
            family = "sans",
            size = 12,
            color = "black",
            hjust = 0,   # left
            vjust = 0  # vertical centering
            
          )
          
    ) 
  
  
  ggsave("_plot_outputs/mms_plot.png", ggplot, width = 9, height = 6, dpi = 900)
  
  return(ggplot)
}



#================================ Figure 1 ================================

#' [Test code]
# space = tar_read(all_plots); time = tar_read(plot_mmdt)

build_fig1 = function(space, time){
  
  divider <- ggdraw() + 
    draw_line(x = c(0.05, 0.95), y = c(0.5, 0.5), color = "black", linewidth = 0.5)
  
  fig1 = plot_grid(space, divider, time,
            rel_heights = c(1, 0.1, 1),
            ncol = 1,
            labels = c("A)", "", "B)"),
            label_size = 14)
  
  ggsave("_plot_outputs/figure_1.png", fig1, bg = "white", width = 9, height = 12)

  
  return(fig1)
  
}


#================================ Figure 2 ================================

#' [Test code]
# erosion_totals = tar_read(erosion_totals_ft)
# figure_2b = "_inputs/figure_2b.png"

build_fig2 = function(erosion_totals, figure_2b){
  
  panel_a <- ggdraw() + draw_image(erosion_totals)
  panel_b <- ggdraw() + draw_image(figure_2b)

  fig2 = plot_grid(panel_a, panel_b,
                   rel_heights = c(1,  1),
                   ncol = 1,
                   labels = c("A)", "B)"),
                   label_size = 12)
  
  ggsave("_plot_outputs/figure_2.png", fig2, bg = "white", width = 9, height = 7)
  
  
  return(fig2)
}












