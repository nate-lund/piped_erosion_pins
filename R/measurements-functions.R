#================================ Pull measurements ================================

pull_measurements = function(path, sheet){
  xls = read_excel(path, sheet = sheet)
  return(xls)
}


#================================ Measurement cleanup ================================

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


#================================ Difference Measurements ================================

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



#================================ QAQC Measurements ================================

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



#================================ Fit lsplines to MMs ================================

#' [Test code]
# data4 = difference_pins(data_cleanup(pull_measurements("C:/Users/natha/Box/_data/_erosion_pins/ARB-LR_raw.xlsx", "2025")))


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
  
  
  # Drop intercept coefs from all_coefs
  all_coefs = all_coefs %>% 
    filter(term != "(Intercept)")
  
  # Join measuremenet data (w/ lspline predictions) with lslope coefs data frame
  fit_data = left_join(ls_data, all_coefs, by = c("forest", "date", "slope_pos"))
  
  # Write to _stats_output
  path = "_stats_outputs/fit_data.xlsx"
  write_xlsx(fit_data, path)
  
  
  return(path)
}

#================================ Summary Stats ================================

##================================ Cumulative by Transect ================================

#' [Test code]
# path = "_stats_outputs/fit_data.xlsx"

overall_transect_stats = function(path){
  
  measurements = read_excel(path)
  
  # Simplify measurement data to have have only the overall elevation change at 
  # each pin plus the second or first measurement
  mms_overall_change = measurements %>% 
    group_by(forest, slope_pos) %>% 
    filter(date == max(date) |
             date == min(date)) %>%  # Last and first measurement 
             #date == sort(unique(date))[2]) %>%  # Last and second measurement
    ungroup() %>% 
  
    # Fit linear models to get mean change of each 'array' (each forest, slope_pos, 
    # transect combo) with summary statistics.
    group_by(forest, slope_pos, transect) %>% 
    summarise(
      estimate = as.numeric(tidy(lm(mm - 0 ~ 1))["estimate"]),
      std_error = as.numeric(tidy(lm(mm - 0 ~ 1))["std.error"]),
      p_value = as.numeric(tidy(lm(mm - 0 ~ 1))["p.value"]),
      .groups = "drop"
    ) %>% 
    ungroup()
  
  return(mms_overall_change)
}


##================================ ROC by Transect ================================

#' [Test code]
# path = "_stats_outputs/fit_data.xlsx"

roc_transect_stats = function(path){
  
  measurements = read_excel(path)
  
  
  # Simplify measurement data to have have only the overall elevation change at 
  # each pin plus the second measurement
  mms_overall_change = measurements %>% 
    group_by(forest, slope_pos) %>% 
    filter(date == max(date) |
             date == min(date)) %>%  # Last and first measurement 
             #date == sort(unique(date))[2]) %>%  # Last and second measurement
    ungroup()
  
  # Create a list of forests in the data set
  forest_list = unique(mms_overall_change$forest)
  
  # Run for loop to fit a lm for each forest that fits a linear model testing 
  # BS significance and whether FS is
  # significantly different than BS.
  
  # First loop filters by forest
  for(i in 1:length(forest_list)){
    # Filter overall dataset for only forest i data
    data = mms_overall_change %>% 
      filter(forest == forest_list[i])
    
    # Fit model using effects coding to produce a BS estimate representing
    # the mean change from baseline to final measurement and an FS estimate 
    # representing the change from the BS, also reflected in p values.
    lm = lm(data = data, mm ~ date * slope_pos * transect - date) # effects coding for model, add "- dayof"
    
    # Create a df from the tibble model summary ouput
    df = tidy(lm) %>% 
      # Rename key columns
      rename(
        std_error = std.error,
        p_value = p.value
      ) %>% 
      # Pull from the term our wanted columns 
      mutate(
        forest = forest_list[i],
        slope_pos = case_when(
          grepl("BS", term) ~ "BS",
          grepl("FS", term) ~ "FS",
          TRUE ~ "BS"
        ),
        transect = case_when(
          grepl("north", term) ~ "north",
          grepl("south", term) ~ "south",
          grepl("east", term) ~ "east",
          grepl("west", term) ~ "west",
          TRUE ~ "central"
        )
      ) %>% 
      # Keep only slopes, not intercepts
      filter(grepl("date", term)) %>% 
      # Reorder columns
      select(forest, transect, slope_pos, estimate, std_error, p_value)
      
    mms_transect <- if (i == 1) df else bind_rows(mms_transect, df)
    
  }
  
  mms_transect %>% mutate(a = estimate * 90)
  
  
  return(mms_transect)
}

##================================ ROC by Slope Pos ================================

#' [Test code]
# path = "_stats_outputs/fit_data.xlsx"

roc_slopepos_stats = function(path){
  
  # Pull data from /_stats_outputs
  measurements = read_excel(path)
  
  # Simplify measurement data to have have only the overall elevation change at 
  # each pin plus the second measurement
  mms_overall_change = measurements %>% 
    group_by(forest, slope_pos) %>% 
    filter(date == max(date) |
             date == min(date)) %>%  # Last and first measurement 
             #date == sort(unique(date))[2]) %>%  # Last and second measurement
    ungroup()
  
  
  # Create a list of forests in the dataset
  forest_list = unique(mms_overall_change$forest)
  
  # Create a df to hold coefs
  nforest = rep(NA, times = length(forest_list))
  mms_slope_pos = data.frame(forest = nforest,
                             estiamte_bs = nforest,
                             std_error_bs = nforest,
                             p_value_bs = nforest,
                             estiamte_fs = nforest,
                             std_error_fs = nforest,
                             p_value_int = nforest) # Not the sig of FS, but diff
  

  # For loop to fit a lm for each forest: estimate ~ date * slope_pos - 1
  # Fit a linear model testing BS significance and whether FS is
  # significantly different than BS.
  for(i in 1:length(forest_list)){
    # Filter overall dataset for only forest i data
    data = mms_overall_change %>% 
      filter(forest == forest_list[i])
    
    # Fit model using effects coding to produce a BS estimate representing
    # the mean change from baseline to final measurement and an FS estimate 
    # representing the change from the BS, also reflected in p values.
    lm = lm(data = data, mm ~ date * slope_pos) # effects coding for model, add "- dayof"
    
    # Build data frame using model outputs
    mms_slope_pos[i,"forest"] = forest_list[i] # Forest column
    
    # Backslope coefs
    mms_slope_pos[i, "estiamte_bs"] = tidy(lm)[2, "estimate"]
    mms_slope_pos[i, "stderror_bs"] = tidy(lm)[2, "std.error"]
    mms_slope_pos[i, "pvalue_bs"] = tidy(lm)[2, "p.value"]
    
    # Footslope coefs
    mms_slope_pos[i, "estiamte_fs"] = tidy(lm)[2, "estimate"] + tidy(lm)[4, "estimate"]
    mms_slope_pos[i, "stderror_fs"] = tidy(lm)[4, "std.error"]
    mms_slope_pos[i, "int_pvalue_fs"] = tidy(lm)[4, "p.value"]
    

  }
  
  return(mms_slope_pos)
}



#================================ Converting Units ================================

convert_units = function(data){
  
  
}

#================================ Building Tables ================================

##================================ Create Datefarme ================================

#' [Test code]
# data = read_xlsx("_stats_outputs/fit_data.xlsx")

frame_mmdt = function(path){
  
  data = read_xlsx(path)
  
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



##================================ Make Flextable ================================

#' [Test code]
# frame = tar_read(mms_frame)

table_mmdt = function(frame){
  
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
    color(~ estimate_slope.1 < 0, color = "red2", j = "estimate_slope.1") %>% 
    color(~ estimate_slope.2 < 0, color = "red2", j = "estimate_slope.2") %>% 
    color(~ estimate_slope.3 < 0, color = "red2", j = "estimate_slope.3") %>% 
    color(~ estimate_slope.4 < 0, color = "red2", j = "estimate_slope.4") %>% 
    color(~ estimate_slope.5 < 0, color = "red2", j = "estimate_slope.5") %>% 
    
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


#================================ Plot dmm/dt ================================

#' [Test code]
# data <- read_excel("_stats_outputs/fit_data.xlsx")

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
           signif = ifelse(p.value <= 0.05, "Y", "N")
    )
  
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

