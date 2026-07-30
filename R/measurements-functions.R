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

##================================ ROC by Transect ================================
# This is computing the total elevation change for each forest - hillslope_pos

#' [Test code]
# path = "_stats_outputs/fit_data.xlsx"

overall_transect_stats = function(path){
  
  mms_overall_change = read_excel(path) %>% 
  
  # Fit linear models to get mean change of each 'array' (each forest, slope_pos, 
  # transect combo) with summary statistics.
    group_by(forest, slope_pos, transect) %>% 
    
    summarise( 
      estimate = as.numeric(tidy(lm(mm ~ dayof))[2, "estimate"] * 30.4), # in mm/month
      std_error = as.numeric(tidy(lm(mm ~ dayof))[2, "std.error"] * 30.4), # in mm/month
      p_value = as.numeric(tidy(lm(mm ~ dayof))[2, "p.value"] * 30.4),
      .groups = "drop"
    ) %>% 
    ungroup()
  
  return(mms_overall_change)
}


#' ##================================ ROC by Transect ================================
#' # This is computing the rate of elevation change for each forest - hillslope_pos -
#' # transect combination using only the first and last measurements.
#' 
#' #' [Test code]
#' # path = "_stats_outputs/fit_data.xlsx"
#' 
#' roc_transect_stats = function(path){
#'   
#'   measurements = read_excel(path)
#'   
#'   
#'   # Simplify measurement data to have have only the overall elevation change at 
#'   # each pin plus the second measurement
#'   mms_overall_change = measurements %>% 
#'     group_by(forest, slope_pos) %>% 
#'     filter(date == max(date) |
#'              date == min(date)) %>%  # Last and first measurement 
#'              #date == sort(unique(date))[2]) %>%  # Last and second measurement
#'     ungroup()
#'   
#'   # Create a list of forests in the data set
#'   forest_list = unique(mms_overall_change$forest)
#'   
#'   # Run for loop to fit a lm for each forest that fits a linear model testing 
#'   # BS significance and whether FS is
#'   # significantly different than BS.
#'   
#'   # First loop filters by forest
#'   for(i in 1:length(forest_list)){
#'     # Filter overall dataset for only forest i data
#'     data = mms_overall_change %>% 
#'       filter(forest == forest_list[i])
#'     
#'     # Fit model using effects coding to produce a BS estimate representing
#'     # the mean change from baseline to final measurement and an FS estimate 
#'     # representing the change from the BS, also reflected in p values.
#'     lm = lm(data = data, mm ~ date * slope_pos * transect - date) # effects coding for model, add "- dayof"
#'     
#'     # Create a df from the tibble model summary ouput
#'     df = tidy(lm) %>% 
#'       # Rename key columns
#'       rename(
#'         std_error = std.error,
#'         p_value = p.value
#'       ) %>% 
#'       # Pull from the term our wanted columns 
#'       mutate(
#'         forest = forest_list[i],
#'         slope_pos = case_when(
#'           grepl("BS", term) ~ "BS",
#'           grepl("FS", term) ~ "FS",
#'           TRUE ~ "BS"
#'         ),
#'         transect = case_when(
#'           grepl("north", term) ~ "north",
#'           grepl("south", term) ~ "south",
#'           grepl("east", term) ~ "east",
#'           grepl("west", term) ~ "west",
#'           TRUE ~ "central"
#'         )
#'       ) %>% 
#'       # Keep only slopes, not intercepts
#'       filter(grepl("date", term)) %>% 
#'       # Reorder columns
#'       select(forest, transect, slope_pos, estimate, std_error, p_value)
#'       
#'     mms_transect <- if (i == 1) df else bind_rows(mms_transect, df)
#'     
#'   }
#'   
#'   mms_transect %>% mutate(a = estimate * 90)
#'   
#'   
#'   return(mms_transect)
#' }

##================================ ROC by Slope Pos ================================
# This is computing the rate of elevation change for each forest - hillslope_pos 

#' [Test code]
# path = "_stats_outputs/fit_data.xlsx"

roc_slopepos_stats = function(path){
  
  # Pull data from /_stats_outputs
  mms_overall_change = read_excel(path)
  
  
  # Create a list of forests in the dataset
  forest_list = unique(mms_overall_change$forest)
  
  # Create a df to hold coefs
  nforest = rep(NA, times = length(forest_list))
  mms_slope_pos = data.frame(forest = nforest,
                             estimate_bs = nforest,
                             std_error_bs = nforest,
                             p_value_bs = nforest,
                             estimate_fs = nforest,
                             std_error_fs = nforest,
                             p_value_fs = nforest) # Not the sig of FS, but diff
  

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
    lm = lm(data = data, mm ~ dayof * slope_pos) # effects coding for model, add "- dayof"
    
    # Build data frame using model outputs
    mms_slope_pos[i,"forest"] = forest_list[i] # Forest column
    
    # Backslope coefs
    mms_slope_pos[i, "estimate_bs"] = tidy(lm)[2, "estimate"] * 30.4 # mm/month
    mms_slope_pos[i, "std_error_bs"] = tidy(lm)[2, "std.error"] * 30.4 # mm/month
    mms_slope_pos[i, "p_value_bs"] = tidy(lm)[2, "p.value"] 
    
    # Footslope coefs
    mms_slope_pos[i, "estimate_fs"] = tidy(lm)[2, "estimate"] + tidy(lm)[4, "estimate"] * 30.4 # mm/month
    mms_slope_pos[i, "std_error_fs"] = tidy(lm)[4, "std.error"] * 30.4 # mm/month
    mms_slope_pos[i, "p_value_fs"] = tidy(lm)[4, "p.value"]
    
    # Supporting columns
     mms_slope_pos[i, "dt"] = as.numeric(max(data$date) - min(data$date), units = "days")

  }
  
  # Pivot table
  mms_slope_pos = mms_slope_pos %>%
    pivot_longer(
      cols = matches("_(bs|fs)$"),
      names_to = c(".value", "type"),
      names_pattern = "(.*)_(bs|fs)$"
    )
  
  return(mms_slope_pos)
}



#================================ Erosion Totals (Units) ================================

#' [Test Code]
# slope_pos_roc = tar_read(slope_pos_roc); bd = tar_read(baumann_bd)

convert_units = function(slope_pos_roc, bd){
  
  # Compute a mean of the bd for the top 5cm, relevant to erosion, for each
  # earthworm species
  baumann_df <- bd %>% 
    filter(Depth_cm == "0-5") %>% # Only top 5cm  
    
    # Standardize worms
    rename(worms = Worm_Type) %>%
    mutate(
      worms = case_when(
        worms == "L" ~ "EW",
        worms == "A" ~ "JW"
      )
    ) %>% 
    
    # Summarize
    group_by(worms) %>% 
    summarise(bd_mean = mean(`Bulk_Density (g/cm^3)`),
              bd_se = sd(`Bulk_Density (g/cm^3)`) / length(`Bulk_Density (g/cm^3)`)) %>% 
    ungroup()
    
  
  # Clean up lm coefficients
  erosion_df = slope_pos_roc %>%
    
    # Create a mm column from estimate
    mutate(
      estimate_mm = estimate / 30.4 * dt,
      std_error_mm = std_error / 30.4 * dt,
    
    # Add worms columns
      worms = case_when(
        forest %in% c("ASH", "LRE", "LRW", "AREF", "LREF") ~ "EW",
        forest %in% c("MAG", "WD", "LRJ") ~ "JW"
      )
    ) 
  
      
  # Join dataframes
  erosion_bd = left_join(
    erosion_df,
    baumann_df,
    by = join_by(worms)
  )  
  
  # Define a function to propogate SE
  propogate_error = function(x, dx, y, dy, z){
   
    dz = sqrt((dx / x)^2 + (dy / y)^2) * abs(z)
    
    return(dz)
  }
  
  # Compute erosion rates, propagate standard error (assuming no covariance) 
  erosion_totals = erosion_bd %>% 
  
    mutate(
      # Compute erosion in metric tons/km2
      tons_km2 = estimate_mm / 10 # Convert to cm
      * bd_mean # Compute in g/cm2,
      * 10000, # Convert to t/km2
      # Compute SE
      tons_km2_se = propogate_error(estimate_mm, std_error_mm, bd_mean, bd_se, tons_km2),
    
      
      # Compute erosion in imperial ton/acre
      tons_acre = tons_km2 / 2.47105 * 0.984207 / 100,
      
        # Compute SE
      tons_acre_se = tons_km2_se / 2.47105 * 0.984207 / 100,
      
           )
  
  # Write to _stats_output
  path = "_stats_outputs/erosion_totals.xlsx"
  write_xlsx(erosion_totals, path)
   
  return(path)
  
}

#================================ Building Tables ================================

##================================ Create DF for mms/dt ================================

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



##================================ Make Flextable for mms/dt ================================

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


##================================ Make Flextable for Erosion Totals ================================

#' [Test Code]
# path = tar_read(erosion_totals_units)

table_erosion_totals = function(path){
  
  data = read_excel("_stats_outputs/erosion_totals.xlsx")

    # Do some cleanup
  df = data %>% 
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
  box_cols <- c("estimate_mm", "tons_km2", "tons_acre")
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
                              "mm/yr**",
                              "tonnes/km2/yr",
                              "tons/acre/yr"),
                   colwidths = c(4, # adds up to total number of cols
                                 2,
                                 3,
                                 4,
                                 3,
                                 2)) %>% 
    
    # * all significant values, add units 
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
        as_chunk(formatC(tons_km2, format = "f", digits = 0)),
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
                 "dt" = "Total days",
                 "estimate_mm" = "Estimate",
                 "std_error_mm" = "SE",
                 "tons_km2" = "Estimate",
                 "tons_km2_se" = "SE",
                 "tons_acre" = "Estimate",
                 "tons_acre_se" = "SE"
      ))
  
  erosion_units_ft
  
  path = "_plot_outputs/erosion_totals.svg"
  save_as_image(erosion_units_ft, path = path)
  
  path2 = "_plot_outputs/erosion_totals.png"
  save_as_image(erosion_units_ft, path = path2)

  
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
           signif = ifelse(p.value <= 0.05, "Y", "N")
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
    
    
    # Plot a lm over the whole period
    geom_smooth(
      aes(group = slope_pos,
          linetype = slope_pos),
      method = "lm",
      se = FALSE,
      fullrange = TRUE,
      linewidth = 1.2,
      color = "grey20") +
    
    
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

