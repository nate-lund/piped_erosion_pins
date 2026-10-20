#================================ Pull measurements ================================

pull_measurements = function(path, sheet){
  path = list.files(path, full.names = TRUE)
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

#================================ Assess Measurement Accuracy ================================

#' [Test code]
# data = tar_read(clean_data)

pin_accuracy = function(data){
  # Difference subsequent measurements
  data2 = data %>% 
    mutate(
      dmm_ch = abs(mm1_ch - mm2_ch)
    )
  
  # See histogram of differences
  hist(data2$dmm_ch, breaks = 30)
  
  # Compute mean and median
  mean(data2$dmm_ch)
  median(data2$dmm_ch)
  max(data2$dmm_ch)
  min(data2$dmm_ch)
  sd(data2$dmm_ch)
  
}


#================================ Difference Measurements ================================

#' [Test code]
# data = tar_read(clean_data)

difference_pins = function(data){
  
  # Average the duplicate measurements for each pin.
  data2 = data %>% mutate(
    mma = (mm1_ch + mm2_ch) / 2, # Change since last baseline
    mmb = (mm1_bl + mm2_bl) / 2, # Baseline for next measurement
  ) %>% 
    select(-mm1_ch, -mm2_ch, -mm1_bl, -mm2_bl)
  
  
  # Create a list where each item is a data frame with all the measurements of each pin 
  nindex <- length(unique(data2$index)) # Number of unique pins
  
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
      
      # Calcualte cumulative dmm and dt for each pin
      mutate(mm = cumsum(dmm),
             t = cumsum(dt))
    
    
    
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
#================================ Model Approach 1 ================================

#' [Test code]
# data = tar_read(differenced_pins)

fit_models_a1 = function(data){
  
  # Setup
  # Create a unique array ID for each array within a forest
  data2 = data %>% 
    mutate(array_ID = paste(transect, slope_pos, sep = "_"))
  # Create a list of forests
  forests = unique(data2$forest)
  
  
  # Fit models
  for(i in 1:length(forests)){
    
    # Filter for forest i
    datai = data2 %>% 
      filter(forest == forests[i])
    
    # Convert t from days to months (helps with model stability, provides more interpret-able estimates)
    datai$t <- datai$t / 30.44
    
    # Fit a mixed effects model to determine BS and FS rate of change and significance
    lmer_rates <- lmer(data = datai, REML = TRUE,
                       mm ~ 0 + slope_pos:t
                       + (0 + t | index)
                       + (0 + t | array_ID)
    )
    
    
    # Fit a mixed effects model to determine BS and FS difference
    lmer_diff <- lmer(data = datai, REML = TRUE,
                      mm ~ 0 + t + t:slope_pos
                      + (0 + t | index)
                      + (0 + t | array_ID)
    )
    
    
    # Fit mixed effects model with a spline
    knots = unique(datai$t)[2:(length(forests)-1)] # Set knots at measurement days
    lmer_spline <- lmer(data = datai, REML = TRUE,
                        mm ~ 0 + slope_pos:lspline(t, knots = knots)
                        + (0 + t | index)
                        + (0 + t | array_ID))
    
    # Because of the optimization error, we should double check to ensure it found
    # a correct fit. Likely a false alarm
    # af <- allFit(lmer_spline)
    # summary(af)$fixef # Period rates across optimizers
    # summary(af)$llik # Log-likelihoods across optimizers
    # isSingular(lmer_rates)
    # VarCorr(lmer_rates) # look for an SD at or near 0
    
    
    # Organize data
    
    #' [ANOVA]
    # Perform an anova to test if the rate of change did vary significantly
    # between time periods. Refits the models as ML vs REML
    anova_coefs = tidy(anova(lmer_rates, lmer_spline))
    
    # Saving anova coefs
    anova_coefs$forest = unique(datai$forest) # Add forest column
    
    ifelse(i == 1,
           anova_out <- anova_coefs,
           anova_out <- merge(anova_out, anova_coefs, all = TRUE)) # Merge data frames
    
    
    
    
    #' [lmer spline] 
    # Re-label tidy outpul
    dates = unique(datai$date) # Grab measurement dates
    spline_coefs = tidy(lmer_spline) %>% 
      # Search for slope position
      mutate(
        forest = unique(datai$forest),
        slope_pos = case_when(
          grepl("slope_posBS", term) ~ "BS",
          grepl("slope_posFS", term) ~ "FS",
          TRUE ~ NA
        ),
        # Relabel segmetns by dates
        period = case_when(
          grepl("knots)1", term) ~ paste(dates[1], dates[2], sep="_"),
          grepl("knots)2", term) ~ paste(dates[2], dates[3], sep="_"),
          grepl("knots)3", term) ~ paste(dates[3], dates[4], sep="_"),
          grepl("knots)4", term) ~ paste(dates[4], dates[5], sep="_"),
          grepl("knots)5", term) ~ paste(dates[5], dates[6], sep="_"),
          TRUE ~ NA
        )
      ) %>% 
      filter(effect == "fixed") %>% 
      select(forest, slope_pos, period, estimate, std.error, p.value)
    
    ifelse(i == 1,
           spline_out <- spline_coefs,
           spline_out <- merge(spline_out, spline_coefs, all = TRUE)) # Merge data frames
    
    
    #' [lmer rate & diff]
    # Saving datai
    datai$ls_predic <- predict(lmer_spline, re.form = NA) # Generate predictiions
    datai$lmer_predic <- predict(lmer_rates, re.form = NA)
    
    ifelse(i == 1,
           data_out <- datai,
           data_out <- merge(data_out, datai, all = TRUE)) # Merge data frames
    
    # Saving coefs
    # Pull coefs from lmer_rates
    coefs_rates = tidy(lmer_rates) %>%
      filter(term == "slope_posBS:t" | term == "slope_posFS:t") %>% 
      mutate(
        slope_pos = case_when(
          term == "slope_posBS:t" ~ "BS",
          term == "slope_posFS:t" ~ "FS"
        )
      ) %>% 
      select("slope_pos", "estimate", "std.error", "p.value") %>% 
      rename(forest_est = estimate,
             forest_err = std.error,
             forest_pval = p.value)
    
    
    # Pull p-value from from lmer_diff
    coefs_diff = tidy(lmer_diff) %>%
      filter(term == "t:slope_posBS") %>% 
      select("p.value") %>% 
      mutate(slope_pos = "BS") %>% 
      rename(bsfs_pval = p.value)
    
    # Generating BLUPS for array slope predication (B + b)
    blups = coef(lmer_rates)$array_ID %>% 
      tibble::rownames_to_column("array") %>% 
      mutate(
        slope_pos = sub(".*_", "", array),
        transect = sub("_.*", "", array),
        deviation = t,
        fixed = if_else(slope_pos == "BS", `slope_posBS:t`, `slope_posFS:t`),
        array_est = fixed + deviation,
        forest = unique(datai$forest),
        t = max(datai$t)
      ) %>% 
      select(forest, t, transect, slope_pos, array_est)
    
    
    # Combine coefficients
    coefs = full_join(blups, coefs_rates, by = "slope_pos") %>% 
      full_join(coefs_diff, by = "slope_pos")
    
    ifelse(i == 1,
           coefs_out <- coefs,
           coefs_out <- merge(coefs_out, coefs, all = TRUE)) # Merge data frames
    
    
    
  }
  
  
  # Write to _stats_output
  coefs_path = "_stats_outputs/approach1_coefs.xlsx"
  write_xlsx(coefs_out, coefs_path) # Overall coefficients
  
  data_path = "_stats_outputs/approach1_data.xlsx"
  write_xlsx(data_out, data_path) # Data
  
  anova_path = "_stats_outputs/anova_coefs.xlsx"
  write_xlsx(anova_out, anova_path) # Anova
  
  spline_path = "_stats_outputs/spline_coefs.xlsx"
  write_xlsx(spline_out, spline_path) # Spline fit
  
  return(list(coefs_path, data_path, anova_path, spline_path))
  
}



#================================ Model Approach 2 ================================

#' [Test code]
# data = tar_read(differenced_pins)

fit_models_a2 = function(data){
  
  # Convert t from days to months (helps with model stability, provides more interpret-able estimates)
  data2$t <- data$t / 30.44
  
  # Number of unique pins
  indexs <- unique(data2$index)

  # Fit an lm and save coefs for each pin
  for(i in indexs){
    
    # Filter for an invididual pin
    datai = data2 %>% 
      filter(index == i)
    
    # Fit lm to mm
    fit = lm(data = datai, mm ~ 0 + t) # replace with mm ~ t to unfix at zero
    
    # Add prediction and rate of change columns
    datai$pin_roc = as.numeric(tidy(fit)[1, "estimate"]) # in mm/day
    datai$pin_predic = predict(fit)
    
    # Rejoin data frames
    ifelse(i == first(indexs),
           data_out <- datai,
           data_out <- merge(data_out, datai, all = TRUE))
    
  }
  
  
  # Aggregate transect information into one df
  transects = data_out %>% 
    group_by(forest, transect, slope_pos) %>% 
    summarise(array_roc = mean(pin_roc)) %>% 
    ungroup()
  
  # Perform 3 t-tests
  # Create a list of forests for the analysis
  forests = unique(transects$forest)
  # Create a df to hold coefs
  coefs = data.frame(forest = forests,
                     BS_estimate = NA,
                     BS_pvalue = NA,
                     FS_estimate = NA,
                     FS_pvalue = NA,
                     BF_estimate = NA,
                     BF_pvalue = NA)
  
  # For loop to apply tests 
  for(i in forests){
    
    # Split BS and FS data and select one forest
    bs = transects %>% filter(forest == i &
                                slope_pos == "BS") 
    fs = transects %>% filter(forest == i &
                                slope_pos == "FS") 
    
    # Perform tests
    bstest = t.test(bs$array_roc, alternative = c("two.sided"))
    fstest = t.test(fs$array_roc, alternative = c("two.sided"))
    bftest = t.test(bs$array_roc, fs$array_roc, alternative = c("two.sided"), paired = TRUE)
    
    # Bind together
    coefs = bind_rows(.id = "slope_pos",
      BS = tidy(bstest),
      FS = tidy(fstest),
      BF = tidy(bftest)
      ) %>% 
      mutate(
        forest = unique(fs$forest)
      ) %>% 
      select(forest, slope_pos, estimate, statistic, p.value)
    
    ifelse(i == first(forests),
           coefs_out <- coefs,
           coefs_out <- merge(coefs_out, coefs, all = TRUE))
    
  }
  
  # Pivot
  coefs_out
  
  # Export to _stats_outputs
  coefs_path = "_stats_outputs/approach2_coefs.xlsx"
  write_xlsx(coefs_out, path) # Coefs
  
  return(coefs_path)
}


#' #================================ Fit lsplines to MMs ================================
#' 
#' #' [Test code]
#' # data4 = tar_read(differenced_pins)
#' 
#' fit_lspines = function(data4){
#'   
#'   # Define number of forests in dataset
#'   forests = unique(data4$forest)
#'   nforests = length(forests) 
#'   
#'   # Split. Create a list where each item is a data frame with all the measurements
#'   # from one forest and one hillslope position.
#'   forest_list <- vector(mode = "list", length = nforests * 2) # Create empty list
#'   
#'   for(i in 1:nforests) { # For loop to split by forest for BS
#'     forest_list[[i]] <- data4 %>%
#'       filter(forest == forests[i] & slope_pos == "BS")
#'   }
#'   
#'   for(i in 1:nforests) { # For loop to split by forest for FS
#'     forest_list[[i + nforests]] <- data4 %>%
#'       filter(forest == forests[i] & slope_pos == "FS")
#'   }
#'   
#'   
#'   # For loop to fit lspline with knots at measurements dates
#'   for (i in 1:(nforests * 2)){
#'     # Define knot dates, do not need knots at beginning or end.
#'     knots <- unique(forest_list[[i]]$date)[2:(length(unique(forest_list[[i]]$dayof))-1)] 
#'     
#'     # Fit lspline
#'     lspline = lm(data = forest_list[[i]], mm ~ lspline(date, knots = knots))
#'     
#'     # Create predictions for plotting later
#'     forest_list[[i]]$predic <- predict(lspline)
#'     
#'     ifelse(i == 1,
#'            ls_data <- forest_list[[i]],
#'            ls_data <- merge(ls_data, forest_list[[i]], all = TRUE)) # merge data frames
#'     
#'     
#'     # Save model coefficients
#'     coefs <- tidy(lspline) # pulls estimate, std.error, statistic, p.value
#'     error <- data.frame(confint(lspline)) %>%   # grab margin of errors
#'       mutate(m_error = (X97.5.. - X2.5..) / 2)
#'     coefs$m_error = error$m_error # combine estimates with margin
#'     
#'     coefs$forest = unique(forest_list[[i]]$forest) # Add a column denoting forest
#'     coefs$slope_pos = unique(forest_list[[i]]$slope_pos) # Add a column denoting slope pos
#'     
#'     # Add a column for dates, here the date refers to the erosion from the previous
#'     # measurement period
#'     coefs$date = unique(forest_list[[i]]$date)
#'     
#'     ifelse(i == 1,
#'            all_coefs <- coefs,
#'            all_coefs <- merge(all_coefs, coefs, all = TRUE)) # Merge data frames
#'     
#'     
#'   } # End loop
#'   
#'   
#'   # Drop intercept coefs from all_coefs
#'   all_coefs = all_coefs %>% 
#'     filter(term != "(Intercept)")
#'   
#'   # Join measuremenet data (w/ lspline predictions) with lslope coefs data frame
#'   fit_data = left_join(ls_data, all_coefs, by = c("forest", "date", "slope_pos"))
#'   
#'   # Write to _stats_output
#'   path = "_stats_outputs/fit_data.xlsx"
#'   write_xlsx(fit_data, path)
#'   
#'   
#'   return(path)
#' }
#' 
#' #================================ Elevation by Transect ================================
#' # This is computing the total elevation change for each forest - hillslope_pos
#' # Using in plotting
#' 
#' #' [Test code]
#' # path = tar_read(lsplines)
#' 
#' overall_transect_stats = function(path){
#' 
#'   mms_overall_change = read_excel(path) %>%
#' 
#'   # Fit linear models to get mean change of each 'array' (each forest, slope_pos,
#'   # transect combo) with summary statistics.
#'     group_by(forest, slope_pos, transect) %>%
#' 
#'     summarise(
#'       estimate = as.numeric(tidy(lm(mm ~ dayof))[2, "estimate"] * 30.4), # in mm/month
#'       std_error = as.numeric(tidy(lm(mm ~ dayof))[2, "std.error"] * 30.4), # in mm/month
#'       p_value = as.numeric(tidy(lm(mm ~ dayof))[2, "p.value"] * 30.4),
#'       .groups = "drop"
#'     ) %>%
#'     ungroup()
#' 
#'   return(mms_overall_change)
#' }
#' 

#' #================================ Elevation ROC by Slope Pos ================================
#' # This is computing the rate of elevation change for each forest - hillslope_pos 
#' 
#' #' [Test code]
#' # path = "_stats_outputs/fit_data.xlsx"
#' 
#' roc_slopepos_stats = function(path){
#'   
#'   # Pull data from /_stats_outputs
#'   mms_overall_change = read_excel(path)
#'   
#'   
#'   # Create a list of forests in the dataset
#'   forest_list = unique(mms_overall_change$forest)
#'   
#'   # Create a df to hold coefs
#'   nforest = rep(NA, times = length(forest_list))
#'   mms_slope_pos = data.frame(forest = nforest,
#'                              #r_squared = nforest,
#'                              estimate_bs = nforest,
#'                              std_error_bs = nforest,
#'                              p_value_bs = nforest,
#'                              estimate_fs = nforest,
#'                              std_error_fs = nforest,
#'                              p_value_fs = nforest # Not the sig of FS, but diff
#'                              ) 
#'   
#' 
#'   # For loop to fit a lm for each forest: estimate ~ date * slope_pos - 1
#'   # Fit a linear model testing BS significance and whether FS is
#'   # significantly different than BS.
#'   for(i in 1:length(forest_list)){
#'     # Filter overall dataset for only forest i data
#'     data = mms_overall_change %>% 
#'       filter(forest == forest_list[i])
#'     
#'     # Fit model using effects coding to produce a BS estimate representing
#'     # the mean change from baseline to final measurement and an FS estimate 
#'     # representing the change from the BS, also reflected in p values.
#'     lm = lm(data = data, mm ~ dayof * slope_pos) # effects coding for model, add "- dayof"
#'     
#'     summary(lm)$adj.r.squared
#'     
#'     # Build data frame using model outputs
#'     mms_slope_pos[i,"forest"] = forest_list[i] # Forest column
#'     
#'     # Overall R squared
#'     #mms_slope_pos[i,"r_squared"] = summary(lm)$adj.r.squared
#'     
#'     # Backslope coefs
#'     mms_slope_pos[i, "estimate_bs"] = tidy(lm)[2, "estimate"] * 30.4 # mm/month
#'     mms_slope_pos[i, "std_error_bs"] = tidy(lm)[2, "std.error"] * 30.4 # mm/month
#'     mms_slope_pos[i, "p_value_bs"] = tidy(lm)[2, "p.value"] 
#'     
#'     # Footslope coefs
#'     mms_slope_pos[i, "estimate_fs"] = tidy(lm)[2, "estimate"] + tidy(lm)[4, "estimate"] * 30.4 # mm/month
#'     mms_slope_pos[i, "std_error_fs"] = tidy(lm)[4, "std.error"] * 30.4 # mm/month
#'     mms_slope_pos[i, "p_value_fs"] = tidy(lm)[4, "p.value"]
#'     
#'     # Supporting columns
#'      mms_slope_pos[i, "dt"] = as.numeric(max(data$date) - min(data$date), units = "days")
#' 
#'   }
#'   
#'   # Pivot table
#'   mms_slope_pos = mms_slope_pos %>%
#'     pivot_longer(
#'       cols = matches("_(bs|fs)$"),
#'       names_to = c(".value", "type"),
#'       names_pattern = "(.*)_(bs|fs)$"
#'     )
#'   
#'   return(mms_slope_pos)
#' }
#' 
#' 

#================================ Erosion Totals (Units) ================================

#' [Test Code]
# path = tar_read(a1_model_outs)[[1]]; bd = tar_read(baumann_bd)

convert_units = function(path, bd){
  
  # Pull measurement averages, "agreagate" by forest/slope_pos
  data = read_excel(path) %>% 
    filter(transect == "central") %>% 
    select(-transect, -array_est)
  
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
  erosion_df = data %>%
    
    # Create a mm column from estimate
    mutate(
      estimate_mm = forest_est * t,
      std_error_mm = forest_err * t,
    
    # Add worms columns
      worms = case_when(
        forest %in% c("ASH", "LRE", "LRW") ~ "EW",
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


