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

#================================ STATS testing ================================
# Fit a lm to each individual pin to address auto coorelation. 

#' [Test code]
# data = tar_read(differenced_pins)

fit_pin_lm = function(data){
  


#'===========================================================================
# Overall Statistical Approach 1
#'===========================================================================

# Split. Create a list where each item is a data frame with all the measurements of each pin 
nindex <- length(unique(data$index)) # Number of unique pins
pin_list <- vector(mode = "list", length = nindex) # Create empty list
for(i in 1:nindex) { # For loop to split apart the main dataframe
  pin_list[[i]] <- data %>% filter(index == i)
}

# Fit an lm and save coefs for each pin
for(i in 1:nindex){
  
  # Fit lm to mm
  fit = lm(data = pin_list[[i]], mm ~ t + 0) # replace with mm ~ t to unfix at zero
  
  # Add prediction and rate of change columns
  pin_list[[i]]$pin_roc = as.numeric(tidy(fit)[1, "estimate"]) # in mm/day
  pin_list[[i]]$pin_predic = predict(fit)
  
  # Rejoin data frames
  ifelse(i == 1,
         data_out <- pin_list[[i]],
         data_out <- merge(data_out, pin_list[[i]], all = TRUE))
  
}

# Aggregate transect information into one df
transects <- aggregate(data = data_out,
                             FUN = mean,
                             pin_roc ~ forest + transect + slope_pos)


# Perform 3 t-tests to answer the following questions:
# 1. Is the BS rate different from zero?
# 2. Is the FS rate different from zero?   
# 3. Do BS and FS differ?

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
for(i in 1:length(forests)){
  
  # Filter for first forest
  foresti = forests[i]
  
  # Split BS and FS data
  bs = transects %>% filter(forest == foresti &
                              slope_pos == "BS") 
  fs = transects %>% filter(forest == foresti &
                              slope_pos == "FS") 
  
  # Perform tests
  bstest = t.test(bs$pin_roc, alternative = c("two.sided"))
  fstest = t.test(fs$pin_roc, alternative = c("two.sided"))
  bftest = t.test(bs$pin_roc, fs$pin_roc, alternative = c("two.sided"), paired = TRUE)
  
  coefs[i,2:7] = c(tidy(bstest)[c("estimate", "p.value")],
                tidy(fstest)[c("estimate", "p.value")],
                tidy(bftest)[c("estimate", "p.value")])
  
  tidy(bstest)[c("estimate", "p.value")]
  
}

tibble(coefs)



#'===========================================================================
# Overall Statistical Approach 2
#'===========================================================================


data = tar_read(differenced_pins)

# Create a unique array ID for each array within a forest. Pins are clustered
# in these arrays.
data2 = data %>% 
  mutate(array_ID = as.numeric(factor((paste(transect, slope_pos, sep = "_")))))

# Create a list of forests
forests = unique(data2$forest)




# For loop to perform lmers
for(i in 1:length(forests)){
  
  # Filter for forest i
  datai = datai %>% 
    filter(forest == forests[i])
  
  # Fit a mixed effects model to determine BS and FS rate of change and significance
  lmer_rates <- lmer(data = datai, REML = FALSE,
                     mm ~ 0 + slope_pos:t
                     + (0 + t | index)
                     + (0 + t | array_ID)
  )
  # Generate predictions for plotting
  datai$lmer_predic <- predict(lmer_rates, re.form = NA)
  
  # Fit a mixed effects model to determine BS and FS difference
  lmer_diff <- lmer(data = datai, REML = FALSE,
                    mm ~ 0 + t + t:slope_pos
                    + (0 + t | index)
                    + (0 + t | array_ID)
  )
  
  
  # Fit mixed effects model with a spline
  # Note: this throws an error, but its probably okay. It does not quite converge
  # but this is probably due to the spline / relatively small slope compared to
  # the measurement error
  knots = unique(datai$t)[2:(length(forests)-1)] # Set knots at measurement days
  lmer_spline <- lmer(data = datai, REML = FALSE,
                      mm ~ 0 + slope_pos:lspline(t, knots = knots)
                      + (0 + t | index)
                      + (0 + t | array_ID))
  # Generate lspine predictions (for plotting)
  datai$ls_predic <- predict(lmer_spline, re.form = NA)
  
  
  confint(lmer_rates)#
  tidy(lmer_rates)[c(1,2), c("estimate", "p.value")] #also need confidence intervals???
  tidy(lmer_diff)["estimate", "p.value"]
  tidy(lmer_spline)["estimate", "p.value"]

  
  ggplot(data = datai) +
    geom_line(mapping = aes(x  = t, y= lmer_predic, group = slope_pos))
  
  # Generating Best Linear Unbiased Predictions (BLUPs) for b1i and b1k
  # print(tidy(lmer_rates, effects="ran_vals"), n = 50)
  
  # Generating BLUPS for overall slope predication (B + b)
  coef(fit_mixed)
  # Hmmm this is needed for the DEM plot, WIP
  
  
    
  }  











# Compare model outputs
modelsummary(list("Naive linear model" = fit.naive, 
                  "Linear model fit to averages" = fit.avg, 
                  "Mixed-effects model" = fit.mixed),
             gof_omit = ".*", estimate  = "{estimate} ({std.error})", statistic=NULL,  
             coef_omit = "SD")


}

#================================ Fit lsplines to MMs ================================

#' [Test code]
# data4 = tar_read(differenced_pins)

fit_lspines = function(data4){
  
  # Define number of forests in dataset
  forests = unique(data4$forest)
  nforests = length(forests) 
  
  # Split. Create a list where each item is a data frame with all the measurements
  # from one forest and one hillslope position.
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


#================================ Elevation by Transect ================================
# This is computing the total elevation change for each forest - hillslope_pos
# Using in plotting

#' [Test code]
# path = tar_read(lsplines)

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


#================================ Elevation ROC by Slope Pos ================================
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
                             #r_squared = nforest,
                             estimate_bs = nforest,
                             std_error_bs = nforest,
                             p_value_bs = nforest,
                             estimate_fs = nforest,
                             std_error_fs = nforest,
                             p_value_fs = nforest # Not the sig of FS, but diff
                             ) 
  

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
    
    summary(lm)$adj.r.squared
    
    # Build data frame using model outputs
    mms_slope_pos[i,"forest"] = forest_list[i] # Forest column
    
    # Overall R squared
    #mms_slope_pos[i,"r_squared"] = summary(lm)$adj.r.squared
    
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


