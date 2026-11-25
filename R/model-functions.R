#================================ Sadler Models ================================

#' [Test code]
# data = tar_read(differenced_pins)

sadler_plots = function(data){
  
  # Create measurement periods for each pin
  indexs = unique(data$index)
  
  for(i in indexs){
    
    di = data %>% 
      filter(index == i)
    
    
    pairs <- combn(nrow(di), 2)
    
    periodsi = data.frame(
      start = di$t[pairs[1, ]],
      end   = di$t[pairs[2, ]],
      zstart = di$mm[pairs[1, ]],
      zend   = di$mm[pairs[2, ]]
    ) %>% 
      mutate(
        # Add some comupations
        dz = zstart - zend,
        dt = end - start, 
        dzdt = dz/dt,
        
        # Add some field characteristics
        index = i,
        forest = unique(di$forest),
        slope_pos = unique(di$slope_pos),
        transect = unique(di$transect)
        
      )
    
    ifelse(i == first(indexs),
           periods <- periodsi,
           periods <- merge(periods, periodsi, all = TRUE))
    
  }
  
  
  
  
  
  # Analysis for one forest
  
  forests = unique(periods$forest)
  
  for(k in forests){
    
    # Filter for forest i
    df = periods %>% 
      filter(forest == k) %>% 
      mutate(
        # Log transform dt and dzdt
        log_dt = log(dt),
        log_dzdt = ifelse(dzdt != 0, log(abs(dzdt)), log(0.001))
      )
    
    name = k
    
    # Mean (regular lm)
    lm_log = lm(data = df, log_dzdt ~ log_dt)
    log_coef = tidy(lm_log)$estimate
    
    # Median
    med_log <- rq(data = df, log_dzdt ~ log_dt, tau = 0.5)
    log_med_coef = coef(med_log)
    
    
    # Plot
    plotk = ggplot(data = df, mapping = aes(x = log_dt, y = log_dzdt)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + # Mean
      geom_quantile(quantiles = 0.5, formula = y ~ x, color = "firebrick") + # Median
      
      geom_hline(yintercept = log(abs(eros_rate))) +
      geom_abline(slope = -1, intercept = log(abs(eros_rate)), linetype = 2) +
      geom_abline(slope = -0.5, intercept = log(abs(eros_rate)), linetype = 3) +
      
      scale_y_continuous(name = "log rate of elevation change") +
      scale_x_continuous(name = "log measurement period") +
      labs(
        title = name,
        subtitle = paste0("Mean - Intercept: ", round(log_coef[[1]], 3), " | Slope: ", round(log_coef[[2]], 4),
                          "\nMedian - Intercept: ", round(log_med_coef[[1]], 3), " | Slope: ", round(log_med_coef[[2]], 4))
      )
    
    assign(paste0(name, "_logplot"), plotk)
    
  }
  
  plot_grid(ASH_logplot, LRE_logplot, LRW_logplot, MAG_logplot, WD_logplot, LRJ_logplot, cols = 3)
  
  
}
