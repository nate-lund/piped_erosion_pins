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
    "ASH", 44.85988792640071, -93.62056309340343,
    "WD", 44.86146495117444, -93.62451266491821,
    "MAG", 44.858404737541555, -93.61694284484315,
    "LRJ", 45.058482385228636, -93.76520775495264,
    "LRW", 45.05234713447312, -93.74745452030763,
    "LRE", 45.05839520030076, -93.73859235869106
  )
  
  forests <- data.frame(forest = unique(file$Forest)) %>%
    left_join(coords, by = "forest")
  
  # Build a vector to collect output file paths
  out_paths <- character(nrow(forests))
  
  # For loop to pull DEMs using OpenTopo's API. This will take a few minutes.
  for (i in 1:nrow(forests)) {
    f <- forests$forest[i] # Pull forest name for naming later
    
    # Establish bounding box
    uleft <- forests[i, 2:3] # Pull top left corner
    delta_lat <- 200 / 111320 # Compute lat
    delta_long <- 200 / (111320 * cos(uleft[1] * pi / 180)) # Compute long, correcting for lat
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

#================================ Plot DEMs ================================

#' [Test code]
#points = get_points("C:/Users/natha/Box/_data/_spatial/_erosion-pins/ARB-LR_erosion-pin-arrays.csv")

#cdfs = "C:/Users/natha/Documents/_git-projects/piped_erosion_pins/_topo_outputs/topo-output_LRJ.nc"


# Define function to plot points and slope raster on same plot. This will be done via 
# branching for each cdf path but using all the points.
plot_all = function(points, cdfs) {
  
  # Extract slope from CDF
  stack = rast(cdfs) # Convert to raster stack
  rast = stack[["slope"]] # Pull slope raster
  
  if (crs(rast) == "") {
    crs(rast) <- "EPSG:32615"
  }
  
  
  # Convert points dataframe to a usable sf
  pts_sf = st_as_sf(points,
                     coords = c("esrignss_longitude", "esrignss_latitude"),
                     crs = 4326) %>% # Points are recorded in WGS lat long
    st_transform(32615) %>% # Transform to UTM Zone 15N
    st_crop(ext(rast), warn = FALSE) # Crop points by DEM extent
  
  # Grab forest name for plotting and saving
  forest_name = gsub("topo-output_|\\.nc", "", basename(cdfs))
  
  # Create plot
  ggplot = ggplot() +
    geom_spatraster(data = rast) +
    geom_sf(data = pts_sf, color = "red", size = 2) +
    scale_fill_viridis_c(na.value = "transparent") +
    ggtitle(forest_name) +
    theme_minimal()
  
  forest_name = gsub("topo-output_|\\.nc", "", basename(cdfs))
  out_path = paste0("_plot_outputs/plot_", forest_name, ".png")
  ggsave(out_path, ggplot, width = 8, height = 6)
  
  return(out_path)

}



