# Load necessary libraries
library(ncdf4)
library(raster)
library(sf)
library(dplyr)

# Paths to CHIRPS data and shapefile
chirps_path <- '/home/laye/Documents/GUIGMA/CHIRPS/chirps_v2.0_1981-2022_senegal_p05_merged.nc'
shapefile_path <- '/home/laye/Documents/GUIGMA/CHIRPS/gadm36_SEN_1.shp'

# Load the CHIRPS dataset and shapefile
chirps_data <- stack(chirps_path)
regions <- st_read(shapefile_path)

# List of years to extract data for (1981-2022)
years <- 1981:2022
results <- list()

# Function to handle extreme values and calculate mean
safe_mean <- function(data) {
  data <- ifelse(data < 0 | is.infinite(data), NA, data)  # Remove invalid values
  data <- pmin(data, 5000)  # Clip values at a reasonable threshold
  mean(data, na.rm = TRUE)  # Calculate the mean, ignoring NA values
}

# Check the names of the layers in the CHIRPS dataset
layer_names <- names(chirps_data)
print(layer_names)  # Print layer names to understand their structure

# Iterate over each region
for (region_name in unique(regions$NAME_1)) {
  region_shape <- regions %>% filter(NAME_1 == region_name)
  
  # Mask CHIRPS data using the geographic bounds of the region
  bounds <- st_bbox(region_shape)
  chirps_region <- crop(chirps_data, extent(bounds))
  
  annual_precipitations <- numeric()
  
  for (year in years) {
    # Construct layer name based on the year
    year_pattern <- as.character(year)
    
    # Find layers that match the year pattern in their names
    matching_layers <- which(grepl(year_pattern, names(chirps_region)))
    
    if (length(matching_layers) > 0) {
      # Extract data for the specified year
      chirps_region_year <- chirps_region[[matching_layers]]
      
      # Calculate total annual precipitation by summing daily precipitation
      total_annual_precipitation <- cellStats(chirps_region_year, stat = 'sum', na.rm = TRUE)
      
      # Calculate the mean precipitation over the region
      total_annual_precipitation_mean <- mean(total_annual_precipitation, na.rm = TRUE)
      
      # Add to the list of annual precipitation for the region
      annual_precipitations <- c(annual_precipitations, total_annual_precipitation_mean)
    }
  }
  
  # Calculate the average annual precipitation for the region over the period
  average_precipitation <- safe_mean(annual_precipitations)
  
  # Add the results for this region
  results[[region_name]] <- average_precipitation
}

# Convert results to DataFrame
hazard_df <- data.frame(
  Region = names(results),
  Average_Annual_Precipitation_mm = unlist(results)
)


# Save results to CSV file
output_path <- '/home/laye/Documents/GUIGMA/dry_spell/R_average_annual_precipitation.csv'
write.csv(hazard_df, output_path, row.names = FALSE)

# Display the results
print(hazard_df)
