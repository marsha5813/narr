# Set up environment ------------------------------------------------------

# Libraries
library(tidyverse)
library(raster)
library(ncdf4)
library(tigris)
library(sf)
library(viridis)
library(furrr)

# Set up parallel backend
plan(multisession)

# Function to calculate wet bulb temp
# From https://journals.ametsoc.org/view/journals/apme/50/11/jamc-d-11-0143.1.xml
calc_wbt <-  function(temp, rh, metric = "c") {
  term1 <- atan(0.151977 * sqrt(rh + 8.313659))
  term2 <- atan(temp + rh)
  term3 <- atan(rh - 1.676331)
  term4 <- 0.00391838 * (rh^(3/2))
  term5 <- atan(0.023101 * rh)
  
  wbt <- temp * term1 + term2 - term3 + term4 * term5 - 4.686035
  
  # Optionally convert to F    
  if(metric == "f"){
    wbt <- (9/5 * wbt) + 32
  }
  
  return(round(wbt,1))
} 

# Test example from literature. Should be 13.7 C, 56.7 F.
calc_wbt(20,50)
calc_wbt(20,50, metric = "f")

# Download data -----------------------------------------------------------

# Download gridded air temp data from NCEP NARR
raw.temp <- tempfile(fileext = ".nc")
download.file("https://downloads.psl.noaa.gov/Datasets/NARR/Monthlies/monolevel/air.2m.mon.mean.nc", 
              raw.temp,
              mode = "wb")

# Download gridded relative humidity data from NCEP NARR
raw.rh <- tempfile(fileext = ".nc")
download.file("https://downloads.psl.noaa.gov/Datasets/NARR/Monthlies/monolevel/rhum.2m.mon.mean.nc", 
              raw.rh,
              mode = "wb")

# Download county geographies
counties <- tigris::counties(cb = FALSE, year = 2021) %>%
  st_as_sf() %>% # Convert from spatial polygons format to sf (this is key)
  dplyr::select(geoid = GEOID, name = NAMELSAD, geometry) # Use same projection as raster

# Get vector of unique states
states <- counties |> 
  mutate(st = substr(geoid, 1, 2)) |> 
  arrange(st) |> 
  pull(st) |> 
  unique()

# Function to download tract geographies
get_tracts <- function(st){
  tigris::tracts(state = st, cb = FALSE, year = 2021) %>%
    st_as_sf() %>% 
    dplyr::select(geoid = GEOID, name = NAMELSAD, geometry) 
}

# Download tract geographies by state, parallelizing over states
tracts <- future_map_dfr(states, ~get_tracts(.x))

# Data prep ---------------------------------------------------------------

# Prep raster data
ras.temp <- brick(raw.temp) # use raster::brick() for layered rasters
ras.rh <- brick(raw.rh) # Create brick from humidity data 
temp.ras.july.2021 <- ras.temp[["X2021.07.01"]]-273.15 # Extract July 2021, convert from K to C
names(temp.ras.july.2021) <- "temp" # Rename temp var
rh.ras.july.2021 <- ras.rh[["X2021.07.01"]] # Extract July 2021 RH
names(rh.ras.july.2021) <- "rh" # Rename RH var
ras <- stack(temp.ras.july.2021, rh.ras.july.2021) # Stack the two rasters

# Convert rasters to sf
temp.sf.july.2021 <- ras[["temp"]] %>%
  as('SpatialPolygonsDataFrame') %>% # Convert to spatial polygons data.frame
  st_as_sf() # Convert to Sf format

rh.sf.july.2021 <- ras[["rh"]] %>%
  as('SpatialPolygonsDataFrame') %>% 
  st_as_sf() 

# Set CRS from geographies equivalent to NARR data
counties <- counties %>%
  st_transform(crs = raster::projection(temp.sf.july.2021)) 

tracts <- tracts %>%
  st_transform(crs = raster::projection(temp.sf.july.2021)) 

# Produce and export estimates --------------------------------------------

# Counties
joined.temp <- counties |> 
  st_join(temp.sf.july.2021, left = T, largest = T) |> 
  as.data.frame() |> 
  dplyr::select(geoid,temp) |> 
  mutate(temp = round(temp,1))

joined.rh <- counties |> 
  st_join(rh.sf.july.2021, left = T, largest = T) |> 
  as.data.frame() |> 
  dplyr::select(geoid,rh) |> 
  mutate(rh = round(rh,1))

out <- joined.temp |> 
  full_join(joined.rh, by = "geoid") |> 
  mutate(wbt = calc_wbt(temp, rh, "f")) |> 
  arrange(geoid)

write.csv(out, "data/narr_counties.csv",
          row.names = F)

# Tracts
joined.temp <- tracts |> 
  st_join(temp.sf.july.2021, left = T, largest = T) |> 
  as.data.frame() |> 
  dplyr::select(geoid,temp) |> 
  mutate(temp = round(temp,1))

joined.rh <- tracts |> 
  st_join(rh.sf.july.2021, left = T, largest = T) |> 
  as.data.frame() |> 
  dplyr::select(geoid,rh) |> 
  mutate(rh = round(rh,1))

out <- joined.temp |> 
  full_join(joined.rh, by = "geoid") |> 
  mutate(wbt = calc_wbt(temp, rh, "f"))

write.csv(out, "data/narr_tracts.csv",
          row.names = F)

