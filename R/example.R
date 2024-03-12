
# Libraries
library(tidyverse)
library(raster)
library(ncdf4)
library(tigris)
library(sf)
library(viridis)

# Download gridded air temp data from NCEP NARR
raw.temp <- tempfile(fileext = ".nc")
download.file("https://downloads.psl.noaa.gov/Datasets/NARR/Monthlies/monolevel/air.2m.mon.mean.nc", 
              raw.temp,
              mode = "wb")


# Prep raster data
ras.temp <- brick(raw.temp) # use raster::brick() for layered rasters
temp.ras.july.2021 <- ras.temp[["X2021.07.01"]]-273.15 # Extract July 2021, convert from K to C
names(temp.ras.july.2021) <- "temp" # Rename temp var

# Visualize raster
par(mar=c(2,2,2,2)) # Set plot margins (prevents " plot margins too small" error)
plot(temp.ras.july.2021, main = "Averagy July 2021 Temp in C, NCEP NARR")

# Convert raster to sf
temp.sf.july.2021 <- temp.ras.july.2021[["temp"]] %>%
  as('SpatialPolygonsDataFrame') %>% # Convert to spatial polygons data.frame
  st_as_sf() # Convert to Sf format

# Import geographic data for Maryland, 2021
tracts.md <- tigris::tracts(state = "MD", cb = FALSE, year = 2021) %>%
  st_as_sf() %>% # Convert from spatial polygons format to sf (this is key)
  dplyr::select(geoid = GEOID, name = NAMELSAD, geometry) %>%
  st_transform(crs = raster::projection(temp.ras.july.2021)) # Use same projection as raster

# Join
joined.tracts <- tracts.md |> 
  st_join(temp.sf.july.2021, left = T, largest = T) 

# Alternate: join based on intersection 
# test <- tracts.md |> 
#   st_join(temp.sf.july.2021, left = T) |> 
#   group_by(geoid) |> 
#   summarize(temp = mean(temp, na.omit = T)) |> 
#   ungroup() 

joined.grid <- temp.sf.july.2021 |> 
  st_join(tracts.md, right = T, largest = T) |> 
  filter(!is.na(geoid))

# Map: Grid alone
ggplot() +
  geom_sf(data = joined.grid, mapping = aes(fill = temp), color = "steelblue", size = 0.05) +
  scale_fill_viridis(option = "plasma", name = "Avg July temp") +
  theme_void() +
  ggtitle("Average July 2021 Temp, grids, Maryland")

# Map: Grid with tracts overlaid
ggplot() +
  geom_sf(data = joined.grid, mapping = aes(fill = temp), color = "steelblue", size = 0.05) +
  scale_fill_viridis(option = "plasma", name = "Avg July temp") +
  geom_sf(data = joined.tracts, fill = NA, color = "white") +
  theme_void() + 
  ggtitle("Grids with tracts overlaid, Avg July 2021 temp, MD")

# Map: tracts showing final temp assigned to tract from grid
ggplot() +
  geom_sf(data = joined.tracts, mapping = aes(fill = temp), color = "steelblue", size = 0.05) +
  scale_fill_viridis(option = "plasma", name = "Avg July temp") +
  theme_void() + 
  ggtitle("Avg July 2021 temp, tracts, MD")
