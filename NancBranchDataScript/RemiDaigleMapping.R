require(arcpullr)
require(sf)
require(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
require(marmap)
require(ggplot2)
require(dplyr)

coastline_northamerica <- rnaturalearth::ne_states(iso_a2=c("CA","US","GL","PM"),returnclass = "sf") |> 
  select(admin,name_en,name_fr,geometry)

# terrestrial part from https://open.canada.ca/data/en/dataset/3862c9fa-dbeb-4f00-ac03-c5da6551bf00
mar_land <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_regions_regions_du_mpo/MapServer/0",
                              where = "Region_EN='Maritimes'")
# marine part from https://open.canada.ca/data/en/dataset/f089a3f3-45e9-47de-b1c4-170e9950d8e7
mar_ocean <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/eastern_canada_marine_spatial_planning_areas/MapServer/0",
                               where = "NAME_E='Scotian Shelf and Bay of Fundy'")

# combine terrestrial and marine parts
maritimes <- st_union(c(mar_land$geoms, mar_ocean$geoms))
maritimes_clean <- st_sf(geometry = st_sfc(lapply(maritimes[[1]], function(x) st_polygon(list(x))))) |> 
  mutate(area = as.numeric(st_area(geometry)),
         NAME_E = "Maritimes") |> 
  filter(area > 1) |> # remove the very small polygons at the land/sea interface
  select(-area) |> 
  st_set_crs(4326)


# set limits for lat and lon for marmap downloading and plotting
xlim <- c(-70, -54)
ylim <- c(40, 50)

# Download bathymetry data
bathy <- getNOAA.bathy(lon1 = xlim[1], lon2 = xlim[2], 
                       lat1 = ylim[1], lat2 = ylim[2], 
                       resolution = 1)

# Convert to data frame for ggplot
bathy_df <- fortify.bathy(bathy)

# Create the plot
ggplot() +
  geom_contour_filled(data = bathy_df, aes(x = x, y = y, z = z),breaks = c(-6000, -4000, -2000, -1000, -500, -200, -100, 0)) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(fill = "Depth (m)")+
  xlab(NULL)+ylab(NULL)+
  geom_sf(data = coastline_northamerica, fill = "#C8AC6A", color = "transparent") +
  geom_sf(data = maritimes_clean, fill = "transparent", color = "red") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_minimal()

