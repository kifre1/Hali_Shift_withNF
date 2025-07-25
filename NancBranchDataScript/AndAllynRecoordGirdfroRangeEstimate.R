On the range edge, here's a demo -- I think you just need to get the range edge coordinates in those kilometer things and swap that in for the simulated date here: library(sf)
library(rnaturalearthdata)
library(rnaturalearthhires)
 
set.seed(123)
n <- 100
dat <- data.frame(
  x = 1000*runif(n, -500, 1100),
  y = 1000*runif(n, 4700, 5300)
)
 
# Convert to sf, using a custom CRS (let's say a local projection in kilometers)
sf_pts <- st_as_sf(dat, coords = c("x", "y"), crs = "EPSG:32621")

# Reproject to WGS84 for mapping
sf_pts_wgs84 <- st_transform(sf_pts, crs = 4326)

# Get high-res coastline
coast <- ne_coastline(scale = "large", returnclass = "sf")

# Plot
ggplot() +
  geom_sf(data = coast, color = "gray50") +
  geom_sf(data = sf_pts_wgs84, color = "blue", size = 1) +
  coord_sf(xlim = c(-80, -40), ylim = c(35, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Sample Projected Coordinates in Northwest Atlantic",
       subtitle = "Reprojected from km-based custom CRS to WGS84")

#Have you all seen or heard about this? https://nrha.shinyapps.io/dataexplorer/#!/survey FishViz 2.0 is great, too, though this is pretty awesome if you are thinking for examples for what you all might make north of the border down the road
