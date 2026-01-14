library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library(stringr)
library(maps)
# Preliminaries
# ------------------------------------------------------------------------------------------------------------
rm( list = ls() )




# ------------------------------------------------------------------------------------------------------------
# Load data and other objects
Prefix_Base <- 'C:/UCLA/Thesis/Code'


Prefix_Project <- '/Fire_Data'


setwd("C:/UCLA/Thesis/Code")


fire_data <-read.csv("Fire_Data.csv")



##Basic EDA
# --------------------------------------------------------------------------------------------------------


head(fire_data)

str(fire_data)

summary(fire_data)

table(fire_data$wind_direction)

table(fire_data$wind_speed)

table(fire_data$pr)

table(fire_data$vpd)


colnames(fire_data)

plot(fire_data$frp)



plot(fire_data$pr, fire_data$frp)


plot(fire_data$vpd, fire_data$frp)



plot(fire_data$wind_speed,fire_data$frp)



#Fire Detections in LA county (major cities)
#----------------------------------------------------------------------------------
# Load US cities dataset
data("us.cities")

# Filter LA County fires
la_fires <- fire_data %>%
  filter(latitude > 33.7, latitude < 34.9,
         longitude > -119.0, longitude < -117.5)

# List of major LA cities
major_la_cities <- c("Los Angeles", "Long Beach", "Santa Clarita", "Glendale", 
                     "Lancaster", "Palmdale", "Pomona", "Pasadena", 
                     "Torrance", "Inglewood", "Burbank", "West Covina", 
                     "Downey", "El Monte", "Santa Monica", "Whittier")

# Filter for LA cities
la_city_labels <- us.cities %>%
  filter(str_detect(name, paste(major_la_cities, collapse = "|")),
         country.etc == "CA",
         lat > 33.7, lat < 34.9,
         long > -119.0, long < -117.5)

# Clean up city names
la_city_labels$name_clean <- str_replace(la_city_labels$name, ", CA", "")

# Get California county boundaries and filter for LA County
ca_counties <- map_data("county")
la_county <- ca_counties %>%
  filter(region == "california", subregion == "los angeles")

# Plot fires, cities, and LA County boundary
ggplot() +
  # LA County outline
  geom_polygon(data = la_county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.8) +
  # Fire points
  geom_point(data = la_fires, aes(x = longitude, y = latitude),
             color = "red", alpha = 0.5, size = 1) +
  # City labels
  geom_text_repel(data = la_city_labels, aes(x = long, y = lat, label = name_clean),
                  size = 3.5, color = "blue", fontface = "bold", max.overlaps = 20) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Fire Detections in Los Angeles County with Major Cities",
       x = "Longitude", y = "Latitude")





# Convert date
fire_data$acq_date <- as.Date(fire_data$acq_date, format = "%m/%d/%Y")

# Filter for LA County (bounding box)
la_fires <- fire_data %>%
  filter(latitude > 33.7, latitude < 34.9,
         longitude > -119.0, longitude < -117.5)


# 1. Summary Statistics
# -------------------------------------------------------------------------------------------------------
summary(la_fires)

# 2. Distribution of Fire Intensity (FRP)
# ----------------------------
ggplot(la_fires, aes(x = frp)) +
  geom_histogram(fill = "firebrick", bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Fire Radiative Power (FRP)",
       x = "FRP (MW)", y = "Count")

# 3. Fires over Time
# -------------------------------------------------------------------------------------
ggplot(la_fires, aes(x = acq_date)) +
  geom_histogram(bins = 100, fill = "darkorange", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Wildfire Detections Over Time in LA County",
       x = "Date", y = "Number of Detections")


# 4. Boxplot: FRP by Day/Night
# ----------------------------------------------------------------------------------------
ggplot(la_fires, aes(x = daynight, y = frp, fill = daynight)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "FRP Distribution: Day vs. Night",
       x = "Time of Detection", y = "FRP (MW)")


# 6. Spatial Fire Density Map
# ----------------------------------------------------------------------------------------
ggplot(la_fires, aes(x = longitude, y = latitude)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.4) +
  geom_point(color = "red", alpha = 0.3, size = 0.5) +
  scale_fill_viridis_c() +
  theme_minimal() +
  coord_fixed(1.3) +
  labs(title = "Spatial Fire Density in Los Angeles County",
       x = "Longitude", y = "Latitude")






#ZZQ   Red dots fire detection potins
#      yellow/purple more instwive/hot Spots
#      Pueple- low fire occurance
#Higher levels indicate fire prone areas



# 7. Top Fire Days
# ------------------------------------------------------------------------------------------------
top_fire_days <- la_fires %>%
  group_by(acq_date) %>%
  summarise(total_fires = n()) %>%
  arrange(desc(total_fires)) %>%
  head(10)

print(top_fire_days)


#8. Brightness Distribution Histogram
# ------------------------------------------------------------------------------------------------

ggplot(fire_data, aes(x = brightness)) +
  geom_histogram(bins = 50, fill = "orange", alpha = 0.7) +
  labs(title = "Distribution of Brightness", x = "Brightness (K)", y = "Count")


#8. Brightness vs FRP Scatterplot
# ------------------------------------------------------------------------------------------------

ggplot(fire_data, aes(x = brightness, y = frp)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Brightness vs FRP", x = "Brightness (K)", y = "FRP (MW)")



library(GGally)


GGally::ggpairs(
  fire_data[, c("frp", "vpd", "wind_speed", "pr", "brightness")],
  progress = FALSE
)


# add log-transformed FRP column
fire_data$log_frp <- log1p(fire_data$frp)

# pairplot with log(FRP) vs predictors
GGally::ggpairs(
  fire_data[, c("log_frp", "vpd", "wind_speed", "pr", "brightness")],
  progress = FALSE
)
#Modeling
#-------------------------------------------------------------------------------------------------


fire_data$lat_bin <- cut(fire_data$latitude, breaks = 10, labels = LETTERS[1:10])
fire_data$lon_bin <- cut(fire_data$longitude, breaks = 10, labels = 0:9)
fire_data$region_id <- paste0(fire_data$lat_bin, fire_data$lon_bin)

fire_data$constant <- 1

model_vars <- c("constant", "vpd", "tmmx_C", "pr", "wind_speed")

fire_data <- fire_data %>% filter(complete.cases(across(all_of(model_vars))))

#λ=x⋅β
#λ= estimated fire instensity
#x=  vector of predictor variables 

#λ=b0​+b1​⋅vpd+b2​⋅temperature+b3​⋅wind
##zzq - 1) Get bounds from the data (use all points)
lat_min <- min(fire_data$latitude,  na.rm = TRUE)
lat_max <- max(fire_data$latitude,  na.rm = TRUE)
lon_min <- min(fire_data$longitude, na.rm = TRUE)
lon_max <- max(fire_data$longitude, na.rm = TRUE)


pad_lat <- (lat_max - lat_min) * 0.001
pad_lon <- (lon_max - lon_min) * 0.001
lat_min <- lat_min - pad_lat; lat_max <- lat_max + pad_lat
lon_min <- lon_min - pad_lon; lon_max <- lon_max + pad_lon


# 2) Build 10×10 breaks and labels

lat_breaks <- seq(lat_min, lat_max, length.out = 11)
lon_breaks <- seq(lon_min, lon_max, length.out = 11)

lat_labels <- LETTERS[1:10]
lon_labels <- 0:9


# 3) Assign bins & region_id to EVERY point (keep all rows)

fire_all <- fire_data %>%
  mutate(
    lat_bin = cut(latitude,  breaks = lat_breaks, labels = lat_labels,
                  include.lowest = TRUE, right = FALSE),
    lon_bin = cut(longitude, breaks = lon_breaks, labels = lon_labels,
                  include.lowest = TRUE, right = FALSE),
    region_id = paste0(lat_bin, lon_bin)
  )

# 4) Grid rectangles for plotting

grid_df <- expand.grid(lat_i = 1:10, lon_j = 1:10) %>%
  mutate(
    lat_min_cell = lat_breaks[lat_i],
    lat_max_cell = lat_breaks[lat_i + 1],
    lon_min_cell = lon_breaks[lon_j],
    lon_max_cell = lon_breaks[lon_j +1],
    lat_bin = lat_labels[lat_i],
    lon_bin = lon_labels[lon_j],
    
    region_id = paste0(lat_bin, lon_bin),
    lat_ctr = (lat_min_cell + lat_max_cell) / 2,
    lon_ctr = (lon_min_cell + lon_max_cell) / 2
  )

  #5 heatmap by cell detections

counts_df <- fire_all %>%
  filter(!is.na(region_id)) %>%
  count(region_id, name = "fires")

grid_counts <- grid_df %>%
  left_join(counts_df, by = "region_id") %>%
  mutate(fires = ifelse(is.na(fires), 0L, fires))

p_counts <- ggplot(grid_counts) +
  geom_rect(aes(xmin = lon_min_cell, xmax = lon_max_cell,
                ymin = lat_min_cell, ymax = lat_max_cell, fill = fires),
            color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(name = "Detections") +
  geom_text(aes(x = lon_ctr, y = lat_ctr, label = region_id),
            size = 2.6, color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "10×10 Regions — Detections per Cell (All Points)",
       x = "Longitude", y = "Latitude")
p_counts

# 5b) Alt: Heatmap by average FRP per cell
frp_df <- fire_all %>%
  group_by(region_id) %>%
  summarise(avg_frp = mean(frp, na.rm = TRUE), .groups = "drop")

grid_frp <- grid_df %>%
  left_join(frp_df, by = "region_id")

p_frp <- ggplot(grid_frp) +
  geom_rect(aes(xmin = lon_min_cell, xmax = lon_max_cell,
                ymin = lat_min_cell, ymax = lat_max_cell, fill = avg_frp),
            color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(name = "Avg FRP", na.value = "grey90") +
  geom_text(aes(x = lon_ctr, y = lat_ctr, label = region_id),
            size = 2.6, color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "10×10 Regions — Average FRP per Cell (All Points)",
       x = "Longitude", y = "Latitude")
p_frp








#Kernal Smoothing
#---------------------------------------------------------------------
library(MASS)  # provides kde2d
library(RColorBrewer)

library(RColorBrewer) # for palettes
#Raw fires
plot(fire_data$longitude, fire_data$latitude,
     pch = 19, cex = 0.3, col = "red",
     xlab = "Longitude", ylab = "Latitude",
     main = "Raw Fire Locations")







# Kernel density on longitude/latitude
k <- kde2d(
  x = fire_data$longitude,
  y = fire_data$latitude,
  n = 400,
  h = c(0.35, 0.35)
)

# White background
plot(NA, xlim = range(k$x), ylim = range(k$y),
     xlab = "Longitude", ylab = "Latitude",
     main = "Kernel Density of Fire Locations",
     type = "n")

# Darker contour colors (outer = green, inner = black)
contour_colors <- colorRampPalette(c("darkgreen", "blue", "purple", "black"))(15)

# Overlay contours
contour(
  k,
  col = contour_colors,
  lwd = 2.5,          # thicker lines
  drawlabels = TRUE,
  labcex = 1.1,
  add = TRUE
)

# Add fire points
points(
  fire_data$longitude, fire_data$latitude,
  pch = 20, cex = 0.25, col = rgb(0,0,0,0.5)
)


#Baseline Model
#--------------------------------------------------------------------------
library(car)
#  linear regression model
model_1 <- lm(frp ~ vpd + wind_speed + pr + tmmx_C, data = fire_data)

summary(model_1)
vif(model_1)



log_model <- lm(log(frp) ~ vpd + wind_speed + pr + brightness, data = fire_data)
summary(log_model)
vif(log_model)
plot(log_model)





#Poisson model
#----------------------------------------------
library(sf)
library(dplyr)
library(units)


grid_size_km <- 5
lon_col <- "longitude"   
lat_col <- "latitude"

crs_wgs84 <- 4326
crs_ca_eq <- 3310  # California Albers (equal-area), good for km grids

# ---- convert fire_data to sf (if needed) and project to equal-area ----
if (!inherits(fire_data, "sf")) {
  stopifnot(all(c(lon_col, lat_col) %in% names(fire_data)))
  fires_sf <- st_as_sf(fire_data, coords = c(lon_col, lat_col), crs = crs_wgs84, remove = FALSE)
} else {
  fires_sf <- fire_data
  if (is.na(st_crs(fires_sf))) st_crs(fires_sf) <- crs_wgs84
}

fires_eq <- st_transform(fires_sf, crs_ca_eq)

# ---- make grid ----
bbox_expanded <- st_as_sfc(st_bbox(fires_eq)) %>% st_buffer(5000) # 5km outward buffer
cellsize_m <- set_units(grid_size_km, km) %>% set_units(m) %>% drop_units()

grid <- st_make_grid(
  bbox_expanded,
  cellsize = c(cellsize_m, cellsize_m),
  what = "polygons",
  square = TRUE
) |> st_as_sf() |>
  mutate(cell_id = dplyr::row_number())

# (quick visual check)
plot(st_geometry(grid), col = NA, border = "grey80")
plot(st_geometry(fires_eq), add = TRUE, pch = 16, cex = 0.35)



# tag each fire with its grid cell_id
fires_in_grid <- st_join(fires_eq, grid[, "cell_id"], join = st_within)

# count fires per cell_id
fire_counts <- fires_in_grid |>
  st_drop_geometry() |>
  count(cell_id, name = "Count")

# attach counts to the grid; missing -> 0
grid_counts <- grid |>
  left_join(fire_counts, by = "cell_id") |>
  mutate(Count = ifelse(is.na(Count), 0L, as.integer(Count)))


fires_in_grid <- st_join(fires_eq, grid[, "cell_id"], join = st_within)
fire_counts <- fires_in_grid |>
  st_drop_geometry() |>
  count(cell_id, name = "Count")
grid_presence <- grid |>
  left_join(fire_counts, by = "cell_id") |>
  mutate(Count   = ifelse(is.na(Count), 0L, as.integer(Count)),
         HasFire = as.integer(Count > 0))
ggplot(grid_presence) +
  geom_sf(aes(fill = factor(HasFire)), color = "grey40", linewidth = 0.1) +
  scale_fill_manual(values = c("0" = "white", "1" = "black"), name = "Fire present") +
  labs(title = "Fire Presence per Grid Cell (0/1)") +
  theme_minimal()








# Intervals: (-Inf,0], (0,1], (1,9], (9,49], (49,99], (99,500], (500, Inf)
grid_counts <- grid_counts |>
  mutate(
    Fire_bin = cut(
      Count,
      breaks = c(-Inf, 0, 1, 9, 49, 99, 500, Inf),
      labels = c("0", "1", "2–9", "10–49", "50–99", "100–500", "501+"),
      right  = TRUE,
      include.lowest = TRUE
    )
  )

ggplot(grid_counts) +
  geom_sf(aes(fill = Fire_bin), color = "grey40", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "0"        = "white",
      "1"        = "aquamarine1",
      "2–9"      = "blueviolet",
      "10–49"    = "red",
      "50–99"    = "royalblue",
      "100–500"  = "orange",
      "501+"     = "#262626"
    ),
    name = "Fire count"
  ) +
  labs(title = "Fire Count per Grid Cell (Binned)") +
  theme_minimal()