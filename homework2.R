install.packages("packages.R")
install.packages("GGally")
install.packages("ggstats")
# Load Packages
library(dplyr)
library(readr)
library(tidyr)
library(viridis)
library(GGally)
library(gg)
# Mapping packages
library(sf)
library(ggspatial)
jp_solar = read_csv("data/jp_solar.csv")
mymunis = read_sf("data/japan/municipalities.geojson")
mypref = read_sf("data/japan/prefectures.geojson")
myregions = read_sf("data/japan/regions.geojson")
mymunis %>% head()
g1 = ggplot() +
geom_sf(data = mymunis, fill = "tan", color = "white", size = 0.05) +
#crop the map
coord_sf(xlim = c(127, 150),
ylim = c(30, 46)) +
# add theme
theme_bw() +
# Add a scale in the corner to help reader measure distance
annotation_scale() +
# Add labels
labs(subtitle = "Japanese Municipalities")
g1
#Question 2
#filter for tohoku region only
# add stjoin with prefectures since you need two spatial sets for st_join
tohoku_region_only <- myregions %>%
filter(region == "Tohoku")
tohoku_munis = mymunis %>%
st_join(tohoku_region_only, join = st_within, left= FALSE) %>%
left_join(by = "muni_code", y = jp_solar)
# mean pre
jp_solar$solar_rate %>% mean() #1.055404
#mean post
tohoku_munis$solar_rate %>% mean(na.rm = TRUE) #0.9169646
g2 = ggplot() +
geom_sf(data = myregions, fill = "lightblue", color = "white", size = 1) +
geom_sf(data = tohoku_munis, fill = "tan", color = "white", size = 0.05) +
#crop the map
coord_sf(xlim = c(138, 143),
ylim = c(37, 42)) +
# Add a scale in the corner to help reader measure distance
annotation_scale() +
#add theme
theme_bw() +
# Add labels
labs(subtitle = "Tohoku Municipalities")
g2
#Question 3: Aggregation
# choosing to aggregate by prefecture
pref_data <- tohoku_munis %>%
st_join(mypref, join = st_within, left = FALSE) # join on prefecture
#summarize data
pref_agg = pref_data %>%
group_by(pref_name) %>%
summarize(
mean_rate = mean(solar_rate, na.rm = TRUE),
median_rate = median(solar_rate, na.rm = TRUE),
sum_rate = sum(solar_rate, na.rm = TRUE)
) %>%
arrange(desc(mean_rate))
#Question 4 : visualize
# choosing to aggregate by municipality this time so use the tohoku_muni
#remove outlier
tohoku_munis = tohoku_munis %>%
filter(date != "2017-09-25")
tohoku_region = myregions %>%
filter(region == "Tohoku")
# make solar_rate not 0
tohoku_munis = tohoku_munis%>%
mutate(solar_rate = case_when(solar_rate == 0 ~ 0.001, TRUE ~ solar_rate))
g3 = ggplot() +
geom_sf(data = myregions, fill = "lightblue", color = "white", size = 1) +
geom_sf(data = tohoku_region, fill = "lightgreen", color = "white", size = 1) +
geom_sf(data = tohoku_munis, mapping = aes(fill = solar_rate),
# Do ourselves a favor and make the outlines white and thin
color = "black", linewidth = 0.05) +
geom_sf(data = mypref, fill = NA, color = "black", linewidth = 0.3) +
#crop the map
coord_sf(xlim = c(139, 142.5),
ylim = c(37, 41.5)) +
scale_fill_viridis_c(option = "C", direction = -1, na.value = "gray80", limits = c(0,1)) +
# limits = c(0, 1), # make the map more distinct
# oob = scales::squish) +
# Add a scale in the corner to help reader measure distance
annotation_scale(location = "br") +
#add theme
theme_bw() +
# Add labels
labs(title = "Average Solar Rate per Municipality in the Tohoku Region",
caption = " The hotter the color, the higher the rate of solar installations per capita in
the highlighted municipality") +
theme(
plot.title = element_text(hjust = 0.5), # center title
plot.caption = element_text(hjust = 0.5) # left align caption (or use 0.5 for center)
)
g3
ggsave(g3, filename = "myplot.png", dpi = 500, width = 8, height = 5)
#Question 5
# im using polygon data so i don't have points for kmeans computation
# i can use the centroid formula to get them though
tohoku_munis_centroids = tohoku_munis %>%
mutate(
points = st_centroid(geometry),
coords = st_coordinates(points),
x = coords[,1],
y = coords[,2]
) %>%
select(muni_code, solar_rate, x, y)
m <- kmeans(x = tohoku_munis_centroids$x, centers = 6)
# View cluster details
broom::tidy(m)
broom::glance(m)
tohoku_munis_centroids = tohoku_munis_centroids %>%
mutate(cluster = m$cluster)
cluster_polygons = tohoku_munis_centroids %>%
# For each cluster...
group_by(cluster) %>%
# union points into multipoints, then find the boundaries of the multipoints
summarize(geometry = geometry %>% st_union() %>% st_convex_hull())
# Visualize it!
g5 = ggplot() +
# Plot a simple background of precincts
geom_sf(data = mypref, fill = "black", color = "#373737") +
# Plot polls over top
geom_sf(data = tohoku_munis_centroids,
mapping = aes(fill = factor(cluster) ),
size = 3, shape = 21, color = "white") +
# Visualize the clusters
geom_sf(data = cluster_polygons,
mapping = aes(fill = factor(cluster) ),
alpha = 0.5, color = "white") +
coord_sf(xlim = c(139, 142.5),
ylim = c(37, 41.5)) +
labs(fill = "Tohoku Municipality by Cluster") +
theme_bw()
g5
cluster_summary <- tohoku_munis_centroids %>%
st_drop_geometry() %>%
group_by(cluster) %>%
summarize(
mean_rate = mean(solar_rate, na.rm = TRUE),
sd_rate = sd(solar_rate, na.rm = TRUE),
n = n()
)
print(cluster_summary)
