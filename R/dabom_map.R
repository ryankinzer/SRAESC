# Build map !!

#------------------------------------------------------------------------------
# Gathers and prepares data for Steelhead NOSA presentation
# Ryan N. Kinzer
# Created: 02/13/18
#------------------------------------------------------------------------------
# load packages
#------------------------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(maptools)
#library(leaflet) # better for interactive maps
library(ggmap)

#------------------------------------------------------------------------------
# load model estimates
#------------------------------------------------------------------------------
#load('./DABOM_estimates/LGR_PIT_estimates.rda')

#------------------------------------------------------------------------------
# load default pop and metric data
#------------------------------------------------------------------------------

pop_df <- read_csv('./Document/Presentation/Snake_pops.csv') %>%
  select(Species:POP_NAME, Pittag_Array, Population)

site_df <- read_csv('./Document/Presentation/site_metadata.csv') %>%
  #mutate(Estimates = factor(Estimates, level = c('Valid', 'Biased'))) %>%
  arrange(Estimates)

#------------------------------------------------------------------------------
# Gather map data stuff
#------------------------------------------------------------------------------

geo_path <- 'C:/Users/ryank/Documents/ArcGIS/Snake_River_Basin_Pops.gdb'


# Create Population polygons
st_pop <- readOGR(dsn = geo_path, layer = 'Snake_River_Extant_Steelhead_POP')
st_pop <- spTransform(st_pop,"+proj=longlat")
st_mpg <- unionSpatialPolygons(st_pop, factor(st_pop$MPG))
st_esu <- unionSpatialPolygons(st_pop, factor(st_pop$ESU_DPS))

map_df <- data.frame(id = rownames(st_pop@data), TRT_POPID = st_pop@data$TRT_POPID, stringsAsFactors = F)
map_df <- left_join(map_df, pop_df, by = 'TRT_POPID')

st_pop_df <- fortify(st_pop)

st_pop_df <- left_join(st_pop_df, map_df, by = 'id') %>%
  as.tibble()

# Plot abundances on maps NEED MODEL ESTS loaded from plot_results.R

coord_df <- as.tibble(cbind(coordinates(st_pop), st_pop@data))
names(coord_df) <- c('long', 'lat', names(st_pop@data))

coord_df <- coord_df %>%
right_join(
  report_summ %>%
    filter(species == spp), by = 'TRT_POPID') %>%
  inner_join(pop_df %>%
               select(TRT_POPID, Population))

mpg_df <- fortify(st_mpg)
esu_df <- fortify(st_esu)

# Create Stream layer
pnw_rivers <- readOGR(dsn = geo_path, layer = 'PNW_Rivers')
snake_rivers <- readOGR(dsn = geo_path, layer = 'Snake_River_Monitored_Streams')

pnw_rivers <- spTransform(pnw_rivers,"+proj=longlat")
pnw_df <- fortify(pnw_rivers)

snake_rivers <- spTransform(snake_rivers,"+proj=longlat")
snake_df <- fortify(snake_rivers)

# Get some detection sites
ptagis_sites <- readOGR(dsn = geo_path, layer = 'Snake_River_PTAGIS_Sites')
ptagis_sites <- spTransform(ptagis_sites,"+proj=longlat")
ptagis_sites <- as.tibble(ptagis_sites)
#------------------------
# use ggmap
# get base map
b <- bbox(st_pop) # get bounding box

large_b <- geocode('Umatilla, OR')
base_map_bw <- get_map(location = large_b, source = 'stamen', maptype = 'toner', zoom = 6, color = 'bw')

fig_map <- ggmap(base_map_bw, extent = 'device') # btw 7 and 8 zoom

large_map <- fig_map +
  geom_polygon(data = st_pop_df,
               aes(x = long, y = lat, group = group), size = 1.25, colour = 'black', fill = 'lightgrey') +
  geom_path(data = mpg_df, aes(x = long, y = lat, group = id), size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_point(data = ptagis_sites, aes(x = Longitude, y = Latitude),colour = 'red', shape = 16, size = 1) +
  geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'none')

fig_large_map <- large_map +
  scale_x_continuous(limits = c(-124, -112)) +
  scale_y_continuous(limits = c(42.5, 48.5), expand = c(0,0))

ggsave(file = './Document/Presentation/fig_large_map.png', fig_large_map,units = 'in', width = 10, height = 6)



fig_small_map <- fig_map +
  #geom_polygon(data = filter(st_pop_df, Population),
  #             aes(x = long, y = lat, group = group), fill = 'lightgrey') + 
  geom_path(data = st_pop_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1) +
  geom_path(data = esu_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  #geom_point(data = filter(site_df, SiteID != 'GRA'), aes(x = Longitude, y = Latitude, colour = Estimates, shape = map_label2), size = 8) +
  geom_label(data = filter(site_df,!is.na(Estimates)), aes(x = Longitude, y = Latitude, label = SiteID, fill = Estimates), colour = 'white', size = 2) +
  geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_shape_manual(values = c('=', '-')) +
  #scale_colour_manual(values = c('darkorange','darkblue')) +
  scale_fill_manual(values = c('darkorange','darkblue')) +
  scale_x_continuous(limits = c(-120, -112)) +
  scale_y_continuous(limits = c(43.8, 47.5), expand = c(0,0)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(shape = 'Node Type',
       color = 'Estimates',
       fill = 'Estimates') +
  theme(legend.position = c(.9,.8), legend.direction = 'vertical', legend.box = 'vertical')

ggsave(file = './Document/Presentation/small_map.png', fig_small_map,units = 'in', width = 10, height = 6)


fig_pop_map <- fig_map +
  geom_polygon(data = filter(st_pop_df, Population),
               aes(x = long, y = lat, group = group), fill = 'lightgrey') + 
  geom_path(data = st_pop_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1) +
  geom_path(data = esu_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  #geom_point(data = filter(site_df, SiteID != 'GRA'), aes(x = Longitude, y = Latitude, colour = Estimates, shape = map_label2), size = 8) +
  geom_label(data = filter(coord_df, species == spp & spawn_yr == 2017 & Population), aes(x = long, y = lat, label = TRT_POPID), size = 4, fill = 'darkblue', colour = 'white') +
  #geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  #geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_shape_manual(values = c('=', '-')) +
  #scale_colour_manual(values = c('darkorange','darkblue')) +
  #scale_fill_manual(values = c('darkorange','darkblue')) +
  scale_x_continuous(limits = c(-120, -112)) +
  scale_y_continuous(limits = c(43.8, 47.5), expand = c(0,0)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(shape = 'Node Type',
       color = 'Estimates',
       fill = 'Estimates') +
  theme(legend.position = c(.9,.8), legend.direction = 'vertical', legend.box = 'vertical')

ggsave(file = './Document/Presentation/pop_map.png', fig_pop_map,units = 'in', width = 10, height = 6)


fig_pop_abund <- fig_map +
  geom_polygon(data = filter(st_pop_df, Population),
               aes(x = long, y = lat, group = group), fill = 'lightgrey') + 
  geom_path(data = st_pop_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1) +
  geom_path(data = esu_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  #geom_point(data = filter(site_df, SiteID != 'GRA'), aes(x = Longitude, y = Latitude, colour = Estimates, shape = map_label2), size = 8) +
  geom_label(data = filter(coord_df, species == spp & spawn_yr == 2017 & Population), aes(x = long, y = lat, label = estimate, size = estimate),fill = 'darkblue', colour = 'white') +
  #geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  #geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_shape_manual(values = c('=', '-')) +
  #scale_colour_manual(values = c('darkorange','darkblue')) +
  #scale_fill_manual(values = c('darkorange','darkblue')) +
  scale_x_continuous(limits = c(-120, -112)) +
  scale_y_continuous(limits = c(43.8, 47.5), expand = c(0,0)) +
  scale_size_continuous(range(5,15)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(shape = 'Node Type',
       color = 'Abundance',
       fill = 'Abundance',
       size = 'Abundance') +
  theme(legend.position = 'none', legend.direction = 'vertical', legend.box = 'vertical')

ggsave(file = './Document/Presentation/pop_abund.png', fig_pop_abund,units = 'in', width = 10, height = 6)


trib_summ_tmp <- trib_summ %>%
  inner_join(site_df, by = 'SiteID')



imnaha <- geocode('Imnaha, OR')
imnaha <- matrix(c(-117.5,-116.5,45,45.9), nrow = 2, ncol = 2, byrow = TRUE)
imnaha_map <- get_map(location = 'imnaha',source = 'google', maptype = 'hybrid', zoom = 9)

fig_map <- ggmap(imnaha_map, extent = 'device') # btw 7 and 8 zoom extent = 'device'


fig_trib_abund <- fig_map +
  geom_polygon(data = filter(st_pop_df, TRT_POPID == 'IRMAI-s'),
               aes(x = long, y = lat, group = group),colour = 'black',,fill = 'grey70', size = 1, alpha = .25) + 
  #geom_path(data = st_pop_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1) +
  #geom_path(data = esu_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  #geom_point(data = filter(site_df, SiteID != 'GRA'), aes(x = Longitude, y = Latitude, colour = Estimates, shape = map_label2), size = 8) +
  geom_label(data = filter(trib_summ_tmp, species == spp & spawn_yr == 2017 & Group %in% c('ImnahaRiver', 'CowCreek')), aes(x = Longitude, y = Latitude, label = estimate, fill = Estimates), colour = 'white') +
  #geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  #geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_shape_manual(values = c('=', '-')) +
  #scale_colour_manual(values = c('darkorange','darkblue')) +
  scale_fill_manual(values = c('darkorange','darkblue')) +
  scale_x_continuous(limits = c(-117.4, -116.5)) +
  scale_y_continuous(limits = c(45, 45.9), expand = c(0,0)) +
  scale_size_continuous(range(5,15)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(shape = 'Node Type',
       color = 'Abundance',
       fill = 'Abundance',
       size = 'Abundance') +
  theme(legend.position = 'none', legend.direction = 'vertical', legend.box = 'vertical')

ggsave(file = './Document/Presentation/trib_abund.png', fig_trib_abund,units = 'in', width = 4, height = 5)



sfsr <- geocode('Warm Lake, ID')
sfsr <- matrix(c(-116.5,-115,44.25,45.9), nrow = 2, ncol = 2, byrow = TRUE)
sfsr_map <- get_map(location = sfsr,source = 'google', maptype = 'hybrid', zoom = 9)

fig_sfsr <- ggmap(sfsr_map, extent = 'device') # btw 7 and 8 zoom extent = 'device'


fig_trib_sfsr <- fig_sfsr +
  geom_polygon(data = filter(st_pop_df, TRT_POPID %in% c('SFMAI-s', 'SFSEC-s')),
               aes(x = long, y = lat, group = group),colour = 'black',fill = 'grey70', size = 1, alpha = .25) + 
  #geom_path(data = st_pop_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1) +
  #geom_path(data = esu_df, aes(x = long, y = lat, group = group), colour = 'black', size = 1.5) +
  geom_path(data = pnw_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  geom_path(data = snake_df, aes(x = long, y = lat, group = group), colour = 'royalblue') +
  #geom_point(data = filter(site_df, SiteID != 'GRA'), aes(x = Longitude, y = Latitude, colour = Estimates, shape = map_label2), size = 8) +
  geom_label(data = filter(trib_summ_tmp, species == spp & spawn_yr == 2017 & Group %in% c('SFSalmon')), aes(x = Longitude, y = Latitude, label = estimate, fill = Estimates), colour = 'white') +
  #geom_point(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude), shape = 17, size = 3) +
  #geom_text(data = filter(site_df, SiteID == 'GRA'), aes(x = Longitude, y = Latitude, label = SiteID), vjust = -2, size = 3) +
  #scale_shape_manual(values = c('=', '-')) +
  #scale_colour_manual(values = c('darkorange','darkblue')) +
  scale_fill_manual(values = c('darkorange','darkblue')) +
  scale_x_continuous(limits = c(-116.1, -115.2)) +
  scale_y_continuous(limits = c(44.25, 45.4), expand = c(0,0)) +
  scale_size_continuous(range(5,15)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  labs(shape = 'Node Type',
       color = 'Abundance',
       fill = 'Abundance',
       size = 'Abundance') +
  theme(legend.position = 'none', legend.direction = 'vertical', legend.box = 'vertical')

ggsave(file = './Document/Presentation/trib_sfsr.png', fig_trib_sfsr,units = 'in', width = 4, height = 5)


# Summary Data

pop_df %>%
  filter(Species == 'Steelhead') %>%
  #filter(Population) %>%
  #select(MPG) %>%
  group_by(MPG) %>%
  summarise(n = n_distinct(TRT_POPID))

site_df %>%
  group_by(map_label2, Estimates) %>%
  summarise(n = n())



  

