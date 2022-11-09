# Authors: Mike Ackerman
# Purpose: Prep data to be used for QRF assessments in the Grande Ronde
# Created: 8/11/2022
# Last Modified: 11/9/2022
# Notes:

#--------------------------
# clear environment
rm(list = ls())

#--------------------------
# load libraries
library(sf)
library(magrittr)
library(tidyverse)
library(here)

# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

# set default crs
gr_crs = st_crs(4326) # WGS84

#--------------------------
# read in HUC12 watershed boundaries from NAS, majority of PNW
huc12_sf = st_read(paste0(nas_prefix, "main/data/habitat/watershed_boundaries/WBDHU12.shp")) %>%
  st_transform(gr_crs)

#--------------------------
# read in current QRF extrapolations, for entire CRB
# use Morgan Bond's spatially continuous, 200m linear network layer
# use random forest extrapolation model
# sum_juv_sf = st_read(paste0(nas_prefix, "main/data/qrf/gitrepo_data/output/gpkg/Rch_Cap_RF_No_elev_juv_summer.shp")) %>% st_transform(uww_crs)
# win_juv_sf = st_read(paste0(nas_prefix, "main/data/qrf/gitrepo_data/output/gpkg/Rch_Cap_RF_No_elev_juv_winter.shp")) %>% st_transform(uww_crs)
# redd_sf = st_read(paste0(nas_prefix, "main/data/qrf/gitrepo_data/output/gpkg/Rch_Cap_RF_No_elev_redds.shp")) %>% st_transform(uww_crs)

# if needed, these files can be saved locally to decrease read times, e.g.,
# on Mike's machine...
sum_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_No_elev_juv_summer.shp") %>%
  mutate(chnk = as.logical(chnk),
         sthd = as.logical(sthd)) %>%
  st_transform(gr_crs)
win_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_No_elev_juv_winter.shp") %>%
  mutate(chnk = as.logical(chnk),
         sthd = as.logical(sthd)) %>%
  st_transform(gr_crs)
redd_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_No_elev_redds.shp") %>%
  mutate(chnk = as.logical(chnk),
         sthd = as.logical(sthd)) %>%
  st_transform(gr_crs)
# these are too large to reasonably plot

# read in Snake River Basin TRT population polygons
load(paste0(nas_prefix, "main/data/GIS/SR_pops.rda"))

# export polygons for watersheds of interest
cc_poly = spsm_pop %>%
  filter(TRT_POPID == "GRCAT") %>%
  st_transform(gr_crs) %>%
  st_write(here("analysis/data/derived_data/shp/cc_poly.shp"))
ggplot(data = cc_poly) + geom_sf()

ugr_poly = spsm_pop %>%
  filter(TRT_POPID == "GRUMA") %>%
  st_transform(gr_crs) %>%
  st_write(here("analysis/data/derived_data/shp/ugr_poly.shp"))
ggplot(data = ugr_poly) + geom_sf()

look_poly = spsm_pop %>%
  filter(TRT_POPID == "GRLOO") %>%
  st_transform(gr_crs) %>%
  st_write(here("analysis/data/derived_data/shp/look_poly.shp"))
ggplot(data = look_poly) + geom_sf()

#--------------------------
# filter GR QRF extrapolations

# catherine creek, summer parr
cc_sum_sf = sum_juv_sf %>%
  st_intersection(cc_poly)  %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(cc_sum_sf) + geom_sf()

# catherine creek, winter presmolt
cc_win_sf = win_juv_sf %>%
  st_intersection(cc_poly)  %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(cc_win_sf) + geom_sf()

# catherine creek, redds
cc_redd_sf = redd_sf %>%
  st_intersection(cc_poly)  %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(cc_redd_sf) + geom_sf()

# upper grande ronde, summer parr
ugr_sum_sf = sum_juv_sf %>%
  st_intersection(ugr_poly)  %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(ugr_sum_sf) + geom_sf()

# upper grande ronde, winter presmolt
ugr_win_sf = win_juv_sf %>%
  st_intersection(ugr_poly)  %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(ugr_win_sf) + geom_sf()

# upper grande ronde, redds
ugr_redd_sf = redd_sf %>%
  st_intersection(ugr_poly)  %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(ugr_redd_sf) + geom_sf()

# lookingglass, summer parr
look_sum_sf = sum_juv_sf %>%
  st_intersection(look_poly)  %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(look_sum_sf) + geom_sf()

# lookingglass, winter presmolt
look_win_sf = win_juv_sf %>%
  st_intersection(look_poly)  %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(look_win_sf) + geom_sf()

# lookingglass, redds
look_redd_sf = redd_sf %>%
  st_intersection(look_poly)  %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(look_redd_sf) + geom_sf()


#--------------------------
# get 200m reach layer for this area; contains a bunch of habitat metrics for the 200m reaches
load(paste0(nas_prefix, "main/data/qrf/gitrepo_data/input/rch_200.rda"))

# trim down 200m reach layer to watersheds
cc_rch_sf = rch_200 %>%
  filter(UniqueID %in% cc_sum_sf$UniqueID) %>%
  st_transform(gr_crs)
ugr_rch_sf = rch_200 %>%
  filter(UniqueID %in% ugr_sum_sf$UniqueID) %>%
  st_transform(gr_crs)
look_rch_sf = rch_200 %>%
  filter(UniqueID %in% look_sum_sf$UniqueID) %>%
  st_transform(gr_crs)

#--------------------------
# save data for this repository
save(cc_sum_sf,
     cc_win_sf,
     cc_redd_sf,
     ugr_sum_sf,
     ugr_win_sf,
     ugr_redd_sf,
     look_sum_sf,
     look_win_sf,
     look_redd_sf,
     file = here("analysis/data/derived_data/gr_qrf_extrapolations.rda"))

save(cc_poly,
     ugr_poly,
     look_poly,
     file = "analysis/data/derived_data/gr_spatial.rda")

#--------------------------
# save as shapefiles for use in QGIS
# catherine creek
st_write(cc_sum_sf,
         dsn = "analysis/data/derived_data/shp/cc_juv_sum_qrf.shp",
         overwrite = T)
st_write(cc_win_sf,
         dsn = "analysis/data/derived_data/shp/cc_juv_win_qrf.shp",
         append = F)
st_write(cc_redd_sf,
         dsn = "analysis/data/derived_data/shp/cc_redd_qrf.shp",
         append = F)
st_write(cc_rch_sf,
         dsn = "analysis/data/derived_data/shp/cc_rch_200.shp",
         append = F)

# upper grande ronde
st_write(ugr_sum_sf,
         dsn = "analysis/data/derived_data/shp/ugr_juv_sum_qrf.shp",
         append = F)
st_write(ugr_win_sf,
         dsn = "analysis/data/derived_data/shp/ugr_juv_win_qrf.shp",
         append = F)
st_write(ugr_redd_sf,
         dsn = "analysis/data/derived_data/shp/ugr_redd_qrf.shp",
         append = F)
st_write(ugr_rch_sf,
         dsn = "analysis/data/derived_data/shp/ugr_rch_200.shp",
         append = F)

# lookingglass creek
st_write(look_sum_sf,
         dsn = "analysis/data/derived_data/shp/look_juv_sum_qrf.shp",
         append = F)
st_write(look_win_sf,
         dsn = "analysis/data/derived_data/shp/look_juv_win_qrf.shp",
         append = F)
st_write(look_redd_sf,
         dsn = "analysis/data/derived_data/shp/look_redd_qrf.shp",
         append = F)
st_write(look_rch_sf,
         dsn = "analysis/data/derived_data/shp/look_rch_200.shp",
         append = F)

#--------------------------
# just a preliminary plot
# pick a river color
river_color = "lightskyblue1"
cc_sum_sf %>%
  ggplot() +
  geom_sf(data = cc_poly,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = cc_sum_sf %>%
            filter(chnk == 1), # only records in the StreamNet Chinook domain
          aes(color = chnk_per_2),
          size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Catherine Creek",
       color = expression(`Chinook Parr` / m^2))
