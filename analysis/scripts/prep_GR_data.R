# Authors: Mike Ackerman
# Purpose: Prep data to be used for QRF assessments in the Grande Ronde
# Created: 8/11/2022
# Last Modified:
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
  st_transform(gr_crs)
win_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_No_elev_juv_winter.shp") %>%
  st_transform(gr_crs)
redd_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_No_elev_redds.shp") %>%
  st_transform(gr_crs)
# these are too large to reasonably plot

# read in Snake River Basin TRT population polygons
load(paste0(nas_prefix, "main/data/GIS/SR_pops.rda"))

# export polygons for populations of interest
# Chinook salmon
gr_chnk_pops = spsm_pop %>%
  filter(TRT_POPID == "GRCAT" | TRT_POPID == "GRUMA" | TRT_POPID == "GRLOO") %>%
  st_transform(gr_crs) %>%
  st_write(here("analysis/data/derived_data/chnk_gr_trt_pops.shp"),
           append = F)

# Steelhead
gr_sthd_pop = sth_pop %>%
  filter(TRT_POPID == "GRUMA-s") %>%
  st_transform(gr_crs) %>%
  st_write(here("analysis/data/derived_data/GRUMA-s.shp"),
           append = F)

# plot gr_chnk_pops   
ggplot(data = gr_chnk_pops) +
  geom_sf()

#--------------------------
# filter GR QRF extrapolations
# summer parr
gr_sum_sf = sum_juv_sf %>%
  st_intersection(gr_chnk_pops %>%
                    st_union())  %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(gr_sum_sf) + geom_sf()

# winter presmolt
gr_win_sf = win_juv_sf %>%
  st_intersection(gr_chnk_pops %>%
                    st_union())  %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(gr_win_sf) + geom_sf()

# redds
gr_redd_sf = redd_sf %>%
  st_intersection(gr_chnk_pops %>%
                    st_union())  %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_2 * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_2 * reach_leng)
ggplot(gr_redd_sf) + geom_sf()

#--------------------------
# get 200m reach layer for this area; contains a bunch of habitat metrics for the 200m reaches
load(paste0(nas_prefix, "main/data/qrf/gitrepo_data/input/rch_200.rda"))

# trim down 200m reach layer
gr_rch_sf = rch_200 %>%
  filter(UniqueID %in% gr_sum_sf$UniqueID) %>%
  st_transform(gr_crs)
ggplot(gr_rch_sf) + geom_sf()

#--------------------------
# save data for this repository
save(gr_sum_sf,
     gr_win_sf,
     gr_redd_sf,
     file = here("analysis/data/derived_data/gr_qrf_extrapolations.rda"))

save(gr_chnk_pops,
     gr_sthd_pop,
     gr_rch_sf,
     file = "analysis/data/derived_data/gr_spatial.rda")

#--------------------------
# save as geopackages for use in QGIS
st_write(gr_sum_sf,
         dsn = "analysis/data/derived_data/gr_juv_sum_qrf.gpkg",
         append = F)
st_write(gr_win_sf,
         dsn = "analysis/data/derived_data/gr_juv_win_qrf.gpkg",
         append = F)
st_write(gr_redd_sf,
         dsn = "analysis/data/derived_data/gr_redd_qrf.gpkg",
         append = F)
st_write(gr_rch_sf,
         dsn = "analysis/data/derived_data/gr_rch_200.gpkg",
         append = F)

#--------------------------
# just a preliminary plot

# pick a river color
river_color = "lightskyblue1"

gr_sum_sf %>%
  ggplot() +
  geom_sf(data = gr_chnk_pops %>%
            st_union() %>%
            nngeo::st_remove_holes(),
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = gr_sum_sf %>%
            filter(chnk == 1), # only records in the StreamNet Chinook domain
          aes(color = chnk_per_2),
          size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Grande Ronde Basin",
       color = expression(`Chinook Parr` / m^2))
