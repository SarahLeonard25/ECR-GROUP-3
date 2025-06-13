# =============================================================================
# District‐Level ITN Use Workflow (Zambia & Tanzania)
#
# Overview:
#  1. Load & filter ITN/person data to ZM & TZ
#  2. Build human‐readable SurveyId & explore person‐level sample sizes
#  3. Summarise each cluster’s ITN usage (person‐level → cluster)
#  4. Attach cluster‐level household nets from hh_custom
#  5. Load & clean admin2 shapefiles, spatially join clusters → districts
#  6. Aggregate to an admin2 × SurveyId panel
#  7. (Troubleshoot) Check for district name mismatches & fuzzy-match
#  8. Export panel (no geometry) for modelling
#  9. QC‐map: crop stray points & draw final map
# =============================================================================

# ── 0. Packages ─────────────────────────────────────────────────────────────
library(dplyr)      # data wrangling
library(stringr)    # string operations
library(sf)         # spatial operations
library(readr)      # CSV I/O
library(ggplot2)    # plotting
library(scales)     # squish() for color capping
library(stringdist) # fuzzy matching

# # You guys don't have access to these but can check out how I've prepared the data
# # ── 1. Load & filter ITN/person data ────────────────────────────────────────
# cluster_meta <- read_csv("/mnt/s3/malaria_survey_extraction/mse_v10/data/centre/cluster.csv") %>%
#   filter(str_starts(dhsid, "ZM") | str_starts(dhsid, "TZ")) %>%
#   dplyr::select(program, surveyid, clusterid, dhsid,
#          latitude, longitude,
#          admin1_dhs, admin1_name, urban_rural)
# 
# bednet_mse <- read_csv("/mnt/s3/malaria_survey_extraction/mse_v10/data/intervention/bednet/bednet_full_join.csv")
# 
# # keep only ZM/TZ clusters, then attach metadata
# bednet2 <- bednet_mse %>%
#   semi_join(cluster_meta, by = c("program","surveyid","clusterid")) %>%
#   left_join(cluster_meta,   by = c("program","surveyid","clusterid")) %>%
#   mutate(
#     SurveyId = paste0(str_sub(dhsid,1,2), str_sub(dhsid,3,6), program),
#     country  = if_else(str_starts(dhsid,"ZM"), "Zambia", "Tanzania")
#   )
# 
# # Get rid of coordinates for sharing
# person_data <- bednet2 %>% 
#                dplyr::select(-longitude, -latitude)
# # Get hh data
# person_data %>%
#   dplyr::left_join(hh2, by = c("SurveyId","clusterid"))
# 
# # Fitlre to 4 surveys
# person_data_filtered <- person_data %>%
#   filter(
#     (country == "Tanzania" & SurveyId %in% c("TZ2022DHS", "TZ2017DHS")) |
#       (country == "Zambia"   & SurveyId %in% c("ZM2018DHS", "ZM2013DHS"))
#   )
# 
# # Write out
# write.csv(person_data_filtered, "data/DHS_ITN_person_data_filtered.csv")
# #################################################################################
# # ── 2. Person‐count check ───────────────────────────────────────────────────
# person_counts <- bednet2 %>%
#   filter(!is.na(personid)) %>%
#   distinct(country, SurveyId, clusterid, householdid, personid) %>%
#   count(country, SurveyId, name = "n_obs") %>%
#   group_by(country) %>% arrange(SurveyId) %>%
#   mutate(SurveyId = factor(SurveyId, levels = unique(SurveyId))) %>%
#   ungroup()
# 
# ggplot(person_counts, aes(SurveyId, n_obs)) +
#   geom_col(fill="steelblue") +
#   facet_wrap(~ country, nrow=2, scales="free_x") +
#   labs(title="Person‐level observations per survey",
#        x="Survey", y="Unique persons") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle=45, hjust=1))
# 
# # ── 3. Summarise cluster‐level ITN usage ────────────────────────────────────
# cluster_usage <- bednet2 %>%
#   dplyr::filter(!is.na(personid)) %>%
#   dplyr::mutate(slept1 = if_else(slept_under_net %in% c(1,"yes","Yes"), 1, 0)) %>%
#   dplyr::group_by(country, SurveyId, clusterid) %>%
#   dplyr::summarise(
#     successes   = sum(slept1, na.rm=TRUE),
#     trials      = sum(!is.na(slept1)),
#     mean_age    = mean(age_in_person_record, na.rm=TRUE),
#     prop_female = mean(sex == "female", na.rm=TRUE),
#     latitude    = first(latitude),
#     longitude   = first(longitude),
#     .groups     = "drop"
#   )
# 
# 
# # ── 4. Attach household‐level nets (hh_custom) ─────────────────────────────
# hh_custom <- readr::read_csv("/mnt/s3/malaria_survey_extraction/mse_v10/data/custom/20250326/Svy_ALL_ITN_HH_Res.csv")
# 
# hh2 <- hh_custom %>%
#   dplyr::filter(
#     SurveyId  %in% cluster_usage$SurveyId,
#     clusterid %in% cluster_usage$clusterid
#   ) %>%
#   dplyr::rename_with(~ paste0("hh_", .),
#                      c("hh_size","n_itn","n_itn_used",
#                        "n_slept_under_itn","n_citn","n_llin")) %>%
#   dplyr::select(SurveyId, clusterid, dplyr::starts_with("hh_"))
# 
# cluster_summary <- cluster_usage %>%
#   dplyr::left_join(hh2, by = c("SurveyId","clusterid"))
# 
# # ── 5. Load & clean admin2 shapefiles + spatial join ───────────────────────
# tza_adm2 <- sf::st_read("shapefiles/tza_adm2.shp")
# zmb_adm2 <- sf::st_read("shapefiles/zmb_adm2.shp")
# 
# admin2_sf <- dplyr::bind_rows(
#   sf::st_as_sf(zmb_adm2) %>% dplyr::mutate(country = "Zambia"),
#   sf::st_as_sf(tza_adm2) %>% dplyr::mutate(country = "Tanzania")
# ) %>%
#   sf::st_transform(4326) %>%
#   dplyr::rename(admin1_name = name_1, admin2_name = name_2) %>%
#   sf::st_make_valid()
# 
# sf::sf_use_s2(FALSE)
# 
# cluster_sf2 <- cluster_summary %>%
#   sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE) %>%
#   sf::st_join(
#     admin2_sf %>% dplyr::select(country, admin1_name, admin2_name),
#     join = sf::st_intersects
#   ) %>%
#   dplyr::mutate(country = coalesce(country.x, country.y)) %>%
#   dplyr::select(-country.x, -country.y)
# 
# # ── 6. Aggregate to admin2 × SurveyId ───────────────────────────────────────
# admin2_panel <- cluster_sf2 %>%
#   sf::st_drop_geometry() %>%
#   dplyr::group_by(country, admin1_name, admin2_name, SurveyId) %>%
#   dplyr::summarise(
#     n_slept_under_net = sum(successes,    na.rm = TRUE),
#     n_respondents     = sum(trials,       na.rm = TRUE),
#     total_hh          = sum(hh_hh_size,   na.rm = TRUE),
#     total_n_itn       = sum(hh_n_itn,     na.rm = TRUE),
#     total_n_itn_used  = sum(hh_n_itn_used,na.rm = TRUE),
#     prop_use          = n_slept_under_net / n_respondents,
#     mean_age          = mean(mean_age,    na.rm = TRUE),
#     prop_female       = mean(prop_female, na.rm = TRUE),
#     n_clusters        = dplyr::n(),
#     .groups           = "drop"
#   )
# 
# # ── 7. Troubleshoot: name mismatches & fuzzy-match ─────────────────────────
# bad_names <- setdiff(admin2_panel$admin2_name, admin2_sf$admin2_name)
# if(length(bad_names)) {
#   message("Mismatched admin2_name values:")
#   print(bad_names)
#   fix_map <- data.frame(
#     original  = bad_names,
#     corrected = admin2_sf$admin2_name[stringdist::amatch(
#       bad_names, admin2_sf$admin2_name, maxDist = 2
#     )],
#     stringsAsFactors = FALSE
#   )
#   print(fix_map)
#   admin2_panel <- admin2_panel %>%
#     dplyr::left_join(fix_map, by = c("admin2_name" = "original")) %>%
#     dplyr::mutate(
#       admin2_name = if_else(is.na(corrected), admin2_name, corrected)
#     ) %>%
#     dplyr::select(-corrected)
# }
# 
# # ── 8. Export for modelling ─────────────────────────────────────────────────
# # readr::write_csv(admin2_panel, "/mnt/s3/malaria_survey_extraction/output/admin2_ITN_panel.csv")

# ── 9. QC-map: crop stray points & draw final map ───────────────────────────
admin2_map   <- admin2_sf %>%
  dplyr::left_join(admin2_panel,
                   by = c("country","admin2_name","admin1_name"))
admin2_union <- sf::st_union(admin2_sf)

# ── 9a. Prepare cluster points for overlay ─────────────────────────────────
cluster_pts <- cluster_sf2 %>%
  # we only want points that found a district
  dplyr::filter(!is.na(admin2_name)) %>%
  # and crop any strays outside the union of all Admin2 polygons
  sf::st_filter(admin2_union)


# ── 9b. Plot ────────────────────────────────────────────────────────────────
ggplot() +
  geom_sf(
    data   = admin2_map,
    aes(fill = prop_use),
    colour = "grey50", size = 0.1
  ) +
  geom_sf(
    data   = cluster_pts,
    colour = "black", size = 0.4, alpha = 0.6
  ) +
  coord_sf() +
  scale_fill_viridis_c(
    option   = "viridis",
    na.value = "lightgrey",
    limits   = c(0, 0.9),
    oob      = scales::squish,
    name     = "ITN use\nproportion"
  ) +
  labs(
    title    = "District‐level ITN use and survey clusters",
    subtitle = "All Zambia & Tanzania surveys combined"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title  = element_text(size = 12),
    legend.text   = element_text(size = 10)
  )

###################################################################################
# Prep some covariates

# ── A. Packages ─────────────────────────────────────────────────────────────
library(malariaAtlas)   # getRaster()
library(raster)         # raster I/O & projection
library(exactextractr)  # exact_extract()
library(dplyr)          # data wrangling
library(sf)             # spatial data

# ── B. Your admin2_map (must already exist) ────────────────────────────────
# admin2_map is an sf with: country, admin1_name, admin2_name, SurveyId, geometry

# ── C. Download chosen rasters via malariaAtlas ────────────────────────────
# 1. ITN access (Africa 2010 baseline)
itn_access <- getRaster(
  surface = "Interventions__201106_Africa_Insecticide_Treated_Net_Access",
  year    = 2011,   # matches 201106 code
  shape   = admin2_map
)

# 2. Motorized travel time to healthcare (2020)
tt_health <- getRaster(
  surface = "Accessibility__202001_Global_Motorized_Travel_Time_to_Healthcare",
  year    = 2020,
  shape   = admin2_map
)

# 3. Pf parasite rate (2022)
pfpr <- getRaster(
  surface = "Malaria__202206_Global_Pf_Parasite_Rate",
  year    = 2022,
  shape   = admin2_map
)

# ── D. Ensure all share CRS EPSG:4326 ───────────────────────────────────────
for(r in list(itn_access, tt_health, pfpr)) {
  if(!compareCRS(r, admin2_map)) {
    r <- projectRaster(r, crs = st_crs(admin2_map)$proj4string)
  }
}

# ── E. Summarise each raster over polygons ─────────────────────────────────
# Note - sometimes you want the mean, sometimes the total when working with covariates - have a think!
admin2_map <- admin2_map %>%
  mutate(
    mean_itn_access   = exact_extract(itn_access,   geometry, 'mean'),
    mean_tt_health    = exact_extract(tt_health,    geometry, 'mean'),
    mean_pfpr         = exact_extract(pfpr,         geometry, 'mean')
  )

# ── F. Inspect & merge into your panel ─────────────────────────────────────
admin2_panel <- readr::read_csv("/mnt/data/admin2_ITN_panel.csv") %>%
  left_join(
    admin2_map %>%
      st_drop_geometry() %>%
      select(country, admin1_name, admin2_name,
             mean_itn_access, mean_tt_health, mean_pfpr),
    by = c("country","admin1_name","admin2_name")
  )

# ── G. Export enriched panel ────────────────────────────────────────────────
readr::write_csv(
  admin2_panel,
  "data/admin2_ITN_panel_with_MAP_covariates.csv"
)
