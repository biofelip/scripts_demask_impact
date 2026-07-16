library(sf)
library(dplyr)
library(pelaDSM)
library(purrr)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(units)
root <- r"(C:\Users\247404\Documents\2024\Anita\proyect impact wind farms)"
grid <- st_read(file.path(
  root,
  "3_3_sensitivity_per_species",
  "Meeressäuger_Kegelrobbe_Bauphase.shp"
)) %>%
  select("CELLCOD", "geometry")
grid <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Kegelrobbe_Bauphase.shp") %>%
  select("CELLCOD", "geometry")

###### DATA

sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Bauphase.shp")

sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Seehund_Bauphase.dbf") %>%
  left_join(grid, by = "CELLCOD") %>%
  st_as_sf()

sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Kegelrobbe_Bauphase.dbf")

sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Bauphase_fun1_meanmax_scaled_Gewichtung_0.dbf") %>%
  mutate(CELLCOD = CELLCODE) %>%
  mutate(addr__G = aggregiert) %>%
  left_join(grid, by = "CELLCOD") %>%
  st_as_sf()

# # sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_minke_whale_density.shp")

# sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_harbour_porpoise_density.shp")

# st_crs(sensitivity) <- 3035

# sensitivity <- sensitivity[st_intersects(sensitivity, st_union(grid), sparse = FALSE),]

# sensitivity <- sensitivity %>%
#   mutate(addr__G = Vorkommen) %>%
#   mutate(CELLCOD = CELLCODE)

# area <- st_area(sensitivity$geometry)/1000000

# pop <- sensitivity$Vorkommen*area

# total_pop <- sum(pop, na.rm = T)

# sum(st_area(sensitivity$geometry)/1000000, na.rm=T)

# ggplot() +
#   geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # scale_fill_viridis_d()
#   scale_fill_viridis_c()


# example_farms <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing.shp")
# # example_farms <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing_without operational.shp")



# ########

# # buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms.shp")
# # buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms_without_operational.shp")

# create_buffer <- function (owf, senvitivity, already_existing) {

#   vertices <- owf %>%
#     st_coordinates() %>%
#     as_tibble() %>%
#     select("X","Y") %>%
#     slice_head(n = nrow(.) - 1) %>%
#     st_as_sf(., coords=c("X", "Y"), crs = 3035)

#   new_buffers <- st_buffer(vertices, dist = 8000) %>%
#     st_union(., owf) %>%
#     select("geometry")

#   max_imp <- 0
#   max_index <- 0

#   for (i in 1:nrow(new_buffers)) {
#     current_buffer <- already_existing %>% st_union(., new_buffers$geometry[i])

#     int <- sensitivity %>%
#       st_intersection(.,current_buffer)

#     imp <- sum(drop_units(as.numeric(int$addr__G)*st_area(int)), na.rm = T)

#     if (imp > max_imp) {
#       max_imp <- imp
#       max_index <- i
#     }
#   }

#    return(new_buffers$geometry[max_index])
# }

# # new_buffers <- create_buffer(owf, porp_construction)
# #
# # buffers_list <- split(buffers, seq_len(nrow(buffers)))
# #
# # buffer_results <- map(buffers_list, ~ create_buffer(.x, porp_construction))
# #
# # buffer_results <- flatten(buffer_results)
# #
# # geom_col <- st_sfc(buffer_results, crs = 3035)
# #
# # new_buffers <- st_sf(geometry = geom_col)
# #
# # ggplot() +
# #   geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
# #   scale_fill_viridis_d() +
# #   geom_sf(data = new_buffers, color = "red", fill = NA) +
# #   geom_sf(data = example_farms, color = "blue")
# #
# #
# # st_write(new_buffers, "D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/New_Buffers_without_operational.shp")



# ####### LOAD BUFFERS TO USE HERE

# # new_buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/New_Buffers.shp")
# # buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing_BUFFERED.shp")
# # buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing_BUFFERED_routeinfo.shp") %>%
# #   mutate(FID = 1:nrow(buffers))
# buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms.shp") %>%
#   mutate(FID = 1:nrow(.))
# # buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms_without_operational.shp") %>%
# #   mutate(FID = 1:nrow(.))


# ggplot() +
#   geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # scale_fill_viridis_d() +
#   scale_fill_viridis_c() +
#   geom_sf(data = buffers, color = "red", fill = NA) +
#   geom_sf(data = example_farms, color = "blue")


# ###### GREEDY APPROACH

# for (scenario in c(40,50,60,70)) {  #### Iterate over scenarios

#   print(paste0(scenario, "GW ", "SCENARIO"))

#   filtered_buffers <- buffers %>%
#     filter(Scenario <= scenario)  #### Create pool of available owf to choose from

#   buffer_ids <- rep(NA, 5)   #### Results

#   res <- st_sfc(crs = st_crs(3035))

#   for (i in 1:5){  #### Need to select 5 windfarms per scenarios

#     max_imp <- 0
#     max_index <- 0
#     max_buffer <- NULL

#     for (j in 1:nrow(filtered_buffers)){  #### Iterate over all available owf and select the one with most impact

#       if (!is.null(res$geometry)) {      #### Check if some owf have already been selected
#         current_buffer <- create_buffer(filtered_buffers[j, ], sensitivity, res)

#         test_geom <- st_union(res, current_buffer)

#         int <- sensitivity %>%    #### Intersect sensitivity map with current buffer + already selected ones
#           st_intersection(.,test_geom)
#       } else {
#         current_buffer <- create_buffer(filtered_buffers[j, ], sensitivity, res)

#         int <- sensitivity %>%    #### Intersect sensitivity map with current buffer
#           st_intersection(.,current_buffer)
#       }

#       int <- int %>%       #### Ensure no duplicates
#         dplyr::filter(!duplicated(CELLCOD) & !duplicated(CELLCOD, fromLast = TRUE))

#       imp <- sum(drop_units(as.numeric(int$addr__G)*st_area(int)))   #### Calculate impact

#       if (imp > max_imp) {
#         max_imp <- imp
#         max_index <- j
#         max_buffer <- current_buffer
#       }
#     }

#     selected_id <- filtered_buffers$FID[max_index]

#     buffer_ids[i] <- selected_id     #### Save the index of the selected owf
#     filtered_buffers <- filtered_buffers %>% filter(FID != selected_id)     #### Remove the selected owf from the available pool of owf

#     res <- st_union(res,max_buffer)
#   }


#   owf_buffers <- buffers %>%
#     filter(FID %in% buffer_ids) %>%
#     st_union()

#   for (b in buffer_ids) {
#     test_buffer <- buffers %>% filter(FID == b)

#     test_buffer <- create_buffer(test_buffer, sensitivity, owf_buffers)

#     owf_buffers <- owf_buffers %>% st_union(., test_buffer)
#   }

#   if (scenario == 40) {     #### Save results
#     res_40 <- owf_buffers
#   }

#   if (scenario == 50) {
#     res_50 <- owf_buffers
#   }

#   if (scenario == 60) {
#     res_60 <- owf_buffers
#   }

#   if (scenario == 70) {
#     res_70 <- owf_buffers
#   }
# }


# porpoise_special <- st_read("D:/Vincent/NaMaRo/shapes/Special porpoise area/Fortpflanzungsgebiet_Schweinswale_p-geo-n.shp") %>%
#   st_transform(crs = 3035)
# # porpoise_special <- st_read("D:/Vincent/NaMaRo/shapes/Special porpoise area/MaibisAug_2002bis2010_BfN_Bauauschl.shp") %>%
#   # st_transform(crs = 3035)

# # ggplot() +
# #   geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
# #   scale_fill_viridis_d() +
# #   geom_sf(data = owf_buffers, color = NA, fill = "red", alpha = 0.5) +
# #   geom_sf(data = example_farms, color = "blue")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   geom_sf(data = sensitivity, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   # scale_fill_viridis_d() +
#   scale_fill_viridis_c() +
#   geom_sf(data = res_40, color = NA, fill = "red", alpha= 0.5) +
#   geom_sf(data = example_farms %>% filter(Scenario <= 40), color = "blue") +
#   labs(title = "40GW Scenario")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   geom_sf(data = sensitivity, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   # scale_fill_viridis_d() +
#   scale_fill_viridis_c() +
#   geom_sf(data = res_50, color = NA, fill = "red", alpha= 0.5) +
#   geom_sf(data = example_farms %>% filter(Scenario <= 50), color = "blue") +
#   labs(title = "50GW Scenario")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   geom_sf(data = sensitivity, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   # scale_fill_viridis_d() +
#   scale_fill_viridis_c() +
#   geom_sf(data = res_60, color = NA, fill = "red", alpha= 0.5) +
#   geom_sf(data = example_farms %>% filter(Scenario <= 60), color = "blue") +
#   labs(title = "60GW Scenario")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   geom_sf(data = sensitivity, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   # scale_fill_viridis_d() +
#   scale_fill_viridis_c() +
#   geom_sf(data = res_70, color = NA, fill = "red", alpha= 0.5) +
#   geom_sf(data = example_farms %>% filter(Scenario <= 70), color = "blue") +
#   labs(title = "70GW Scenario")


# porpoise_special_40 <- sensitivity %>%
#   st_intersection(., res_40) %>%
#   st_intersection(., porpoise_special) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# porpoise_special_40 <- porpoise_special_40/1e6

# porpoise_special_50 <- sensitivity %>%
#   st_intersection(., res_50) %>%
#   st_intersection(., porpoise_special) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# porpoise_special_50 <- porpoise_special_50/1e6

# porpoise_special_60 <- sensitivity %>%
#   st_intersection(., res_60) %>%
#   st_intersection(., porpoise_special) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# porpoise_special_60 <- porpoise_special_60/1e6

# porpoise_special_70 <- sensitivity %>%
#   st_intersection(., res_70) %>%
#   st_intersection(., porpoise_special) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# porpoise_special_70 <- porpoise_special_70/1e6

# #### INCLUDE ROUTES

# # routes <- st_read("D:/Vincent/NaMaRo/shapes/Buffered routes Complete.shp")
# #
# # res_40 <- res_40 %>%
# #   st_union(., filter(routes, ID.Route %in% res_40$ID.Route_2)) %>%
# #   st_union()
# #
# # res_50 <- res_50 %>%
# #   st_union(., filter(routes, ID.Route %in% res_50$ID.Route_2)) %>%
# #   st_union()
# #
# # res_60 <- res_60 %>%
# #   st_union(., filter(routes, ID.Route %in% res_60$ID.Route_2)) %>%
# #   st_union()
# #
# # res_70 <- res_70 %>%
# #   st_union(., filter(routes, ID.Route %in% res_70$ID.Route_2)) %>%
# #   st_union()




# #### NUMERICAL RESULTS

# area_40 <- sensitivity %>%
#   st_intersection(., res_40) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# area_40 <- area_40/1000000


# area_50 <- sensitivity %>%
#   st_intersection(., res_50) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# area_50 <- area_50/1000000


# area_60 <- sensitivity %>%
#   st_intersection(., res_60) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# area_60 <- area_60/1000000


# area_70 <- sensitivity %>%
#   st_intersection(., res_70) %>%
#   st_union %>%
#   st_area() %>%
#   units::drop_units()
# area_70 <- area_70/1000000




# porpoise_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_harbour_porpoise_density.shp")
# minke_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_minke_whale_density.shp")

# st_crs(porpoise_density) <- 3035
# st_crs(minke_density) <- 3035

# ggplot() +
#   # geom_sf(data = porpoise_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = minke_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = res_40, color = NA, fill = "red", alpha= 0.5) +
#   geom_sf(data = example_farms, color = "blue")


# porpoise_density_40 <- porpoise_density %>%
#   st_intersection(., res_40) %>%
#   mutate(n_ppho = drop_units(Vorkommen * (st_area(geometry)/1000000)))
# n_ppho_40 <- sum(porpoise_density_40$n_ppho, na.rm = T)


# porpoise_density_50 <- porpoise_density %>%
#   st_intersection(., res_50) %>%
#   mutate(n_ppho = drop_units(Vorkommen * (st_area(geometry)/1000000)))
# n_ppho_50 <- sum(porpoise_density_50$n_ppho, na.rm = T)


# porpoise_density_60 <- porpoise_density %>%
#   st_intersection(., res_60) %>%
#   mutate(n_ppho = drop_units(Vorkommen * (st_area(geometry)/1000000)))
# n_ppho_60 <- sum(porpoise_density_60$n_ppho, na.rm = T)


# porpoise_density_70 <- porpoise_density %>%
#   st_intersection(., res_70) %>%
#   mutate(n_ppho = drop_units(Vorkommen * (st_area(geometry)/1000000)))
# n_ppho_70 <- sum(porpoise_density_70$n_ppho, na.rm = T)





# sensitivity_40 <- sensitivity %>%
#   st_intersection(.,res_40)

# # area_2_40 <- sensitivity_40 %>%
# #   filter(addr__G <= 3) %>%
# #   st_area() %>%
# #   sum()/1e6
# #
# # area_3_40 <- sensitivity_40 %>%
# #   filter(addr__G > 3) %>%
# #   st_area() %>%
# #   sum()/1e6

# area_2_40 <- sensitivity_40 %>%
#   filter(addr__G == 2) %>%
#   st_area() %>%
#   sum()/1e6

# area_3_40 <- sensitivity_40 %>%
#   filter(addr__G == 3) %>%
#   st_area() %>%
#   sum()/1e6

# area_4_40 <- sensitivity_40 %>%
#   filter(addr__G == 4) %>%
#   st_area() %>%
#   sum()/1e6

# area_5_40 <- sensitivity_40 %>%
#   filter(addr__G == 5) %>%
#   st_area() %>%
#   sum()/1e6

# sensitivity_40 <- sum(drop_units(as.numeric(sensitivity_40$addr__G)*st_area(sensitivity_40)))/1e8



# sensitivity_50 <- sensitivity %>%
#   st_intersection(.,res_50)

# # area_2_50 <- sensitivity_50 %>%
# #   filter(addr__G <= 3) %>%
# #   st_area() %>%
# #   sum()/1e6
# #
# # area_3_50 <- sensitivity_50 %>%
# #   filter(addr__G > 3) %>%
# #   st_area() %>%
# #   sum()/1e6

# area_2_50 <- sensitivity_50 %>%
#   filter(addr__G == 2) %>%
#   st_area() %>%
#   sum()/1e6

# area_3_50 <- sensitivity_50 %>%
#   filter(addr__G == 3) %>%
#   st_area() %>%
#   sum()/1e6

# area_4_50 <- sensitivity_50 %>%
#   filter(addr__G == 4) %>%
#   st_area() %>%
#   sum()/1e6

# area_5_50 <- sensitivity_50 %>%
#   filter(addr__G == 5) %>%
#   st_area() %>%
#   sum()/1e6

# sensitivity_50 <- sum(drop_units(as.numeric(sensitivity_50$addr__G)*st_area(sensitivity_50)))/1e8



# sensitivity_60 <- sensitivity %>%
#   st_intersection(.,res_60)

# # area_2_60 <- sensitivity_60 %>%
# #   filter(addr__G <= 3) %>%
# #   st_area() %>%
# #   sum()/1e6
# #
# # area_3_60 <- sensitivity_60 %>%
# #   filter(addr__G > 3) %>%
# #   st_area() %>%
# #   sum()/1e6

# area_2_60 <- sensitivity_60 %>%
#   filter(addr__G == 2) %>%
#   st_area() %>%
#   sum()/1e6

# area_3_60 <- sensitivity_60 %>%
#   filter(addr__G == 3) %>%
#   st_area() %>%
#   sum()/1e6

# area_4_60 <- sensitivity_60 %>%
#   filter(addr__G == 4) %>%
#   st_area() %>%
#   sum()/1e6

# area_5_60 <- sensitivity_60 %>%
#   filter(addr__G == 5) %>%
#   st_area() %>%
#   sum()/1e6

# sensitivity_60 <- sum(drop_units(as.numeric(sensitivity_60$addr__G)*st_area(sensitivity_60)))/1e8



# sensitivity_70 <- sensitivity %>%
#   st_intersection(.,res_70)

# # area_2_70 <- sensitivity_70 %>%
# #   filter(addr__G <= 3) %>%
# #   st_area() %>%
# #   sum()/1e6
# #
# # area_3_70 <- sensitivity_70 %>%
# #   filter(addr__G > 3) %>%
# #   st_area() %>%
# #   sum()/1e6

# area_2_70 <- sensitivity_70 %>%
#   filter(addr__G == 2) %>%
#   st_area() %>%
#   sum()/1e6

# area_3_70 <- sensitivity_70 %>%
#   filter(addr__G == 3) %>%
#   st_area() %>%
#   sum()/1e6

# area_4_70 <- sensitivity_70 %>%
#   filter(addr__G == 4) %>%
#   st_area() %>%
#   sum()/1e6

# area_5_70 <- sensitivity_70 %>%
#   filter(addr__G == 5) %>%
#   st_area() %>%
#   sum()/1e6

# sensitivity_70 <- sum(drop_units(as.numeric(sensitivity_70$addr__G)*st_area(sensitivity_70)))/1e8




# ##### OPERATION PHASE

# sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Bauphase_fun1_meanmax_scaled_Gewichtung_0.dbf") %>%
#   mutate(CELLCOD = CELLCODE) %>%
#   mutate(addr__G = aggregiert) %>%
#   left_join(grid, by = "CELLCOD") %>%
#   st_as_sf()

# porpoise_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_harbour_porpoise_density.shp")
# minke_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_minke_whale_density.shp")

# st_crs(porpoise_density) <- 3035
# porpoise_density <- porpoise_density[st_intersects(porpoise_density, st_union(grid), sparse = FALSE),]

# st_crs(minke_density) <- 3035
# minke_density <- minke_density[st_intersects(minke_density, st_union(grid), sparse = FALSE),]


# tot_buffer <- buffers %>%
#   filter(NaMaRo < 40) %>%
#   st_union()

# ggplot() +
#   geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = tot_buffer, color = "red", fill = "red", alpha = 0.5)

# ggplot() +
#   # geom_sf(data = porpoise_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = minke_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = tot_buffer, color = "red", fill = "red", alpha = 0.5)


# area_impacted <- tot_buffer %>%
#   st_intersection(., sensitivity) %>%
#   st_intersection(., porpoise_special) %>%
#   st_area(.)/1e6
# area_impacted <- sum(drop_units(area_impacted), na.rm = T)


# porpoise_impacted <- porpoise_density %>%
#   st_intersection(., tot_buffer) %>%
#   mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
# porpoise_impacted <- sum(porpoise_impacted$ind, na.rm = T)

# minke_impacted <- minke_density %>%
#   st_intersection(., tot_buffer) %>%
#   mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
# minke_impacted <- sum(minke_impacted$ind, na.rm = T)


# tot_sensitivity <- sensitivity %>%
#   st_intersection(., tot_buffer)
# tot_sensitivity <- sum(drop_units(as.numeric(tot_sensitivity$addr__G)*st_area(tot_sensitivity)))/1e8


# area_impact_23 <- sensitivity %>%
#   filter(addr__G <= 3) %>%
#   st_intersection(., tot_buffer)
# area_impact_23 <- sum(drop_units(st_area(area_impact_23))/1e6)

# area_impact_34 <- sensitivity %>%
#   filter(addr__G > 3) %>%
#   st_intersection(., tot_buffer)
# area_impact_34 <- sum(drop_units(st_area(area_impact_34))/1e6)






# ##### TRAFFIC IMPACT

# grid <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Kegelrobbe_Bauphase.shp") %>%
#   select("CELLCOD", "geometry")

# sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Bauphase.shp")

# # sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Seehund_Bauphase.dbf") %>%
# #   left_join(grid, by = "CELLCOD") %>%
# #   st_as_sf()

# # sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Kegelrobbe_Bauphase.dbf")

# # sensitivity <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Bauphase_fun1_meanmax_scaled_Gewichtung_0.dbf") %>%
# #   mutate(CELLCOD = CELLCODE) %>%
# #   mutate(addr__G = aggregiert) %>%
# #   left_join(grid, by = "CELLCOD") %>%
# #   st_as_sf()

# # porpoise_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_harbour_porpoise_density.shp")
# porpoise_density <- st_read("D:/Vincent/NaMaRo/shapes/ppho_5x5_grid_2016_predictions.shp") %>%
#   st_transform(crs=3035)

# # minke_density <- st_read("D:/Vincent/NaMaRo/project impact wind farms/ITAW - Acoumetry - DEMASK - density surfaces/20240503_minke_whale_density.shp")
# minke_density <- st_read("D:/Vincent/NaMaRo/shapes/bacu_10x10_grid_SCANSIII_predictions.shp") %>%
#   st_transform(crs=3035)

# # st_crs(porpoise_density) <- 3035
# # porpoise_density <- porpoise_density[st_intersects(porpoise_density, st_union(grid), sparse = FALSE),]

# # st_crs(minke_density) <- 3035
# # minke_density <- minke_density[st_intersects(minke_density, st_union(grid), sparse = FALSE),]

# route_40 <- st_read("D:/Vincent/NaMaRo/shapes/Serviceverkehr_Routen_NaMaRo/Serviceverkehr_Routen_40GW.shp")
# st_crs(route_40) <- 4326
# route_40 <- st_transform(route_40, crs = 3035)

# route_50 <- st_read("D:/Vincent/NaMaRo/shapes/Serviceverkehr_Routen_NaMaRo/Serviceverkehr_Routen_50GW.shp")
# st_crs(route_50) <- 4326
# route_50 <- st_transform(route_50, crs = 3035)

# route_60 <- st_read("D:/Vincent/NaMaRo/shapes/Serviceverkehr_Routen_NaMaRo/Serviceverkehr_Routen_60GW.shp")
# st_crs(route_60) <- 4326
# route_60 <- st_transform(route_60, crs = 3035)

# route_70 <- st_read("D:/Vincent/NaMaRo/shapes/Serviceverkehr_Routen_NaMaRo/Serviceverkehr_Routen_70GW.shp")
# st_crs(route_70) <- 4326
# route_70 <- st_transform(route_70, crs = 3035)

# porpoise_special <- st_read("D:/Vincent/NaMaRo/shapes/Special porpoise area/Fortpflanzungsgebiet_Schweinswale_p-geo-n.shp") %>%
#   st_transform(crs = 3035)

# example_farms <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing.shp")

# buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms.shp") %>%
#   mutate(FID = 1:nrow(.))

# buffer_40 <- buffers %>%
#   filter(NaMaRo < 40) %>%
#   st_union()

# buffer_50 <- buffers %>%
#   filter(NaMaRo < 50) %>%
#   st_union()

# buffer_60 <- buffers %>%
#   filter(NaMaRo < 60) %>%
#   st_union()

# buffer_70 <- buffers %>%
#   filter(NaMaRo < 70) %>%
#   st_union()



# ggplot() +
#   geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # geom_sf(data = porpoise_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA, linewidth=2) +
#   scale_fill_viridis_d() +
#   geom_sf(data = buffer_40, color = "red", fill = "red", alpha = 0.5) +
#   geom_sf(data = route_40, color = "red")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +q
#   # geom_sf(data = porpoise_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_density, aes(fill = AvgSu, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = buffer_40, color = "red", fill = "red", alpha = 0.5) +
#   geom_sf(data = route_40, color = "red")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # geom_sf(data = minke_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = minke_density, aes(fill = density, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = buffer_60, color = "red", fill = "red", alpha = 0.5) +
#   geom_sf(data = route_60, color = "red")

# ggplot() +
#   # geom_sf(data = sensitivity, aes(fill = addr__G, geometry = geometry), color = NA) +
#   # geom_sf(data = porpoise_density, aes(fill = Vorkommen, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_density, aes(fill = AvgSu, geometry = geometry), color = NA) +
#   geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA) +
#   scale_fill_viridis_c() +
#   geom_sf(data = buffer_70, color = "red", fill = "red", alpha = 0.5) +
#   geom_sf(data = route_70, color = "red")


# area_40 <- route_40 %>%
#   # st_intersection(sensitivity) %>%
#   st_intersection(porpoise_density) %>%
#   # st_intersection(porpoise_special) %>%
#   st_area(.)/1e6
# area_40 <- sum(drop_units(area_40), na.rm = T)

# area_50 <- route_50 %>%
#   # st_intersection(sensitivity) %>%
#   st_intersection(porpoise_density) %>%
#   # st_intersection(porpoise_special) %>%
#   st_area(.)/1e6
# area_50 <- sum(drop_units(area_50), na.rm = T)

# area_60 <- route_60 %>%
#   # st_intersection(sensitivity) %>%
#   st_intersection(porpoise_density) %>%
#   # st_intersection(porpoise_special) %>%
#   st_area(.)/1e6
# area_60 <- sum(drop_units(area_60), na.rm = T)

# area_70 <- route_70 %>%
#   # st_intersection(sensitivity) %>%
#   st_intersection(porpoise_density) %>%
#   # st_intersection(porpoise_special) %>%
#   st_area(.)/1e6
# area_70 <- sum(drop_units(area_70), na.rm = T)

# tot_area <- porpoise_density %>%
#   st_area(.)/1e6
# tot_area <- sum(drop_units(tot_area), na.rm = T)

# tot_area_special <- porpoise_special %>%
#   st_area(.)/1e6
# tot_area_special <- sum(drop_units(tot_area_special), na.rm = T)

# n_porpoise_40 <- porpoise_density %>%
#   st_intersection(route_40) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = AvgSu * drop_units(st_area(.))/1e6)
# n_porpoise_40 <- sum(n_porpoise_40$ind, na.rm = T)

# n_porpoise_50 <- porpoise_density %>%
#   st_intersection(route_50) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = AvgSu * drop_units(st_area(.))/1e6)
# n_porpoise_50 <- sum(n_porpoise_50$ind, na.rm = T)

# n_porpoise_60 <- porpoise_density %>%
#   st_intersection(route_60) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = AvgSu * drop_units(st_area(.))/1e6)
# n_porpoise_60 <- sum(n_porpoise_60$ind, na.rm = T)

# n_porpoise_70 <- porpoise_density %>%
#   st_intersection(route_70) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = AvgSu * drop_units(st_area(.))/1e6)
# n_porpoise_70 <- sum(n_porpoise_70$ind, na.rm = T)

# tot_porpoise <- porpoise_density %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = AvgSu * drop_units(st_area(.))/1e6)
# tot_porpoise <- sum(tot_porpoise$ind, na.rm = T)


# n_minke_40 <- minke_density %>%
#   st_intersection(route_40) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = density * drop_units(st_area(.))/1e6)
# n_minke_40 <- sum(n_minke_40$ind, na.rm = T)

# n_minke_50 <- minke_density %>%
#   st_intersection(route_50) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = density * drop_units(st_area(.))/1e6)
# n_minke_50 <- sum(n_minke_50$ind, na.rm = T)

# n_minke_60 <- minke_density %>%
#   st_intersection(route_60) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = density * drop_units(st_area(.))/1e6)
# n_minke_60 <- sum(n_minke_60$ind, na.rm = T)

# n_minke_70 <- minke_density %>%
#   st_intersection(route_70) %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = density * drop_units(st_area(.))/1e6)
# n_minke_70 <- sum(n_minke_70$ind, na.rm = T)

# tot_minke <- minke_density %>%
#   # mutate(ind = Vorkommen * drop_units(st_area(.))/1e6)
#   mutate(ind = density * drop_units(st_area(.))/1e6)
# tot_minke <- sum(tot_minke$ind, na.rm = T)


# impact_40 <- sensitivity %>%
#   st_intersection(route_40)
# impact_40 <- sum(drop_units(as.numeric(impact_40$addr__G)*st_area(impact_40)))/1e8

# impact_50 <- sensitivity %>%
#   st_intersection(route_50)
# impact_50 <- sum(drop_units(as.numeric(impact_50$addr__G)*st_area(impact_50)))/1e8

# impact_60 <- sensitivity %>%
#   st_intersection(route_60)
# impact_60 <- sum(drop_units(as.numeric(impact_60$addr__G)*st_area(impact_60)))/1e8

# impact_70 <- sensitivity %>%
#   st_intersection(route_70)
# impact_70 <- sum(drop_units(as.numeric(impact_70$addr__G)*st_area(impact_70)))/1e8


# # area_impact_40_23 <- sensitivity %>%
# #   filter(addr__G <= 3) %>%
# #   st_intersection(., route_40)
# # area_impact_40_23 <- sum(drop_units(st_area(area_impact_40_23))/1e6)
# #
# # area_impact_40_34 <- sensitivity %>%
# #   filter(addr__G > 3) %>%
# #   st_intersection(., route_40)
# # area_impact_40_34 <- sum(drop_units(st_area(area_impact_40_34))/1e6)
# #
# # area_impact_50_23 <- sensitivity %>%
# #   filter(addr__G <= 3) %>%
# #   st_intersection(., route_50)
# # area_impact_50_23 <- sum(drop_units(st_area(area_impact_50_23))/1e6)
# #
# # area_impact_50_34 <- sensitivity %>%
# #   filter(addr__G > 3) %>%
# #   st_intersection(., route_50)
# # area_impact_50_34 <- sum(drop_units(st_area(area_impact_50_34))/1e6)
# #
# # area_impact_60_23 <- sensitivity %>%
# #   filter(addr__G <= 3) %>%
# #   st_intersection(., route_60)
# # area_impact_60_23 <- sum(drop_units(st_area(area_impact_60_23))/1e6)
# #
# # area_impact_60_34 <- sensitivity %>%
# #   filter(addr__G > 3) %>%
# #   st_intersection(., route_60)
# # area_impact_60_34 <- sum(drop_units(st_area(area_impact_60_34))/1e6)
# #
# # area_impact_70_23 <- sensitivity %>%
# #   filter(addr__G <= 3) %>%
# #   st_intersection(., route_70)
# # area_impact_70_23 <- sum(drop_units(st_area(area_impact_70_23))/1e6)
# #
# # area_impact_70_34 <- sensitivity %>%
# #   filter(addr__G > 3) %>%
# #   st_intersection(., route_70)
# # area_impact_70_34 <- sum(drop_units(st_area(area_impact_70_34))/1e6)


# area_impact_40_0 <- sensitivity %>%
#   filter(addr__G == 0) %>%
#   st_intersection(., route_40)
# area_impact_40_0 <- sum(drop_units(st_area(area_impact_40_0))/1e6)

# area_impact_40_1 <- sensitivity %>%
#   filter(addr__G == 1) %>%
#   st_intersection(., route_40)
# area_impact_40_1 <- sum(drop_units(st_area(area_impact_40_1))/1e6)

# area_impact_40_2 <- sensitivity %>%
#   filter(addr__G == 2) %>%
#   st_intersection(., route_40)
# area_impact_40_2 <- sum(drop_units(st_area(area_impact_40_2))/1e6)

# area_impact_40_3 <- sensitivity %>%
#   filter(addr__G == 3) %>%
#   st_intersection(., route_40)
# area_impact_40_3 <- sum(drop_units(st_area(area_impact_40_3))/1e6)

# area_impact_40_4 <- sensitivity %>%
#   filter(addr__G == 4) %>%
#   st_intersection(., route_40)
# area_impact_40_4 <- sum(drop_units(st_area(area_impact_40_4))/1e6)

# area_impact_40_5 <- sensitivity %>%
#   filter(addr__G == 5) %>%
#   st_intersection(., route_40)
# area_impact_40_5 <- sum(drop_units(st_area(area_impact_40_5))/1e6)


# area_impact_50_0 <- sensitivity %>%
#   filter(addr__G == 0) %>%
#   st_intersection(., route_50)
# area_impact_50_0 <- sum(drop_units(st_area(area_impact_50_0))/1e6)

# area_impact_50_1 <- sensitivity %>%
#   filter(addr__G == 1) %>%
#   st_intersection(., route_50)
# area_impact_50_1 <- sum(drop_units(st_area(area_impact_50_1))/1e6)

# area_impact_50_2 <- sensitivity %>%
#   filter(addr__G == 2) %>%
#   st_intersection(., route_50)
# area_impact_50_2 <- sum(drop_units(st_area(area_impact_50_2))/1e6)

# area_impact_50_3 <- sensitivity %>%
#   filter(addr__G == 3) %>%
#   st_intersection(., route_50)
# area_impact_50_3 <- sum(drop_units(st_area(area_impact_50_3))/1e6)

# area_impact_50_4 <- sensitivity %>%
#   filter(addr__G == 4) %>%
#   st_intersection(., route_50)
# area_impact_50_4 <- sum(drop_units(st_area(area_impact_50_4))/1e6)

# area_impact_50_5 <- sensitivity %>%
#   filter(addr__G == 5) %>%
#   st_intersection(., route_50)
# area_impact_50_5 <- sum(drop_units(st_area(area_impact_50_5))/1e6)


# area_impact_60_0 <- sensitivity %>%
#   filter(addr__G == 0) %>%
#   st_intersection(., route_60)
# area_impact_60_0 <- sum(drop_units(st_area(area_impact_60_0))/1e6)

# area_impact_60_1 <- sensitivity %>%
#   filter(addr__G == 1) %>%
#   st_intersection(., route_60)
# area_impact_60_1 <- sum(drop_units(st_area(area_impact_60_1))/1e6)

# area_impact_60_2 <- sensitivity %>%
#   filter(addr__G == 2) %>%
#   st_intersection(., route_60)
# area_impact_60_2 <- sum(drop_units(st_area(area_impact_60_2))/1e6)

# area_impact_60_3 <- sensitivity %>%
#   filter(addr__G == 3) %>%
#   st_intersection(., route_60)
# area_impact_60_3 <- sum(drop_units(st_area(area_impact_60_3))/1e6)

# area_impact_60_4 <- sensitivity %>%
#   filter(addr__G == 4) %>%
#   st_intersection(., route_60)
# area_impact_60_4 <- sum(drop_units(st_area(area_impact_60_4))/1e6)

# area_impact_60_5 <- sensitivity %>%
#   filter(addr__G == 5) %>%
#   st_intersection(., route_60)
# area_impact_60_5 <- sum(drop_units(st_area(area_impact_60_5))/1e6)


# area_impact_70_0 <- sensitivity %>%
#   filter(addr__G == 0) %>%
#   st_intersection(., route_70)
# area_impact_70_0 <- sum(drop_units(st_area(area_impact_70_0))/1e6)

# area_impact_70_1 <- sensitivity %>%
#   filter(addr__G == 1) %>%
#   st_intersection(., route_70)
# area_impact_70_1 <- sum(drop_units(st_area(area_impact_70_1))/1e6)

# area_impact_70_2 <- sensitivity %>%
#   filter(addr__G == 2) %>%
#   st_intersection(., route_70)
# area_impact_70_2 <- sum(drop_units(st_area(area_impact_70_2))/1e6)

# area_impact_70_3 <- sensitivity %>%
#   filter(addr__G == 3) %>%
#   st_intersection(., route_70)
# area_impact_70_3 <- sum(drop_units(st_area(area_impact_70_3))/1e6)

# area_impact_70_4 <- sensitivity %>%
#   filter(addr__G == 4) %>%
#   st_intersection(., route_70)
# area_impact_70_4 <- sum(drop_units(st_area(area_impact_70_4))/1e6)

# area_impact_70_5 <- sensitivity %>%
#   filter(addr__G == 5) %>%
#   st_intersection(., route_70)
# area_impact_70_5 <- sum(drop_units(st_area(area_impact_70_5))/1e6)
