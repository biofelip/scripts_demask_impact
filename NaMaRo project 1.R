library(sf)
library(dplyr)
library(pelaDSM)
library(purrr)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(units)

# grid <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Kegelrobbe_Bauphase.shp") %>%
#   select("CELLCOD", "geometry")
#   
#   
# porp_construction <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Bauphase.dbf") %>%
#   left_join(grid, by = "CELLCOD")
# 
# porp_operation <- st_read("D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Betriebsphase.dbf") %>%
#   left_join(grid, by = "CELLCOD")
# 
#   
# ggplot() +
#   # geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
#   geom_sf(data = porp_operation, aes(fill = addr__G, geometry = geometry), color = NA) +
#   scale_fill_viridis_d()
# 
# 
# st_write(porp_construction, "D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Bauphase.shp")
# 
# st_write(porp_operation, "D:/Vincent/NaMaRo/project impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Betriebsphase.shp")

###### DATA

porp_construction <- st_read(r"(C:\Users\247404\Documents\proyect impact wind farms\3_3_sensitivity_per_species\Meeressäuger_Schweinswal_Bauphase.shp)")

porp_operation <- st_read("file:///C:/Users/247404/Documents/proyect impact wind farms/3_3_sensitivity_per_species/Meeressäuger_Schweinswal_Betriebsphase.shp")


ggplot() +
  geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
  scale_fill_viridis_d()

ggplot() +
  geom_sf(data = porp_operation, aes(fill = addr__G, geometry = geometry), color = NA) +
  scale_fill_viridis_d()



example_farms <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing.shp")


########

buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Buffers_all_mock_farms.shp")


create_buffer <- function (owf, senvitivity) {
  
  vertices <- owf %>%
    st_coordinates() %>%
    as_tibble() %>%
    select("X","Y") %>%
    slice_head(n = nrow(.) - 1) %>%
    st_as_sf(., coords=c("X", "Y"), crs = 3035)
  
  new_buffers <- st_buffer(vertices, dist = 8000) %>%
    st_union(., owf) %>%
    select("geometry")
  
  max_imp <- 0
  max_index <- 0
  
  for (i in 1:nrow(new_buffers)) {
    int <- sensitivity %>%
      st_intersection(.,new_buffers$geometry[i])
    
    imp <- sum(drop_units(as.numeric(int$addr__G)*st_area(int)))
    
    if (imp > max_imp) {
      max_imp <- imp
      max_index <- i
    }
  }
   
   return(new_buffers$geometry[max_index])
}

# new_buffers <- create_buffer(owf, porp_construction)


buffers_list <- split(buffers, seq_len(nrow(buffers)))

buffer_results <- map(buffers_list, ~ create_buffer(.x, porp_construction)) 

buffer_results <- flatten(buffer_results)

geom_col <- st_sfc(buffer_results, crs = 3035)

new_buffers <- st_sf(geometry = geom_col)


st_write(new_buffers, "D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/New_Buffers.shp")



####### LOAD BUFFERS TO USE HERE

# new_buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/New_Buffers.shp")
buffers <- st_read("D:/Vincent/NaMaRo/project impact wind farms/impact wind farms/Example_Farms_manual_placing_BUFFERED.shp")


ggplot() +
  geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
  scale_fill_viridis_d() +
  geom_sf(data = buffers, color = "red", fill = NA) +
  geom_sf(data = example_farms, color = "blue")




###### GREEDY APPROACH

sensitivity <- porp_construction  #### Define what sensitivity map to take (porp_construction, porp_operation, ...)

for (scenario in c(40,50,60,70)) {  #### Iterate over scenarios
  
  print(paste0(scenario, "GW ", "SCENARIO"))
  
  filtered_buffers <- buffers %>%
    filter(Scenario <= scenario)  #### Create pool of available owf to choose from
  
  buffer_ids <- rep(NA, 5)   #### Results
  
  for (i in 1:5){  #### Need to select 5 windfarms per scenarios
    
    max_imp <- 0
    max_index <- 0
    
    already_selected <- buffers %>% filter(FID %in% buffer_ids)   #### Owfs that were selected in previous iterations
    
    for (j in 1:nrow(filtered_buffers)){  #### Iterate over all available owf and select the one with most impact
      
      if (length(already_selected) > 0) {      #### Check if some owf have already been selected
        already_selected <- already_selected %>% st_union()   #### Aggregate geometries
        
        test_geom <- st_union(already_selected, filtered_buffers$geometry[j])
        
        int <- sensitivity %>%    #### Intersect sensitivity map with current buffer + already selected ones
          st_intersection(.,test_geom)
      } else {
        int <- sensitivity %>%    #### Intersect sensitivity map with current buffer
          st_intersection(.,filtered_buffers$geometry[j])
      }
      
      int <- int %>%       #### Ensure no duplicates
        dplyr::filter(!duplicated(CELLCOD) & !duplicated(CELLCOD, fromLast = TRUE))
      
      imp <- sum(drop_units(as.numeric(int$addr__G)*st_area(int)))   #### Calculate impact
      
      if (imp > max_imp) {
        max_imp <- imp
        max_index <- j
      }
    }
    
    selected_id <- filtered_buffers$FID[max_index]
    
    buffer_ids[i] <- selected_id     #### Save the index of the selected owf
    filtered_buffers <- filtered_buffers %>% filter(FID != selected_id)     #### Remove the selected owf from the available pool of owf
  }
  
  if (scenario == 40) {     #### Save results
    res_40 <- buffers %>% filter(FID %in% buffer_ids)
  }
  
  if (scenario == 50) {
    res_50 <- buffers %>% filter(FID %in% buffer_ids)
  }
  
  if (scenario == 60) {
    res_60 <- buffers %>% filter(FID %in% buffer_ids)
  }
  
  if (scenario == 70) {
    res_70 <- buffers %>% filter(FID %in% buffer_ids)
  }
}





ggplot() +
  geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
  scale_fill_viridis_d() +
  geom_sf(data = res_40, color = "red", fill = NA) +
  geom_sf(data = example_farms, color = "blue")



ggplot() +
  geom_sf(data = porp_construction, aes(fill = addr__G, geometry = geometry), color = NA) +
  scale_fill_viridis_d() +
  geom_sf(data = filtered_buffers, color = "red", fill = NA) +
  geom_sf(data = example_farms, color = "blue")



