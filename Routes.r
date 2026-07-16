##### load libraries
library(sf)
library(dplyr)
library(pelaDSM)
library(purrr)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(units)
library(patchwork)
library(glue)
library(openxlsx)

# because of issues with the encoding of files we will do it like this
Sys.setlocale("LC_CTYPE", "de_DE.UTF-8")
# root <- r"(C:\Users\247404\TiHo Hannover\ITAW - Acoumetry - DEMASK - DEMASK)"
root <- r"(C:\Users\felip\TiHo Hannover\ITAW - Acoumetry-DEMASK - DEMASK)"
all_files <- list.files(path = root, pattern = ".*\\.(shp|dbf)$", full.names = TRUE, recursive = TRUE)

source("utils.R")

nice_table_p <- function(res_list = NULL) {
    res_areas_porp <- sapply(res_list, \(x) x %>%
        purrr::discard(names(.) %in% c("area_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
        tibble::as_tibble(), simplify = TRUE) %>%
        t() %>%
        as.data.frame() %>%
        mutate(n_ppho = as.numeric(n_ppho)) %>%
        mutate(perc_increase = n_ppho / lag(n_ppho) - 1) %>%
        select(-n_ppho) %>%
        mutate(perc_increase = paste(round(perc_increase * 100, 2), " %"))

    res_cat_porp <- map_dfr(res_list, ~ {
        # Extract the "string_areas_snc" element from each sublist
        string_areas <- .x$string_areas_snc
        # Convert to tibble
        as_tibble(t(string_areas))
    }, .id = "scenario") %>%
        tidyr::unnest(cols = everything()) %>%
        select(scenario, rev(everything())) %>%
        select(-scenario)
    return(list(res_areas_porp, res_cat_porp))
}

nice_table_seals <- function(res_list = NULL) {
    res_areas_shund <- sapply(res_list, \(x) x$string_AWZ, simplify = TRUE)
    res_cat_shund <- map_dfr(res_list, ~ {
        # Extract the "string_areas_snc" element from each sublist
        string_areas <- .x$string_areas_snc
        # Convert to tibble
        as_tibble(t(string_areas))
    }, .id = "scenario") %>%
        tidyr::unnest(cols = everything()) %>%
        select(scenario, rev(everything())) %>%
        select(-c(scenario))
    return(list(res_areas_shund, res_cat_shund))
}

nice_table_zwerg <- function(res_list, EEZ_result = FALSE) {
    if (EEZ_result) {
        val_erase <- "n_ppho"
    } else {
        val_erase <- "string_pop"
    }
    res_areas_zwerg_d <- sapply(res_list, \(x) x %>%
        purrr::discard(names(.) %in% c("string_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
        tibble::as_tibble(), simplify = TRUE) %>%
        t() %>%
        as.data.frame() %>%
        mutate(n_ppho = as.numeric(n_ppho)) %>%
        mutate(perc_increase = n_ppho / lag(n_ppho) - 1)
    return(res_areas_zwerg_d[, colnames(res_areas_zwerg_d) != val_erase])
}


# Load the divided routes for each scenario

all_routes <- list(
    "40Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_40GW.shp")),
    "50Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_50GW.shp")),
    "60Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_60GW.shp")),
    "70Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_70GW.shp"))
)
all_routesC <- lapply(all_routes, \(x) filter(x, ZONE == "COASTAL"))
all_routesE <- lapply(all_routes, \(x) filter(x, ZONE == "AWZ"))
all_routesNG <- lapply(all_routes, \(x) filter(x, ZONE == "AUSLAND"))


# load the two big density maps for porpoises and minkes

porpoise_dens <- st_read(file.path(root, "sensitivity_NaMaRo", "density surfaces", "ppho_5x5_grid_2016_predictions.shp"))
zwergwal_dens <- st_read(file.path(root, "sensitivity_NaMaRo", "density surfaces", "bacu_10x10_grid_SCANSIII_predictions.shp"))
porpoise_dens <- st_transform(porpoise_dens, crs = 3035)
zwergwal_dens <- st_transform(zwergwal_dens, crs = 3035)

porpoise_dens$Vorkommen <- porpoise_dens$AvgSu
zwergwal_dens$Vorkommen <- zwergwal_dens$density
zwergwal_dens$density <- NULL

# cut the densities to the max BBox
max_bbox <- sapply(all_routes, st_bbox) |> apply(1, range)
max_bbox <- c(max_bbox[1, 1], max_bbox[1, 2], max_bbox[2, 3], max_bbox[2, 4]) + c(-40000, 15000, -15000, 15000)

# cut the densities to the max BBox
porpoise_dens <- porpoise_dens %>% st_crop(max_bbox)
zwergwal_dens <- zwergwal_dens %>% st_crop(max_bbox)

# load the porpoise sensitivity and EEZ map
porpoise_sensitivity <- st_read(all_files[grep("Schweinswal_Bauphase.shp", all_files)])
grid <- porpoise_sensitivity %>%
    select("CELLCOD", "geometry")
# all german waters
porpoise_density_EEZ_C <- st_read(all_files[grep("20240503_harbour_porpoise_density.shp", all_files)])
German_EEZ_poly <- st_read(r"(Q:\itaw\Open\Shapes\EEZ\awz-12sm-grenze_l-geo-n_POLYGON.shp)") %>% st_transform(crs = 3035)
# Only EEZ
porpoise_density_EEZ <- st_intersection(porpoise_density_EEZ_C, German_EEZ_poly)
# Only COastal
porpoise_density_C <- st_difference(porpoise_density_EEZ_C, German_EEZ_poly)
# Set crs for all for them
st_crs(porpoise_density_EEZ_C) <- st_crs(porpoise_density_EEZ) <- st_crs(porpoise_density_C) <- 3035
# correct names
colnames(porpoise_density_EEZ_C)[1] <- colnames(porpoise_density_EEZ)[1] <- colnames(porpoise_density_C)[1] <- "CELLCOD"

## Are of special porpoise importance
porpoise_special <- st_read(all_files[grep("Fortpflanzungsgebiet_Schweinswale_p-geo-n.shp", all_files)]) %>%
    st_transform(crs = 3035)

# load the zwergwal density for the EEZ
Zwergwal_densityEEZ_C <- st_read(all_files[grep("20240503_minke_whale_density.shp", all_files)])
st_crs(Zwergwal_densityEEZ_C) <- 3035
Zwergwal_densityEEZ_C <- st_transform(Zwergwal_densityEEZ_C, crs = 3035)
# Fill the NAs cells of the density with the mean of the surronding area
Zwer_NAs_buffered <- st_buffer(Zwergwal_densityEEZ_C[is.na(Zwergwal_densityEEZ_C$Vorkommen), ], dist = 10000)
Zwer_NAs_buffered_int <- Zwergwal_densityEEZ_C %>% st_intersection(Zwer_NAs_buffered, na.rm = T)
NAs_values <- Zwer_NAs_buffered_int %>%
    group_by(CELLCODE.1) %>%
    summarise("mean" = mean(Vorkommen, na.rm = TRUE))
Zwergwal_densityEEZ_C$Vorkommen[is.na(Zwergwal_densityEEZ_C$Vorkommen)] <- NAs_values$mean
Zwergwal_densityEEZ <- st_intersection(Zwergwal_densityEEZ_C, German_EEZ_poly)
Zwergwal_densityC <- st_difference(Zwergwal_densityEEZ_C, German_EEZ_poly)

st_crs(Zwergwal_densityEEZ_C) <- st_crs(Zwergwal_densityEEZ) <- st_crs(Zwergwal_densityC) <- 3035
colnames(Zwergwal_densityEEZ_C)[1] <- colnames(Zwergwal_densityEEZ)[1] <- colnames(Zwergwal_densityC)[1] <- "CELLCOD"
# Seehund
seehund_sensitivity <- grid %>%
    left_join(
        st_read(all_files[grep("Seehund_Betriebsphase.dbf", all_files)]) %>% st_drop_geometry(),
        by = "CELLCOD"
    )
# Kegelrobe
kegelrobbe_sensitivity <- st_read(all_files[grep("Kegelrobbe_Betriebsphase.shp", all_files)])

# General sensitivity map
Marine_sensitivity <- grid %>%
    left_join(st_read(all_files[grep("Schiffsverkehr zu OWP_fun1_meanmax_scaled_Gewichtung_0.dbf", all_files)]) %>% st_drop_geometry(),
        by = join_by("CELLCOD" == "CELLCODE")
    )
Marine_sensitivity$Vorkommen <- Marine_sensitivity$aggregiert
Marine_sensitivity$addr__G <- cut(Marine_sensitivity$aggregiert, c(1.4,1.9, 2.5, 3, 3.5, 4.1))

# load the example farms from the operation phase  and the porpoiseSA and the german EEZ
example_farms <- st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Example_Farms_manual_placing_operationphase.shp"))
porpoise_special <- st_read(all_files[grep("Fortpflanzungsgebiet_Schweinswale_p-geo-n.shp", all_files)]) %>%
    st_transform(crs = 3035)
# German_EEZ <- st_read(r"(Q:\itaw\Open\Shapes\EEZ\awz-12sm-grenze_l-geo-n.shp)") %>% st_transform(crs = 3035)
library(sf)

Sys.setenv(SHAPE_RESTORE_SHX = "YES")

German_EEZ <- st_read(r"(D:\proyect impact wind farms\ITAW - Acoumetry - DEMASK - density surfaces\awz-12sm-grenze_l-geo-n.shp)")
# Plots
library(ggnewscale)
plot_routes_porp <- plot_scenarios(all_routes, sensitivity = porpoise_dens, density = TRUE, porpoiseSA = TRUE)
plot_routes_zwer <- plot_scenarios(all_routes, sensitivity = zwergwal_dens, density = TRUE, porpoiseSA = FALSE)
plot_routes_seeh <- plot_scenarios(all_routesE, sensitivity = seehund_sensitivity, density = FALSE, porpoiseSA = FALSE)
plot_routes_kege <- plot_scenarios(all_routesE, sensitivity = kegelrobbe_sensitivity, density = FALSE, porpoiseSA = FALSE)
plot_routes_mars <- plot_scenarios(all_routesE, sensitivity = Marine_sensitivity, density = FALSE, porpoiseSA = FALSE)

# Add new elements to each plot

for (i in seq_along(plot_routes_porp)) {
    plot_routes_porp[[i]] <- plot_routes_porp[[i]] +
        # Add your new line layer
        geom_sf(data = German_EEZ, color = "black", size = 1) +
        # Reset the fill scale for the next layer
        new_scale_fill() +
        # Add scenario layer with new fill aesthetic
        geom_sf(data = all_routes[[i]], aes(fill = ZONE), color = NA, alpha = 1) +
        # Add the new fill scale for scenarios
        scale_fill_viridis_d(name = "Zone", option = "plasma", labels = c("Ausland", "AWZ", "Küstengewässer")) # or scale_fill_viridis_c() if continuous

    plot_routes_zwer[[i]] <- plot_routes_zwer[[i]] +
        # Add your new line layer
        geom_sf(data = German_EEZ, color = "black", size = 1) +
        scale_fill_viridis_c(breaks = c(0.01, 0.036), labels = scales::label_number(accuracy = 0.001)) +
        # Reset the fill scale for the next layer
        new_scale_fill() +
        # Add scenario layer with new fill aesthetic
        geom_sf(data = all_routes[[i]], aes(fill = ZONE), color = NA, alpha = 1) +
        # Add the new fill scale for scenarios
        scale_fill_viridis_d(name = "Zone", option = "plasma", labels = c("Ausland", "AWZ", "Küstengewässer")) # or scale_fill_viridis_c() if continuous
    plot_routes_seeh[[i]] <- plot_routes_seeh[[i]] +
        # Add your new line layer
        geom_sf(data = German_EEZ, color = "black", size = 1) +
        # Reset the fill scale for the next layer
        new_scale_fill() +
        # Add scenario layer with new fill aesthetic
        geom_sf(data = all_routesE[[i]], aes(fill = ZONE), color = NA, alpha = 1) +
        # Add the new fill scale for scenarios
        scale_fill_manual(values = c("AWZ" = "#cc4678"), labels = c("AWZ"))
    plot_routes_kege[[i]] <- plot_routes_kege[[i]] +
        # Add your new line layer
        geom_sf(data = German_EEZ, color = "black", size = 1) +
        # Reset the fill scale for the next layer
        new_scale_fill() +
        # Add scenario layer with new fill aesthetic
        geom_sf(data = all_routesE[[i]], aes(fill = ZONE), color = NA, alpha = 1) +
        # Add the new fill scale for scenarios
        scale_fill_manual(values = c("AWZ" = "#cc4678"), labels = c("AWZ")) # or scale_fill_viridis_c() if continuous(name = "Zone", option = "plasma", labels = c( "AWZ"))

    plot_routes_mars[[i]] <- plot_routes_mars[[i]] +
        labs(fill = "Gesamtsensivitätindex") +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
        # Add your new line layer
        geom_sf(data = German_EEZ, color = "black", size = 1) +
        # Reset the fill scale for the next layer
        new_scale_fill() +
        # Add scenario layer with new fill aesthetic
        geom_sf(data = all_routesE[[i]], aes(fill = ZONE), color = NA, alpha = 1) +
        # Add the new fill scale for scenarios
        scale_fill_manual(values = c("AWZ" = "#cc4678"), labels = c("AWZ")) # or scale_fill_viridis_c() if continuous(name = "Zone", option = "plasma", labels = c( "AWZ"))

    if (i != 1) {
        plot_routes_porp[[i]] <- plot_routes_porp[[i]] + theme(legend.position = "none")
        plot_routes_zwer[[i]] <- plot_routes_zwer[[i]] + theme(legend.position = "none")
        plot_routes_seeh[[i]] <- plot_routes_seeh[[i]] + theme(legend.position = "none")
        plot_routes_kege[[i]] <- plot_routes_kege[[i]] + theme(legend.position = "none")
        plot_routes_mars[[i]] <- plot_routes_mars[[i]] + theme(legend.position = "none")
    }
}

all_plots <- list(
    plot_routes_porp,
    plot_routes_zwer,
    plot_routes_seeh,
    plot_routes_kege,
    plot_routes_mars
)

names(all_plots) <- c(
    "Routen_porp_plot", "Routen_zwerg_plot", "Routen_sehnd_plot",
    "Routen_kgrob_plot", "Routen_mar_plot"
)

for (i in seq_along(all_plots)) {
    print(paste0("doing: ", names(all_plots)[i]))
    ggsave(
        filename = paste0(names(all_plots)[i], ".png"),
        plot = all_plots[[i]], path = file.path(root, "scenarios_NaMaRo", "results"),
        width = 60,
        height = 50,
        units = "cm",
        dpi = 300
        # device = svglite::svglite
    )
}
## NUmerical results
# coastal
porp_route_resC <- lapply(all_routesC, \(x)  calculate_impact(scenario = x, density = porpoise_density_C, porpoiseSA = FALSE, tot_population = 41532.3))
zwerg_route_resC <- lapply(all_routesC, \(x)  calculate_impact(scenario = x, density = Zwergwal_densityC, porpoiseSA = FALSE, tot_population = 195.35))
#  EEZ
porp_route_resE <- lapply(all_routesE, \(x)  calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density_EEZ, porpoiseSA = TRUE, tot_population = 41532.3))
zwerg_route_resE <- lapply(all_routesE, \(x)  calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = Zwergwal_densityEEZ, porpoiseSA = FALSE, tot_population = 195.35))
seeh_route_resE <- lapply(all_routesE, \(x)  calculate_impact(scenario = x, sensitivity = seehund_sensitivity, porpoiseSA = FALSE))
keg_route_resE <- lapply(all_routesE, \(x)  calculate_impact(scenario = x, sensitivity = kegelrobbe_sensitivity, porpoiseSA = FALSE))
marine_route_resE <- lapply(all_routesE, \(x)  calculate_impact(scenario = x, sensitivity = Marine_sensitivity, porpoiseSA = FALSE, marine = TRUE))
#  Non-german-waters
porp_route_resNG <- lapply(all_routesNG, \(x)  calculate_impact(scenario = x, density = porpoise_dens, porpoiseSA = FALSE))
zwerg_route_resNG <- lapply(all_routesNG, \(x)  calculate_impact(scenario = x, density = zwergwal_dens, porpoiseSA = FALSE))

## tables
# EEZ results
porpoises_results_EEZ <- nice_table_p(porp_route_resE)


zwergwal_results_EEZ <- nice_table_zwerg(zwerg_route_resE, EEZ_result = TRUE) %>%
    mutate(
        area_AWZ = paste(area_AWZ, " km\u00B2"),
        perc_increase = paste(round(perc_increase * 100, 2), "%")
    )

seehund_results <- nice_table_seals(seeh_route_resE)
kegelrobbe_results <- nice_table_seals(keg_route_resE)
marine_results <- nice_table_seals(marine_route_resE)

# Coastal waters
# the area of the coastal waters is 11939 km2 the population of porpoise is ~ 6005
# the population of minkes is ~ 14
porpoises_results_C <- nice_table_zwerg(porp_route_resC, EEZ_result = TRUE) %>%
    mutate(
        area_AWZ = paste(area_AWZ, "km\u00B2"),
        perc_increase = paste(round(perc_increase * 100, 2), "%")
    )
zwergwal_results_C <- nice_table_zwerg(zwerg_route_resC, EEZ_result = TRUE) %>%
    mutate(
        area_AWZ = paste(area_AWZ, " km\u00B2"),
        perc_increase = paste(round(perc_increase * 100, 2), "%")
    )
# Non EEZ results (only porpoise and zwergwal)
porpoises_results_noEEZ <- nice_table_zwerg(porp_route_resNG, EEZ_result = FALSE) %>%
    mutate(
        area_AWZ = paste(area_AWZ, "km\u00B2"),
        perc_increase = paste(round(perc_increase * 100, 2), "%")
    )
zwergwal_results_noEEZ <- nice_table_zwerg(zwerg_route_resNG, EEZ_result = FALSE) %>%
    mutate(
        area_AWZ = paste(area_AWZ, " km\u00B2"),
        perc_increase = paste(round(perc_increase * 100, 2), "%")
    )


# Write in Excel file
# load excel
# res_exc <- loadWorkbook(file.path(root, "scenarios_NaMaRo", "results", "Numerical Results 2backup.xlsx"))
# res_exc <- loadWorkbook(r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)")
res_exc <- loadWorkbook(r"(C:\Users\felip\Desktop\Numerical Results 2backup.xlsx)")
### PORPOISE
# porpoise EEZ
writeData(res_exc, sheet = "porpoise_s", x = porpoises_results_EEZ[[1]], startRow = 28, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_s", x = porpoises_results_EEZ[[2]], startRow = 34, startCol = 2, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# porpoise coastal
writeData(res_exc,
    sheet = "porpoise_s",
    x = porpoises_results_C,
    startRow = 41, startCol = 3,
    rowNames = FALSE, colNames = FALSE, headerStyle = NULL
)
# porpoise Non german waters
writeData(res_exc,
    sheet = "porpoise_s",
    x = porpoises_results_noEEZ,
    startRow = 48, startCol = 3,
    rowNames = FALSE, colNames = FALSE, headerStyle = NULL
)

# seehund
writeData(res_exc, sheet = "seehund", x = seehund_results[[1]], startRow = 17, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "seehund", x = seehund_results[[2]][, 1:4], startRow = 17, startCol = 4, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# kegelrobbe
writeData(res_exc, sheet = "kegelrobe", x = kegelrobbe_results[[1]], startRow = 17, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "kegelrobe", x = kegelrobbe_results[[2]], startRow = 17, startCol = 7, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
## ZWERGWAL
# EEZ
writeData(res_exc, sheet = "Zwergwal", x = zwergwal_results_EEZ, startRow = 19, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# COASTAL
writeData(res_exc,
    sheet = "Zwergwal",
    x = zwergwal_results_C,
    startRow = 25, startCol = 3,
    rowNames = FALSE, colNames = FALSE, headerStyle = NULL
)
# Non german waters
writeData(res_exc,
    sheet = "Zwergwal",
    x = zwergwal_results_noEEZ,
    startRow = 31, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL
)

# Marine
writeData(res_exc, sheet = "Marine_mammals", x = marine_results[[1]], startRow = 17, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "Marine_mammals", x = marine_results[[2]], startRow = 17, startCol = 4, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)

# saveWorkbook(res_exc, file.path(root, "scenarios_NaMaRo", "results", "Numerical Results 2backup.xlsx"), overwrite = TRUE)
# saveWorkbook(res_exc, r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
saveWorkbook(res_exc, r"(C:\Users\felip\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
