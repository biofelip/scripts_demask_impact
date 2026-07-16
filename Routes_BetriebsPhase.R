## Calculate the overlap between the routes and their farms in the operation phase
## populate the new table with the correct numbers

library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
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

# load the porpoise  density in the EEZ
porpoise_sensitivity <- st_read(all_files[grep("Schweinswal_Betriebsphase.shp", all_files)])
porpoise_density <- st_read(all_files[grep("20240503_harbour_porpoise_density.shp", all_files)])
st_crs(porpoise_density) <- 3035
colnames(porpoise_density)[1] <- "CELLCOD"

# load routes

all_routes <- list(
    "40Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_40GW.shp")),
    "50Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_50GW.shp")),
    "60Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_60GW.shp")),
    "70Gw" = st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Serviceverkehr_Routen_NaMaRo", "Divided_Serviceverkehr_Routen_70GW.shp"))
)
all_routesC <- lapply(all_routes, \(x) filter(x, ZONE == "COASTAL"))
all_routesE <- lapply(all_routes, \(x) filter(x, ZONE == "AWZ"))
all_routesNG <- lapply(all_routes, \(x) filter(x, ZONE == "AUSLAND"))

# load operation phase
construction_phase <- st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Betriebsphase_Buffers_all_mock_farms_plus_operational.shp"))
example_farms <- st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Example_Farms_manual_placing_operationphase.shp"))
btrbs_scn <- list(
    "40Gw" = filter(construction_phase, !(NaMaRo %in% c(40, 50, 60))),
    "50Gw" = filter(construction_phase, !(NaMaRo %in% c(50, 60))),
    "60Gw" = filter(construction_phase, NaMaRo != 60),
    "70Gw" = construction_phase
)

# intersect the area of the routes with their corresponding operation phase
routes_btrsb_int <- Map(st_intersection, all_routesE, btrbs_scn)
par(mfrow = c(2, 2))
plot(routes_btrsb_int[[1]]$geometry)
plot(routes_btrsb_int[[2]]$geometry)
plot(routes_btrsb_int[[3]]$geometry)
plot(routes_btrsb_int[[4]]$geometry)

# calculate the impact of the intersected areas
porpoise_bau__routes_res_d <- lapply(routes_btrsb_int, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density, porpoiseSA = FALSE))

overlap_results <- sapply(porpoise_bau__routes_res_d, \(x) x %>%
    purrr::discard(names(.) %in% c("area_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
    tibble::as_tibble(), simplify = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(n_ppho = as.numeric(n_ppho)) %>%
    mutate(perc_increase = n_ppho / lag(n_ppho) - 1) %>%
    select(-n_ppho)

ovelapp_indv <- sapply(porpoise_bau__routes_res_d, \(x) x$n_ppho)

# read excel and the result from the routes  and the operation phase

res_exc <- loadWorkbook(r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)")
operation_results <- readWorkbook(res_exc, sheet = "porpoise_s", rows = 14:18, cols = 1:6, colNames = TRUE)[, 5]
routes_results <- readWorkbook(res_exc, sheet = "porpoise_s", rows = 27:31, cols = 1:6, colNames = TRUE)[, 5]

operation_results <- sub("^(\\d+).*", "\\1", operation_results) |> as.numeric()
routes_results <- sub("^(\\d+).*", "\\1", routes_results) |> as.numeric()

# Total results
Total_results <- operation_results + routes_results
corrected_res <- Total_results - ovelapp_indv
pop_perc <- sapply(corrected_res, function(x) round(x / 41532.29, 3))
corrected_res_perc <- round((corrected_res / lag(corrected_res) - 1), 2)

# create data to populate the table

Affected_animals <- glue("{corrected_res} ({pop_perc*100} %)")

excel_results <- matrix(c(Affected_animals, corrected_res_perc, ovelapp_indv), nrow = 4, ncol = 3, byrow = FALSE)


writeData(res_exc, sheet = "porpoise_s", x = excel_results, startRow = 57, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
saveWorkbook(res_exc, r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
