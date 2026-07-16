library(sf)
library(dplyr)
# library(pelaDSM)
library(purrr)
library(stringr)
library(ggplot2)
# library(rnaturalearth)
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

## Betriebsphase



# Load sensitivity maps
# Porpoise
porpoise_sensitivity <- st_read(all_files[grep("Schweinswal_Betriebsphase.shp", all_files)])
porpoise_density <- st_read(all_files[grep("20240503_harbour_porpoise_density.shp", all_files)])
German_EEZ <- st_read(r"(Q:\itaw\Open\Shapes\EEZ\awz-12sm-grenze_l-geo-n_POLYGON.shp)") %>% st_transform(crs = 3035)
# porpoise_density <- st_intersection(porpoise_density, German_EEZ)
st_crs(porpoise_density) <- 3035
colnames(porpoise_density)[1] <- "CELLCOD"
porpoise_special <- st_read(all_files[grep("Fortpflanzungsgebiet_Schweinswale_p-geo-n.shp", all_files)]) %>%
    st_transform(crs = 3035)
# extract the grid from the porpoise density to be used with the others sentivities.
grid <- porpoise_sensitivity %>%
    select("CELLCOD", "geometry")
# Seehund
seehund_sensitivity <- grid %>%
    left_join(
        st_read(all_files[grep("Seehund_Betriebsphase.dbf", all_files)]) %>% st_drop_geometry(),
        by = "CELLCOD"
    )
# Kegelrobe
kegelrobbe_sensitivity <- st_read(all_files[grep("Kegelrobbe_Betriebsphase.shp", all_files)])
# Zwergwal
# Zwergwal
Zwergwal_density <- st_read(all_files[grep("20240503_minke_whale_density.shp", all_files)])

# # Fill the NAs cells of the density with the mean of the surronding area
Zwer_NAs_buffered <- st_buffer(Zwergwal_density[is.na(Zwergwal_density$Vorkommen), ], dist = 10000)
Zwer_NAs_buffered_int <- Zwergwal_density %>% st_intersection(Zwer_NAs_buffered, na.rm = T)
NAs_values <- Zwer_NAs_buffered_int %>%
    group_by(CELLCODE.1) %>%
    summarise("mean" = mean(Vorkommen, na.rm = TRUE))
Zwergwal_density$Vorkommen[is.na(Zwergwal_density$Vorkommen)] <- NAs_values$mean
st_crs(Zwergwal_density) <- 3035
colnames(Zwergwal_density)[1] <- "CELLCOD"
# Zwergwal_density <- st_intersection(Zwergwal_density, German_EEZ)

# General sensitivity map # there is nothing for construction_phase?
Marine_sensitivity <- grid %>%
    left_join(st_read(all_files[grep("Betriebsphase_fun1_meanmax_scaled_Gewichtung_0.dbf", all_files)]) %>% st_drop_geometry(),
        by = join_by("CELLCOD" == "CELLCODE")
    )
Marine_sensitivity$Vorkommen <- Marine_sensitivity$aggregiert
Marine_sensitivity$addr__G <- cut(Marine_sensitivity$aggregiert, c(1.4,1.9, 2.5, 3, 3.5, 4.1))



# load the  buffered area for the construction phase, this area has the toy farms of the construction phase
# as well as the current FEP shapes of the already operating windfarms
construction_phase <- st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Betriebsphase_Buffers_all_mock_farms_plus_operational.shp"))
example_farms <- st_read(file.path(root, "scenarios_NaMaRo", "shapes", "Example_Farms_manual_placing_operationphase.shp"))
btrbs_scn <- list(
    "40Gw" = filter(construction_phase, !(NaMaRo %in% c(40, 50, 60))),
    "50Gw" = filter(construction_phase, !(NaMaRo %in% c(50, 60))),
    "60Gw" = filter(construction_phase, NaMaRo != 60),
    "70Gw" = construction_phase
)

btrbs_porp_plot <- plot_scenarios(btrbs_scn,
    sensitivity = porpoise_sensitivity,
    density = FALSE, porpoiseSA = TRUE
)
btrbs_porp_plot_d <- plot_scenarios(btrbs_scn, sensitivity = porpoise_density, density = TRUE, porpoiseSA = TRUE)
btrbs_zwerg_plot <- plot_scenarios(btrbs_scn, sensitivity = Zwergwal_density, density = TRUE, porpoiseSA = FALSE)
btrbs_sehnd_plot <- plot_scenarios(btrbs_scn, sensitivity = seehund_sensitivity, density = FALSE, porpoiseSA = FALSE)
btrbs_kgrob_plot <- plot_scenarios(btrbs_scn, sensitivity = kegelrobbe_sensitivity, density = FALSE, porpoiseSA = FALSE)
btrbs_mar_plot <- plot_scenarios(btrbs_scn, sensitivity = Marine_sensitivity, density = FALSE, porpoiseSA = FALSE)

all_plots_btrbs <- list(
    btrbs_porp_plot, btrbs_zwerg_plot,
    btrbs_porp_plot_d, btrbs_sehnd_plot,
    btrbs_kgrob_plot, btrbs_mar_plot
)
names(all_plots_btrbs) <- c(
    "btrbs_porp_plot", "btrbs_zwerg_plot",
    "btrbs_porp_plot_d", "btrbs_sehnd_plot",
    "btrbs_kgrob_plot", "btrbs_mar_plot"
)

for (i in seq_along(all_plots_btrbs)) {
    print(paste0("doing: ", names(all_plots_btrbs)[i]))
    ggsave(
        filename = paste0(names(all_plots_btrbs)[i], ".png"),
        plot = all_plots_btrbs[[i]], path = file.path(root, "scenarios_NaMaRo", "results"),
        width = 60,
        height = 40,
        units = "cm",
        dpi = 300
        # device = svglite::svglite
    )
}

# NUMERICAL Results

porpoise_bau_res_s <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density, porpoiseSA = TRUE))
porpoise_bau_res_d <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density, porpoiseSA = TRUE))
Zwergwal_bau_res <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = Zwergwal_density, porpoiseSA = FALSE))
seehund_bau_res <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = seehund_sensitivity))
kegelrobbe_bau_res <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = kegelrobbe_sensitivity))
Marine_bau_res <- lapply(btrbs_scn, \(x) calculate_impact(scenario = x, sensitivity = Marine_sensitivity, marine = TRUE))

## Save into excel
# load excel
# res_exc <- loadWorkbook(file.path(root, "scenarios_NaMaRo", "results", "Numerical Results 2.xlsx"))
# res_exc <- loadWorkbook(r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)")
res_exc <- loadWorkbook(r"(C:\Users\felip\Desktop\Numerical Results 2backup.xlsx)")

# porpoise based on sensitivity part a
res_areas_porp <- sapply(porpoise_bau_res_s, \(x) x %>%
    purrr::discard(names(.) %in% c("area_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
    tibble::as_tibble(), simplify = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(n_ppho = as.numeric(n_ppho)) %>%
    mutate(perc_increase = n_ppho / lag(n_ppho) - 1) %>%
    select(-n_ppho)

# porpoise based on sensitivity part b
res_cat_porp <- map_dfr(porpoise_bau_res_s, ~ {
    # Extract the "string_areas_snc" element from each sublist
    string_areas <- .x$string_areas_snc
    # Convert to tibble
    as_tibble(t(string_areas))
}, .id = "scenario") %>%
    tidyr::unnest(cols = everything()) %>%
    select(scenario, rev(everything())) %>%
    select(-scenario)

# porpoise based on density part a
res_areas_porp_d <- sapply(porpoise_bau_res_d, \(x) x %>%
    purrr::discard(names(.) %in% c("area_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
    tibble::as_tibble(), simplify = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(n_ppho = as.numeric(n_ppho)) %>%
    mutate(perc_increase = n_ppho / lag(n_ppho) - 1) %>%
    select(-n_ppho)

# porpoise based on sensitivity part b
res_cat_porp_d <- map_dfr(porpoise_bau_res_d, ~ {
    # Extract the "string_areas_snc" element from each sublist
    string_areas <- .x$string_areas_snc
    # Convert to tibble
    as_tibble(t(string_areas))
}, .id = "scenario") %>%
    tidyr::unnest(cols = everything()) %>%
    select(scenario, rev(everything())) %>%
    select(-scenario)

# seehund only sensitivity
res_areas_shund <- sapply(seehund_bau_res, \(x) x$string_AWZ, simplify = TRUE)
res_cat_shund <- map_dfr(seehund_bau_res, ~ {
    # Extract the "string_areas_snc" element from each sublist
    string_areas <- .x$string_areas_snc
    # Convert to tibble
    as_tibble(t(string_areas))
}, .id = "scenario") %>%
    tidyr::unnest(cols = everything()) %>%
    select(scenario, rev(everything())) %>%
    select(-c(scenario))

# kegelrobe only sensitivity
res_areas_krob <- sapply(kegelrobbe_bau_res, \(x) x$string_AWZ, simplify = TRUE)
res_cat_krob <- map_dfr(kegelrobbe_bau_res, ~ {
    # Extract the "string_areas_snc" element from each sublist
    string_areas <- .x$string_areas_snc
    # Convert to tibble
    as_tibble(t(string_areas))
}, .id = "scenario") %>%
    tidyr::unnest(cols = everything()) %>%
    select(scenario, rev(everything())) %>%
    select(-scenario)

# Zwergwal only density
res_areas_zwerg_d <- sapply(Zwergwal_bau_res, \(x) x %>%
    purrr::discard(names(.) %in% c("area_AWZ", "perc_AWZ", "area_HKG", "perc_HKG", "perc_pop", "string_areas_snc")) %>%
    tibble::as_tibble(), simplify = TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(n_ppho = as.numeric(n_ppho)) %>%
    mutate(perc_increase = n_ppho / lag(n_ppho) - 1) %>%
    select(-n_ppho)

# Marine mammals
res_areas_mar_mm <- sapply(Marine_bau_res, \(x) x$string_AWZ, simplify = TRUE)
res_cat_mar_mm <- map_dfr(Marine_bau_res, ~ {
    # Extract the "string_areas_snc" element from each sublist
    string_areas <- .x$string_areas_snc
    # Convert to tibble
    as_tibble(t(string_areas))
}, .id = "scenario") %>%
    tidyr::unnest(cols = everything()) %>%
    select(scenario, rev(everything())) %>%
    select(-scenario)
# View(res_cat_mar_mm)
# write results to the workbook
# porpoise
writeData(res_exc, sheet = "porpoise_s", x = res_areas_porp, startRow = 15, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_s", x = res_cat_porp, startRow = 21, startCol = 2, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_d", x = res_areas_porp_d, startRow = 15, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_d", x = res_cat_porp_d, startRow = 21, startCol = 2, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# seehund
writeData(res_exc, sheet = "seehund", x = res_areas_shund, startRow = 10, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "seehund", x = res_cat_shund, startRow = 10, startCol = 5, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# kegelrobbe
writeData(res_exc, sheet = "kegelrobe", x = res_areas_krob, startRow = 10, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "kegelrobe", x = res_cat_krob, startRow = 10, startCol = 7, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# Zwergwal
writeData(res_exc, sheet = "Zwergwal", x = res_areas_zwerg_d, startRow = 11, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# Marine
writeData(res_exc, sheet = "Marine_mammals", x = res_areas_mar_mm, startRow = 10, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "Marine_mammals", x = res_cat_mar_mm, startRow = 10, startCol = 4, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)

# saveWorkbook(res_exc, r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
saveWorkbook(res_exc, r"(C:\Users\felip\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
