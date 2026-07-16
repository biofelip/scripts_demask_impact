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
root <- r"(C:\Users\247404\TiHo Hannover\ITAW - Acoumetry - DEMASK - DEMASK)"
all_files <- list.files(path = root, pattern = ".*\\.(shp|dbf)$", full.names = TRUE, recursive = TRUE)

source("utils.R")



# Load the buffers and toy the wind farms

buffers <- st_read(all_files[grep("Buffers_all_mock_farms_without_operational.shp", all_files)]) %>%
    mutate(FID = 1:nrow(.))

example_farms <- st_read(all_files[grep("Example_Farms_manual_placing_without operational.shp", all_files)])

# Load sensitivity maps
# Porpoise
porpoise_sensitivity <- st_read(all_files[grep("Schweinswal_Bauphase.shp", all_files)])
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
        st_read(all_files[grep("Seehund_Bauphase.dbf", all_files)]) %>% st_drop_geometry(),
        by = "CELLCOD"
    )
# Kegelrobe
kegelrobbe_sensitivity <- grid %>%
    left_join(st_read(all_files[grep("Kegelrobbe_Bauphase.dbf", all_files)]) %>% st_drop_geometry(),
        by = "CELLCOD"
    )
# Zwergwal
Zwergwal_density <- st_read(all_files[grep("20240503_minke_whale_density.shp", all_files)])

# Fill the NAs cells of the density with the mean of the surronding area
Zwer_NAs_buffered <- st_buffer(Zwergwal_density[is.na(Zwergwal_density$Vorkommen), ], dist = 10000)
Zwer_NAs_buffered_int <- Zwergwal_density %>% st_intersection(Zwer_NAs_buffered, na.rm = T)
NAs_values <- Zwer_NAs_buffered_int %>%
    group_by(CELLCODE.1) %>%
    summarise("mean" = mean(Vorkommen, na.rm = TRUE))
Zwergwal_density$Vorkommen[is.na(Zwergwal_density$Vorkommen)] <- NAs_values$mean
st_crs(Zwergwal_density) <- 3035
colnames(Zwergwal_density)[1] <- "CELLCOD"
# Zwergwal_density <- st_intersection(Zwergwal_density, German_EEZ)
# General sensitivity map
Marine_sensitivity <- grid %>%
    left_join(st_read(all_files[grep("Bauphase_fun1_meanmax_scaled_Gewichtung_0.dbf", all_files)]) %>% st_drop_geometry(),
        by = join_by("CELLCOD" == "CELLCODE")
    )
Marine_sensitivity$Vorkommen <- Marine_sensitivity$aggregiert

# new approch: cut using quantiles
# Marine_sensitivity$addr__G <- cut(Marine_sensitivity$aggregiert, breaks = quantile(Marine_sensitivity$aggregiert, probs = seq(0, 1, by = 0.25), na.rm = TRUE), include.lowest = TRUE)
# old approach cut using prefefined breaks
Marine_sensitivity$addr__G <- cut(Marine_sensitivity$aggregiert, c(1.9, 2.5, 3, 3.5, 4.1))

## plot and compare approaches if needed
# windows()
# Marine_sensitivity |>ggplot(aes(x=Vorkommen)) +
#   geom_histogram()+
#   geom_vline(xintercept = c(2, 2.7, 3.3, 3.5, 4.1), color = "red")+
#   ggtitle("Division with quantiles")

# Marine_sensitivity %>%
#   group_by(addr__G) %>%
#   summarise(area = sum(as.numeric(st_area(geometry)), na.rm = TRUE)) %>%
#   mutate(
#     total_area = sum(area),
#     pct_area   = (area / total_area) * 100
#   )

###### GREEDY APPROACH


# porpoise_scn_s <- greedy_search(sensitivity = porpoise_sensitivity, buffers = buffers)
# porpoise_scn_d <- greedy_search(sensitivity = porpoise_density, buffers = buffers, density = TRUE)
# seehund_scn <- greedy_search(sensitivity = seehund_sensitivity, buffers = buffers)
# kegelrobbe_scn <- greedy_search(sensitivity = kegelrobbe_sensitivity, buffers = buffers)
# Zwergwal_scn <- greedy_search(sensitivity = Zwergwal_density, buffers = buffers, density = TRUE)
# Marine_scn <- greedy_search(sensitivity = Marine_sensitivity, buffers = buffers)

# ## The scenarios take really long to calculate so lets save them
# saveRDS(porpoise_scn_s, file = file.path(root, "scenarios_NaMaRo", "results", "porpoise_scn_s.rds"))
# saveRDS(porpoise_scn_d, file = file.path(root, "scenarios_NaMaRo", "results", "porpoise_scn_d.rds"))
# saveRDS(seehund_scn, file = file.path(root, "scenarios_NaMaRo", "results", "seehund_scn.rds"))
# saveRDS(kegelrobbe_scn, file = file.path(root, "scenarios_NaMaRo", "results", "kegelrobbe_scn.rds"))
# saveRDS(Zwergwal_scn, file = file.path(root, "scenarios_NaMaRo", "results", "Zwergwal_scn.rds"))
# saveRDS(Marine_scn, file = file.path(root, "scenarios_NaMaRo", "results", "Marine_scn.rds"))

# load rds from previus session if needed
porpoise_scn_s <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "porpoise_scn_s.rds"))
porpoise_scn_d <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "porpoise_scn_d.rds"))
seehund_scn <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "seehund_scn.rds"))
kegelrobbe_scn <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "kegelrobbe_scn.rds"))
Zwergwal_scn <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "Zwergwal_scn.rds"))
Marine_scn <- readRDS(file = file.path(root, "scenarios_NaMaRo", "results", "Marine_scn.rds"))
## scenarion names
names(porpoise_scn_s) <- names(porpoise_scn_d) <- names(seehund_scn) <- names(kegelrobbe_scn) <- names(Zwergwal_scn) <- names(Marine_scn) <- c("40Gw", "50Gw", "60Gw", "70Gw")

# plot everythin


porpoise_scn_s_plot <- plot_scenarios(porpoise_scn_s, sensitivity = porpoise_sensitivity, porpoiseSA = TRUE)
porpoise_scn_d_plot <- plot_scenarios(porpoise_scn_d, sensitivity = porpoise_density, porpoiseSA = TRUE, density = TRUE)
seehund_scn_plot <- plot_scenarios(seehund_scn, sensitivity = seehund_sensitivity)
kegelrobbe_scn_plot <- plot_scenarios(kegelrobbe_scn, sensitivity = kegelrobbe_sensitivity)
Zwergwal_scn_plot <- plot_scenarios(Zwergwal_scn, sensitivity = Zwergwal_density, density = TRUE)
Marine_scn_plot <- plot_scenarios(Marine_scn, sensitivity = Marine_sensitivity, density = FALSE)

all_plots_bau <- list(
    porpoise_scn_s_plot, porpoise_scn_d_plot, seehund_scn_plot,
    kegelrobbe_scn_plot, Zwergwal_scn_plot, Marine_scn_plot
)
names(all_plots_bau) <- c(
    "bauph_porpoise_plot", "bauph_porpoise_d_plot", "bauph_seehund_plot",
    "bauph_kegelrobbe_plot", "bauph_Zwergwal_plot", "bauph_Marine_plot"
)


for (i in 1:length(all_plots_bau)) {
    print(paste0("doing: ", names(all_plots_bau)[i]))
    ggsave(
        filename = paste0(names(all_plots_bau)[i], ".svg"),
        plot = all_plots_bau[[i]], path = file.path(root, "scenarios_NaMaRo", "results"),
        width = 40,
        height = 40,
        units = "cm",
        dpi = 300,
        device = svglite::svglite
    )
}



# #### NUMERICAL RESULTS
porpoise_bau_res_s <- lapply(porpoise_scn_s, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density, porpoiseSA = TRUE))
porpoise_bau_res_d <- lapply(porpoise_scn_d, \(x) calculate_impact(scenario = x, sensitivity = porpoise_sensitivity, density = porpoise_density, porpoiseSA = TRUE))
Zwergwal_bau_res <- lapply(Zwergwal_scn, \(x) calculate_impact(scenario = x, density = Zwergwal_density, porpoiseSA = FALSE))
seehund_bau_res <- lapply(seehund_scn, \(x) calculate_impact(scenario = x, sensitivity = seehund_sensitivity))
kegelrobbe_bau_res <- lapply(kegelrobbe_scn, \(x) calculate_impact(scenario = x, sensitivity = kegelrobbe_sensitivity))
Marine_bau_res <- lapply(Marine_scn, \(x) calculate_impact(scenario = x, sensitivity = Marine_sensitivity, marine = TRUE))

## Save into excel
# load excel
# res_exc <- loadWorkbook(file.path(root, "scenarios_NaMaRo", "results", "Numerical Results 2.xlsx"))
res_exc <- loadWorkbook(r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)")
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
    select(-c(scenario, "category 1", "category 0"))

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

# write results to the workbook
# porpoise
writeData(res_exc, sheet = "porpoise_s", x = res_areas_porp, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_s", x = res_cat_porp, startRow = 8, startCol = 2, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_d", x = res_areas_porp_d, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "porpoise_d", x = res_cat_porp_d, startRow = 8, startCol = 2, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# seehund
writeData(res_exc, sheet = "seehund", x = res_areas_shund, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "seehund", x = res_cat_shund, startRow = 2, startCol = 5, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# kegelrobbe
writeData(res_exc, sheet = "kegelrobe", x = res_areas_krob, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "kegelrobe", x = res_cat_krob, startRow = 2, startCol = 6, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# Zwergwal
writeData(res_exc, sheet = "Zwergwal", x = res_areas_zwerg_d, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
# Marine
writeData(res_exc, sheet = "Marine_mammals", x = res_areas_mar_mm, startRow = 2, startCol = 3, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)
writeData(res_exc, sheet = "Marine_mammals", x = res_cat_mar_mm, startRow = 2, startCol = 4, rowNames = FALSE, colNames = FALSE, headerStyle = NULL)

saveWorkbook(res_exc, r"(C:\Users\247404\Desktop\Numerical Results 2backup.xlsx)", overwrite = TRUE)
