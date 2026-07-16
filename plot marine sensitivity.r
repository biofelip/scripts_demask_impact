#plot marine sensitivity 
library(ggplo2)
library(sf)
# Plot marine sensitivity construction phase

Marine_sensitivity <- grid %>%
    left_join(st_read(all_files[grep("Bauphase_fun1_meanmax_scaled_Gewichtung_0.dbf", all_files)]) %>% st_drop_geometry(),
        by = join_by("CELLCOD" == "CELLCODE")
    )
Marine_sensitivity$Vorkommen <- Marine_sensitivity$aggregiert
# new approch: cut using quantiles
Marine_sensitivity$addr__G <- cut(Marine_sensitivity$aggregiert, breaks = quantile(Marine_sensitivity$aggregiert, probs = seq(0, 1, by = 0.25), na.rm = TRUE), include.lowest = TRUE)
Marine_sensitivity  %>% 
    ggplot() +
    geom_sf(aes(fill = addr__G), colour = NA) +
    scale_fill_brewer(palette = "RdBu",direction = -1)+
    theme_minimal()

