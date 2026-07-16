create_buffer <- function(owf, sensitivity, already_existing, density = FALSE) {
    vertices <- owf %>%
        st_coordinates() %>%
        as_tibble() %>%
        select("X", "Y") %>%
        slice_head(n = nrow(.) - 1) %>%
        st_as_sf(., coords = c("X", "Y"), crs = 3035)

    new_buffers <- st_buffer(vertices, dist = 8000) %>%
        st_union(., owf) %>%
        select("geometry")

    max_imp <- 0
    max_index <- 0

    for (i in 1:nrow(new_buffers)) {
        current_buffer <- already_existing %>% st_union(., new_buffers$geometry[i])

        int <- sensitivity %>%
            st_intersection(., current_buffer)
        if (!density) {
            imp <- sum(drop_units(as.numeric(int$addr__G) * st_area(int)), na.rm = T)
        } else {
            imp <- sum(drop_units(as.numeric(int$Vorkommen) * st_area(int)), na.rm = T)
        }
        if (imp > max_imp) {
            max_imp <- imp
            max_index <- i
        }
    }

    return(new_buffers$geometry[max_index])
}

greedy_search <- function(sensitivity, buffers, density = FALSE) {
    for (scenario in c(40, 50, 60, 70)) { #### Iterate over scenarios

        print(paste0(scenario, "GW ", "SCENARIO"))

        filtered_buffers <- buffers %>%
            filter(Scenario <= scenario) #### Create pool of available owf to choose from

        buffer_ids <- rep(NA, 5) #### Results

        res <- st_sfc(crs = st_crs(3035))

        for (i in 1:5) { #### Need to select 5 windfarms per scenarios

            max_imp <- 0
            max_index <- 0
            max_buffer <- NULL

            for (j in 1:nrow(filtered_buffers)) { #### Iterate over all available owf and select the one with most impact

                if (!is.null(res$geometry)) { #### Check if some owf have already been selected
                    current_buffer <- create_buffer(filtered_buffers[j, ], sensitivity, res, density = density)

                    test_geom <- st_union(res, current_buffer)

                    int <- sensitivity %>% #### Intersect sensitivity map with current buffer + already selected ones
                        st_intersection(., test_geom)
                } else {
                    current_buffer <- create_buffer(filtered_buffers[j, ], sensitivity, res, density = density)

                    int <- sensitivity %>% #### Intersect sensitivity map with current buffer
                        st_intersection(., current_buffer)
                }

                int <- int %>% #### Ensure no duplicates
                    dplyr::filter(!duplicated(CELLCOD) & !duplicated(CELLCOD, fromLast = TRUE))
                if (!density) {
                    imp <- sum(drop_units(as.numeric(int$addr__G) * st_area(int))) #### Calculate impact
                } else {
                    imp <- sum(drop_units(as.numeric(int$Vorkommen) * st_area(int))) #### Calculate impact
                }
                if (imp > max_imp) {
                    max_imp <- imp
                    max_index <- j
                    max_buffer <- current_buffer
                }
            }

            selected_id <- filtered_buffers$FID[max_index]

            buffer_ids[i] <- selected_id #### Save the index of the selected owf
            filtered_buffers <- filtered_buffers %>% filter(FID != selected_id) #### Remove the selected owf from the available pool of owf

            res <- st_union(res, max_buffer)
        }


        owf_buffers <- buffers %>%
            filter(FID %in% buffer_ids) %>%
            st_union()

        for (b in buffer_ids) {
            test_buffer <- buffers %>% filter(FID == b)

            test_buffer <- create_buffer(test_buffer, sensitivity, owf_buffers, density = density)

            owf_buffers <- owf_buffers %>% st_union(., test_buffer)
        }

        if (scenario == 40) { #### Save results
            res_40 <- owf_buffers
        }

        if (scenario == 50) {
            res_50 <- owf_buffers
        }

        if (scenario == 60) {
            res_60 <- owf_buffers
        }

        if (scenario == 70) {
            res_70 <- owf_buffers
        }
    }
    return(list(res_40, res_50, res_60, res_70))
}


plot_scenarios <- function(scenarios, sensitivity, density = FALSE, porpoiseSA = FALSE) {
    # fixed variable name from 'scenario' to 'scenarios[1]'
    plots_scn <- list()

    for (i in seq_along(scenarios)) {
        scenario <- scenarios[[i]] # assuming each element is a spatial object like sf
        scenario_number <- as.numeric(gsub("\\D", "", names(scenarios)[i]))
        fill_column <- if (density) "Vorkommen" else "addr__G"
        p <- ggplot() +
            geom_sf(data = sensitivity, aes(fill = .data[[fill_column]], geometry = geometry), color = NA)

        if (density) {
            breaks <- quantile(sensitivity$Vorkommen, probs = c(0, 0.5, 1), na.rm = TRUE)
            p <- p + scale_fill_viridis_c(breaks = breaks, labels = scales::label_number())
        } else {
            p <- p + scale_fill_viridis_d()
        }

        if (porpoiseSA) {
            p <- p + geom_sf(data = porpoise_special, aes(geometry = geometry), color = "red", fill = NA)
        }

        p <- p +
            geom_sf(data = scenario, color = NA, fill = "red", alpha = 0.5) +
            geom_sf(data = example_farms %>% filter(Scenario <= scenario_number), color = "blue") +
            ylab("") + xlab("") +
            theme(
                title = element_text(size = 20, face = "bold"),
                axis.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20, margin = margin(r = 20, unit = "pt")),
                legend.position = "bottom", legend.direction = "horizontal",
            ) +
            labs(fill = if (density) "Dichte (ind/km\u00B2)" else "Sensitivitätsindex")
        plots_scn[[i]] <- p
    }
    plots_scn[[1]] <- plots_scn[[1]] + ggtitle("Szenario 40 GW")
    plots_scn[[2]] <- plots_scn[[2]] + ggtitle("Szenario 50 GW")
    plots_scn[[3]] <- plots_scn[[3]] + ggtitle("Szenario 60 GW")
    plots_scn[[4]] <- plots_scn[[4]] + ggtitle("Szenario 70 GW")
    # Combine all plots with patchwork
    combined_plots <- wrap_plots(plots_scn, ncol = 2, axes = "collect", byrow = TRUE) & theme(plot.margin = margin(0, 0, 0, 0), legend.position = "bottom")

    # Since legend is only in first plot and at bottom, patchwork will handle it correctly
    # Just print combined plots (legend shows only once)
    return(combined_plots + plot_layout(guides = "collect"))
}

calculate_impact <- function(scenario, sensitivity = NULL, density = NULL, porpoiseSA = FALSE, marine = FALSE, tot_population = NULL) {
    res_list <- list()
    int <- if (!is.null(sensitivity)) {
        int <- sensitivity %>% st_intersection(., scenario)
    } else {
        int <- density %>% st_intersection(., scenario)
    }

    area_AWZ <- int %>%
        st_union() %>%
        st_area() %>%
        units::drop_units()
    area_AWZ <- area_AWZ / 1e6
    area_AWZ <- round(area_AWZ)
    perc_AWZ <- round((area_AWZ / 28504) * 100, 1)
    string_AWZ <- glue("{area_AWZ} km\u00B2 ({perc_AWZ} %)")
    res_list <- append(res_list, mget(c("area_AWZ", "perc_AWZ", "string_AWZ"), envir = environment()))

    if (!is.null(sensitivity)) {
        areas_sens <- list()
        string_areas_snc <- list()
        names_cat <- list()
        for (i in unique(sensitivity$addr__G)) {
            search_f <- ifelse(marine, i, as.numeric(i))
            area_scn <- int %>%
                filter(addr__G == search_f) %>%
                st_union() %>%
                st_area() %>%
                sum() / 1e6
            areas_sens[[i]] <- area_scn
            string_areas_snc <- append(string_areas_snc, glue("{round(area_scn)} km\u00B2"))
            names_cat[[i]] <- glue("category {i}")
        }
        names(string_areas_snc) <- names_cat
        vars <- c("string_areas_snc")
        res_list <- append(res_list, mget(vars, envir = environment()))
    }


    if (porpoiseSA) {
        area_HKG <- porpoise_special %>%
            st_intersection(., scenario) %>%
            st_union() %>%
            st_area() %>%
            units::drop_units()
        area_HKG <- ifelse(length(area_HKG) == 0, 0, area_HKG)
        area_HKG <- area_HKG / 1e6
        area_HKG <- round(area_HKG)
        perc_HKG <- round((area_HKG / 8500.175) * 100, 1)
        string_HKG <- glue("{area_HKG} km\u00B2 ({perc_HKG} %)")
        vars <- c("area_HKG", "perc_HKG", "string_HKG")
        res_list <- append(res_list, mget(vars, envir = environment()))
    }


    if (!is.null(density)) {
        if (is.null(tot_population)) {
            total_pop <- sum(drop_units(density$Vorkommen * st_area(density) / 1e6), na.rm = T)
        } else {
            total_pop <- tot_population
        }
        print(glue("the percentage of populations will be calculated assuming a total population of {total_pop}"))
        sp_density <- density %>%
            st_intersection(., scenario) %>%
            mutate(n_ppho = drop_units(Vorkommen * (st_area(geometry) / 1e6)))
        n_ppho <- round(sum(sp_density$n_ppho, na.rm = T))
        perc_pop <- round((n_ppho / total_pop) * 100, 1)
        string_pop <- glue("{n_ppho} Ind. ({perc_pop} %)")
        vars <- c("n_ppho", "perc_pop", "string_pop")
        res_list <- append(res_list, mget(vars, envir = environment()))
    }
    return(res_list)
}
