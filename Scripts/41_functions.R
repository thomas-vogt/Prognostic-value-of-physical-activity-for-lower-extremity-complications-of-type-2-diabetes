
crr_plot_results <- function(results, 
                             y_name, 
                             model_number,
                             subgroup_name) {
  
  if (is.na(subgroup_name[1])) {                             
    plot_data <- results[is.na(results$subgroup), ]
  } else {
    plot_data <- results[results$subgroup %in% subgroup_name, ]
  }  
  
  plot_data <- plot_data |> 
    dplyr::filter(
      outcome %in% y_name
      & grepl("fysisk_aktivitet_", term)
      & model %in% model_number
    ) |>
    dplyr::mutate(
      landmark_age = as.integer(landmark_age),
      term = factor(
        term, 
        levels = c(
          "fysisk_aktivitet_1", 
          "fysisk_aktivitet_2", 
          "fysisk_aktivitet_3", 
          "fysisk_aktivitet_4"),
        labels = paste0(
          c(
            "Never",
            "<1 times/week",
            "Regularly 1-2 times/week",
            "Regularly 3-5 times/week"
          ),
          " (vs daily)"
        )
      )
    )
  
  diff_between_estimates <- ""
  y_axis_label           <- "log(sHR)"
  # sub_group              <- subgroup_name
  
  if (length(subgroup_name) > 1) {                   
    plot_data <- plot_data |> 
      as_tibble() |> 
      pivot_wider(
        names_from = subgroup, 
        values_from = c(estimate, p.value, conf.low, conf.high, log_estimate, std.error, log_conf.low, log_conf.high)
      )
    
    plot_data$estimate_diff <- 
      plot_data[[paste0("log_estimate_", subgroup_name[1])]] - plot_data[[paste0("log_estimate_", subgroup_name[2])]]
    
    plot_data$diff_se <- sqrt(
      (plot_data[[paste0("std.error_", subgroup_name[1])]] ^ 2) + (plot_data[[paste0("std.error_", subgroup_name[2])]] ^ 2)
    )
    
    plot_data$diff_conf_low  <- plot_data$estimate_diff - (1.96 * plot_data$diff_se)
    plot_data$diff_conf_high <- plot_data$estimate_diff + (1.96 * plot_data$diff_se)
    
    plot_data <- plot_data |> 
      mutate(
        log_estimate  = estimate_diff,
        log_conf.low  = diff_conf_low,
        log_conf.high = diff_conf_high
      )
    # if ("Male" %in% subgroup_name)    sub_group <- "sex/gender"
    # if ("EducLow" %in% subgroup_name) sub_group <- "education"
    
    diff_between_estimates <- ", difference between estimates"
    y_axis_label <- "Difference between log(sHR)"
  }                                               
  
  if (y_name %in% c("neuropathy", "ulcer")) {
    x_axis_ticks <- seq(
      min(plot_data$landmark_age), max(plot_data$landmark_age), 5L
    )
  } else {
    x_axis_ticks <- seq(
      min(plot_data$landmark_age), max(plot_data$landmark_age), 5L
    )
  }
  unrealistic_shr <- plot_data$landmark_age[abs(plot_data$log_estimate) > 5]
  plot_data <- plot_data[!abs(plot_data$log_estimate) > 5, ]
  
  if(length(unrealistic_shr) > 0) {
    vert_dash <- "\nVertical dashed lines: unrealistic sHR removed from the plot"
  } else {
    vert_dash <- ""
  }
  
  ggplot(
    plot_data, 
    aes(x = landmark_age, y = log_estimate)
  ) + 
    geom_hline(yintercept = 0, color = see::bluebrown_colors()["darkbrown"]) + 
    geom_vline(
      xintercept = unrealistic_shr, 
      color = see::bluebrown_colors()["darkbrown"], 
      linetype = "dashed"
    ) +
    # geom_ribbon(
    #   aes(x = landmark_age, ymin = log_conf.low, ymax = log_conf.high), 
    #   fill  = see::bluebrown_colors()["blue"],
    #   alpha = 0.2
    # ) +
    geom_errorbar(
      aes(x = landmark_age, ymin = log_conf.low, ymax = log_conf.high), 
      width = 0.1,
      colour = see::bluebrown_colors()["darkblue"]
    ) + 
    geom_point(colour = see::bluebrown_colors()["darkblue"]) +
    # geom_line(colour = see::bluebrown_colors()["darkblue"]) +
    scale_x_continuous(
      limits = c(min(plot_data$landmark_age), max(plot_data$landmark_age)), 
      breaks = seq(20, 100, by = 5)
    ) +
    labs(
      # title = paste0("event: ", y_name),
      # subtitle = paste0( 
      #   "model: ", stringr::str_extract(model_number, "[[:digit:]]"), 
      #   ", subgroup: ", sub_group,
      #   diff_between_estimates
      # ),
      # caption = paste0("Shaded region: 95% confidence intervals", vert_dash),
      x = "Landmark age" 
      # y = y_axis_label
    ) +
    theme_classic() +
    facet_wrap(vars(term), nrow = 4) +
    theme(
      strip.background = element_rect(fill = alpha(see::bluebrown_colors()["lightbrown"], 0.1)),
      axis.title.x = element_text(margin = margin(t = 15)),
      # axis.title.y = element_text(margin = margin(r = 15)),
      plot.caption = element_text(margin = margin(t = 10)),
      axis.title.y = element_blank(),
      text = element_text(size = 15),
      panel.grid.major.x = element_line(),
      panel.grid.major.y = element_line()
    )
}


diff_in_estimates <- function(results, y_name, subgroup_name) {
  
  if (is.na(subgroup_name[1])) {                             
    plot_data <- results[is.na(results$subgroup), ]
  } else {
    plot_data <- results[results$subgroup %in% subgroup_name, ]
  }  
  
  plot_data <- plot_data |> 
    dplyr::filter(
      outcome %in% y_name
      & grepl("fysisk_aktivitet_", term)
      & model %in% "model3"
    ) |>
    dplyr::mutate(
      landmark_age = as.integer(landmark_age),
      term = factor(
        term, 
        levels = c(
          "fysisk_aktivitet_1", 
          "fysisk_aktivitet_2", 
          "fysisk_aktivitet_3", 
          "fysisk_aktivitet_4"),
        labels = paste0(
          c(
            "Never",
            "<1 times/week",
            "Regularly 1-2 times/week",
            "Regularly 3-5 times/week"
          ),
          " (vs daily)"
        )
      )
    )
  
  if (length(subgroup_name) > 1) {                   
    plot_data <- plot_data |> 
      as_tibble() |> 
      pivot_wider(
        names_from = subgroup, 
        values_from = c(estimate, p.value, conf.low, conf.high, log_estimate, std.error, log_conf.low, log_conf.high)
      )
    
    plot_data$estimate_diff <- 
      plot_data[[paste0("log_estimate_", subgroup_name[1])]] - plot_data[[paste0("log_estimate_", subgroup_name[2])]]
    
    plot_data$diff_se <- sqrt(
      (plot_data[[paste0("std.error_", subgroup_name[1])]] ^ 2) + (plot_data[[paste0("std.error_", subgroup_name[2])]] ^ 2)
    )
    
    plot_data$diff_conf_low  <- plot_data$estimate_diff - (1.96 * plot_data$diff_se)
    plot_data$diff_conf_high <- plot_data$estimate_diff + (1.96 * plot_data$diff_se)
    
    plot_data <- plot_data |> 
      mutate(
        log_estimate  = estimate_diff,
        log_conf.low  = diff_conf_low,
        log_conf.high = diff_conf_high
      )
    # if ("Male" %in% subgroup_name)    sub_group <- "sex/gender"
    # if ("EducLow" %in% subgroup_name) sub_group <- "education"
    
    diff_between_estimates <- ", difference between estimates"
    y_axis_label <- "Difference between log(sHR)"
  }
  
  plot_data <- plot_data[, c("outcome", "landmark_age", "model", "term", names(plot_data)[grepl("diff", names(plot_data))])]
  
  return(plot_data)
}
