
plot_performance <- function(dat, outcome_name) {
  # One plot by outcome
  if (outcome_name %in% c("neuropathy", "ulcer")) {
    x_axis_ticks <- seq(
      min(dat$landmark_age), max(dat$landmark_age), 5L
    )
  } else {
    x_axis_ticks <- seq(
      min(dat$landmark_age), max(dat$landmark_age), 2L
    )
  }
  dat$term  <- as.factor(dat$term)
  yAxisLine <- data.frame(term = dat$term, y = c(0, 0, 0.5, NA, 0.5, NA))
  
  dat %>% 
    filter(outcome == outcome_name) %>% 
    ggplot(aes(x = landmark_age, y = estimate)) +
    # geom_ribbon(
    #   aes(x = landmark_age, ymin = conf.low, ymax = conf.high), 
    #   fill  = see::bluebrown_colors()["blue"],
    #   alpha = 0.2
    # ) +
    geom_errorbar(
      aes(x = landmark_age, ymin = conf.low, ymax = conf.high), 
      width = 0.1,
      colour = see::bluebrown_colors()["darkblue"]
    ) + 
    geom_hline(data = yAxisLine, aes(yintercept = y), color = see::bluebrown_colors()["darkbrown"]) +
    geom_point(colour = see::bluebrown_colors()["darkblue"]) +
    # geom_line(colour = see::bluebrown_colors()["darkblue"]) +
    scale_x_continuous(
      limits = c(min(dat$landmark_age[dat$outcome == outcome_name]), max(dat$landmark_age[dat$outcome == outcome_name])), 
      breaks = seq(20, 100, by = 5)
    ) +
    labs(
      # caption = paste0("Shaded region: 95% confidence intervals"),
      x = "Landmark age", 
      y = "Estimate"
    ) +
    theme_classic() +
    facet_wrap(
      vars(term), 
      nrow = 3, 
      scales = "free", 
      labeller = labeller(term = c(
        "deltaAUC"   = "Delta AUC",
        "deltaBrier" = "Delta Brier",
        "mod3AUC"    = "Model 3 AUC",
        "mod2AUC"    = "Model 2 AUC",
        "mod3Brier"  = "Model 3 Brier score",
        "mod2Brier"  = "Model 2 Brier score"
      ))
    ) +
    theme(
      strip.background = element_rect(fill = alpha(see::bluebrown_colors()["lightbrown"], 0.1)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.caption = element_text(margin = margin(t = 5)),
      panel.spacing = unit(1, "lines"),
      text = element_text(size = 15),
      panel.grid.major.x = element_line(),
      panel.grid.major.y = element_line()
    )
}

plot_delta_performance <- function(dat, outcome_name) {
  # Same as plot_performance(), but only plot the top two panes (deltas).
  x_axis_ticks <- if (outcome_name %in% c("neuropathy", "ulcer")) {
   seq(min(dat$landmark_age), max(dat$landmark_age), 5L)
  } else {
    seq(min(dat$landmark_age), max(dat$landmark_age), 2L)
  }
  
  dat %>% 
    filter(outcome == outcome_name) %>% 
    filter(term %in% c("deltaAUC", "deltaBrier")) %>% 
    mutate(term = factor(term, levels = c("deltaBrier", "deltaAUC"))) %>% 
    ggplot(aes(x = landmark_age, y = estimate)) +
    geom_errorbar(
      aes(x = landmark_age, ymin = conf.low, ymax = conf.high), 
      width = 0.1,
      colour = see::bluebrown_colors()["darkblue"]
    ) + 
    geom_hline(yintercept = 0, color = see::bluebrown_colors()["darkbrown"]) +
    geom_point(colour = see::bluebrown_colors()["darkblue"]) +
    scale_x_continuous(
      limits = c(min(dat$landmark_age[dat$outcome == outcome_name]), max(dat$landmark_age[dat$outcome == outcome_name])), 
      breaks = seq(20, 100, by = 5)
    ) +
    labs(
      x = "Landmark age",
      y = "Estimate"
    ) +
    theme_classic() +
    facet_wrap(
      vars(term), 
      nrow = 2, 
      scales = "free", 
      labeller = labeller(term = c(
        "deltaAUC"   = "Delta AUC",
        "deltaBrier" = "Delta Brier"
      ))
    ) +
    theme(
      strip.background = element_rect(fill = alpha(see::bluebrown_colors()["lightbrown"], 0.1)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.caption = element_text(margin = margin(t = 5)),
      panel.spacing = unit(1, "lines"),
      text = element_text(size = 15),
      panel.grid.major.x = element_line(),
      panel.grid.major.y = element_line()
    )
}
