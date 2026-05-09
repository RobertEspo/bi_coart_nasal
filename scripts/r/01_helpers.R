###############################################################################
# This function is used in the following function
# to produce a df that combines the dfs of each plot's predictions.
get_smooth_df <- function(model, blp_value, dominance_label) {
  as.data.frame(
    plot_smooth(
      model,
      view = "point_vwlpct",
      cond = list(interviewee_final_blp_score = blp_value),
      rm.ranef = FALSE
    )$fv
  ) %>%
    mutate(
      dominance = dominance_label,
      blp_score = blp_value
    )
}

# This function plots a GAMM that has BLP as a predictor
# It plots BLP at 3 specified BLP scores.
# Note that you must define the ylim.
# It also gives you dfs of model summaries.

plot_smooths_dominance <- function(model,
                                   spanish_blp,
                                   balanced_blp,
                                   english_blp,
                                   ylim=c(x,y)) {
  
  # get dfs for each of the plot's predictions
  # and combine them into one df
  
  combined_df <- bind_rows(
    get_smooth_df(model, balanced_blp, "Balanced"),
    get_smooth_df(model, spanish_blp, "Spanish"),
    get_smooth_df(model, english_blp, "English")
  )
  
  model_name <- deparse(substitute(model))
  
  df_name <- paste0(model_name, "_dominance_effect_df")
  
  assign(df_name, combined_df, envir = .GlobalEnv)
  
  # balanced
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = balanced_blp),
              rm.ranef = FALSE,
              col = "#DC267F",
              lwd = 3,
              add = FALSE,
              ylim = ylim)
  
  # spanish dominant
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = spanish_blp),
              rm.ranef = FALSE,
              col = "#648FFF",
              lwd = 3,
              add = TRUE)
  
  # english dominant
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = english_blp),
              rm.ranef = FALSE,
              col = "#FFB000",
              lwd = 3,
              add = TRUE)
  
  legend("topright",
         title = "Dominance",
         legend = c("Balanced", "Spanish", "English"),
         lty = 1,
         lwd = 3,
         col = c("#DC267F", "#648FFF", "#FFB000"))
  
}


# This model gives you a df for the summary output of a gam fit with mgcv
# produces the following dfs:
## [model_name]_param_df
## [model_name]_smooth_df
## [model_name]_summary_df

model_summary_table <- function(model,
                                model_name = deparse(substitute(model))) {
  
  param_df <- as.data.frame(summary(model)$p.table) %>%
    rownames_to_column("term") %>%
    mutate(type = "parametric")
  
  smooth_df <- as.data.frame(summary(model)$s.table) %>%
    rownames_to_column("term") %>%
    mutate(type = "smooth")
  
  summary_df <- bind_rows(param_df, smooth_df)
  
  param_name  <- paste0(model_name, "_param_df")
  smooth_name <- paste0(model_name, "_smooth_df")
  summary_name <- paste0(model_name, "_summary_df")
  
  assign(param_name, param_df, envir = .GlobalEnv)
  assign(smooth_name, smooth_df, envir = .GlobalEnv)
  assign(summary_name, summary_df, envir = .GlobalEnv)

}

# This function finds ALL local convergence points:
# points where the dominance curves are maximally similar
# relative to neighboring points (local minima in divergence).
# This allows for 1, 2, or more convergence points.
# It will output a df based on your input df:
# [input_df_name]_convergence_points

find_dominance_convergence <- function(df) {
  
  convergence_df <- df %>%
    
    # Keep  relevant columns
    select(point_vwlpct, dominance, fit) %>%
    
    # Convert dominance levels to columns
    pivot_wider(
      names_from = dominance,
      values_from = fit
    ) %>%
    
    # Calculate divergence
    mutate(
      range_all = pmax(Balanced, Spanish, English) -
        pmin(Balanced, Spanish, English),
      
      sd_all = apply(
        select(., Balanced, Spanish, English),
        1,
        sd
      )
    )
  
  
  # Find local minima in range_all
  convergence_points <- convergence_df %>%
    mutate(
      prev_range = lag(range_all),
      next_range = lead(range_all)
    ) %>%
    
    # A point is a convergence point if it's lower than both neighbors
    filter(
      (is.na(prev_range) | range_all <= prev_range) &
        (is.na(next_range) | range_all <= next_range)
    ) %>%
    
    # Sort by strongest convergence first
    arrange(range_all) %>%
    
    # Remove helper columns
    select(-prev_range, -next_range)
  
  
  # Dynamic object name
  df_name <- deparse(substitute(df))
  
  convergence_name <- paste0(df_name, "_convergence_points")
  
  # Save all convergence points to global environment
  assign(convergence_name, convergence_points, envir = .GlobalEnv)
  
  # Return invisibly
  invisible(convergence_points)
}
