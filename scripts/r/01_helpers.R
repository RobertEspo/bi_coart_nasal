plot_smooths_dominance <- function(model,
                                   spanish_blp,
                                   balanced_blp,
                                   english_blp,
                                   vowel_duration = 0) {
  
  # balanced
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = balanced_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "black",
              add = FALSE,
              ylim = c(-6,-2))
  
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = balanced_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "black",
              add = TRUE)
  
  # spanish dominant
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = spanish_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "red",
              add = TRUE)
  
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = spanish_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "red",
              add = TRUE)
  
  # english dominant
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = english_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "green",
              add = TRUE)
  
  plot_smooth(model,
              view = "point_vwlpct",
              cond = list(interviewee_final_blp_score = english_blp,
                          vwl_duration_z = vowel_duration),
              rm.ranef = FALSE,
              col = "green",
              add = TRUE)
  
  legend("topright",
         title = "Dominance",
         legend = c("Balanced", "Spanish", "English"),
         lty = 1,
         lwd = 2,
         col = c("black", "red", "green"))
}



plot_diff(
  m_gam_ar1,
  view = "point_vwlpct",
  
  comp = list(
    previous_sonorant = c(1,0)
  ),
  
  cond = list(
    vwl_duration_z = 0,
    interviewee_final_blp_score = 0
  ),
  
  rm.ranef = FALSE
)

plot_diff(
  m_gam_ar1,
  view = "point_vwlpct",
  
  comp = list(
    interviewee_final_blp_score = c(-100, 100)
  ),
  
  cond = list(
    vwl_duration_z = 0,
    previous_sonorant = 1
  ),
  
  rm.ranef = FALSE
)
