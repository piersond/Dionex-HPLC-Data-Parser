# Ftn to apply calibration curve to run data

library(tidyverse)

apply_cal <- function(cal_tbl, ion, unknown) {
  
  # Isolate low and high curves for specific ion
  curve_low <- cal_tbl %>% filter(analyte == ion) %>% filter(level == "Low")
  curve_high <- cal_tbl %>% filter(analyte == ion) %>% filter(level == "High")

  # Calculate unknown concentration using the appropriate curve
  if(is.na(unknown)) {
    conc <- NA
    note <- "Peak area not detected"
  } else if(unknown < curve_low$min_signal) {
      conc <- unknown * curve_low$slope + curve_low$intercept
      note <- "WARNING: Analyte signal below calibration"
  } else if(unknown < curve_low$max_signal) {
      conc <- unknown * curve_low$slope + curve_low$intercept
      note <- "Low calibration curve applied"
  } else if(unknown < curve_high$max_signal) {
      conc <- unknown * curve_high$slope + curve_high$intercept
      note <- "High calibration curve applied"
  } else if(unknown > curve_high$max_signal) {
      conc <- unknown * curve_high$slope + curve_high$intercept
      note <- "WARNING: Analyte signal above calibration"
  } else {
      conc <- NA
      note <- "ERROR: Calibration apply error"
  } 

  return(data.frame(Concentration = conc,
                    RNote = note))
}


# Ftn to apply calibration curves to full run data table
apply_cal_to_run <- function(cal_curves, run_output) {

  df <- run_output %>% split(1:nrow(run_output)) %>% 
    map(~apply_cal(cal_tbl = cal_curves,
                    ion = .x$Analyte, 
                    unknown = .x$Area)) %>%
    bind_rows() %>%
    cbind(run_output, .)
  
  return(df)
}


### Example usage ###

# setwd("C:/github/Dionex-HPLC-Data-Parser")
# 
# source("get_data_ftn.R")
# source("calibrate_ftns.R")
# 
# path_to_file <- "data/DionexTest_raw.xlsx"
# 
# cal_vals <- get_data(file_name = path_to_file)$cal_values
# run_data <- get_data(file_name = path_to_file)$run_data
# 
# ### Generate table of calibration curves
# curves <- data.frame(analyte =      c(rep("Chloride", 2), rep("Nitrate", 2), rep("Sulfate", 2)),
#                      level =        rep(c("High", "Low"),3),
#                      ex_std_from =  rep(c(1,5),3),
#                      ex_std_to =    rep(c(3,8),3),
#                      rmv_std =      rep(c("Std 8"), 6))
# 
# curves_all <- curves %>% split(1:nrow(curves)) %>%
#   map(~ generate_cal_curve(
#     analyte=.x$analyte,
#     cal_values = cal_vals,
#     run_df = run_data,
#     remove_std = .x$rmv_std,
#     exclude_std = paste0("Std ", seq(from=.x$ex_std_from, to=.x$ex_std_to)))) %>%
#   bind_rows() %>%
#   cbind(curves, .)
# 
# run_data <- apply_cal_to_run(cal_curves = curves_all, run_output = run_data)
