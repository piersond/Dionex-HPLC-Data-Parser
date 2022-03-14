## Calibration ftns
# Generate calibration curves
# Apply cal curve to data

library(tidyverse)


# Function to find slope, intercept and r2 for calibration data
cal_curve <- function(value, signal, thru_zero=F, name="", plot_out=F){
  
  if(thru_zero == T) {
    value <- c(0, value)
    signal <- c(0, signal)
  }
  
  model <- lm(value ~ signal)
  slope <- summary(model)$coefficients[2]
  intercept <- summary(model)$coefficients[1]
  r2 <- summary(model)$r.squared
  
  eq <- as.character(as.expression(substitute(italic(y) == b %.% italic(x) + a*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(model)[1]), digits = 2),
                        b = format(unname(coef(model)[2]), digits = 2),
                        r2 = format(summary(model)$r.squared, digits = 4)))))
  
  p <- ggplot(data.frame(sig=signal, val=value), aes(x=sig, y=val)) + 
        ggtitle(name) + theme_minimal() +
        geom_smooth(method = "lm", se = TRUE, color="dark red", alpha=0.2, linetype = "dashed") +
        geom_point(size=4, alpha=0.8) +
        geom_text(x = 0.2*max(signal), y = 0.9*max(value), label = eq, parse = TRUE) +
        xlab("Signal") + ylab(paste0(name, " (ppm)"))
  suppressMessages(print(p))

  # Create a named vector to return
  out <- data.frame(slope = slope, 
                    intercept = intercept, 
                    rsq = r2,
                    min_signal = min(signal),
                    max_signal = max(signal),
                    n = length(signal))

  # Output calibration curve data or the curve plot
  if(plot_out) {
    return(p)
  } else {
    return(out)
  }
}



generate_cal_curve <- function(analyte, cal_values, run_df, exclude_std = c(""), remove_std=c(""), plot_out=F) {
  
  # Isolate calibration standard data for the analyte
  analyte_df <- run_df %>% filter(Analyte == analyte) %>%
                             filter(str_detect(SampleID, 'Std ')) %>%
                             filter(!str_detect(SampleID, 'Std 0')) 
    
  cal_targets_df <- cal_values %>% filter(toupper(Analyte) == toupper(analyte)) 
  cal_targets_df <- data.frame(SampleID = paste0("Std ", seq(1:(ncol(cal_targets_df)-1))),
                               Known_Conc = as.numeric(cal_targets_df[,2:ncol(cal_targets_df)]))
    
  analyte_df <- suppressMessages(left_join(analyte_df, cal_targets_df)) %>%
                filter(!SampleID %in% exclude_std) %>%
                filter(!SampleID %in% remove_std)
  
  #print(analyte_df)
    
  # get standard curve values
  return(cal_curve(value = as.numeric(analyte_df$Known_Conc),
            signal = as.numeric(analyte_df$Area),
            name=analyte,
            thru_zero=F,
            plot_out=plot_out))
}
  

### Example usage ###

setwd("C:/github/Dionex-HPLC-Data-Parser")

source("get_data_ftn.R")

path_to_file <- "data/DionexTest_raw.xlsx"

cal_vals <- get_data(file_name = path_to_file)$cal_values
run_data <- get_data(file_name = path_to_file)$run_data

### Generate a singal calibration curve
curve1 <- generate_cal_curve(analyte = "Sulfate", 
                   cal_values = cal_vals, 
                   run_df = run_data, 
                   exclude_std = c(""),
                   remove_std = c("Std 8"),
                   plot_out=F)


### Generate table of calibration curves
curves <- data.frame(analyte =      c(rep("Chloride", 2), rep("Nitrate", 2), rep("Sulfate", 2)),
                     level =        rep(c("High", "Low"),3),
                     ex_std_from =  rep(c(1,5),3),
                     ex_std_to =    rep(c(3,8),3),
                     rmv_std =      rep(c("Std 8"), 6)) 

curves_all <- curves %>% split(1:nrow(curves)) %>% 
                         map(~ generate_cal_curve(
                                  analyte=.x$analyte,                    
                                  cal_values = cal_vals, 
                                  run_df = run_data,
                                  remove_std = .x$rmv_std,
                                  exclude_std = paste0("Std ", seq(from=.x$ex_std_from, to=.x$ex_std_to)))) %>%
                         bind_rows() %>% 
                         cbind(curves, .)

