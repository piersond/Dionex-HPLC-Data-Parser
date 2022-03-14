# Description: Create a function to pull in and format raw HPLC major anion data 
# Author: Derek Pierson, created March, 14, 2022

# Notes: Script is specific to the raw data format from the Lohse Lab at Idaho 
#        State University. See DionexTest_raw.xlsx in the data folder for an
#        example of the raw data format.

# Future needs:
#   1) Script expect Cl, NO3 and SO4 data. This may need to be dynamic. 3/14/22, DP

# Load required libraries
library(dplyr)
library(readxl)

## Function to format and reshape the raw Dionex data
# Outputs a list containing 2 data frames named "cal_data" and "run_data"

get_data <- function(file_name, cal_sheet_name="Cal", data_sheet_name="Raw") {
  
  #DEBUG
  # file_name = "data/DionexTest_raw.xlsx" 
  # cal_sheet_name="Cal" 
  # data_sheet_name="Raw"
  
  # Read excel sheet with calibration standard values, then set column names
  cal_df <- read_excel(file_name, sheet = cal_sheet_name)
  colnames(cal_df) <- c("Analyte", paste0("Std ", seq(1:8)))
  
  # Read excel sheet with Dionex run data
  data_df <- read_excel(file_name, sheet = data_sheet_name, skip=0, na = c("n.a."))
  
  # Format and reshape raw run data
    # Creates an analyte column and aligns data vertically into 9 uniform columns
  data_reshape <- bind_rows(data_df[14:nrow(data_df),1:8] %>% cbind(data.frame(analyte=data_df[13,3])),
                            data_df[14:nrow(data_df),10:17] %>% cbind(data.frame(analyte=data_df[13,12])), 
                            data_df[14:nrow(data_df),19:26] %>% cbind(data.frame(analyte=data_df[13,21])))  
  
  # Set column names
  colnames(data_reshape) <- c(data_df[10,1:8], "Analyte")
  
  # Fix "Injection Name" column name
  colnames(data_reshape)[2] <- "SampleID"
  
  # Convert specific data columns to numeric type
  data_reshape[,3:7] <- suppressWarnings(sapply(data_reshape[,3:7],as.numeric))
  
  # Create list for ftn output
  out <- list()
  out$cal_values <- cal_df
  out$run_data <- data_reshape
  
  # Output the list
  return(out)
}


### How to use the get_data ftn ###

# setwd("C:/github/Dionex-HPLC-Data-Parser")
# path_to_file <- "data/DionexTest_raw.xlsx"
# 
# cal_values <- get_data(file_name = path_to_file)$cal_values
# run_data <- get_data(file_name = path_to_file)$run_data


