# Remove Dionex run duplicates and bad values

remove_dups <- function(uniqID, tbl) {
  
  #DEBUG
  #uniqID <- "O220216h"
  #tbl <- unknowns_tbl
  
  df <- tbl %>% filter(SampleID == uniqID)
  
  if(nrow(df) > 1) {
    df <- df %>% filter(!grepl("WARNING", RNote))
    if(nrow(df) == 1) {
      out <- df
    } else {
      df <- df[which.min(df$Dilution_Factor),]
      out <- df
    }
  } else {
    out <- df
  }
  
  
  # Format the data table
  ion <- out$Analyte
  out <- out %>% select(SampleID, No., Concentration, Dilution_Factor, RNote)
  colnames(out) <- c("SampleID", "Run_Pos", ion, 
                          paste0(ion,"_DilF"),
                          paste0(ion,"_Note"))
  
  return(out)
} 

# Example usage 
#output <- unique(unknowns_tbl$SampleID) %>% map(~remove_dups(uniqID = ., 
#                                                             tbl = unknowns_tbl)) %>%
#                                            bind_rows()
