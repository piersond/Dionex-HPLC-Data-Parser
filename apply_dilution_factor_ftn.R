# Ftn for dilution correction

apply_dil_factor <- function(row) {
  if(grepl("_D", row$SampleID)) {

    Dfactor <- as.numeric(strsplit(row$SampleID, "\\:")[[1]][2])
    if(is.na(Dfactor)){
      row$RNote <- paste0("ERROR: NA dilution factor; ", row$RNote) 
    } else{
      row$SampleID <- strsplit(row$SampleID, "_D")[[1]][1]
      row$Concentration <- row$Concentration * Dfactor
      row$RNote <- paste0(row$RNote, "; Applied dilution factor x", as.character(Dfactor))
      return(row)
    }
    
  } else {
    return(row)
  }
}
