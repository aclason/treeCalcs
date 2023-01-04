

# April 2022
# By: Leah Walker

##################### ALLOMETRY FUNCTIONS ############################
# Based on previous studies, we are calculating the height of trees using allometry
# rather than using the diameter-only Carbon equations


#' Title
#'
#' @param Species
#' @param DBH
#'
#' @return
#' @export
#'
#' @examples
DiamHgtFN <- function(Species, DBH){
  if(is.na(Species)){
    print(paste("Species is not found"))
    HT <- NA
  } else if(Species == "Sx"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH))
  } else if(Species == "Pl"){
    HT <- 1.35 + (23.346836 - 1.35)*(1 - exp(-(0.0707280)*DBH))
  } else if(Species == "Bl"){
    HT <- 1.35 + (30.000000 - 1.35)*(1 - exp(-(0.03496783)*DBH))
  } else if(Species == "Ba"){
    HT <- 1.35 + (30.000000 - 1.35)*(1 - exp(-(0.03496783)*DBH)) # using Bl
  } else if(Species == "At"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH))
  } else if(Species == "Lw"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH))
  } else if(Species == "Fd"){
    HT <- 1.35 + (35.000000 - 1.35)*(1 - exp(-(0.0299364)*DBH))
  } else if(Species == "Ac"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH))
  } else if(Species == "Ep"){
    HT <- 1.35 + (33.530000 - 1.35)*(1 - exp(-(0.0374600)*DBH))
  }
  return(HT)
}

