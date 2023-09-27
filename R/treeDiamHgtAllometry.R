# April 2022
# By: Leah Walker

#' Diameter-Height functions
#'
#' @param Species
#' @param DBH
#' @description  The "standard" diameter-height relationship from SORTIE.
#' The parameter values were extracted from the SBS and ICH parameter files
#' @return
#' @export
#'
#' @details allometry varies with environment, so we use the paramter estimates
#' based on BEC zones for these functions and assigned a default of the SBS:
#'
#' From ICH parameter file:
#' Slope of asymptotic height:
#' "Western_Hemlock" 0.0299,"Western_redcedar" 0.0241,
#' "Amabalis_Fir" 0.0263,"Subalpine_Fir" 0.0264,
#' "Hybrid_spruce" 0.0264,"Lodgepole_Pine" 0.0333,
#' "Trembling_Aspen" 0.0352, "Black_Cottonwood" 0.0347,
#' "Paper_Birch" 0.0454
#'
#' Canopy tree height:
#' "Western_Hemlock" 39.48, "Western_redcedar" 39.54,
#' "Amabalis_Fir" 40.0, "Subalpine_Fir" 40.0,
#' "Hybrid_spruce" 45.0, "Lodgepole_Pine" 40.0,
#' "Trembling_Aspen" 39.14, "Black_Cottonwood" 39.47,
#' "Paper_Birch" 33.18
#'
#' From the SBS parameter file:
#' "Interior_Spruce" 0.0299364, "Lodgepole_Pine" 0.070728,
#' "Subalpine_Fir" 0.0305068, "Trembling_Aspen" 0.03746
#'
#' Canopy tree height:
#' "Interior_Spruce" 35.0, "Lodgepole_Pine" 23.346836,
#' "Subalpine_Fir" 30.0, "Trembling_Aspen" 33.53
#'
#' @examples
height_dbh <- function(Species, DBH, BECzone = "SBS"){
if(BECzone == "SBS"){
  if(is.na(Species)){
    print(paste("Species is not found"))
    HT <- NA
  } else if(Species == "Sx"){
    HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH))
  } else if(Species == "Pl"){
    HT <- 1.35 + (23.346836 - 1.35)*(1 - exp(-(0.070728)*DBH))
  } else if(Species == "Bl"){
    HT <- 1.35 + (30.0 - 1.35)*(1 - exp(-(0.0305068)*DBH))
  } else if(Species == "Ba"){
    HT <- 1.35 + (30.0 - 1.35)*(1 - exp(-(0.0263)*DBH)) #slope from ICH file, height same as At
  } else if(Species == "At"){
    HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.03746)*DBH))
  } else if(Species == "Lw"){
    HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH)) #using Sx
  } else if(Species == "Fd"){
    HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH)) #using Sx
  } else if(Species == "Ac"){
    HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.03746)*DBH)) #using Ac
  } else if(Species == "Ep"){
    HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.0454)*DBH)) #slope from ICH file, height same as At
  }


  } else if(BECzone=="ICH"){
    if(is.na(Species)){
      print(paste("Species is not found"))
      HT <- NA
    }else if(Species == "Sx"){
      HT <- 1.35 + (45.0 - 1.35)*(1 - exp(-(0.0264)*DBH))
    } else if(Species == "Pl"){
      HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0333)*DBH))
    } else if(Species == "Bl"){
      HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0264)*DBH))
    } else if(Species == "Ba"){
      HT <- 1.35 + (40.0 - 1.35)*(1 - exp(-(0.0263)*DBH))
    } else if(Species == "At"){
      HT <- 1.35 + (39.14 - 1.35)*(1 - exp(-(0.0352)*DBH))
    } else if(Species == "Ac"){
      HT <- 1.35 + (39.47 - 1.35)*(1 - exp(-(0.0347)*DBH))
    } else if(Species == "Ep"){
      HT <- 1.35 + (33.18 - 1.35)*(1 - exp(-(0.0454)*DBH))
    } else if(Species == "Hw"){
      HT <- 1.35 + (39.48 - 1.35)*(1 - exp(-(0.0299)*DBH))
    } else if(Species == "Cw"){
      HT <- 1.35 + (39.54 - 1.35)*(1 - exp(-(0.0241)*DBH))
    } else if(Species == "UC"){
      HT <- 1.35 + (39.48 - 1.35)*(1 - exp(-(0.0299)*DBH))
    }else if(Species == "U"){ #use hemlock?
      HT <- 1.35 + (39.48 - 1.35)*(1 - exp(-(0.0299)*DBH))
    }

  }

  return(HT)
}

#' Diam-height function residuals
#'
#' @param Species
#' @param DBH
#' @param BECzone
#'
#' @return
#' @export
#'
#' @examples
height_dbh_Residuals <- function(Species, DBH, BECzone = "ICH"){
  if(BECzone == "SBS") {
    #no change
    if (is.na(Species)) {
      print(paste("Species is not found"))
      HT <- NA
    } else if (Species == "Sx") {
      HT <- 1.35 + (35.0 - 1.35) * (1 - exp(-(0.0299364) * DBH))
    } else if (Species == "Pl") {
      HT <- 1.35 + (23.346836 - 1.35) * (1 - exp(-(0.070728) * DBH))
    } else if (Species == "Bl") {
      HT <- 1.35 + (30.0 - 1.35) * (1 - exp(-(0.0305068) * DBH))
    } else if (Species == "Ba") {
      HT <-
        1.35 + (30.0 - 1.35) * (1 - exp(-(0.0263) * DBH)) #slope from ICH file, height same as At
    } else if (Species == "At") {
      HT <- 1.35 + (33.53 - 1.35) * (1 - exp(-(0.03746) * DBH))
    } else if (Species == "Lw") {
      HT <- 1.35 + (35.0 - 1.35) * (1 - exp(-(0.0299364) * DBH)) #using Sx
    } else if (Species == "Fd") {
      HT <- 1.35 + (35.0 - 1.35) * (1 - exp(-(0.0299364) * DBH)) #using Sx
    } else if (Species == "Ac") {
      HT <- 1.35 + (33.53 - 1.35) * (1 - exp(-(0.03746) * DBH)) #using Ac
    } else if (Species == "Ep") {
      HT <- 1.35 + (33.53 - 1.35) * (1 - exp(-(0.0454) * DBH)) #slope from ICH file, height same as At
    }


  } else if (BECzone == "ICH") {
    if (is.na(Species)) {
      print(paste("Species is not found"))
      HT <- NA
    } else if (Species == "Ac") {
      #no change
      HT <- 1.35 + (39.47465 - 1.35) * (1 - exp(-0.03472 * DBH))
    } else if (Species == "At") {
      #no change
      HT <- 1.35 + (39.14183 - 1.35) * (1 - exp(-0.03517 * DBH))
    } else if (Species == "Ba") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.02556 * DBH))
    } else if (Species == "Bl") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.034117 * DBH))
    } else if (Species == "Cw") {
      #new beta
      HT <- 1.35 + (39.5376 - 1.35) * (1 - exp(-0.02345 * DBH))
    } else if (Species == "Ep") {
      #no change
      HT <- 1.35 + (33.18361 - 1.35) * (1 - exp(-0.04543 * DBH))
    } else if (Species == "Hw") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "U") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "UC") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "Pl") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.045657 * DBH))
    } else if (Species == "Sx") {
      #new beta
      HT <- 1.35 + (45 - 1.35) * (1 - exp(-0.03772775 * DBH))
    }

  }
  return(HT)
}


#' Diam - height function in clearcuts (high light)
#'
#' @param Species
#' @param DBH
#' @param BECzone
#'
#' @return
#' @export
#'
#' @examples
height_dbh_plantations <- function(Species, DBH, BECzone = "ICH"){
  if(BECzone == "SBS"){
    if(is.na(Species)){
      print(paste("Species is not found"))
      HT <- NA
    } else if(Species == "Sx"){
      HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH))
    } else if(Species == "Pl"){
      HT <- 1.35 + (23.346836 - 1.35)*(1 - exp(-(0.070728)*DBH))
    } else if(Species == "Bl"){
      HT <- 1.35 + (30.0 - 1.35)*(1 - exp(-(0.0305068)*DBH))
    } else if(Species == "Ba"){
      HT <- 1.35 + (30.0 - 1.35)*(1 - exp(-(0.0263)*DBH)) #slope from ICH file, height same as At
    } else if(Species == "At"){
      HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.03746)*DBH))
    } else if(Species == "Lw"){
      HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH)) #using Sx
    } else if(Species == "Fd"){
      HT <- 1.35 + (35.0 - 1.35)*(1 - exp(-(0.0299364)*DBH)) #using Sx
    } else if(Species == "Ac"){
      HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.03746)*DBH)) #using Ac
    } else if(Species == "Ep"){
      HT <- 1.35 + (33.53 - 1.35)*(1 - exp(-(0.0454)*DBH)) #slope from ICH file, height same as At
    }


  } else if (BECzone == "ICH") {
    if (is.na(Species)) {
      print(paste("Species is not found"))
      HT <- NA
    } else if (Species == "Ac") {
      #no change
      HT <- 1.35 + (39.47465 - 1.35) * (1 - exp(-0.03472 * DBH))
    } else if (Species == "At") {
      #no change
      HT <- 1.35 + (39.14183 - 1.35) * (1 - exp(-0.03517 * DBH))
    } else if (Species == "Ba") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.02556 * DBH))
    } else if (Species == "Bl") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.034117 * DBH))
    } else if (Species == "Cw") {
      #new beta
      HT <- 1.35 + (39.5376 - 1.35) * (1 - exp(-0.02345 * DBH))
    } else if (Species == "Ep") {
      #no change
      HT <- 1.35 + (33.18361 - 1.35) * (1 - exp(-0.04543 * DBH))
    } else if (Species == "Hw") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "U") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "UC") {
      #new beta and new MaxHt
      HT <- 1.35 + (47.4 - 1.35) * (1 - exp(-0.02247587 * DBH))
    } else if (Species == "Pl") {
      #new beta
      HT <- 1.35 + (40 - 1.35) * (1 - exp(-0.045657 * DBH))
    } else if (Species == "Sx") {
      #new beta
      HT <- 1.35 + (45 - 1.35) * (1 - exp(-0.03772775 * DBH))
    }

  }

  return(HT)
}


