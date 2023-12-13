
#' Coarse Woody Debris Carbon
#'
#' @description This function calculates the amount of carbon in a piece or plot of coarse woody debris based
#' on the species and decay class. Data is passed to the function by species and decay class. The function will
#' estimate the carbon of unknown species based on the BEC zone (currently SBS or ICH only, default is set to SBS).
#'
#' Currently, the function estimates carbon for: Lodgepole pine, hybrid spruce, subalpine fir, trembling aspen,
#' paper birch, red alder, cottonwood, amabilis fir, western hemlock, western redcedar, Douglas-fir and
#' western larch
#'
#' @param volume_ha volume/ha of CWD piece or plot
#' @param Decay_class Decay class of CWD piece or plot
#' @param Species Species of CWD piece or plot
#'
#' @return
#' @export
#' @details cwdCARBON (Mg/ha) = volume(m3/ha)
#'  x structural reduction factor (decay class specific (Fraver et al. 2013))
#' x Absolute density(g/cm3) (species and decay class specific (Harmon et al. 2008))
#' x CarbonConcentration (species and decay class specific (Harmon et al. 2013))
#' @examples
calc_cwd_c <- function(volume_ha, Decay_class, Species, BECzone = "SBS"){
  if(is.na(Species)){
    print(paste("Species is not found"))
    DC_Sp_C <- NA
  } else if (Species == "Pl"){
    if(Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.378*0.497)
      } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.367*0.496)
      } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.276*0.499)
      } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.169*0.519)
      } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.164*0.526)
      }
  } else if (Species == "Sx"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.393*0.496)
      } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.285*0.498)
      } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.28*0.505)
      } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.136*0.521)
      } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.129*0.535)
      }
  } else if (Species == "Bl"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.371*0.498)
      } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.288*0.501)
      } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.233*0.498)
      } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.501)
      } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.117*0.521)
      }
    } else if (Species == "At"){
      if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.353*0.488)
      } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.422*0.489)
      } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.299*0.495)
      } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.16*0.465)
      } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
      }
    } else if (Species == "Ac"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.353*0.478)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.422*0.477)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.299*0.481)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.16*0.474)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
      }
    } else if (Species == "Ep"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.469*0.478)
      } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.403*0.477)
      } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.352*0.481)
      } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.17*0.474)
      } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
      }
    } else if (Species == "Dr"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.386*0.478)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.326*0.477)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.197*0.481)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.108*0.474)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.117*0.473)
      }
    }else if (Species == "Ba"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.36*0.487)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.332*0.485)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.212*0.505)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.142*0.521)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.106*0.535)
      }
    } else if (Species == "Hw"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.399*0.484)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.346*0.498)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.258*0.515)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.166*0.534)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.108*0.57)
      }
    } else if (Species == "Cw"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.318*0.496)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.259*0.498)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.248*0.505)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.132*0.521)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.143*0.535)
      }
    } else if (Species == "Fd"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.386*0.488)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.308*0.499)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.152*0.495)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.123*0.539)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.148*0.558)
      }
    } else if (Species == "Lw"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.381*0.496)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.318*0.498)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.257*0.505)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.162*0.521)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.143*0.535)
      }
    } else if (Species == "UC"){
      if(BECzone == "ICH"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock","Western red cedar"),
                             .(mean = mean(AbsoluteDensity)), by=DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock","Western red cedar"),
                             .(mean = mean(AbsoluteDensity)), by=DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock","Western red cedar"),
                             .(mean = mean(AbsoluteDensity)), by=DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock","Western red cedar"),
                             .(mean = mean(AbsoluteDensity)), by=DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock","Western red cedar"),
                             .(mean = mean(AbsoluteDensity)), by=DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.496)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.498)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*0.8*dc3*0.521)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.521)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.535)
      }
    } else if(BECzone == "SBS"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.496) #check
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.498)#check
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*0.8*dc3*0.521)#check
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.521)#check
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.535)#check
      }
    } else {
      print("No valid BEC zome provided to calculate average unknown deciduous tree density")
    }
  } else if (Species == "UD"){
    if(BECzone == "ICH"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.478)
        } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.477)
        } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*dc3*0.481)
        } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.474)
        } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.473)
        }
    } else if(BECzone =="SBS"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.478)
        } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.477)
        } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*dc3*0.481)
        } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.474)
        } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.473)
        }
    } else{
      print("No valid BEC zome provided to calculate average unknown deciduous tree density")
    }
  } else if (Species == "U"){
    if(BECzone == "ICH"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                              "Amabalis fir", "Western hemlock", "Western red cedar",
                                              "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock", "Western red cedar",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock", "Western red cedar",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock", "Western red cedar",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Amabalis fir", "Western hemlock", "Western red cedar",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.496)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.498)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*0.8*dc3*0.521)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.521)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.535)
      }
    } else if(BECzone == "SBS"){
      dc1 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 1, sum(mean)]
      dc2 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 2, sum(mean)]
      dc3 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 3, sum(mean)]
      dc4 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 4, sum(mean)]
      dc5 <- treeCalcs::cwdC_conv_table[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                               "Trembling aspen", "Black cottonwood", "Paper birch"),
                             .(mean = mean(AbsoluteDensity)), by = DecayClass][DecayClass == 5, sum(mean)]
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*dc1*0.478)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*dc2*0.477)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*dc3*0.481)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*dc4*0.474)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*dc5*0.473)
      }
    } else {
      print("No valid BEC zome provided to calculate average unknown deciduous tree density")
    }
  } else {
    DC_Sp_C <- 0
  }
  return(DC_Sp_C)
}



#' Calculate fine woody debris carbon
#'
#' @param Diam_class
#' @param volume
#' fwdCARBON (Mg/ha) = volume(m3/ha)
#' x Live wood density(g/cm3) # use unknown species (Harmon et al. 2008)
#' x Decay reduction factor for each size class (Harmon and Fasth website)
#' x CarbonConcentration
#' use 50% (Harmon and Fasth website)
#'
#' @return
#' @export
#'
#' @examples
calc_fwd_c <- function(Diam_class, volume){
  if(is.na(Diam_class)){
    print(paste("Diam_class is not found"))
    C <- NA
  } else {
    if (Diam_class == "1.1-2.5"){
      C <- (volume*0.41*0.81*0.5)
    } else if (Diam_class == "2.6-5"){
      C <- (volume*0.41*1*0.5)
    } else if (Diam_class == "5.1-7.5"){
      C <- (volume*0.39*0.99*0.5)
    } else {
      print(paste("Diam_class",Species,"not found"))
      C <- NA
    }
  }
  return(C)
}
