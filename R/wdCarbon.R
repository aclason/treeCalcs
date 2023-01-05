
#' Coarse Woody Debris Carbon
#'
#' @param volume_ha
#' @param Decay_class
#' @param Species
#'
#' @return
#' @export
#' @details cwdCARBON (Mg/ha) = volume(m3/ha)
#' x structural reduction factor # decay class specific (Fraver et al. 2013)
#' x Absolute density(g/cm3) # species and decay class specific (Harmon et al. 2008)
#' x CarbonConcentration # species and decay class specific (Harmon et al. 2013)
#' @examples
cwdCarbonFN <- function(volume_ha, Decay_class, Species, BECzone = "SBS"){
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
    } else if (Decay_class == "5") {
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
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.117*0.521)
    }
  } else if (Species == "UC"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.381*0.496)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.313*0.498)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.521)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.152*0.521)
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.137*0.535)
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
    } else if (Decay_class == "5") {
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
    } else if (Decay_class == "5") {
      DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
    }
  } else if (Species == "UD"){
    if(BECzone == "SBS"){
      dc1 <- cwdC_conv_table[CommonName== "" & CommonName=="", mean(`Absolute density_g.cm3`), by=.(DecayClass)]


      if (Decay_class == "1"){

        DC_Sp_C <-(volume_ha*1*0.392*0.478)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.416*0.477)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.317*0.481)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.163*0.474)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
      }
    } else if(BECzone =="ICH"){
      if (Decay_class == "1"){
        DC_Sp_C <-(volume_ha*1*0.392*0.478)
      } else if (Decay_class == "2"){
        DC_Sp_C <-(volume_ha*1*0.416*0.477)
      } else if (Decay_class == "3"){
        DC_Sp_C <-(volume_ha*1*0.317*0.481)
      } else if (Decay_class == "4"){
        DC_Sp_C <-(volume_ha*0.8*0.163*0.474)
      } else if (Decay_class == "5"){
        DC_Sp_C <-(volume_ha*0.412*0.11*0.473)
      }

    } else{
      print("No valid BEC zome provided to calculate average unknown deciduous tree density")
    }

  } else if (Species == "U"){
    if (Decay_class == "1"){
      DC_Sp_C <-(volume_ha*1*0.386*0.487)
    } else if (Decay_class == "2"){
      DC_Sp_C <-(volume_ha*1*0.365*0.488)
    } else if (Decay_class == "3"){
      DC_Sp_C <-(volume_ha*1*0.29*0.493)
    } else if (Decay_class == "4"){
      DC_Sp_C <-(volume_ha*0.8*0.158*0.498)
    } else if (Decay_class == "5"){
      DC_Sp_C <-(volume_ha*0.412*0.123*0.504)
    }
  } else {
    DC_Sp_C <- 0
  }
  return(DC_Sp_C)
}



#' Title
#' @param VolGrid the table of volumes from SORTIE
#' @param speciesGroupings data.frame of species and groups
#' @param SizeClassCut cutoff between small and large logs (20 right now)
#'
#' @return
#' @export
#'
#' @examples
cwdCfromSORTIE <- function(VolGrid,speciesGroupings, SizeClassCut){

  #Species, SpGroup
  # then this function would use that dataframe to calculate group densities from the conversion table

  #need the density conversion table - for groupings here, and unknown above




}


#' Title
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
fwdCarbonFN <- function(Diam_class, volume){
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
