
#' Merchantable Timber Volume
#' @description This function calculates the volume (m^3) of merchantable timber. Data is passed to the function by species and BECzone.
#' The function will estimate the merchantable volume of unknown species based on the BEC zone (currently SBS or ICH only, default is set to SBS).
#'
#' Currently, the function estimates volume for: Lodgepole pine, hybrid spruce, subalpine fir, trembling aspen,
#' paper birch, red alder, cottonwood, amabilis fir, western hemlock, western redcedar, Douglas-fir and
#' western larch
#'
#' @param DBH diameter (cm) at breast height (DBH) of an indovidual tree
#' @param HT total tree height (m) of an individual tree
#' @param BECzone Biogeoclimatic zone. Currently only sub boreal spruce (SBS) and interior cedar hemlock (ICH)
#' @param Species tree species
#'
#' @return
#' @export
#' @details merchantable timber volumber (m^3) = e^b0 x DBH^b1 x HT^b2
#'  where b0, b1, b2 are model parameters from North et al. 2016 Total and merchantable volume equations for common tree species in BC
#' @examples
TreeMerchVolFN <- function(Species, DBH, HT, BECzone="SBS"){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_MV <- NA
  } else if (BECzone=="SBS"){
    if (Species=="At"){
      Sp_MV <- exp(-10.724)*DBH^1.896*HT^1.208
    } else if (Species=="Ac"){
      Sp_MV <-exp(-10.840)*DBH^1.768*HT^1.336
    } else if (Species=="Ep"){
      Sp_MV <- exp(-10.798)*DBH^1.943*HT^1.177
    } else if (Species=="Bl"){
      Sp_MV <- exp(-9.980)*DBH^1.792*HT^1.102
    } else if (Species=="Fd"){
      Sp_MV <- exp(-10.503)*DBH^1.724*HT^1.296
    } else if (Species=="Hw"){
      Sp_MV <- exp(-10.245)*DBH^1.828*HT^1.139
    } else if (Species=="Pl"){
      Sp_MV <- exp(-10.013)*DBH^1.843*HT^1.076
    } else if (Species=="Sx"){
      Sp_MV <- exp(-10.280)*DBH^1.769*HT^1.206
    } else if (Species=="UC"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="SBS", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="SBS", sum(mean)]
      b2 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine"),
                              .(mean=mean(b2)), by=BECzone][BECzone=="SBS", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    } else if (Species=="UD"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="SBS", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="SBS", sum(mean)]
      b2 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b2)), by=BECzone][BECzone=="SBS", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    } else if (Species=="U"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="SBS", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="SBS", sum(mean)]
      b2 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b2)), by=BECzone][BECzone=="SBS", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    }
  } else if (BECzone=="ICH"){
    if (Species=="At"){
      Sp_MV <- exp(-10.994)*DBH^1.894*HT^1.314
    } else if (Species=="Ac"){
      Sp_MV <- exp(-10.814)*DBH^1.663*HT^1.437
    } else if (Species=="Ep"){
      Sp_MV <- exp(-10.613)*DBH^1.923*HT^1.157
    } else if (Species=="Dr"){
      Sp_MV <- exp(-10.430)*DBH^1.869*HT^1.169 # used CWH bec zone
    } else if (Species=="Ba"){
      Sp_MV <- exp(-9.977)*DBH^1.767*HT^1.155 # used CWH bec zone
    } else if (Species=="Bl"){
      Sp_MV <- exp(-10.132)*DBH^1.916*HT^1.010
    } else if (Species=="Cw"){
      Sp_MV <- exp(-9.749)*DBH^1.687*HT^1.115
    } else if (Species=="Hw"){
      Sp_MV <- exp(-10.382)*DBH^1.882*HT^1.116
    } else if (Species=="Pl"){
      Sp_MV <- exp(-10.098)*DBH^1.798*HT^1.146
    } else if (Species=="Sx"){
      Sp_MV <- exp(-10.119)*DBH^1.754*HT^1.175
    } else if (Species=="UC"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock","Western red cedar"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="ICH", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock","Western red cedar"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="ICH", sum(mean)]
      b2 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock","Western red cedar"),
                              .(mean=mean(b2)), by=BECzone][BECzone=="ICH", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    } else if (Species=="UD"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="ICH", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="ICH", sum(mean)]
      b2 <- MerchVolCalcConst[CommonName %in% c("Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b2)), by=BECzone][BECzone=="ICH", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    } else if (Species=="U"){
      b0 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock", "Western red cedar",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b0)), by=BECzone][BECzone=="ICH", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock", "Western red cedar",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="ICH", sum(mean)]
      b1 <- MerchVolCalcConst[CommonName %in% c("Subalpine fir", "Hybrid spruce", "Lodgepole pine",
                                                "Amabalis fir", "Western hemlock", "Western red cedar",
                                                "Trembling aspen", "Black cottonwood", "Paper birch"),
                              .(mean=mean(b1)), by=BECzone][BECzone=="ICH", sum(mean)]
      Sp_MV <- exp(b0)*DBH^b1*HT^b2
    }
  } else{
    Sp_MV <- 0
  }
  return(Sp_MV)
}
