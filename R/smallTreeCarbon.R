

#' calculate small tree carbon
#'
#' @param Species current valid species are At, Ac, Cw, Bl, Ep, Hw, Pl, Sx, Fd, UC, Lw
#' @param Height_class
#' @param Diam_est
#' @param Health
#'
#' @return
#' @export
#'
#' @details
#' Live_Dead - should we specify live and dead? Is it worth it?
#'
#' @examples
calc_sm_tree_c_Ung <- function(Species, Height_class, Diam_est, Health){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Reg_C2 <- NA
  } else if(Species == "At"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0143*Diam_est^1.9369*0.15^1.0579)+(0.0063*Diam_est^2.0744*0.15^0.6691)+
                     (0.0150*Diam_est^2.9068*0.15^-0.6306)+(0.0284*Diam_est^1.6020))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0143*Diam_est^1.9369*0.15^1.0579)+(0.0063*Diam_est^2.0744*0.15^0.6691)+
                     (0.0150*Diam_est^2.9068*0.15^-0.6306)+(0.0284*Diam_est^1.6020))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0143*Diam_est^1.9369*0.80^1.0579)+(0.0063*Diam_est^2.0744*0.80^0.6691)+
                     (0.0150*Diam_est^2.9068*0.80^-0.6306)+(0.0284*Diam_est^1.6020))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0143*Diam_est^1.9369*0.80^1.0579)+(0.0063*Diam_est^2.0744*0.80^0.6691)+
                     (0.0150*Diam_est^2.9068*0.80^-0.6306)+(0.0284*Diam_est^1.6020))*0.5*0.95
      }
    }
  }else if(Species=="Ac"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0051*Diam_est^1.0697*0.15^2.2748)+(0.0009*Diam_est^1.3061*0.15^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.15^1.8368))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0051*Diam_est^1.0697*0.15^2.2748)+(0.0009*Diam_est^1.3061*0.15^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.15^1.8368))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0051*Diam_est^1.0697*0.80^2.2748)+(0.0009*Diam_est^1.3061*0.80^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.80^1.8368))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0051*Diam_est^1.0697*0.80^2.2748)+(0.0009*Diam_est^1.3061*0.80^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.80^1.8368))*0.5*0.95
      }
    }
  } else if(Species=="Cw"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0188*Diam_est^1.3376*0.15^1.5293)+(0.0002*Diam_est^2.4369*0.15^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0188*Diam_est^1.3376*0.15^1.5293)+(0.0002*Diam_est^2.4369*0.15^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0188*Diam_est^1.3376*0.80^1.5293)+(0.0002*Diam_est^2.4369*0.80^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0188*Diam_est^1.3376*0.80^1.5293)+(0.0002*Diam_est^2.4369*0.80^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.5*0.95
      }
    }
  } else if(Species=="Bl"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                     (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))*0.5
      } else if (Health=="D"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                     (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                     (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))*0.5
      } else if (Health=="D"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                     (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))*0.5*0.95
      }
    }
  } else if(Species=="Ba"){ #using Bl
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                     (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))*0.5
      } else if (Health=="D"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                     (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                     (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))*0.5
      } else if (Health=="D"){
        Reg_C2 <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                     (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))*0.5*0.95
      }
    }
  } else if(Species=="Ep"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0333*Diam_est^2.0794*0.15^0.6811)+(0.0079*Diam_est^1.9905*0.15^0.6553)+
                     (0.0253*Diam_est^3.1518*0.15^-0.9083)+(0.1361*Diam_est^2.2978*0.15^-1.0934))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0333*Diam_est^2.0794*0.15^0.6811)+(0.0079*Diam_est^1.9905*0.15^0.6553)+
                     (0.0253*Diam_est^3.1518*0.15^-0.9083)+(0.1361*Diam_est^2.2978*0.15^-1.0934))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0333*Diam_est^2.0794*0.80^0.6811)+(0.0079*Diam_est^1.9905*0.80^0.6553)+
                     (0.0253*Diam_est^3.1518*0.80^-0.9083)+(0.1361*Diam_est^2.2978*0.80^-1.0934))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0333*Diam_est^2.0794*0.80^0.6811)+(0.0079*Diam_est^1.9905*0.80^0.6553)+
                     (0.0253*Diam_est^3.1518*0.80^-0.9083)+(0.1361*Diam_est^2.2978*0.80^-1.0934))*0.5*0.95
      }
    }
  } else if(Species=="Hw"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0113*Diam_est^1.9332*0.15^1.1125)+(0.0019*Diam_est^2.3356*0.15^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.15^-0.7963))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0113*Diam_est^1.9332*0.15^1.1125)+(0.0019*Diam_est^2.3356*0.15^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.15^-0.7963))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0113*Diam_est^1.9332*0.80^1.1125)+(0.0019*Diam_est^2.3356*0.80^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.80^-0.7963))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0113*Diam_est^1.9332*0.80^1.1125)+(0.0019*Diam_est^2.3356*0.80^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.80^-0.7963))*0.5*0.95
      }
    }
  } else if(Species=="Pl"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0239*Diam_est^1.6827*0.15^1.1878)+(0.0117*Diam_est^1.6398*0.15^0.6524)+
                     (0.0285*Diam_est^3.3764*0.15^-1.4395)+(0.0769*Diam_est^2.6834*0.15^-1.2484))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0239*Diam_est^1.6827*0.15^1.1878)+(0.0117*Diam_est^1.6398*0.15^0.6524)+
                     (0.0285*Diam_est^3.3764*0.15^-1.4395)+(0.0769*Diam_est^2.6834*0.15^-1.2484))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0239*Diam_est^1.6827*0.80^1.1878)+(0.0117*Diam_est^1.6398*0.80^0.6524)+
                     (0.0285*Diam_est^3.3764*0.80^-1.4395)+(0.0769*Diam_est^2.6834*0.80^-1.2484))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0239*Diam_est^1.6827*0.80^1.1878)+(0.0117*Diam_est^1.6398*0.80^0.6524)+
                     (0.0285*Diam_est^3.3764*0.80^-1.4395)+(0.0769*Diam_est^2.6834*0.80^-1.2484))*0.5*0.95
      }
    }
  } else if(Species=="Sx"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <-((0.0133*Diam_est^1.3303*0.15^1.6877)+(0.0086*Diam_est^1.6216*0.15^0.8192)+
                    (0.0428*Diam_est^2.7965*0.15^-0.7328)+(0.0854*Diam_est^2.4388*0.15^-0.7630))*0.5
      }else if(Health=="D"){
        Reg_C2 <-((0.0133*Diam_est^1.3303*0.15^1.6877)+(0.0086*Diam_est^1.6216*0.15^0.8192)+
                    (0.0428*Diam_est^2.7965*0.15^-0.7328)+(0.0854*Diam_est^2.4388*0.15^-0.7630))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0133*Diam_est^1.3303*0.80^1.6877)+(0.0086*Diam_est^1.6216*0.80^0.8192)+
                     (0.0428*Diam_est^2.7965*0.80^-0.7328)+(0.0854*Diam_est^2.4388*0.80^-0.7630))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0133*Diam_est^1.3303*0.80^1.6877)+(0.0086*Diam_est^1.6216*0.80^0.8192)+
                     (0.0428*Diam_est^2.7965*0.80^-0.7328)+(0.0854*Diam_est^2.4388*0.80^-0.7630))*0.5*0.95
      }
    }
  } else if(Species=="Fd"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0191*Diam_est^1.5365*0.15^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.15^-0.4744))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0191*Diam_est^1.5365*0.15^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.15^-0.4744))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0191*Diam_est^1.5365*0.80^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.80^-0.4744))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0191*Diam_est^1.5365*0.80^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.80^-0.4744))*0.5*0.95
      }
    }
  } else if(Species=="UC"){ # average of conifers used
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.5*0.95
      }
    }
  } else if(Species=="Lw"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.5
      }else if(Health=="D"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.5*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.5
      } else if(Health=="D"){
        Reg_C2 <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.5*0.95
      }
    } else {
      print(paste("Species",Species,"not found"))
      Reg_C2 <- NA
    }
  }
  return(Reg_C2)
}



#Regen biomass & carbon - Annigofer height allometric equation
# Using the species allometric equation from Annighofer et al. (2016) - they are European species so we will use the
# broadleaf and conifer. These equations use height. We will use the mid-point height for our height class regen.
# y = B1 * H^B2
# where y = biomass (g)
# H = height (cm)
# B1 and B2 = fitted coefficients from Annigofer et al. (2016)
# *0.5 for carbon concentration

#' Small tree Annigofer equations
#'
#' @param Species
#' @param Height_class
#'
#' @return
#' @export
#'
#' @examples
calc_sm_tree_c_Annigofer <- function(Species, Height_class){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Reg_C1 <- NA
  } else if(Species == "At"){ # using broadleaf
    if(Height_class == "0-30"){
      Reg_C1 <-(0.002*(15^2.249))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.002*(80^2.249))*0.5
    }
  }else if(Species=="Ac"){ # using broadleaf
    if(Height_class == "0-30"){
      Reg_C1 <-(0.002*(15^2.249))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.002*(80^2.249))*0.5
    }
  } else if(Species=="Cw"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Bl"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Ep"){ # using broadleaf
    if(Height_class == "0-30"){
      Reg_C1 <-(0.002*(15^2.249))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.002*(80^2.249))*0.5
    }
  } else if(Species=="Hw"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Pl"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Sx"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Fd"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.55
    }
  } else if(Species=="UC"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    }
  } else if(Species=="Lw"){ # using conifer
    if(Height_class == "0-30"){
      Reg_C1 <-(0.024*(15^1.982))*0.5
    } else if (Height_class == "31-130"){
      Reg_C1 <-(0.024*(80^1.982))*0.5
    } else {
      print(paste("Species",Species,"not found"))
      Reg_C1 <- NA
    }
  }
  return(Reg_C1)
}

