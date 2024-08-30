



#two different equations for stubs “conical frustrum”... just a cylinder if DBH is used for D at the base and D at the upper end

StubVol_ConicFru <-function(Height, DBH) {
  Ab<-pi *(DBH/200)^2 #area of the base
  Au<-pi *(DBH/200)^2 #area of the top
  Height/3 *
    (Ab +#area at the base
       Au + #area at the upper end
       (Ab*Au)^0.5 ) #square root of area at the base times area at the upper end
}


#' Title
#'
#' @param dbh
#' @param h
#' @param hi
#' @param p
#' @param a1
#' @param a2
#' @param a3
#' @param a4
#' @param a5
#' @param a6
#' @param a7
#' @param a8
#'
#' @return
#' @export
#'
#' @examples
kozak88_di<-function(dbh, h, hi, p, a1, a2, a3, a4, a5, a6, a7, a8){
  Xi = (1-(hi/h)^0.5)/(1-p^0.5)
  z = hi/h
  a1 * dbh^a2 * a3^dbh * Xi^(a4 * z^2 + a5 * log(z + 0.001) + a6*z^0.5 + a7 * (dbh/h) + a8 *exp(z))
}

#Kozak model 88 (Kozak 2004) coefficients for BC commercial species from timber pricing branch compilation rules for Date Creek
#p, a1, a2, a3, a4, a5, a6, a7, a8

colnames(Kozak_coef)<-c("p", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8")
rownames(Kozak_coef)<-c("Cw", "Hw", "Bl", "Ba", "Sx", "Pl", "Ac", "Ep", "At", "UC")
#test
kozak88_di(dbh = 10, h= 10, hi = 0.1, p = Cw_Kozak[1], a1= Cw_Kozak[2], a2= Cw_Kozak[3],
           a3= Cw_Kozak[4], a4= Cw_Kozak[5], a5= Cw_Kozak[6], a6= Cw_Kozak[7], a7= Cw_Kozak[8], a8= Cw_Kozak[9])

#Taper equations make very large diameters near 0 so using 0.3 as height of the base diameter instead of 0
#' Title
#'
#' @param Height
#' @param DBH
#' @param Species
#' @param StubHT
#'
#' @return returns the estimate of volume/ha (not sure how it becomes per hectare - check w Erica)
#' @export
#'
#' @examples
StubVol_ConicPara <-function(Height, DBH, Species, StubHT) {

  Cw_Kozak <- c(0.3,	1.033575,	0.896971,	0.999079,	1.59826,
                -0.411541,	2.40242,	0.094283,	-1.25217)
  Hw_Kozak <-c(0.25,	0.752027,	1.02897,	0.99866,	1.1748,
               -0.263576,	2.23333,	0.045184,	-1.00202)
  Bl_Kozak <- c(0.3,	1.008741,	0.916357,	1.001159,	1.41599,
                -0.325671,	2.79327,	0.108427,	-1.32679)
  Ba_Kozak <- c(0.3,	1.008741,	0.916357,	1.001159,	1.41599,
                -0.325671,	2.79327,	0.108427,	-1.32679)
  Sx_Kozak <- c(0.3,	0.897311,	0.95709,	0.99937,	1.53227,
                -0.364679,	2.74121,	0.117756,	-1.36276)
  Pl_Kozak <- c(0.25,	0.774601,	1.04032,	0.996984,	0.74575,
                -0.130177,	0.558818,	0.198687,	-0.324178)
  Ac_Kozak <- c(0.25,	0.802839,	0.993776,	0.998974,	0.706093,
                -0.096789,	0.312724,	0.119634,	-0.080057)
  Ep_Kozak <- c(0.25,	0.64883,	1.12139,	0.992077,	0.865974,
                -0.106757,	0.257139,	0.254574,	-0.149926)
  At_Kozak <- c(0.2,	0.855966,	0.987014,	0.999828,	0.424473,
                -0.037553,	-0.51754,	0.102211,	0.303931)
  UC_Kozak <- c(0.25,	0.752027,	1.02897,	0.99866,	1.1748,
                -0.263576,	2.23333,	0.045184,	-1.00202) #Hw parameters for unknown conifer

  Kozak_coef <- rbind(Cw_Kozak, Hw_Kozak, Bl_Kozak, Ba_Kozak, Sx_Kozak,
                    Pl_Kozak, Ac_Kozak, Ep_Kozak, At_Kozak, UC_Kozak)


  Kozak.row <- Kozak_coef[c(Species),]
  BarkThickness <- DBH - kozak88_di(dbh = DBH, h= Height, hi = 1.3, p = Kozak.row[1],
                                    a1= Kozak.row[2], a2= Kozak.row[3],
                                  a3= Kozak.row[4], a4= Kozak.row[5], a5= Kozak.row[6],
                                  a6= Kozak.row[7], a7= Kozak.row[8], a8= Kozak.row[9])

  Db <-BarkThickness + kozak88_di(dbh = DBH, h= Height, hi = 0.3, p = Kozak.row[1],
                                  a1= Kozak.row[2], a2= Kozak.row[3],
                                  a3= Kozak.row[4], a4= Kozak.row[5], a5= Kozak.row[6],
                                  a6= Kozak.row[7], a7= Kozak.row[8], a8= Kozak.row[9])
  Du <-BarkThickness + kozak88_di(dbh = DBH, h= Height, hi = StubHT, p = Kozak.row[1],
                                  a1= Kozak.row[2], a2= Kozak.row[3],
                                  a3= Kozak.row[4], a4= Kozak.row[5], a5= Kozak.row[6],
                                  a6= Kozak.row[7], a7= Kozak.row[8], a8= Kozak.row[9])

  Ab <- pi *(Db/200)^2 #area of the base
  Au <- pi *(Du/200)^2 #area of the top
  stub_vol_ha <- StubHT/12 *(5 * Ab + #area at the base
                            5 * Au+ #area at the upper end
                            2 * (Ab*Au)^0.5 ) #square root of area at the base times area at the upper end

  return(stub_vol_ha)
}
