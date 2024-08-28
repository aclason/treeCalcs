

#' read SORTIE
#'
#' @param outputs expecting a single csv for each site/plot with all timesteps or a vector of
#' filenames to read in
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @return
#'
#' @examples
read_sortie <- function(outputs) {

  if (is.data.table(outputs)) {
    message("The input is a data.table")

    # Perform operations specific to data.table
    return(outputs)

  } else if (is.character(outputs) && all(file.exists(outputs))) {
    message("The input is a vector of filenames")
    #sort_out <- readSortie(filenames = outputs)
    # Perform operations specific to vector of filenames
    soL <- lapple(outputs,fread)
    soDT <- rbindlist(soL,fill = TRUE)

    return(soDT)

  } else {
    stop("Input is neither a data.table nor a valid vector of filenames")
  }
}


#' Title
#'
#' @param sortie_outputs sortie output saved in a data.table
#' @param dead include snags in the calculation
#' @param BEC Biogeoclimatic ecosystem classification zone
#' @param Ht_from_diam calculate the height based on the diameter (allometry) == TRUE
#' or use Height from Sortie == FALSE
#'
#' @return
#' @export
#' @importFrom treeCalcs calc_tree_c
#' @importFrom treeCalcs height_dbh
#' @import data.table
#'
#' @details
#' Currently only supports common species in the SBS and ICH
#' To do: add argument for seedling to call the seedling carbon function
#'
#' @examples
sortie_tree_carbon <- function(sortie_outputs, dead = FALSE, BEC, Ht_from_diam = TRUE){

  sort_out <- read_sortie(outputs = sortie_outputs)

  #change species names to match carbon function
  sort_out[, Species:= ifelse(tree_species == "Western_redcedar", "Cw",
                          ifelse(tree_species == "Western_Hemlock", "Hw",
                              ifelse(tree_species == "Subalpine_Fir", "Bl",
                                ifelse(tree_species == "Hybrid_spruce","Sx",
                                  ifelse(tree_species == "Interior_Spruce","Sx",
                                    ifelse(tree_species == "Paper_Birch","Ep",
                                      ifelse(tree_species == "Black_Cottonwood","Ac",
                                         ifelse(tree_species == "Amabalis_Fir","Ba",
                                            ifelse(tree_species == "Trembling_Aspen","At",
                                               ifelse(tree_species == "Lodgepole_Pine","Pl",
                                                  ifelse(tree_species == "Douglas_Fir","Fd",
                                                     ifelse(tree_species == "Western_Larch", "Lw",NA))))))))))))]

  if(any(is.na(sort_out$Species),na.rm = FALSE)) cat("at least one tree species is not identified\n")

  #if using the height by diameter function, apply it here
  if(Ht_from_diam == TRUE){

    sort_out[, calc_height := treeCalcs::height_dbh(Species = Species,
                                                    DBH = DBH,
                                                    BECzone = BEC),
             by =seq_len(nrow(sort_out))]
  }

  #sort_out_d <- sort_out[Type=="Snag"]
  #sort_out_l <- sort_out[Type!="Snag"]

  if(dead == TRUE){
    #to do: get snag decay class from SnagDecayClass column
    #sort_out_d[, Class := 3]
    #sort_out_l[, Class := 1]
    #sort_out <- rbind(sort_out_l,sort_out_d)

    sort_out[, Class := ifelse(Type == "Snag",3,1)]
    #calculate carbon from trees
    #sort_out[, Kg_treec:= TreeCarbonFN(Species = Species, DBH = DBH, HT=Height, Tree_class = Class),
    #       by = seq_len(nrow(sort_out))]
    if(Ht_from_diam == TRUE){
      sort_out[, Kg_treec := calc_tree_c(Species = Species,
                                         DBH = DBH,
                                         HT = calc_height,
                                         Tree_class = Class),
               by = seq_len(nrow(sort_out))]
    }else {
      sort_out[, Kg_treec := calc_tree_c(Species = Species,
                                         DBH = DBH,
                                         HT = Height,
                                         Tree_class = Class),
               by = seq_len(nrow(sort_out))]
    }


  }else{
    #live only
    #sort_out_l[, Class:=1]
    sort_out <- sort_out[Type != "Snag"]
    sort_out[, Class := 1]

    if(Ht_from_diam == TRUE){
      #calculate carbon from trees
      sort_out[, Kg_treec := TreeCarbonFN(Species = Species,
                                          DBH = DBH,
                                          HT = calc_height,
                                          Tree_class = Class),
               by = seq_len(nrow(sort_out))]
    } else{
      #calculate carbon from trees
      sort_out[, Kg_treec := TreeCarbonFN(Species = Species,
                                          DBH = DBH,
                                          HT=Height,
                                          Tree_class = Class),
               by = seq_len(nrow(sort_out))]
    }

  }

  # Translate to Mg C/ tree
  sort_out[, ':='(Mg_treeC = Kg_treec/1000)]
  #sort_out[, ':='(C_tree = Carbon/1000)]

  return(sort_out)


}


#' Coarse woody debris carbon
#'
#' @param volGrid
#' @param spGroups
#'
#' @return
#' @export
#' @description
#' To do - make a default species group table and throw errors if species don't match
#'
#'
#' @examples
sortie_cwd_carbon <- function(volGrid, spGroups){
  cwdC_conv <- merge(treeCalcs::cwdC_conv_table, spGroups, by.x = "BCname", by.y = "Sp")

  decay_grp_dens <- cwdC_conv[ ,.(mnAbsDens = mean(AbsoluteDensity),
                                  mnCarbConc = mean(CarbonConversionFactor)),
                               by=c("DecayClass", "SpGrp")]

  volGrid[,`:=`(DecFac = ifelse(decay==1,1,
                                ifelse(decay==2,1,
                                       ifelse(decay==3,0.8,
                                              ifelse(decay==4,0.8,
                                                     ifelse(decay==5,0.412,NA))))))]
  volGrid <- merge(decay_grp_dens, volGrid, by.x = c("DecayClass","SpGrp"), by.y = c("decay","group"))

  volGrid[, MgHa := values*DecFac*mnAbsDens*mnCarbConc, by=seq_len(nrow(volGrid))]

  return(volGrid)

}
