
#' Title
#'
#' @param Soc
#' @param BD
#' @param depth
#' @param CoarseFrags
#'
#' @return
#' @export
#'
#' @examples
Min_SOC <- function(Soc, BD, depth, CoarseFrags){
  Min_SOC <- Soc * BD * depth * (1- CoarseFrags)
  return(Min_SOC)
}
