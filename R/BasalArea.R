

#' Calculate tree basal area
#'
#' @param DBH
#'
#' @return
#' @export
#'
#' @examples
calc_BA <- function(DBH) {
  pi*(DBH/200)^2
}
