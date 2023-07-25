

#' Calculate tree basal area
#'
#' @param DBH
#'
#' @return
#' @export
#'
#' @examples
calcBA <- function(DBH) {
  pi*(DBH/200)^2
}
