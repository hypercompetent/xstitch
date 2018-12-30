#' cut() an array and retain dimensions
#'
#' @param arr The array to be processed by cut()
#' @param ... Arguments passed to the cut() function. See ?cut for details.
#'
#' @return A factor array divided using cut().
#' @export
#'
#' @examples
#' cut_volcano <- cut_array(volcano, breaks = 5)
cut_array <- function(arr,
                      ...) {
  array(cut(arr, ...), dim = dim(arr))
}
