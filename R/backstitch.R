backstitch_segments <- list(
  simple = list(
    .setdims = list(xmin = 0,
                    xmax = 2,
                    ybase = 0,
                    ymin = -2,
                    ymax = 4,
                    spacing = 1),
    A = data.frame(x    = c(0,0,1,2,0),
                   xend = c(0,1,2,2,2),
                   y    = c(0,2,4,2,2),
                   yend = c(2,4,2,0,2)),
    B = data.frame(x    = c(0,0,0,0,1,1,1,1),
                   xend = c(0,1,1,1,2,2,2,2),
                   y    = c(0,0,2,4,0,2,2,4),
                   yend = c(4,0,2,4,1,1,3,3)),
    C = data.frame(x    = c(0,0,0,1,1),
                   xend = c(0,1,1,2,2),
                   y    = c(1,3,1,0,4),
                   yend = c(3,4,0,0,4)),
    D = data.frame(x    = c(0,0,0,1,1,2),
                   xend = c(0,1,1,2,2,2),
                   y    = c(0,0,4,4,0,1),
                   yend = c(4,0,4,3,1,3)),
    E = data.frame(x    = c(0,0,0,0),
                   xend = c(0,2,2,2),
                   y    = c(0,0,2,4),
                   yend = c(4,0,2,4)),
    "F" = data.frame(x  = c(0,0,0),
                   xend = c(0,2,2),
                   y    = c(0,2,4),
                   yend = c(4,2,4)),
    G = data.frame(x    = c(0,0,0,1,1,2),
                   xend = c(0,1,1,2,2,2),
                   y    = c(1,1,3,0,4,0),
                   yend = c(3,0,4,0,4,2)),
    H = data.frame(x    = c(0,2,0),
                   xend = c(0,2,2),
                   y    = c(0,0,2),
                   yend = c(4,4,2)),
    "I" = data.frame(x    = c(0,0,1),
                   xend = c(2,2,1),
                   y    = c(0,4,0),
                   yend = c(0,4,4)),
    J = data.frame(x    = c(0,0,1,2),
                   xend = c(2,1,2,2),
                   y    = c(4,0,0,1),
                   yend = c(4,0,1,4)),
    K = data.frame(x    = c(0,0,0),
                   xend = c(0,2,2),
                   y    = c(0,2,2),
                   yend = c(4,0,4)),
    L = data.frame(x    = c(0,0),
                   xend = c(0,2),
                   y    = c(0,0),
                   yend = c(4,0)),
    M = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(0,4,2,0),
                   yend = c(4,2,4,4)),
    N = data.frame(x    = c(0,0,2),
                   xend = c(0,2,2),
                   y    = c(0,4,0),
                   yend = c(4,0,4)),
    O = data.frame(x    = c(0,0,0,1,1,2),
                   xend = c(0,1,1,2,2,2),
                   y    = c(1,1,3,0,4,1),
                   yend = c(3,0,4,1,3,3)),
    P = data.frame(x    = c(0,0,0,1,1),
                   xend = c(0,1,1,2,2),
                   y    = c(0,2,4,2,4),
                   yend = c(4,2,4,3,3)),
    Q = data.frame(x    = c(0,0,0,1,1,2,1),
                   xend = c(0,1,1,2,2,2,2),
                   y    = c(1,1,3,0,4,1,1),
                   yend = c(3,0,4,1,3,3,0)),
    R = data.frame(x    = c(0,0,0,0,1,1),
                   xend = c(0,2,1,1,2,2),
                   y    = c(0,2,2,4,2,4),
                   yend = c(4,0,2,4,3,3)),
    S = data.frame(x    = c(0,1,0,0,1),
                   xend = c(1,2,2,1,2),
                   y    = c(0,0,3,3,4),
                   yend = c(0,1,1,4,4)),
    "T" = data.frame(x  = c(0,1),
                   xend = c(2,1),
                   y    = c(4,0),
                   yend = c(4,4)),
    U = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(1,1,0,0),
                   yend = c(4,0,0,4)),
    V = data.frame(x    = c(0,1),
                   xend = c(1,2),
                   y    = c(4,0),
                   yend = c(0,4)),
    W = data.frame(x    = c(0,1,2,2),
                   xend = c(0,0,1,2),
                   y    = c(0,2,0,0),
                   yend = c(4,0,2,4)),
    X = data.frame(x    = c(0,0),
                   xend = c(2,2),
                   y    = c(4,0),
                   yend = c(0,4)),
    Y = data.frame(x    = c(0,1,1),
                   xend = c(1,2,1),
                   y    = c(4,2,0),
                   yend = c(2,4,2)),
    Z = data.frame(x    = c(0,0,0),
                   xend = c(2,2,2),
                   y    = c(0,0,4),
                   yend = c(0,4,4)),
    a = data.frame(x    = c(0,0,0,1,1,2),
                   xend = c(0,1,1,2,2,2),
                   y    = c(0,0,1,2,0,0),
                   yend = c(1,0,2,2,1,2)),
    b = data.frame(x    = c(0,0,0,1,1),
                   xend = c(0,1,1,2,2),
                   y    = c(0,0,2,0,2),
                   yend = c(4,0,2,1,1)),
    "c" = data.frame(x  = c(0,0,1,1),
                   xend = c(1,1,2,2),
                   y    = c(1,1,0,2),
                   yend = c(0,2,0,2)),
    d = data.frame(x    = c(0,0,1,1,2),
                   xend = c(1,1,2,2,2),
                   y    = c(1,1,0,2,0),
                   yend = c(0,2,0,2,4)),
    e = data.frame(x    = c(0,0,1,0,2,1),
                   xend = c(1,1,2,2,2,2),
                   y    = c(1,1,2,1,1,0),
                   yend = c(0,2,2,1,2,0)),
    f = data.frame(x    = c(0,0,1,0),
                   xend = c(0,1,2,1),
                   y    = c(0,3,4,2),
                   yend = c(3,4,4,2)),
    g = data.frame(x    = c(0,0,1,1, 2, 0,1),
                   xend = c(1,1,2,2, 2, 1,2),
                   y    = c(1,1,0,2,-1,-1,-2),
                   yend = c(0,2,0,2, 2,-2,-1)),
    h = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(0,1,2,0),
                   yend = c(4,2,2,2)),
    i = data.frame(x    = c(1,1,1),
                   xend = c(1,2,1),
                   y    = c(1,1,3),
                   yend = c(2,0,4)),
    j = data.frame(x    = c( 0, 1, 2,2),
                   xend = c( 1, 2, 2,2),
                   y    = c(-1,-2,-1,3),
                   yend = c(-2,-1, 2,4)),
    k = data.frame(x    = c(0,0,1),
                   xend = c(0,2,2),
                   y    = c(0,0,1),
                   yend = c(4,2,0)),
    l = data.frame(x    = c(1,1),
                   xend = c(1,2),
                   y    = c(1,1),
                   yend = c(3,0)),
    m = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(0,2,0,0),
                   yend = c(2,0,2,2)),
    n = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(0,2,2,1),
                   yend = c(2,2,1,0)),
    o = data.frame(x    = c(0,0,1,1),
                   xend = c(1,1,2,2),
                   y    = c(1,1,2,0),
                   yend = c(0,2,1,1)),
    p = data.frame(x    = c( 0,0,0,1,1),
                   xend = c( 0,1,1,2,2),
                   y    = c(-2,2,0,0,2),
                   yend = c( 2,2,0,1,1)),
    q = data.frame(x    = c(0,0,1,1,2),
                   xend = c(1,1,2,2,2),
                   y    = c(1,1,0,2,-2),
                   yend = c(0,2,0,2,2)),
    r = data.frame(x    = c(0,0,1),
                   xend = c(0,1,2),
                   y    = c(0,1,2),
                   yend = c(2,2,2)),
    s = data.frame(x    = c(0,1,0,0,1),
                   xend = c(1,2,2,1,2),
                   y    = c(0,0,1,1,2),
                   yend = c(0,1,1,2,2)),
    t = data.frame(x    = c(0,0,0,1),
                   xend = c(0,1,1,2),
                   y    = c(1,1,2,0),
                   yend = c(3,0,2,0)),
    u = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(1,1,0,0),
                   yend = c(2,0,0,2)),
    v = data.frame(x    = c(0,1),
                   xend = c(1,2),
                   y    = c(2,0),
                   yend = c(0,2)),
    w = data.frame(x    = c(0,0,1,2),
                   xend = c(0,1,2,2),
                   y    = c(0,0,2,0),
                   yend = c(2,2,0,2)),
    x = data.frame(x    = c(0,0),
                   xend = c(2,2),
                   y    = c(0,2),
                   yend = c(2,0)),
    y = data.frame(x    = c(0,0,1, 0, 1, 2),
                   xend = c(0,1,2, 1, 2, 2),
                   y    = c(1,1,0,-1,-2,-1),
                   yend = c(2,0,0,-2,-1, 2)),
    z = data.frame(x    = c(0,0,0),
                   xend = c(2,2,2),
                   y    = c(0,0,2),
                   yend = c(0,2,2)),
    "0" = data.frame(x    = c(0,0,0,1,1,2,0),
                     xend = c(0,1,1,2,2,2,2),
                     y    = c(1,1,3,0,4,1,3),
                     yend = c(3,0,4,1,3,3,1)),
    "1" = data.frame(x    = c(1,2),
                     xend = c(2,2),
                     y    = c(3,0),
                     yend = c(4,4)),
    "2" = data.frame(x    = c(0,0,1,0),
                     xend = c(2,1,2,2),
                     y    = c(0,3,4,0),
                     yend = c(0,4,3,3)),
    "3" = data.frame(x    = c(0,0,1,1,1,1),
                     xend = c(1,1,2,2,2,2),
                     y    = c(1,3,0,4,2,2),
                     yend = c(0,4,1,3,3,1)),
    "4" = data.frame(x    = c(0,0,1),
                     xend = c(1,2,1),
                     y    = c(1,1,0),
                     yend = c(4,1,2)),
    "5" = data.frame(x    = c(0,0,0,0,1,1),
                     xend = c(0,2,1,1,2,2),
                     y    = c(2,4,2,0,2,0),
                     yend = c(4,4,2,0,1,1)),
    "6" = data.frame(x    = c(0,0,0,0,1,1),
                     xend = c(1,0,1,1,2,2),
                     y    = c(2,1,1,2,0,2),
                     yend = c(4,2,0,2,1,1)),
    "7" = data.frame(x    = c(0,1,1),
                     xend = c(2,2,2),
                     y    = c(4,0,2),
                     yend = c(4,4,2)),
    "8" = data.frame(x    = c(0,0,0,1,0,1),
                     xend = c(2,2,1,2,1,2),
                     y    = c(1,3,3,4,1,0),
                     yend = c(3,1,4,3,0,1)),
    "9" = data.frame(x    = c(0,0,1,1,2,1),
                     xend = c(1,1,2,2,2,2),
                     y    = c(3,3,4,2,2,0),
                     yend = c(4,2,3,2,3,2)),
    "?" = data.frame(x    = c(0,1,1,1),
                     xend = c(1,2,2,1),
                     y    = c(3,4,3,0),
                     yend = c(4,3,2,1)),
    "!" = data.frame(x    = c(1,1),
                     xend = c(1,1),
                     y    = c(2,0),
                     yend = c(4,1)),
    "." = data.frame(x    = c(1),
                     xend = c(1),
                     y    = c(0),
                     yend = c(1)),
    "," = data.frame(x    = c(1),
                     xend = c(1),
                     y    = c(-1),
                     yend = c(0)),
    "-" = data.frame(x    = c(0),
                     xend = c(1),
                     y    = c(2),
                     yend = c(2)),
    ";" = data.frame(x    = c(1,1),
                     xend = c(1,1),
                     y    = c(1,-1),
                     yend = c(2,0)),
    .alpha = data.frame(x    = c(0,0,0),
                        xend = c(0,2,2),
                        y    = c(0,0,2),
                        yend = c(2,2,0)),
    .beta = data.frame(x    = c(0,0,0,0,1,1,1,1),
                       xend = c(0,1,1,1,2,2,2,2),
                       y    = c(-1,0,2,4,0,2,2,4),
                       yend = c(4,0,2,4,1,1,3,3)),
    .gamma = data.frame(x    = c(0,1,1),
                        xend = c(1,2,1),
                        y    = c(3,0,-1),
                        yend = c(0,3,0)),
    .delta = data.frame(x    = c(0,0,0,0,0,0,1),
                        xend = c(0,1,1,2,0,1,2),
                        y    = c(1,2,1,3,3,4,0),
                        yend = c(2,2,0,1,4,4,1)),
    .epsilon = data.frame(x    = c(0,0,0,0),
                          xend = c(2,2,2,2),
                          y    = c(2,1,1,0),
                          yend = c(2,2,0,0)),
    .lambda = data.frame(x    = c(0,0),
                         xend = c(2,1),
                         y    = c(4,0),
                         yend = c(0,2)),
    .mu = data.frame(x    = c(0,0,1,2),
                     xend = c(0,1,2,2),
                     y    = c(-1,0,0,1),
                     yend = c(2,0,1,2)),
    .pi = data.frame(x    = c(0,0,1,1),
                     xend = c(0,2,2,2),
                     y    = c(0,2,1,1),
                     yend = c(2,2,2,0)),
    .sigma = data.frame(x    = c(0,0,0,1,1),
                        xend = c(0,2,1,2,2),
                        y    = c(1,2,1,2,0),
                        yend = c(2,2,0,1,1))
  ),
  italic = list(

  )

)

#' Join backstitch lettering into a single segment data.frame
#'
#' @param x A character vector containing the lettering to use.
#' @param anchor The x,y position of the bottom-left corner of the first letter. Default is c(0,0)
#' @param scale Scale of the lettering. I recommend you use integer values to make stitching easier! Default is 1.
#' @param set Backstitch set to use. Default is "simple". Use names(backstitch_segments) to see all available.
#'
#' @return A segment data.frame with x, xend, y, and yend for use with geom_segment in ggplot2
#' @export
#'
#' @examples
#' backstitch_df <- backstitch_join("Easy backstitch phrases!",
#'                                  anchor = c(-10, 10),
#'                                  scale = 2,
#'                                  set = "simple")
#'
#' head(backstitch_df)
#'
backstitch_join <- function(x,
                            anchor = c(0,0),
                            scale = 1,
                            set = "simple") {

  char_dims <- backstitch_segments[[set]]$.setdims
  available_chars <- names(backstitch_segments[[set]])
  available_chars <- available_chars[available_chars != ".setdims"]

  chars <- unlist(strsplit(x, ""))

  x_spacing <- char_dims$xmax + char_dims$spacing

  out_df <- data.frame(x = numeric(),
                        xend = numeric(),
                        y = numeric(),
                        yend = numeric())

  for(i in 1:length(chars)) {
    char_df <- backstitch_segments[[set]][[chars[i]]]
    char_df$x = char_df$x + x_spacing * (i - 1)
    char_df$xend = char_df$xend + x_spacing * (i - 1)
    out_df <- rbind(out_df, char_df)
  }

  out_df <- out_df * scale

  out_df$x <- out_df$x + anchor[1]
  out_df$xend <- out_df$xend + anchor[1]
  out_df$y <- out_df$y + anchor[2]
  out_df$yend <- out_df$yend + anchor[2]

  out_df
}


#' Add backstitch lettering into a ggplot2 object.
#'
#' @param p The ggplot2 plot to use.
#' @param x A character vector with the phrase to add to the plot.
#' @param anchor The bottom-left position of the phrase in the plot.
#' @param scale Scale of the lettering. I recommend you use integer values to make stitching easier! Default is 1.
#' @param set Backstitch set to use. Default is "simple". Use names(backstitch_segments) to see all available.
#'
#' @return A ggplot2 plot object with an added geom_segment() layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' p <- ggplot() +
#'   geom_point(data = iris,
#'              aes(x = Sepal.Length,
#'                  y = Petal.Length,
#'                  color = Species))
#'
#' p %>%
#'   stitch_lettering("Nice 150 irises!",
#'                    anchor = c(4,6),
#'                    scale = 0.1,
#'                    set = "simple")
#'
stitch_lettering <- function(p, x,
                             anchor = c(0,0),
                             scale = 1,
                             set = "simple") {

  lettering_df <- backstitch_join(x, anchor, scale, set)

  p +
    geom_segment(data = lettering_df,
                 aes(x = x, xend = xend,
                     y = y, yend = yend))

}
