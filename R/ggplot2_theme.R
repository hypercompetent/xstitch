#' Add gridding, arrows, and plotting theme for a cross-stitch plot
#'
#' @param p The plot object to use.
#' @param x_range The range of the x values in the plot. Default is c(0,100), but should be entered per-project.
#' @param y_range The range of the y values in the plot. Default is c(0,100), but should be entered per-project.
#' @param pad Stitch padding to add around the edges of the pattern. Default is 5.
#' @param center_arrows Logical, whether or not to include arrows pointing to the center stitch. Default is TRUE.
#' @param base_size Base font size for the plot theme. Default is 10. See ?ggplot2::theme() for more information.
#' @param base_family Base font family for the plot theme. Default is "", which will use system defaults.
#'
#' @return A ggplot2 object with grid, optional arrows, and theme applied.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' vmat <- floor((volcano - 100)/30)
#' vdf <- reshape2::melt(vmat)
#'
#' p <- ggplot() +
#'   geom_point(data = vdf,
#'              aes(x = Var1,
#'                  y = Var2,
#'                  pch = as.factor(value),
#'                  color = as.factor(value)))
#'
#' add_grid(p,
#'          x_range = c(1,87),
#'          y_range = c(1,61),
#'          center_arrows = T)
#'
add_grid <- function(p,
                     x_range = c(0, 100),
                     y_range = c(0, 100),
                     pad = 5,
                     center_arrows = TRUE,
                     base_size = 10,
                     base_family = "") {

  x_center <- floor(mean(x_range))
  y_center <- floor(mean(y_range))

  x_maj <- round(x_range / 10) * 10
  x_min <- x_range + c(-1 * pad, pad)

  y_maj <- round(y_range / 10) * 10
  y_min <- y_range + c(-1 * pad, pad)

  if(center_arrows) {

    top_arrow <- data.frame(x = x_center, xend = x_center,
                     y = y_range[2] + 3,
                     yend = y_range[2] + 1)
    bottom_arrow <- data.frame(x = x_center, xend = x_center,
                               y = y_range[1] - 3,
                               yend = y_range[1] - 1)
    left_arrow <- data.frame(x = x_range[1] - 3,
                             xend = x_range[1] - 1,
                             y = y_center, yend = y_center)
    right_arrow <- data.frame(x = x_range[2] + 3,
                              xend = x_range[2] + 1,
                              y = y_center, yend = y_center)

    arrow_df <- rbind(top_arrow, bottom_arrow, left_arrow, right_arrow)

    p <- p +
      geom_segment(data = arrow_df,
                   aes(x = x, xend = xend,
                       y = y, yend = yend),
                   arrow = arrow(type = "closed", length = unit(0.05, "inches")))
  }

  p +
    scale_x_continuous("",
                       labels = seq(x_maj[1],x_maj[2], by = 10),
                       breaks = seq(x_maj[1],x_maj[2], by = 10) + 0.5,
                       minor_breaks = x_min[1]:x_min[2] + 0.5,
                       expand = c(0,0)) +
    scale_y_continuous("",
                       labels = seq(y_maj[1],y_maj[2], by = 10),
                       breaks = seq(y_maj[1],y_maj[2], by = 10) + 0.5,
                       minor_breaks = y_min[1]:y_min[2] + 0.5,
                       expand = c(0,0)) +
    expand_limits(x = x_min,
                  y = y_min) +
    theme_bw(base_size = base_size,
             base_family = base_family) +
    theme(panel.grid.major = element_line(color = "gray50", size = 0.2))

}
