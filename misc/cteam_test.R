library(dplyr)
library(purrr)
library(png)
library(ggplot2)
options(stringsAsFactors = F)
source("R/swatches.R")

# Excellent C-Team pixel art by Eric Alloway
# Twitter: @Iscaneus Patreon: https://www.patreon.com/ericalloway
# Post: https://twitter.com/Iscaneus/status/1115405487049199616
test.png <- "misc/iscaneus_cteam_test.png"

cteam_raw <- readPNG(test.png)

color_to_trim <- extract_swatch(cteam_raw,
                             1,1,1,1)

extract_png <- function(arr, xmin, xmax, ymin, ymax) {
  n <- length(xmin:xmax) * length(ymin:ymax)
  res <- matrix(0, ncol = 3, nrow = n)
  res[,1] <- as.numeric(arr[ymin:ymax, xmin:xmax, 1])
  res[,2] <- as.numeric(arr[ymin:ymax, xmin:xmax, 2])
  res[,3] <- as.numeric(arr[ymin:ymax, xmin:xmax, 3])
  colnames(res) <- c("r","g","b")
  res
}

trim_color <- function(arr,
                       margin,
                       color) {
  n_rows <- dim(arr)[1]
  n_cols <- dim(arr)[2]

  if(margin == 1) {
    remove_rows <- integer()
    for(i in seq_along(1:n_rows)) {
      row_colors <- extract_png(arr,
                                   1, n_cols,
                                   i, i)
      row_matches <- apply(row_colors,
                           1,
                           function(x) {sum(x == color) == 3})

      if(sum(row_matches) == n_cols) {
        remove_rows <- c(remove_rows, i)
      }
    }
    arr <- arr[-remove_rows, , ]

  } else if(margin == 2) {
    remove_cols <- integer()
    for(i in seq_along(1:n_cols)) {
      col_colors <- extract_png(arr,
                                   i, i,
                                   1, n_rows)
      col_matches <- apply(col_colors,
                           1,
                           function(x) {sum(x == color) == 3})

      if(sum(col_matches) == n_rows) {
        remove_cols <- c(remove_cols, i)
      }
    }
    print(remove_cols)

    arr <- arr[, -remove_cols, ]
  }

  arr

}

cteam_trimmed <- trim_color(cteam_raw,
                            1,
                            color_to_trim)

cteam_trimmed <- trim_color(cteam_trimmed,
                            2,
                            color_to_trim)

writePNG(cteam_trimmed,
         "misc/iscaneus_cteam_trimmed.png")

png_to_xyhex <- function(arr,
                         bg_color = NULL) {

  if(!is.null(bg_color)) {
    if(class(bg_color) == "character") {
      bg_color <- t(col2rgb(bg_color))
      colnames(bg_color) <- c("r","g","b")
    }
  }

  n_pixels <- dim(arr)[1] * dim(arr)[2]

  out_df <- data.frame(x = numeric(),
                       y = numeric(),
                       color = character())

  n_rows <- dim(arr)[1]
  n_cols <- dim(arr)[2]
  for(i in seq_along(1:n_rows)) {
    row_colors <- extract_png(arr,
                              1, n_cols,
                              i, i)

    if(!is.null(bg_color)) {
      keep_cols <- which(apply(row_colors,
                                 1,
                                 function(x) {sum(x == color) != 3}))
    } else {
      keep_cols <- 1:n_cols
    }

    row_hexes <- rgb(row_colors[keep_cols,1],
                     row_colors[keep_cols,2],
                     row_colors[keep_cols,3])

    row_df <- data.frame(x = keep_cols,
                         y = n_rows - i + 1,
                         color = row_hexes)

    out_df <- rbind(out_df,
                    row_df)
  }
  out_df
}

cteam_df <- png_to_xyhex(cteam_trimmed,
                         bg_color = color_to_trim)

cteam_df_half <- cteam_df %>%
  filter(x %% 2 == 0,
         y %% 2 == 0) %>%
  mutate(x = x / 2,
         y = y / 2)

library(xstitch)
p <- ggplot(cteam_df_half) +
  geom_point(aes(x = x,
                 y = y,
                 color = color),
             shape = 15,
             size = 0.5) +
  scale_color_identity()

p <- stitch_lettering(p,
                      "Time Is The Enemy",
                      anchor = c(70,100),
                      scale = 2)

p <- add_grid(p,
              x_range = c(1,240),
              y_range = c(1,110))

p

ggsave("misc/cteam_test.pdf",
       p,
       width = 10,
       height = 10 * 110 / 240)
ggsave("misc/cteam_test.png",
       p,
       width = 10,
       height = 10 * 110 / 240)

# 71 colors is a bit complex.

colorspace_plot(unique(cteam_df_half$color))

color_freq <- cteam_df_half %>%
  group_by(color) %>%
  summarise(n = n())

ggplot(cteam_df_half) +
  geom_bar(aes(x = color,
               fill = color)) +
  scale_fill_identity() +
  coord_flip()

color_mean <- function(color_vec) {
  rgbmat <- grDevices::col2rgb(color_vec)/255
  means <- rowMeans(rgbmat)
  rgb(means[1], means[2], means[3])
}

merge_nearest_colors <- function(hexes,
                                 k = 12) {
  unique_hexes <- unique(hexes)

  while(length(unique_hexes) > k) {
    unique_rgb <- col2rgb(unique_hexes)
    color_distances <- as.matrix(dist(t(unique_rgb)))
    color_distances[lower.tri(color_distances, diag = TRUE)] <- NA
    colnames(color_distances) <- rownames(color_distances) <- unique_hexes

    min_distance <- min(color_distances[upper.tri(color_distances)])

    min_matches <- subset(na.omit(data.frame(expand.grid(dimnames(color_distances)),
                                             value = c(color_distances))),
                          value == min_distance)
    min_matches$Var1 <- as.character(min_matches$Var1)
    min_matches$Var2 <- as.character(min_matches$Var2)
    col1 <- min_matches[1,1]
    col2 <- min_matches[1,2]

    freq1 <- sum(hexes == col1)
    freq2 <- sum(hexes == col2)

    if(freq1 > freq2) {
      hexes[hexes == col2] <- col1
    } else if(freq1 < freq2) {
      hexes[hexes == col1] <- col2
    } else if(freq1 == freq2) {
      new_col <- color_mean(col1, col2)
      hexes[hexes %in% c(col1, col2)] <- new_col
    }

    unique_hexes <- unique(hexes)
  }

  hexes
}

simpler_cteam_df_half <- cteam_df_half %>%
  mutate(color = merge_nearest_colors(color,
                                      k = 20))
ggplot(simpler_cteam_df_half) +
  geom_bar(aes(x = color,
               fill = color)) +
  scale_fill_identity() +
  coord_flip()

p <- ggplot(simpler_cteam_df_half) +
  geom_point(aes(x = x,
                 y = y,
                 color = color),
             shape = 15,
             size = 0.5) +
  scale_color_identity()

p <- stitch_lettering(p,
                      "Time Is The Enemy",
                      anchor = c(70,100),
                      scale = 2)

p <- add_grid(p,
              x_range = c(1,240),
              y_range = c(1,110))

p

ggsave("misc/simpler_cteam_test.png",
       p,
       width = 10,
       height = 10 * 110 / 240)
