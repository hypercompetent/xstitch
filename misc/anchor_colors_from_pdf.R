library(pdftools)
library(dplyr)
library(purrr)
options(stringsAsFactors = F)

pdf_file <- "misc/yarn_shadecard_anchor stranded cotton5c05c434898ea.pdf"

test.png <- "misc/test.png"

# render the page as a raw bitmap to extract colors
p2 <- pdf_render_page(pdf_file, page = 2)

dim(p2)
# 3d bitmap array.
# dims [1:3, ,] are r,g,b as raw hex values
# dim [4, ,] is alpha?
# other dims are [,x,y] positions in the bitmap

# dialing in swatch locations

px <- p2

# Top-left of first column with a little padding
px[1, ,148] <- as.raw(0)
px[1, 313,] <- as.raw(0)

# Bottom-right of the first swatch
px[1, ,164] <- as.raw(0)
px[1, 342,] <- as.raw(0)

png::writePNG(px, test.png)

# looks like swatches are ~ 29 x 17 (with padding) at 72 dpi
# spacing is probably vertically ~ 4-5 px.
# there are 29 swatches in the first column and 31 in the other 2 columns.
# let's try finding the first column's pixels and see how it goes

sw_h <- 13
sw_w <- 29
sw_vp <- 8

px <- p2
px[1,313:(313 + sw_w),148:160 + rep((sw_h + sw_vp) * 0:28, sw_h)] <- as.raw(0)
# column 2
px[1,381:(381 + sw_w),148:160 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)
# column 3
px[1,450:(450 + sw_w),148:160 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)
# column 4
px[1,515:(515 + sw_w),148:160 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)

png::writePNG(px, test.png)

# knocking down the height a bit and increasing the vertical padding
# results in better targeting without hitting whitespace or adjacent colors
# Note that the columns aren't consistently horizontally spaced.

# This won't be consistent across pages because the swatch positions are a bit different
# on each. ugh.

extract_swatch <- function(arr, xmin, xmax, ymin, ymax) {
  n <- length(xmin:xmax) * length(ymin:ymax)
  res <- matrix(0, ncol = 3, nrow = n)
  res[,1] <- as.numeric(arr[1, xmin:xmax, ymin:ymax])
  res[,2] <- as.numeric(arr[2, xmin:xmax, ymin:ymax])
  res[,3] <- as.numeric(arr[3, xmin:xmax, ymin:ymax])
  colnames(res) <- c("r","g","b")
  res
}

swatch_most_frequent_hex <- function(swatch_mat) {
  u <- unique(swatch_mat)
  uc <- apply(u, 1, function(x) {
    sum(apply(swatch_mat, 1, function(y) {
      identical(y,x)
    }))
  })
  mfu <- u[which(uc == max(uc)),]

  if(!is.null(dim(mfu))) {
    mfu <- mfu[1,]
  }

  rgb(mfu["r"],
      mfu["g"],
      mfu["b"],
      maxColorValue = 255)
}

swatch_median_hex <- function(swatch_mat) {
  sums <- rowSums(swatch_mat)
  sums_med <- median(sums)
  if(!sums_med %in% sums) {
    med_diffs <- abs(sums_med - sums)
    sums_med <- sums[which(med_diffs == min(med_diffs))]
  }

  med_mat <- swatch_mat[which(sums %in% sums_med),,drop = FALSE]

  r <- floor(median(med_mat[,"r"]))
  g <- floor(median(med_mat[,"g"]))
  b <- floor(median(med_mat[,"b"]))

  rgb(r, g, b, maxColorValue = 255)
}

swatch_min_hex <- function(swatch_mat) {
  sums <- rowSums(swatch_mat)
  sums_min <- min(sums)

  min_mat <- swatch_mat[which(sums == sums_min),,drop = FALSE]

  r <- floor(median(min_mat[,"r"]))
  g <- floor(median(min_mat[,"g"]))
  b <- floor(median(min_mat[,"b"]))

  rgb(r, g, b, maxColorValue = 255)
}

swatch_max_hex <- function(swatch_mat) {
  sums <- rowSums(swatch_mat)
  sums_max <- max(sums)

  max_mat <- swatch_mat[which(sums == sums_max),,drop = FALSE]

  r <- floor(median(max_mat[,"r"]))
  g <- floor(median(max_mat[,"g"]))
  b <- floor(median(max_mat[,"b"]))

  rgb(r, g, b, maxColorValue = 255)
}

swatch_filter_value <- function(swatch_mat,
                                prop = 0.3) {
  swatch_hsv <- rgb2hsv(t(swatch_mat))
  cutoff <- quantile(swatch_hsv["v",], probs = prop)
  keep <- swatch_hsv["v",] >= cutoff
  swatch_mat[keep,]
}

sw1 <- extract_swatch(p2, 313, 313 + sw_w, 148, 148 + sw_h)
# filtering to a hsv value >= 0.5 of what's displayed
# feels truer to the percieved color to me. This
# keeps the lowlights in the swatch image from
# dominating the colors.

sw1 <- swatch_filter_value(sw1, prop = 0.5)
sw1_cols <- c(swatch_min_hex(sw1),
              swatch_median_hex(sw1),
              swatch_max_hex(sw1))

ggplot() +
  geom_tile(aes(x = 1, y = 1:3),
            fill = sw1_cols) +
  scale_fill_identity()

# looks pretty good. Let's do it systematically.
# here's a reminder of the parameters defined above.
sw_h <- 13
sw_w <- 29
sw_vp <- 81

build_col_locs <- function(xmin, ymin, sw_w, sw_h, sw_vp, n) {
  data.frame(xmin = xmin,
             xmax = xmin + sw_w,
             ymin = ymin + rep((sw_h + sw_vp) * (1:n - 1)),
             ymax = ymin + sw_h + rep((sw_h + sw_vp) * (1:n - 1)))
}


p2_col_xmins <- c(313, 381, 450, 515)
p2_col_ymin <- 148
p2_col_n <- c(29, 31, 31, 31)

p2_locs <- map_dfr(1:4,
                   function(x) {
                     df <- build_col_locs(xmin = p2_col_xmins[x],
                                          ymin = p2_col_ymin,
                                          sw_w = sw_w,
                                          sw_h = sw_h,
                                          sw_vp = sw_vp,
                                          n = p2_col_n[x])
                     df$page <- 2
                     df$pcol <- x
                     df$coln <- 1:p2_col_n[x]
                     df
                   })

p2_colors <- map_dfr(1:nrow(p2_locs),
                     function(i) {
                       sw <- extract_swatch(p2,
                                            xmin = p2_locs$xmin[i],
                                            xmax = p2_locs$xmax[i],
                                            ymin = p2_locs$ymin[i],
                                            ymax = p2_locs$ymax[i]) %>%
                         swatch_filter_value(prop = 0.3)
                       data.frame(page = p2_locs$page[i],
                                  pcol = p2_locs$pcol[i],
                                  coln = p2_locs$coln[i],
                                  min_color = swatch_min_hex(sw),
                                  med_color = swatch_median_hex(sw),
                                  max_color = swatch_max_hex(sw))
                     })
ggplot() +
  geom_tile(aes(x = 1,
                y = 1:nrow(p2_colors)),
            fill = p2_colors$min_color) +
  geom_tile(aes(x = 2,
                y = 1:nrow(p2_colors)),
            fill = p2_colors$med_color) +
  geom_tile(aes(x = 3,
                y = 1:nrow(p2_colors)),
            fill = p2_colors$max_color) +
  scale_fill_identity()

# I think I like the 0.3 cutoff better here.
# Some of the colors are better distinguished
# by their lowlights/min than their medians.

# dialing in page 3
p3 <- pdf_render_page(pdf_file, page = 3)

sw_h <- 13
sw_w <- 29
sw_vp <- 8

px <- p3
# column 1: 32 colors
px[1,31:(31 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:31, sw_h)] <- as.raw(0)
# column 2: 30 colors
px[1,99:(99 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:29, sw_h)] <- as.raw(0)
# column 3: 31 colors
px[1,168:(168 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)
# column 4: 31 colors
px[1,237:(237 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)
# column 5: 33 colors
px[1,306:(306 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:32, sw_h)] <- as.raw(0)
# column 6: 31 colors
px[1,375:(375 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:30, sw_h)] <- as.raw(0)
# column 7: 33 colors
px[1,444:(444 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:32, sw_h)] <- as.raw(0)
# column 8: 33 colors
px[1,514:(514 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:32, sw_h)] <- as.raw(0)

png::writePNG(px, test.png)

p3_col_xmins <- c(31, 99, 168, 237, 306, 375, 444, 514)
p3_col_ymin <- 111
p3_col_n <- c(32, 30, 31, 31, 33, 31, 33, 33)

p3_locs <- map_dfr(1:8,
               function(x) {
                 df <- build_col_locs(xmin = p3_col_xmins[x],
                                      ymin = p3_col_ymin,
                                      sw_w = sw_w,
                                      sw_h = sw_h,
                                      sw_vp = sw_vp,
                                      n = p3_col_n[x])
                 df$page <- 3
                 df$pcol <- x
                 df$coln <- 1:p3_col_n[x]
                 df
               })

p3_colors <- map_dfr(1:nrow(p3_locs),
                     function(i) {
                       sw <- extract_swatch(p3,
                                            xmin = p3_locs$xmin[i],
                                            xmax = p3_locs$xmax[i],
                                            ymin = p3_locs$ymin[i],
                                            ymax = p3_locs$ymax[i]) %>%
                         swatch_filter_value(prop = 0.3)
                       data.frame(page = p3_locs$page[i],
                                  pcol = p3_locs$pcol[i],
                                  coln = p3_locs$coln[i],
                                  min_color = swatch_min_hex(sw),
                                  med_color = swatch_median_hex(sw),
                                  max_color = swatch_max_hex(sw))
                     })

# Dialing in page 4 (only 2 columns)
p4 <- pdf_render_page(pdf_file, page = 4)

sw_h <- 13
sw_w <- 29
sw_vp <- 8

px <- p4
# column 1: 34 colors
px[1,29:(29 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:33, sw_h)] <- as.raw(0)
# column 2: 30 colors
px[1,93:(93 + sw_w),111:123 + rep((sw_h + sw_vp) * 0:33, sw_h)] <- as.raw(0)

png::writePNG(px, test.png)


p4_col_xmins <- c(29, 93)
p4_col_ymin <- 111
p4_col_n <- c(34, 34)

p4_locs <- map_dfr(1:2,
                   function(x) {
                     df <- build_col_locs(xmin = p4_col_xmins[x],
                                          ymin = p4_col_ymin,
                                          sw_w = sw_w,
                                          sw_h = sw_h,
                                          sw_vp = sw_vp,
                                          n = p4_col_n[x])
                     df$page <- 4
                     df$pcol <- x
                     df$coln <- 1:p4_col_n[x]
                     df
                   })

p4_colors <- map_dfr(1:nrow(p4_locs),
                     function(i) {
                       sw <- extract_swatch(p4,
                                            xmin = p4_locs$xmin[i],
                                            xmax = p4_locs$xmax[i],
                                            ymin = p4_locs$ymin[i],
                                            ymax = p4_locs$ymax[i]) %>%
                         swatch_filter_value(prop = 0.3)
                       data.frame(page = p4_locs$page[i],
                                  pcol = p4_locs$pcol[i],
                                  coln = p4_locs$coln[i],
                                  min_color = swatch_min_hex(sw),
                                  med_color = swatch_median_hex(sw),
                                  max_color = swatch_max_hex(sw))
                     })

# Now I need to extract the ID numbers.

data <- pdf_data(pdf_file)
names(data) <- c("p1","p2","p3","p4")

p2_ids <- data.frame(page = 2,
                     pcol = unlist(map(1:4, function(x) {rep(x, p2_col_n[x])})),
                     coln = unlist(map(1:4, function(x) {1:p2_col_n[x]})),
                     id = c(data$p2$text[data$p2$x == 349],
                            data$p2$text[data$p2$x %in% c(413,418)],
                            data$p2$text[data$p2$x %in% c(483,487)],
                            data$p2$text[data$p2$x %in% c(547,551)])
                     )
p3_ids <- data.frame(page = 3,
                     pcol = unlist(map(1:8, function(x) {rep(x, p3_col_n[x])})),
                     coln = unlist(map(1:8, function(x) {1:p3_col_n[x]})),
                     id = c(data$p3$text[data$p3$x == 63],
                            data$p3$text[data$p3$x == 132],
                            data$p3$text[data$p3$x == 200],
                            data$p3$text[data$p3$x == 270],
                            data$p3$text[data$p3$x == 338],
                            data$p3$text[data$p3$x == 407],
                            data$p3$text[data$p3$x == 476],
                            data$p3$text[data$p3$x == 547])
)
p4_ids <- data.frame(page = 4,
                     pcol = unlist(map(1:2, function(x) {rep(x, p4_col_n[x])})),
                     coln = unlist(map(1:2, function(x) {1:p4_col_n[x]})),
                     id = c(data$p4$text[data$p4$x == 60],
                            data$p4$text[data$p4$x == 125])
)

all_colors <- rbind(p2_colors,
                    p3_colors,
                    p4_colors)
all_ids <- rbind(p2_ids,
                 p3_ids,
                 p4_ids)

anchor_colors <- left_join(all_ids, all_colors)
# pad the values with zeroes to match the anchor table

library(stringr)

anchor_colors$id <- str_pad(anchor_colors$id,
                            5, pad = "0")

anchor_colors <- anchor_colors %>%
  select(-page, -pcol, -coln)

write.csv(anchor_colors,
          "inst/data/anchor_solid_colors.csv",
          row.names = F)
