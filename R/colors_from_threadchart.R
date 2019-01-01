library(dplyr)
library(purrr)
library(rvest)
options(stringsAsFactors = F)

home_url <- "http://threadchart.info"

home_html <- read_html(home_url)

home_links <- html_nodes(home_html, xpath = "//li/a") %>%
  html_attr("href")

link_names <- html_nodes(home_html, xpath = "//li/a") %>%
  html_text()

home_links <- paste0(home_url, home_links)

threadchart_colors <- map_dfr(1:length(home_links), function(i){
  chart_html <- read_html(home_links[i])
  color_table <- html_nodes(chart_html, xpath = "//table/tr/td/table")
  color_cells <- html_nodes(color_table, xpath = "//tr/td")
  color_hexes <- html_attr(color_cells, "bgcolor")
  color_hexes <- color_hexes[!is.na(color_hexes)]

  color_names <- html_nodes(color_cells, xpath = "//font") %>%
    as.character()

  color_strings <- strsplit(color_names, "<br>")

  color_id <- map_chr(color_strings,
                      function(x) {
                        sub("<.+>","",x[1])
                      })

  color_source <- map_chr(color_strings, 2)

  color_name <- map_chr(color_strings,
                        function(x) {
                          sub("<.+>","",x[3])
                        })

  data.frame(source = color_source,
             id = color_id,
             name = color_name,
             color = color_hexes)
})

final_colors <- threadchart_colors %>%
  filter(source != "") %>%
  mutate(source = sub("DesignsbySick","DesignsBySick", source)) %>%
  mutate(source = sub(" - Embroidex - "," Embroidex", source))

final_colors <- final_colors %>%
  mutate(r_color = nearest_r_color(color))

# how close are these to what I parsed from the Anchor pdf?

tc_anchor_colors <- final_colors %>%
  filter(grepl("Anchor",source)) %>%
  mutate(id = as.numeric(sub(".+#","",id)))

names(tc_anchor_colors)[-2] <- paste0("tc_",names(tc_anchor_colors)[-2])

pdf_anchor_colors <- read.csv(system.file("inst/data/","anchor_solid_colors.csv", package = "xstitch"))

both <- pdf_anchor_colors %>%
  left_join(tc_anchor_colors)

library(ggplot2)

ggplot() +
  geom_tile(aes(x = 1:10,
                y = 1),
            fill = both$med_color[1:10]) +
  geom_tile(aes(x = 1:10,
                y = 2),
            fill = both$tc_color[1:10])

# Overall, I think the tc colors are quite good. Maybe a bit more saturated
# than they look in the pdf, but I think they match quite well.

sources <- unique(final_colors$source)

walk(sources, function(x) {
  source_colors <- final_colors %>%
    filter(source == x)

  out_name <- paste0("inst/data/",gsub(" ","_",x),".csv")

  write.csv(source_colors, out_name, row.names = F)
})

col2ab <- function(hexes) {
  rgbs <- col2rgb(hexes) / 255
  alphas <- rgbs["red",] - 0.5 * (rgbs["green",] + rgbs["blue",])
  betas <- sqrt(3) / 2 * (rgbs["green",] - rgbs["blue",])
  return(data.frame(color = hexes,
                    alpha = alphas,
                    beta = betas))
}

plot_colorspace <- function(hexes,
                            show_pures = TRUE) {

  data <- col2ab(hexes)

  p <- ggplot(data) +
    geom_point(aes(x = alpha,
                   y = beta,
                   color = color),
               size = 2) +
    scale_color_identity() +
    theme_classic() +
    scale_x_continuous(limits = c(-1.1,1.1)) +
    scale_y_continuous(limits = c(-1.1,1.1))

  if(show_pures) {
    pure_colors <- c("#FF0000","#FFFF00","#00FF00","#00FFFF","#0000FF","#FF00FF")
    pure_df <- col2ab(pure_colors)
    p <- p + geom_point(data = pure_df,
                        aes(x = alpha,
                            y = beta,
                            fill = color),
                        size = 4,
                        pch = 21) +
      scale_fill_identity()
  }

  return(p)
}

r_rgb <- col2rgb(colors(distinct = TRUE))
colnames(r_rgb) <- colors(distinct = TRUE)
r_hex <- map_chr(1:ncol(r_rgb), function(x) {v <- r_rgb[,x]; rgb(v[1],v[2],v[3], maxColorValue = 255)})

plot_colorspace(r_hex)

plot_colorspace(final_colors$color[final_colors$source == "FuFu Polyester"])
