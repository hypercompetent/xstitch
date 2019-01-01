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
