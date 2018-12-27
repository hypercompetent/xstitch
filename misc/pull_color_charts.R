library(rvest)
library(purrr)
library(dplyr)

read_colors <- function(u) {
  h <- read_html(u)
  table_rows <- html_nodes(h, xpath = "//table/tr")
  id <- sub("\\r.+","",
            sub("<tr>\\n<td>\\r\\n[ ]+","",
                table_rows))
  id <- id[!grepl("<",id)]
  name <- sub("\\r.+","",
              sub(".+</span>[ ]+","",
                  table_rows))
  name <- name[!grepl("<",name)]
  swatches <- html_nodes(h, xpath = "//table/tr/td/span")
  colors <- sub(";.+","",
                sub(".+background-color:","",
                    as.character(swatches)))
  return(data.frame(id = id,
              color = colors,
              color_name = name))
}

dmc_anchor_url <- "https://www.cyberstitchers.com/stitching_tools/floss_conversion_charts/dmc_to_anchor/"

dmc_urls <- paste0(dmc_anchor_url,"page",1:7,"of7/")

dmc_colors <- map_dfr(dmc_urls,
                      read_colors)

dmc_colors <- dmc_colors %>%
  mutate(color = ifelse(grepl("^[^#]",color),
                        paste0("#",color),
                        color))
saveRDS(dmc_colors, "inst/dmc_colors.RData")
