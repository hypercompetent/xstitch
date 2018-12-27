# xstitch
### Functions to assist with cross-stitch pattern generation


### Installation
```
if(!"devtools" %in% installed_packages()) {
  install.packages("devtools")
}

devtools::install_github("hypercompetent/xstitch")
```

### Demo

```
library(reshape2)
library(ggplot2)
library(xstitch)

binned_volcano <- floor(volcano / 20)

volcano_df <- melt(binned_volcano)
names(volcano_df) <- c("x","y","value")

# factor conversion for plotting
volcano_df$value <- as.factor(volcano_df$value)

p <- ggplot(vdf) +
       geom_point(aes(x = x,
                      y = y, 
                      pch = value, 
                      color = value))

p <- stitch_lettering(p,
                      "Maunga Whau", 
                      anchor = c(30,64))

p <- add_grid(p, 
              x_range = c(1,90), 
              y_range = c(1,70))

p
```
