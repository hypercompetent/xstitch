# Volcano pattern
vdf <- reshape2::melt(floor((volcano)/20))
vdf$value <- factor(vdf$value)

p <- ggplot(vdf) +
geom_point(aes(x = Var1,y = Var2, pch = value, color = value))

p <- stitch_lettering(p, "Maunga Whau", anchor = c(30,64))

p <- add_grid(p, x_range = c(1,90), y_range = c(1,70))

p
