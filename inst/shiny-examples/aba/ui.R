library(shiny)
library(ggplot2)

array_dims <- c(67, 41, 58)

ui <- pageWithSidebar(
  headerPanel("Allen Brain Atlas Cross-Stitch Generator"),
  sidebarPanel(
    selectInput(label = "Gene",
                inputId = "select_gene",
                choices = c("Drd1","Mapt","Npy","Pvalb","Rorb","Slc17a6","Sst","Vgf"),
                selected = "Drd1"),
    selectInput(label = "Plane",
                inputId = "select_plane",
                choices = c("Coronal","Saggital","Horizontal"),
                selected = "Coronal"),
    conditionalPanel("input.select_plane=='Coronal'",
                     sliderInput(label = "Coronal section",
                                  inputId = "numeric_section",
                                  min = 1,
                                  max = array_dims[1],
                                  value = 42,
                                  step = 1,
                                  width = "100%")),
    conditionalPanel("input.select_plane=='Saggital'",
                     sliderInput(label = "Saggital section",
                                  inputId = "numeric_section",
                                  min = 1,
                                  max = array_dims[2],
                                  value = 42,
                                  step = 1,
                                  width = "100%")),
    conditionalPanel("input.select_plane=='Horizontal'",
                     sliderInput(label = "Horizontal section",
                                  inputId = "numeric_section",
                                  min = 1,
                                  max = array_dims[3],
                                  value = 42,
                                  step = 1,
                                  width = "100%")),
    numericInput(label = "Number of colors",
                 inputId = "numeric_colors",
                 min = 1,
                 max = 12,
                 value = 4,
                 step = 1),
    checkboxInput(label = "Outline structures",
                  inputId = "logical_outline",
                  value = FALSE),
    checkboxInput(label = "Log scale",
                  inputId = "logical_log",
                  value = FALSE),
    selectInput(label = "Display mode",
                inputId = "select_mode",
                choices = c("Pattern","Stitch Preview"),
                selected = "Pattern",
                width = "100%")
  ),
  mainPanel()
)
