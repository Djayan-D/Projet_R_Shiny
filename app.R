library(readr)
Food_Recipe <- read_csv("data/Food_Recipe.csv")

library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  navlistPanel(
    id = "onglet",
    tabPanel("Recherche par carte"),
    tabPanel("Recherche selon caractéristiques"),
    tabPanel("Recherche"),
    tabPanel("Information",
      "Cette application a été créée dans le cadre du cours Dataviz : RShiny du Master 1 ECAP.\n Pour plus d'information, contactez "
    )
  )
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)
