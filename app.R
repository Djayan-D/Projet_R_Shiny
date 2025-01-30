#---------- 1. PACKAGES ----------
#--- Djayan

library(readr)
library(shiny)
library(deeplr)


#--- Isaline






#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/Food_Recipe.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(data)


#---------- 3. UI ----------

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





#---------- 4. SERVEUR ----------

server <- function(input, output, session){
  
}





#---------- 5. LANCER L'APPLICATION ----------

shinyApp(ui = ui, server = server)
