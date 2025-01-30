#---------- 1. PACKAGES ----------
#--- Djayan

library(readr)
library(shiny)


#--- Isaline
library(stringr)





#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/Food_Recipe.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(recette)

recette$cuisine <- as.factor(recette$cuisine)
recette$course <- as.factor(recette$course)
recette$diet <- as.factor(recette$diet)




regimes_disponibles <- c("None", unique(na.omit(recette$diet)))

#---------- 3. UI ----------

# inverser régime et allergènes + régime marche pas

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),tabsetPanel(
    id = "onglet",
    tabPanel("Recherche selon caractéristiques",
             sidebarLayout(
               sidebarPanel(
                 textInput("ing1", "Ingrédient 1"),
                 textInput("ing2", "Ingrédient 2"),
                 textInput("ing3", "Ingrédient 3"),
                 h4("Allergie"), 
                 textInput("allergie", "Ingrédients à éviter (séparés par espace, virgule, chiffre...)"),
                 h4("Choix du régime"),  
                 selectInput("diet", "Régime alimentaire :", choices = regimes_disponibles, selected = "None"),
                 actionButton("search", "Rechercher")
               ),
               
               mainPanel(
                 tableOutput("recette_table")
               )
             )),
    tabPanel("Recherche par carte"),
    tabPanel("Recherche"),
    tabPanel("Information",
             "Cette application a été créée dans le cadre du cours Dataviz : RShiny du Master 1 ECAP.\n Pour plus d'information, contactez "
    )
  )
)





#---------- 4. SERVEUR ----------

server <- function(input, output, session){

  #----- RECHERCHE CARACTERISTIQUES -----
  observeEvent(input$search, {
    
    output$recette_table <- renderTable({
      
      ingredients <- c(input$ing1, input$ing2, input$ing3) |> 
        tolower() |> 
        trimws()
      ingredients <- ingredients[ingredients != ""]  
      
      allergenes <- tolower(input$allergie) |> trimws()
      allergenes <- unlist(strsplit(allergenes, "[^a-zA-Z]+"))  
      allergenes <- allergenes[allergenes != ""]  
      
      diet_selected <- input$diet  
      
      if (length(ingredients) == 0 && length(allergenes) == 0 && diet_selected == "None") return(NULL)
      
      recettes_filtrees <- recette
      
      if (length(ingredients) > 0) {
        recettes_filtrees <- recettes_filtrees |>
          filter(sapply(tolower(ingr_name), function(ing) any(sapply(ingredients, grepl, ing, ignore.case = TRUE))))
      }
      
      if (length(allergenes) > 0) {
        recettes_filtrees <- recettes_filtrees |>
          filter(!sapply(tolower(ingr_name), function(ing) any(sapply(allergenes, grepl, ing, ignore.case = TRUE))))
      }
      
      if (diet_selected != "None") {
        recettes_filtrees <- recettes_filtrees |> filter(diet == diet_selected)
      }
      
      recettes_filtrees[, c("name", "description")]
    })
  })
  #----- RECHERCHE PAR CARTE -----
  
  #----- RECHERCHE -----
  
  #----- INFORMATION -----
  
}
  





#---------- 5. LANCER L'APPLICATION ----------

shinyApp(ui = ui, server = server)
