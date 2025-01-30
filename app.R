#---------- 1. PACKAGES ----------
#--- Djayan

library(readr)
library(shiny)


#--- Isaline






#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/Food_Recipe.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(data)


#---------- 3. UI ----------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),tabsetPanel(
    id = "onglet",
    tabPanel("Recherche selon caractéristiques",
             sidebarLayout(
               sidebarPanel(
                 textInput("ing1", "Ingrédient 1"),
                 textInput("ing2", "Ingrédient 2"),
                 textInput("ing3", "Ingrédient 3"),
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
  observeEvent(input$search, {
    output$recette_table <- renderTable({
      ingredients <- c(input$ing1, input$ing2, input$ing3) |> 
        tolower() |> 
        trimws()
      
      ingredients <- ingredients[ingredients != ""]
      
      if (length(ingredients) == 0) return(NULL)
      
      recettes_filtrees <- recette |>
        filter(sapply(tolower(ingr_name), function(ing) any(sapply(ingredients, grepl, ing, ignore.case = TRUE))))
      
      recettes_filtrees[, c("name", "description")]
    })
  })
}
  





#---------- 5. LANCER L'APPLICATION ----------

shinyApp(ui = ui, server = server)
