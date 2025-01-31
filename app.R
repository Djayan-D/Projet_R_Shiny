#---------- 1. PACKAGES ----------
#--- Djayan

library(readr)
library(shiny)


#--- Isaline
library(stringr)
library(DT)
library(dplyr)



#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/Food_Recipe.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(recette)

recette$cuisine <- as.factor(recette$cuisine)
recette$course <- as.factor(recette$course)

regimes_disponibles <- c("None", unique(na.omit(recette$diet)))

#---------- 3. UI ----------

# inverser régime et allergènes + régime marche pas

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),tabsetPanel(
    id = "onglet",
# ----- RECHERCHE PAR CARACTERISTIQUES -----
tabPanel("Recherche selon caractéristiques",
         sidebarLayout(
           sidebarPanel(
             h4("Choix du régime"),  
             selectInput("diet", "Régime alimentaire :", choices = regimes_disponibles, selected = "None"),
             
             h4("Ingrédients souhaités"),
             textInput("ing1", "Ingrédient 1"),
             textInput("ing2", "Ingrédient 2"),
             textInput("ing3", "Ingrédient 3"),
             
             h4("Allergènes"), 
             textInput("allergie", "Ingrédients à éviter"),
             
             h4("Temps de préparation maximal"),
             sliderInput("max_prep_time", "Temps (minutes) :", min = 0, max = max(recette$prep_time, na.rm = TRUE), value = max(recette$prep_time, na.rm = TRUE)),
             
             actionButton("search", "Rechercher")
           ),
           
           mainPanel(
             uiOutput("recette_details"),
             DTOutput("recette_table")
           )
         )),

# ----- RECHERCHE PAR CARTE -----
    tabPanel("Recherche par carte"),


# ----- BARRE DE RECHERCHE -----
    tabPanel("Recherche"),


# ----- INFORMATION -----
    tabPanel("Information",
             "Cette application a été créée dans le cadre du cours Dataviz : RShiny du Master 1 ECAP.\n Pour plus d'information, contactez "
    )
  )
)





#---------- 4. SERVEUR ----------

server <- function(input, output, session){

  #----- RECHERCHE CARACTERISTIQUES -----
  recettes_filtrees <- reactiveVal(data.frame())  # Stocke les recettes filtrées
  selected_recipe <- reactiveVal(NULL)
  
  observeEvent(input$search, {
    ingredients <- c(input$ing1, input$ing2, input$ing3) |> 
      tolower() |> 
      trimws()
    ingredients <- ingredients[ingredients != ""]  
    
    allergenes <- tolower(input$allergie) |> trimws()
    allergenes <- unlist(strsplit(allergenes, "[^a-zA-Z]+"))  
    allergenes <- allergenes[allergenes != ""]  
    
    diet_selected <- input$diet  
    max_prep <- input$max_prep_time  
    
    recettes_filtrees_data <- recette
    
    if (length(ingredients) > 0) {
      recettes_filtrees_data <- recettes_filtrees_data |>
        filter(sapply(tolower(ingr_name), function(ing) any(sapply(ingredients, grepl, ing, ignore.case = TRUE))))
    }
    
    if (length(allergenes) > 0) {
      recettes_filtrees_data <- recettes_filtrees_data |>
        filter(!sapply(tolower(ingr_name), function(ing) any(sapply(allergenes, grepl, ing, ignore.case = TRUE))))
    }
    
    if (diet_selected != "None") {
      recettes_filtrees_data <- recettes_filtrees_data |> filter(diet == diet_selected)
    }
    
    if (!is.null(max_prep) && !is.na(max_prep)) {
      recettes_filtrees_data <- recettes_filtrees_data |> filter(prep_time <= max_prep)
    }
    
    recettes_filtrees(recettes_filtrees_data) 
  })
  
  output$recette_table <- renderDT({
    if (nrow(recettes_filtrees()) == 0) return(NULL)
    
    datatable(
      recettes_filtrees()[, c("name", "description", "prep_time")],
      selection = "single",
      options = list(pageLength = 5)
    )
  })
  
  observeEvent(input$recette_table_rows_selected, {
    selected_row <- input$recette_table_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(recettes_filtrees()[selected_row, ])
    }
  })
  
  output$recette_details <- renderUI({
    req(selected_recipe())  
    
    recipe <- selected_recipe()
    
    ingredients_list <- strsplit(recipe$ingr_name, ",")[[1]]
    quantities_list <- strsplit(recipe$ingr_qt, ",")[[1]]
    
    ingredients_html <- lapply(1:length(ingredients_list), function(i) {
      paste0("<li>", ingredients_list[i], " - ", quantities_list[i], "</li>")
    }) |> paste(collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9;",
          fluidRow(
            column(3, img(src = recipe$image_url, width = "100%")), 
            column(9, 
                   h3(recipe$name),
                   p(strong("Régime : "), recipe$diet),
                   p(strong("Temps de préparation : "), recipe$prep_time, " min"),
                   p(strong("Temps de cuisson : "), recipe$cook_time, " min")
            )
          ),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      )
    )
  })
  
  #----- RECHERCHE PAR CARTE -----
  
  #----- BARRE DE RECHERCHE -----
  
  #----- INFORMATION -----
  
}
  





#---------- 5. LANCER L'APPLICATION ----------

shinyApp(ui = ui, server = server)
