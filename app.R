#---------- 1. PACKAGES ----------
#----- Djayan -----

library(readr)
library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinyjs)





#----- Isaline -----

library(stringr)
library(DT)
library(dplyr)
library(shinyWidgets)





#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/recettes.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(recette)

regimes_disponibles <- c("None", unique(na.omit(recette$diet)))

recette$total_time <- recette$prep_time + recette$cook_time

temps_labels <- c("0 min" = 0, "15 min" = 15, "30 min" = 30, "45 min" = 45,
                  "1h" = 60, "1h15" = 75, "1h30" = 90, "1h45" = 105, "2h ou plus" = 120)





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
                 
                 h4("Temps de préparation (cuisson comprise) maximal"),
                 sliderTextInput("max_prep_time", "Temps maximal :", 
                                 choices = names(temps_labels), selected = "2h ou plus"),
                 
                 actionButton("search", "Rechercher")
               ),
               
               mainPanel(
                 uiOutput("recette_details"),
                 DTOutput("recette_table")
               )
             )),
    
    
    # ----- RECHERCHE PAR CARTE -----
    
    tabPanel("Recherche par carte",
             tabsetPanel(
               id = "carte_tabs",
               tabPanel("Carte",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Choix de la région"),
                            selectInput("region_select", "Sélectionnez une région :", 
                                        choices = c("Neutre", unique(na.omit(recette$cuisine))),
                                        selected = "Neutre"),
                            actionButton("reset_map", "Réinitialiser la carte")
                          ),
                          mainPanel(
                            leafletOutput("map", height = "500px"),
                            DTOutput("table_carte")
                          )
                        )
               ),
               tabPanel("Recette", 
                        uiOutput("recette_details_carte")
               )
               )
             ),
    
    
    
    
    
    # ----- BARRE DE RECHERCHE -----
    
    tabPanel("Recherche",
             sidebarLayout(
               sidebarPanel(
                 h4("Recherche par nom de recette"),
                 textInput("recette_search", "Nom de la recette :"),
                 actionButton("search_by_name", "Rechercher")
               ),
               mainPanel(
                 uiOutput("recette_details_search")
               )
             )),
    
    
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
  
  output$formatted_time <- renderText({
    label <- names(temps_labels)[temps_labels == input$max_prep_time]
    if (length(label) > 0) label else "Inconnu"
  })
  
  observeEvent(input$search, {
    ingredients <- c(input$ing1, input$ing2, input$ing3) |> 
      tolower() |> 
      trimws()
    ingredients <- ingredients[ingredients != ""]  
    
    allergenes <- tolower(input$allergie) |> trimws()
    allergenes <- unlist(strsplit(allergenes, "[^a-zA-Z]+"))  
    allergenes <- allergenes[allergenes != ""]  
    
    diet_selected <- input$diet  
    max_prep <- temps_labels[input$max_prep_time]  
    
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
      recettes_filtrees_data <- recettes_filtrees_data |> filter(total_time <= max_prep)
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
            column(4, 
                   p(strong("Régime : "), recipe$diet),
                   p(strong("Temps de préparation : "), recipe$prep_time, " min"),
                   p(strong("Temps de cuisson : "), recipe$cook_time, " min")
            ),
            column(8, 
                   h3(recipe$name),
                   img(src = recipe$image_url, width = "100%", 
                       style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;")  
            )
          ),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      )
    )
  })
  
  
  
  # ---- RECHERCHE PAR CARTE ----
  
  # ---- Définition des régions pour le zoom ----
  region_coords <- list(
    "Inde" = list(lat = 22, lon = 78, zoom = 5),
    "Inde du Nord" = list(lat = 28, lon = 77, zoom = 6),
    "Inde du Sud" = list(lat = 12, lon = 78, zoom = 6),
    "France" = list(lat = 46, lon = 2, zoom = 5),
    "Europe" = list(lat = 50, lon = 10, zoom = 4),
    "Neutre" = list(lat = 20, lon = 0, zoom = 2)  
  )
  
  # ---- Chargement des formes des pays ----
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Créer un mappage manuel des noms français vers anglais
  country_mapping_fr_to_en <- c(
    "Inde du Nord" = "India",
    "Inde" = "India",
    "Inde du Sud" = "India",
    "Europe" = "Europe",
    "Continental" = "Continental Europe",
    "Moyen-Orient" = "Middle East",
    "Népal" = "Nepal",
    "Inde du Nord-Est" = "India",
    "Thaïlande" = "Thailand",
    "Italie" = "Italy",
    "Chine" = "China",
    "Méditerranée" = "Mediterranean",
    "Asie" = "Asia",
    "Indonésie" = "Indonesia",
    "Vietnam" = "Vietnam",
    "États-Unis" = "United States",
    "Grèce" = "Greece",
    "Pakistan" = "Pakistan",
    "France" = "France",
    "Mexique" = "Mexico",
    "Japon" = "Japan",
    "Afrique" = "Africa",
    "Sri Lanka" = "Sri Lanka",
    "Suède" = "Sweden",
    "Afghanistan" = "Afghanistan",
    "Inde du Centre" = "India",
    "Caraïbes" = "Caribbean",
    "Corée" = "Korea",
    "Malaisie" = "Malaysia",
    "Birmanie" = "Myanmar",
    "Royaume-Uni" = "United Kingdom",
    "Bangladesh" = "Bangladesh",
    "Singapour" = "Singapore"
  )
  
  # Appliquer le mappage aux noms des pays dans recette$cuisine
  recette$cuisine_english <- recode(recette$cuisine, !!!country_mapping_fr_to_en)
  
  # Normaliser les noms des pays dans recette$cuisine_english
  normalized_recipes_cuisine <- tolower(trimws(recette$cuisine_english))
  
  # Normaliser les noms des pays dans world
  normalized_world_names <- tolower(trimws(world$name))
  
  # Liste des pays avec des recettes, en normalisant les noms
  countries_with_recipes <- unique(normalized_recipes_cuisine)
  
  # Filtrer les pays qui ont des recettes
  world_with_recipes <- world[normalized_world_names %in% countries_with_recipes, ]
  
  # Créer un mappage inverse pour convertir les noms anglais en français
  country_mapping_en_to_fr <- names(country_mapping_fr_to_en)
  names(country_mapping_en_to_fr) <- country_mapping_fr_to_en
  
  # Créer la carte Leaflet
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles(options = tileOptions(minZoom = 2, maxZoom = 5)) %>%
      addPolygons(
        data = world_with_recipes,
        fillColor = ~colorFactor("viridis", world_with_recipes$region_un)(world_with_recipes$region_un),
        fillOpacity = 0.6,
        weight = 1,
        highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8),
        label = ~name,
        layerId = ~name
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(
        lng1 = -180, lat1 = -85,  # Coin supérieur gauche
        lng2 = 180, lat2 = 85     # Coin inférieur droit
      )
  })
  
  # ---- Mise à jour du zoom sur sélection ----
  observeEvent(input$region_select, {
    region <- input$region_select
    if (!is.null(region_coords[[region]])) {
      leafletProxy("map") %>%
        setView(lng = region_coords[[region]]$lon, lat = region_coords[[region]]$lat, zoom = region_coords[[region]]$zoom)
    }
  })
  
  observeEvent(input$reset_map, {
    leafletProxy("map") %>%
      setView(lng = 0, lat = 20, zoom = 2) 
  })
  
  # ---- Mise à jour du menu déroulant quand un pays est cliqué ----
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id  # Récupère le pays cliqué
    
    # Convertir le nom en anglais (clicked_country) en français en utilisant le mappage inverse
    if (!is.null(clicked_country) && clicked_country %in% names(country_mapping_en_to_fr)) {
      french_country_name <- country_mapping_en_to_fr[clicked_country]
      updateSelectInput(session, "region_select", selected = french_country_name)
    }
  })
  
  # ---- Filtrage des recettes selon la région ou le pays sélectionné ----
  recettes_par_carte <- reactive({
    selected_region <- input$region_select
    
    if (selected_region == "Neutre") {
      return(data.frame())  # Retourne un tableau vide si "Neutre" est sélectionné
    } else {
      return(recette %>% filter(cuisine == selected_region))
    }
  })
  
  # ---- Mise à jour du tableau en fonction du pays sélectionné ----
  output$table_carte <- renderDT({
    data <- recettes_par_carte()
    if (nrow(data) == 0) return(NULL)
    
    data$description <- substr(data$description, 1, 100)  # Affiche uniquement les 100 premiers caractères
    
    datatable(data[, c("name", "description", "prep_time")],
              selection = "single",
              options = list(pageLength = 5))
  })
  
  # ---- Sélection d'une recette et redirection vers l'onglet "Recette" ----
  observeEvent(input$table_carte_rows_selected, {
    selected_row <- input$table_carte_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(recettes_par_carte()[selected_row, ])
      updateTabsetPanel(session, "carte_tabs", selected = "Recette")
    }
  })
  
  # ---- Affichage des détails de la recette sélectionnée ----
  output$recette_details_carte <- renderUI({
    req(selected_recipe())  # Assurez-vous qu'une recette soit sélectionnée
    recipe <- selected_recipe()
    
    ingredients_list <- strsplit(recipe$ingr_name, ",")[[1]]
    quantities_list <- strsplit(recipe$ingr_qt, ",")[[1]]
    
    ingredients_html <- lapply(1:length(ingredients_list), function(i) {
      paste0("<li>", ingredients_list[i], " - ", quantities_list[i], "</li>")
    }) |> paste(collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9;",
          fluidRow(
            column(4, 
                   p(strong("Régime : "), recipe$diet),
                   p(strong("Temps de préparation : "), recipe$prep_time, " min"),
                   p(strong("Temps de cuisson : "), recipe$cook_time, " min")
            ),
            column(8, 
                   h3(recipe$name),
                   img(src = recipe$image_url, width = "100%", 
                       style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;")  
            )
          ),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      )
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #----- BARRE DE RECHERCHE -----
  
  observeEvent(input$search_by_name, {
    recipe_name_search <- input$recette_search
    
    if (recipe_name_search != "") {
      recette_found <- recette |>
        filter(str_to_lower(name) == str_to_lower(recipe_name_search))
      
      if (nrow(recette_found) > 0) {
        selected_recipe(recette_found)
      } else {
        selected_recipe(NULL)
        showModal(modalDialog(
          title = "Recette non trouvée",
          "Aucune recette ne correspond à ce nom. Veuillez vérifier l'orthographe.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  output$recette_details_search <- renderUI({
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
            column(4, 
                   p(strong("Régime : "), recipe$diet),
                   p(strong("Temps de préparation : "), recipe$prep_time, " min"),
                   p(strong("Temps de cuisson : "), recipe$cook_time, " min")
            ),
            column(8, 
                   h3(recipe$name),
                   img(src = recipe$image_url, width = "100%", 
                       style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;")  
            )
          ),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      )
    )
  })
  
  #----- INFORMATION -----
  
}





#---------- 5. LANCER L'APPLICATION ----------

shinyApp(ui = ui, server = server)