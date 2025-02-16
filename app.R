#---------- 1. PACKAGES ----------
#----- Djayan -----

library(readr)
library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(htmltools)
library(webshot)


#----- Isaline -----

library(stringr)
library(DT)
library(dplyr)
library(shinyWidgets)
library(bslib)





#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/recettes_v2.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(recette)

regimes_disponibles <- c("Aucun", unique(na.omit(recette$diet)))

recette$total_time <- recette$prep_time + recette$cook_time

temps_labels <- c("0 min" = 0, "15 min" = 15, "30 min" = 30, "45 min" = 45,
                  "1h" = 60, "1h15" = 75, "1h30" = 90, "1h45" = 105, "2h ou plus" = 120)





#---------- 3. UI ----------

ui <- fluidPage(
  
  theme = bs_theme(
    bootswatch = "united",
    base_font = font_google("Lato", wght = 400),  # Police plus moderne et lisible
    "nav-tabs-link-active-bg" = "#D29B42",  # Brun clair au lieu de marron foncé
    "nav-tabs-link-active-color" = "white",
    "nav-pills-link-active-bg" = "#E0B97A",  # Brun un peu plus clair
    "nav-pills-link-active-color" = "white"
  ),
  
  useShinyjs(),
  
  tags$style(HTML("
    /* Garde l'image de fond */
    body {
      background-image: url('fond.png') !important;
      background-size: cover !important;
      background-position: center !important;
      background-attachment: fixed !important;
      font-family: 'Lato', sans-serif !important;
      font-size: 14px !important;  /* Taille de texte plus fine */
    }

    /* Style de la barre d'onglets */
    .nav-tabs {
      background-color: rgba(0, 0, 0, 0.1) !important;
      border-radius: 10px !important;
      padding: 5px !important;
    }

    .nav-tabs > li > a {
      color: white !important;
      font-weight: bold !important;
      font-size: 14px !important;
    }

    /* Correction de la couleur des onglets actifs */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:active {
      background-color: #A0522D !important; /* Brun plus clair */
      border-radius: 10px !important;
      color: white !important;
    }

    /* Effet au survol des onglets */
    .nav-tabs > li > a:hover {
      background-color: #B87333 !important; /* Brun légèrement doré */
      color: white !important;
    }

    /* Style des sous-onglets */
    .nav-pills {
      background-color: rgba(0, 0, 0, 0.2) !important;
      border-radius: 10px !important;
      padding: 5px !important;
    }

    .nav-pills > li > a {
      color: white !important;
      font-weight: bold !important;
      font-size: 14px !important;
    }

    /* Correction de la couleur des sous-onglets actifs */
    .nav-pills > li.active > a,
    .nav-pills > li.active > a:focus,
    .nav-pills > li.active > a:active {
      background-color: #D2691E !important; /* Brun plus doux */
      border-radius: 10px !important;
      color: white !important;
    }

    /* Effet au survol des sous-onglets */
    .nav-pills > li > a:hover {
      background-color: #CD853F !important; /* Brun encore plus clair */
      color: white !important;
    }
  ")),
  
  
  
  
  
  
  
  tabsetPanel(
    id = "onglet",
    
    
    # ----- PRESENTATION -----
    tabPanel("Nom du site", HTML(
    "<div style='text-align: center;'>
             Bienvenu sur ... !<br><br>
             Notre site contient actuellement plus de 7000 recettes provenant de plus de ... de pays différents.<br><br>
             En espérant que vous trouverez de quoi vous régaler !<br><br><br><br>mettre logo<br><br><br><br><br>
             <span style='font-size:12px;'>
             Depuis février 2025.<br>
             Cette application a été créée dans le cadre du cours Dataviz : RShiny du Master 1 ECAP.<br>
             En cas de questions, n'hésitez pas à contacter Isaline HERVE ou Djayan DAERON :<br>
             isalineherve@gmail.com<br>
             daeron.djayan@gmail.com
             </span>
             </div>
             ")),
    
    
    # ----- RECHERCHE PAR CARACTERISTIQUES -----
    tabPanel("Recherche selon caractéristiques",
             tabsetPanel(
               id = "carac_tabs", 
               tabPanel("Caractéristiques", 
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
                            sliderTextInput("max_prep_time", "Temps maximal :", choices = names(temps_labels), selected = "2h ou plus"),
                            actionButton("search", "Rechercher")
                          ),
                          mainPanel(
                            uiOutput("recette_table_ui")
                          ))),
               tabPanel("Recette", 
                        uiOutput("recette_details")
               ))),
    
    
    
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
                            leafletOutput("map", height = "400px"),  # Hauteur de la carte
                            DTOutput("table_carte", width = "100%")  # Largeur du tableau à 100%
                          ))),
               tabPanel("Recette", 
                        uiOutput("recette_details_carte")
               ))),
    
    
    
    
    
    
    
    # ----- FOND DE PLACARD -----
    tabPanel("Fond de placard",
             tabsetPanel(
               id = "placard_tabs",
               tabPanel("Ingrédients", 
                        sidebarLayout(
                          sidebarPanel(
                            h4("Sélection d'ingrédients (max 10)"),
                            textInput("ing1", "Ingrédient 1 :"),
                            textInput("ing2", "Ingrédient 2 :"),
                            textInput("ing3", "Ingrédient 3 :"),
                            textInput("ing4", "Ingrédient 4 :"),
                            textInput("ing5", "Ingrédient 5 :"),
                            textInput("ing6", "Ingrédient 6 :"),
                            textInput("ing7", "Ingrédient 7 :"),
                            textInput("ing8", "Ingrédient 8 :"),
                            textInput("ing9", "Ingrédient 9 :"),
                            textInput("ing10", "Ingrédient 10 :"),
                            actionButton("search_by_ingredients", "Rechercher")
                          ),
                          mainPanel(
                            DTOutput("recette_table_ingredients")
                          ))),
               tabPanel("Recette",
                        uiOutput("recette_details_placard")
               ))),
    
    
    
    # ----- BARRE DE RECHERCHE -----
    tabPanel("Recherche",
             tabsetPanel(
               id = "barre_tabs",
               tabPanel("Nom de la recette",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Recherche par nom de recette"),
                            textInput("recette_search", "Nom de la recette :"),
                            actionButton("search_by_name", "Rechercher")
                          ),
                          mainPanel(
                            DTOutput("recette_table_search")
                          ))
               ),
               tabPanel("Recette",
                        uiOutput("recette_details_barre")
               ))),
    
    #------ FAVORIS ------
    tabPanel("Favoris",
             tabsetPanel(
               id = "favoris_tabs",
               tabPanel("Liste des favoris",
                        DTOutput("fav_table")
               ),
               tabPanel("Recette",
                        uiOutput("fav_details")
               ))),
    
  ))





#---------- 4. SERVEUR ----------

server <- function(input, output, session){
  
  favorites <- reactiveVal(recette[0, ])
  
  #----- PRESENTATION -----
  
  
  #----- RECHERCHE CARACTERISTIQUES -----
  
  recettes_filtrees <- reactiveVal(data.frame())  
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
    
    if (diet_selected != "Aucun") {
      recettes_filtrees_data <- recettes_filtrees_data |> filter(diet == diet_selected)
    }
    
    if (!is.null(max_prep) && !is.na(max_prep)) {
      recettes_filtrees_data <- recettes_filtrees_data |> filter(total_time <= max_prep)
    }
    
    recettes_filtrees(recettes_filtrees_data) 
  })
  
  output$recette_table_ui <- renderUI({
    if (input$search == 0) {
      
      return(NULL)
    }
    
    if (nrow(recettes_filtrees()) == 0) {
      div(
        style = "text-align: center; margin-top: 20px;",
        h4("Aucune recette trouvée. Veuillez modifier votre sélection.")
      )
    } else {
      DTOutput("recette_table")
    }
  })
  
  
  output$recette_table <- renderDT({
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
      updateTabsetPanel(session, "carac_tabs", selected = "Recette")
    }
  })
  
  output$recette_details <- renderUI({
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_list <- strsplit(recipe$ingr_qt, "(?<=[^\\d/])(?=\\d)|,\\s*", perl = TRUE)[[1]]
    ingredients_list <- ingredients_list[trimws(ingredients_list) != ""]
    ingredients_html <- paste0("<li>", ingredients_list, "</li>", collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
          actionButton("add_to_fav_carac", " Favoris ", icon = icon("heart"),
                       style = "position: absolute; top: 5px; right: 50px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"),
          actionButton("close_recipe", "✖", 
                       style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
          
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
            )),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      ))
  })
  
  observeEvent(input$close_recipe, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "carac_tabs", selected = "Caractéristiques")
  })
  
  # ---- RECHERCHE PAR CARTE ----
  
  # ---- Définition des régions pour le zoom ----
  
  countries_to_keep <- c(
    "Pakistan", "Népal", "Bangladesh", "Afghanistan", "Sri Lanka", "Birmanie", 
    "Malaisie", "Maurice", "Fidji", "Inde", "France", "États-Unis", "Liban", 
    "Thaïlande", "Italie", "Syrie", "Chine", "Maroc", "Grèce", "Indonésie", 
    "Turquie", "Vietnam", "Irlande", "Canada", "NA", "Égypte", "Royaume-Uni", 
    "Espagne", "Irak", "Allemagne", "Oman", "Cameroun", "Iran", "Mexique", 
    "Suisse", "Autriche", "Japon", "Pérou", "Russie", "Pologne", "Corée du Sud", 
    "Suède", "Hongrie", "Argentine", "Mozambique", "Pays-Bas", "Palestine", 
    "Colombie", "Caraïbes", "Nouvelle-Zélande", "Cuba", "Pays de Galles", "Taïwan", 
    "Chypre", "Jordanie", "Arménie", "Singapour", "Tunisie", "Afrique du Sud", 
    "Israël", "Yémen", "Danemark"
  )
  
  region_coords <- list(
    "Pakistan" = list(lat = 30, lon = 70, zoom = 6),
    "Népal" = list(lat = 28, lon = 84, zoom = 7),
    "Bangladesh" = list(lat = 24, lon = 90, zoom = 6),
    "Afghanistan" = list(lat = 33, lon = 65, zoom = 6),
    "Sri Lanka" = list(lat = 7, lon = 81, zoom = 7),
    "Birmanie" = list(lat = 21, lon = 96, zoom = 6),
    "Malaisie" = list(lat = 3, lon = 101, zoom = 6),
    "Maurice" = list(lat = -20, lon = 57, zoom = 8),
    "Fidji" = list(lat = -17, lon = 178, zoom = 8),
    "Inde" = list(lat = 22, lon = 78, zoom = 5),
    "France" = list(lat = 46, lon = 2, zoom = 5),
    "États-Unis" = list(lat = 37, lon = -95, zoom = 4),
    "Liban" = list(lat = 33, lon = 35, zoom = 7),
    "Thaïlande" = list(lat = 15, lon = 100, zoom = 6),
    "Italie" = list(lat = 42, lon = 12, zoom = 6),
    "Syrie" = list(lat = 34, lon = 38, zoom = 6),
    "Chine" = list(lat = 35, lon = 105, zoom = 5),
    "Maroc" = list(lat = 32, lon = -5, zoom = 6),
    "Grèce" = list(lat = 39, lon = 22, zoom = 6),
    "Indonésie" = list(lat = -5, lon = 120, zoom = 6),
    "Turquie" = list(lat = 38, lon = 35, zoom = 6),
    "Vietnam" = list(lat = 14, lon = 108, zoom = 6),
    "Irlande" = list(lat = 53, lon = -8, zoom = 7),
    "Canada" = list(lat = 56, lon = -106, zoom = 4),
    "Égypte" = list(lat = 26, lon = 30, zoom = 6),
    "Royaume-Uni" = list(lat = 54, lon = -2, zoom = 5),
    "Espagne" = list(lat = 40, lon = -3, zoom = 5),
    "Irak" = list(lat = 33, lon = 44, zoom = 6),
    "Allemagne" = list(lat = 51, lon = 10, zoom = 5),
    "Oman" = list(lat = 21, lon = 57, zoom = 6),
    "Cameroun" = list(lat = 4, lon = 12, zoom = 6),
    "Iran" = list(lat = 32, lon = 53, zoom = 5),
    "Mexique" = list(lat = 23, lon = -102, zoom = 5),
    "Suisse" = list(lat = 46, lon = 8, zoom = 6),
    "Autriche" = list(lat = 47, lon = 13, zoom = 6),
    "Japon" = list(lat = 36, lon = 138, zoom = 6),
    "Pérou" = list(lat = -10, lon = -76, zoom = 6),
    "Russie" = list(lat = 55, lon = 37, zoom = 5),
    "Pologne" = list(lat = 52, lon = 19, zoom = 6),
    "Corée du Sud" = list(lat = 37, lon = 127, zoom = 6),
    "Suède" = list(lat = 60, lon = 18, zoom = 5),
    "Hongrie" = list(lat = 47, lon = 19, zoom = 6),
    "Argentine" = list(lat = -34, lon = -64, zoom = 6),
    "Mozambique" = list(lat = -18, lon = 35, zoom = 6),
    "Pays-Bas" = list(lat = 52, lon = 5, zoom = 7),
    "Palestine" = list(lat = 32, lon = 35, zoom = 6),
    "Colombie" = list(lat = 4, lon = -72, zoom = 6),
    "Caraïbes" = list(lat = 15, lon = -60, zoom = 5),
    "Nouvelle-Zélande" = list(lat = -40, lon = 175, zoom = 6),
    "Cuba" = list(lat = 21, lon = -79, zoom = 6),
    "Pays de Galles" = list(lat = 53, lon = -4, zoom = 6),
    "Taïwan" = list(lat = 23, lon = 121, zoom = 8),
    "Chypre" = list(lat = 35, lon = 33, zoom = 7),
    "Jordanie" = list(lat = 31, lon = 36, zoom = 6),
    "Arménie" = list(lat = 40, lon = 45, zoom = 6),
    "Singapour" = list(lat = 1.3521, lon = 103.8198, zoom = 8),
    "Tunisie" = list(lat = 33, lon = 9, zoom = 7),
    "Afrique du Sud" = list(lat = -30, lon = 25, zoom = 5),
    "Israël" = list(lat = 31.5, lon = 34.75, zoom = 7),
    "Yémen" = list(lat = 15, lon = 48, zoom = 7),
    "Danemark" = list(lat = 56, lon = 10, zoom = 6)
  )
  
  
  
  # ---- Chargement des formes des pays ----
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Créer un mappage manuel des noms français vers anglais
  country_mapping_fr_to_en <- c(
    "Pakistan" = "Pakistan", "Népal" = "Nepal", "Bangladesh" = "Bangladesh", 
    "Afghanistan" = "Afghanistan", "Sri Lanka" = "Sri Lanka", "Birmanie" = "Myanmar", 
    "Malaisie" = "Malaysia", "Maurice" = "Mauritius", "Fidji" = "Fiji", 
    "Inde" = "India", "France" = "France", "États-Unis" = "United States of America", 
    "Liban" = "Lebanon", "Thaïlande" = "Thailand", "Italie" = "Italy", "Syrie" = "Syria", 
    "Chine" = "China", "Maroc" = "Morocco", "Grèce" = "Greece", "Indonésie" = "Indonesia", 
    "Turquie" = "Turkey", "Vietnam" = "Vietnam", "Irlande" = "Ireland", "Canada" = "Canada", 
    "Égypte" = "Egypt", "Royaume-Uni" = "United Kingdom", "Espagne" = "Spain", "Irak" = "Iraq", 
    "Allemagne" = "Germany", "Oman" = "Oman", "Cameroun" = "Cameroon", "Iran" = "Iran", 
    "Mexique" = "Mexico", "Suisse" = "Switzerland", "Autriche" = "Austria", "Japon" = "Japan", 
    "Pérou" = "Peru", "Russie" = "Russia", "Pologne" = "Poland", "Corée du Sud" = "South Korea", 
    "Suède" = "Sweden", "Hongrie" = "Hungary", "Argentine" = "Argentina", "Mozambique" = "Mozambique", 
    "Pays-Bas" = "Netherlands", "Palestine" = "Palestine", "Colombie" = "Colombia", 
    "Caraïbes" = "Caribbean", "Nouvelle-Zélande" = "New Zealand", "Cuba" = "Cuba", 
    "Pays de Galles" = "Wales", "Taïwan" = "Taiwan", "Chypre" = "Cyprus", "Jordanie" = "Jordan", 
    "Arménie" = "Armenia", "Singapour" = "Singapore", "Tunisie" = "Tunisia", 
    "Afrique du Sud" = "South Africa", "Israël" = "Israel", "Yémen" = "Yemen", "Danemark" = "Denmark"
  )
  
  # Filtrer la liste des coordonnées pour ne garder que les pays présents dans la liste
  region_coords_filtered <- region_coords[names(region_coords) %in% countries_to_keep]
  
  # Filtrer la liste des mappings pour ne garder que les pays présents dans la liste
  country_mapping_fr_to_en_filtered <- country_mapping_fr_to_en[names(country_mapping_fr_to_en) %in% countries_to_keep]
  
  # Afficher les résultats
  region_coords_filtered
  country_mapping_fr_to_en_filtered
  
  
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
  
  # Ajouter une colonne avec les noms en français
  world_with_recipes$name_fr <- country_mapping_en_to_fr[world_with_recipes$name]
  
  # Remplace les valeurs NA par les noms anglais si pas de correspondance en français
  world_with_recipes$name_fr[is.na(world_with_recipes$name_fr)] <- world_with_recipes$name[is.na(world_with_recipes$name_fr)]
  
  # Mise à jour de l'affichage de la carte
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles(options = tileOptions(minZoom = 2, maxZoom = 5)) %>%
      addPolygons(
        data = world_with_recipes,
        fillColor = ~colorFactor("viridis", world_with_recipes$region_un)(world_with_recipes$region_un),
        fillOpacity = 0.6,
        weight = 1,
        highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8),
        label = ~name_fr,  # Afficher les noms en français
        layerId = ~name
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
  })
  
  # ---- Mise à jour du zoom sur sélection ----
  observeEvent(input$region_select, {
    region_fr <- input$region_select  # Récupère la région sélectionnée en français
    leafletProxy("map") %>%
      setView(lng = region_coords[[region_fr]]$lon, lat = region_coords[[region_fr]]$lat, zoom = region_coords[[region_fr]]$zoom)
  })
  
  observeEvent(input$reset_map, {
    leafletProxy("map") %>%
      setView(lng = 0, lat = 20, zoom = 2) 
    
    # Réinitialiser la sélection dans le menu déroulant à "Aucun"
    updateSelectInput(session, "region_select", selected = "Aucun")
  })
  
  
  # ---- Mise à jour du menu déroulant quand un pays est cliqué ----
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id  # Récupère le pays cliqué
    if (!is.null(clicked_country) && clicked_country %in% names(country_mapping_en_to_fr)) {
      french_country_name <- country_mapping_en_to_fr[clicked_country]
      updateSelectInput(session, "region_select", selected = french_country_name)
    }
  })
  
  # ---- Filtrage des recettes selon la région ou le pays sélectionné ----
  recettes_par_carte <- reactive({
    selected_region <- input$region_select
    if (selected_region == "Aucun") {
      return(data.frame())  # Retourne un tableau vide si "Aucun" est sélectionné
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
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_list <- strsplit(recipe$ingr_qt, "(?<=[^\\d/])(?=\\d)|,\\s*", perl = TRUE)[[1]]
    ingredients_list <- ingredients_list[trimws(ingredients_list) != ""]
    ingredients_html <- paste0("<li>", ingredients_list, "</li>", collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
          actionButton("add_to_fav_carte", " Favoris ", icon = icon("heart"),
                       style = "position: absolute; top: 5px; right: 50px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"),
          downloadButton("download_recipe", "", 
                         style = "position: absolute; top: 5px; right: 100px; background: none; border: 1px solid #007bff; font-size: 14px; color: #007bff; cursor: pointer;"),
          actionButton("close_recipe_carte", "✖", 
                       style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
          
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
  
  # ---- Générer le fichier de la recette pour le téléchargement ----
  output$download_recipe <- downloadHandler(
    filename = function() {
      paste0(selected_recipe()$name, "_recette.txt")
    },
    content = function(file) {
      recipe <- selected_recipe()
      
      # Créer le contenu de la recette
      content <- paste0(
        "Nom de la recette : ", recipe$name, "\n\n",
        "Régime : ", recipe$diet, "\n",
        "Temps de préparation : ", recipe$prep_time, " min\n",
        "Temps de cuisson : ", recipe$cook_time, " min\n\n",
        "Ingrédients :\n"
      )
      
      # Ajouter les ingrédients
      ingredients_list <- strsplit(recipe$ingr_name, ",")[[1]]
      quantities_list <- strsplit(recipe$ingr_qt, ",")[[1]]
      
      for (i in seq_along(ingredients_list)) {
        if (is.na(quantities_list[i]) || quantities_list[i] == "") {
          content <- paste0(content, "- ", ingredients_list[i], "\n")
        } else {
          content <- paste0(content, "- ", ingredients_list[i], " - ", quantities_list[i], "\n")
        }
      }
      
      content <- paste0(content, "\nInstructions :\n", recipe$instructions)
      
      # Écrire dans le fichier
      writeLines(content, con = file)
    }
  )
  
  # ---- Fermeture de la carte de recette ----
  observeEvent(input$close_recipe_carte, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "carte_tabs", selected = "Carte")
  })
  
  
  
  
  
  
  
  #----- FOND DE PLACARD -----
  recettes_found_ingredients <- reactiveVal(data.frame())
  
  observeEvent(input$search_by_ingredients, {
    ingredients <- c(input$ing1, input$ing2, input$ing3, input$ing4, input$ing5,
                     input$ing6, input$ing7, input$ing8, input$ing9, input$ing10)
    ingredients <- tolower(trimws(ingredients))
    ingredients <- ingredients[ingredients != ""]
    
    req(length(ingredients) > 0)
    
    result <- recette |>
      mutate(ingr_lower = tolower(ingr_name),
             score = sapply(ingr_lower, function(x) {
               sum(sapply(ingredients, function(ing) as.numeric(grepl(ing, x, ignore.case = TRUE))))
             })
      ) |>
      filter(score > 0) |>
      arrange(desc(score))
    
    recettes_found_ingredients(result)
    
    output$recette_table_ingredients <- renderDT({
      datatable(result[, c("name", "description", "prep_time", "score")],
                options = list(pageLength = 5),
                selection = "single")
    })
  })
  
  observeEvent(input$recette_table_ingredients_rows_selected, {
    selected_row <- input$recette_table_ingredients_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(recettes_found_ingredients()[selected_row, ])
      updateTabsetPanel(session, "placard_tabs", selected = "Recette")
    }
  })
  
  output$recette_details_placard <- renderUI({
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_list <- strsplit(recipe$ingr_qt, "(?<=[^\\d/])(?=\\d)|,\\s*", perl = TRUE)[[1]]
    ingredients_list <- ingredients_list[trimws(ingredients_list) != ""]
    ingredients_html <- paste0("<li>", ingredients_list, "</li>", collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
          actionButton("add_to_fav_placard", " Favoris ", icon = icon("heart"),
                       style = "position: absolute; top: 5px; right: 50px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"),
          actionButton("close_recipe_placard", "✖", 
                       style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
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
            )),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      ))
  })

  observeEvent(input$close_recipe_placard, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "placard_tabs", selected = "Ingrédients")
  })
  
  #----- BARRE DE RECHERCHE -----
  recettes_found_name <- reactiveVal(data.frame())
  
  observeEvent(input$search_by_name, {
    req(input$recette_search)
    
    query <- tolower(trimws(input$recette_search))
    mots_recherche <- unlist(strsplit(query, "\\s+"))
    
    result <- recette |>
      mutate(name_lower = tolower(name),
             score = sapply(name_lower, function(x) {
               sum(sapply(mots_recherche, function(mot) as.numeric(grepl(mot, x, ignore.case = TRUE))))
             })
      ) |>
      filter(score > 0) |>
      arrange(desc(score))
    
    recettes_found_name(result)
    
    output$recette_table_search <- renderDT({
      datatable(result[, c("name", "description", "prep_time", "score")],
                options = list(pageLength = 5),
                selection = "single")
    })
  })
  
  observeEvent(input$recette_table_search_rows_selected, {
    selected_row <- input$recette_table_search_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(recettes_found_name()[selected_row, ])
      updateTabsetPanel(session, "barre_tabs", selected = "Recette")
    }
  })
  
  output$recette_details_barre <- renderUI({
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_list <- strsplit(recipe$ingr_qt, "(?<=[^\\d/])(?=\\d)|,\\s*", perl = TRUE)[[1]]
    ingredients_list <- ingredients_list[trimws(ingredients_list) != ""]
    ingredients_html <- paste0("<li>", ingredients_list, "</li>", collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
          actionButton("add_to_fav_barre", " Favoris ", icon = icon("heart"),
                       style = "position: absolute; top: 5px; right: 50px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"),
          actionButton("close_recipe_barre", "✖", 
                       style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
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
            )),
          h4("Ingrédients"),
          HTML(paste0("<ul>", ingredients_html, "</ul>")),
          h4("Instructions"),
          p(recipe$instructions)
      ))
  })
  
  observeEvent(input$close_recipe_barre, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "barre_tabs", selected = "Nom de la recette")
  })
  
  
  #----- FAVORIS -----
  observeEvent(input$add_to_fav_carac, {
    req(selected_recipe())
    new_recipe <- selected_recipe()
    new_recipe <- new_recipe[, !colnames(new_recipe) %in% c("name_lower", "ingr_lower", "score")]
    current_fav <- favorites()
    if (nrow(current_fav) == 0) {
      favorites(new_recipe)
    } else {
      all_columns <- union(colnames(current_fav), colnames(new_recipe))
      new_recipe <- new_recipe[, all_columns, drop = FALSE]
      current_fav <- current_fav[, all_columns, drop = FALSE]
      if (!(new_recipe$name %in% current_fav$name)) {
        favorites(rbind(current_fav, new_recipe))
      }
    }
    shinyjs::runjs("$('#add_to_fav_carac').css('color', 'red');")
    showNotification("Recette ajoutée aux favoris", type = "message")
  })
  
  observeEvent(input$add_to_fav_carte, {
    req(selected_recipe())
    new_recipe <- selected_recipe()
    new_recipe <- new_recipe[, !colnames(new_recipe) %in% c("name_lower", "ingr_lower", "score")]
    current_fav <- favorites()
    
    if (nrow(current_fav) == 0) {
      favorites(new_recipe)
    } else {
      all_columns <- union(colnames(current_fav), colnames(new_recipe))
      new_recipe <- new_recipe[, all_columns, drop = FALSE]
      current_fav <- current_fav[, all_columns, drop = FALSE]
      
      if (!(new_recipe$name %in% current_fav$name)) {
        favorites(rbind(current_fav, new_recipe))
      }}
    shinyjs::runjs("$('#add_to_fav_carte').css('color', 'red');")
    showNotification("Recette ajoutée aux favoris", type = "message")
  })
  
  observeEvent(input$add_to_fav_placard, {
    req(selected_recipe())
    new_recipe <- selected_recipe()
    new_recipe <- new_recipe[, !colnames(new_recipe) %in% c("name_lower", "ingr_lower", "score")]
    current_fav <- favorites()
    if (nrow(current_fav) == 0) {
      favorites(new_recipe)
    } else {
      all_columns <- union(colnames(current_fav), colnames(new_recipe))
      new_recipe <- new_recipe[, all_columns, drop = FALSE]
      current_fav <- current_fav[, all_columns, drop = FALSE]
      if (!(new_recipe$name %in% current_fav$name)) {
        favorites(rbind(current_fav, new_recipe))
      }}
    shinyjs::runjs("$('#add_to_fav_placard').css('color', 'red');")
    showNotification("Recette ajoutée aux favoris", type = "message")
  })
  
  
  observeEvent(input$add_to_fav_barre, {
    req(selected_recipe())
    new_recipe <- selected_recipe()
    new_recipe <- new_recipe[, !colnames(new_recipe) %in% c("name_lower", "ingr_lower", "score")]
    current_fav <- favorites()
    if (nrow(current_fav) == 0) {
      favorites(new_recipe)
    } else {
      all_columns <- union(colnames(current_fav), colnames(new_recipe))
      new_recipe <- new_recipe[, all_columns, drop = FALSE]
      current_fav <- current_fav[, all_columns, drop = FALSE]
      if (!(new_recipe$name %in% current_fav$name)) {
        favorites(rbind(current_fav, new_recipe))
      }}
    shinyjs::runjs("$('#add_to_fav_barre').css('color', 'red');")
    showNotification("Recette ajoutée aux favoris", type = "message")
  })
  
  output$fav_table <- renderDT({
    fav_data <- favorites()
    if (nrow(fav_data) == 0) return(NULL)
    fav_data <- fav_data[, !colnames(fav_data) %in% c("ingr_lower", "score")]
    datatable(fav_data[, c("name", "description", "prep_time")],
              selection = "single",
              options = list(pageLength = 5))
  })
  
  observeEvent(input$fav_table_rows_selected, {
    selected_row <- input$fav_table_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(favorites()[selected_row, ])
      updateTabsetPanel(session, "favoris_tabs", selected = "Recette")
    }
  })
  
  output$fav_details <- renderUI({
    req(selected_recipe())
    recipe <- selected_recipe()
    ingredients_list <- strsplit(recipe$ingr_qt, "(?<=[^\\d/])(?=\\d)|,\\s*", perl = TRUE)[[1]]
    ingredients_list <- ingredients_list[trimws(ingredients_list) != ""]
    ingredients_html <- paste0("<li>", ingredients_list, "</li>", collapse = "")
    
    tagList(
      div(style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
          actionButton("add_to_fav_barre", " Favoris ", icon = icon("heart"),
                       style = "position: absolute; top: 5px; right: 50px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
          
          actionButton("close_recipe_fav", "✖", 
                       style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"),
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
  
  observeEvent(input$close_recipe_fav, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "favoris_tabs", selected = "Liste des favoris")
})
  
}
#---------- 5. LANCER L'APPLICATION ----------
shinyApp(ui = ui, server = server)