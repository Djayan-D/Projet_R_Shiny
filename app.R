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
library(styler)
library(rmarkdown)
library(tinytex)
library(knitr)
library(shinyWidgets)




#----- Isaline -----

library(stringr)
library(DT)
library(dplyr)
library(shinyWidgets)
library(bslib)
library(stringi)





#---------- 2. BASE DE DONNÉES ----------

recette <- read_csv("data/recettes_v3.csv")

colnames(recette)[c(6:9)] <- c("ingr_name", "ingr_qt", "prep_time", "cook_time")

str(recette)

regimes_disponibles <- unique(na.omit(recette$diet))

recette$total_time <- recette$prep_time + recette$cook_time

temps_labels <- c(
  "0 min" = 0, "15 min" = 15, "30 min" = 30, "45 min" = 45,
  "1h" = 60, "1h15" = 75, "1h30" = 90, "1h45" = 105, "2h ou plus" = 120
)

recette$ingr_qt[32] <- "1/2 tasse de riz (riz rouge à grains courts) 1/2 tasse d'Arhar dal (Split Toor Dal) 1/2 tasse de Chana dal (Bengal Gram Dal) 2 piments verts hachés 1 pouce de gingembre 1 piment rouge sec, Sel au goût 1/4 cuillère à café d'asafoetida (hing) 1 oignon finement haché Huile pour arroser"


#---------- 3. UI ----------

loginModal <- function() {
  modalDialog(
    title = div(style = "text-align: center; font-size: 22px; font-weight: bold; color: #D29B42;", 
                icon("user"), " Connexion / Inscription"),
    
    div(style = "padding: 10px; text-align: center;",
        div(id = "error_message", style = "color: red; font-weight: bold; margin-bottom: 10px;"), # Zone d'affichage des erreurs
        textInput("user_id", NULL, placeholder = "Nom d'utilisateur"),
        passwordInput("password", NULL, placeholder = "Mot de passe"),
        
        # ✅ Ajout du message d'avertissement
        div(style = "font-size: 12px; color: red; margin-top: 5px;",
            "⚠️ Ne mettez pas de mot de passe sensible, ils ne sont pas stockés de manière sécurisée.")
    ),
    
    # Boutons en pied de page
    footer = tagList(
      actionButton("btn_login", "Se connecter", class = "btn-toggle btn-selected"),
      actionButton("btn_register", "S'inscrire", class = "btn-toggle"),
      actionButton("cancel_login", "Annuler", class = "btn-cancel")
    )
  )
}













ui <- fluidPage(
  
  useShinyjs(),
  div(style = "position: absolute; top: 1.5px; right: 20px;",  
      actionButton("open_login", "Se connecter", icon = icon("user"), class = "btn-login")
  ),
  
  
  theme = bs_theme(
    bootswatch = "united",
    base_font = font_google("Lato", wght = 400), # Police moderne et plus sobre
    "nav-tabs-link-active-bg" = "#D29B42", # Brun clair pour les onglets actifs
    "nav-tabs-link-active-color" = "white",
    "nav-pills-link-active-bg" = "#E0B97A", # Brun plus doux pour les sous-onglets
    "nav-pills-link-active-color" = "white"
  ),
  useShinyjs(),
  tags$style(HTML("
    /* ====== Personnalisation du Slider ====== */
  /* Sélectionne les éléments internes du slider générés */
  .irs-bar {
    background-color: #E0B97A !important; /* Couleur de la barre */
  }
  
  .irs-bar-edge {
    background-color: #D29B42 !important; /* Couleur des bords de la barre */
  }
  
  .irs-single {
    background-color: #D29B42 !important; /* Couleur du bouton (single handle) */
  }
  
  .irs-from, .irs-to {
    color: #D29B42 !important; /* Couleur des valeurs affichées */
  }
  
  .irs-line {
    border-top: 3px solid #D29B42 !important; /* Définir l'épaisseur et la couleur de la ligne de séparation */
  }

  /* Définir la couleur de fond de la partie où le curseur se déplace */
  .irs-slider {
    background-color: #D29B42 !important;
  }
    
    /* ====== Image de fond ====== */
body {
  background-image: url('fond2.png') !important;
  background-size: cover !important;
  background-position: center !important;
  background-attachment: fixed !important;
  font-family: 'Lato', sans-serif !important;
  font-size: 15px !important;
  color: #333 !important;
}

/* ====== Barre d'onglets principale ====== */
.nav-tabs {
  background-color: rgba(255, 255, 255, 0.8) !important;
  border-radius: 10px !important;
  padding: 5px !important;
}

.nav-tabs > li > a {
  color: #5A3E1B !important;
  font-weight: bold !important;
  font-size: 14px !important;
  transition: all 0.3s ease-in-out;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:active {
  background-color: #D29B42 !important;
  border-radius: 10px !important;
  color: white !important;
}

.nav-tabs > li > a:hover {
  background-color: #B87333 !important;
  color: white !important;
}

/* ====== Style des sous-onglets ====== */
.nav-pills {
  background-color: rgba(255, 255, 255, 0.8) !important;
  border-radius: 10px !important;
  padding: 5px !important;
}

.nav-pills > li > a {
  color: #5A3E1B !important;
  font-weight: bold !important;
  font-size: 14px !important;
}

.nav-pills > li.active > a,
.nav-pills > li.active > a:focus,
.nav-pills > li.active > a:active {
  background-color: #D2691E !important;
  border-radius: 10px !important;
  color: white !important;
}

.nav-pills > li > a:hover {
  background-color: #CD853F !important;
  color: white !important;
}

/* ====== Amélioration de la sidebar ====== */
.sidebar {
  background-color: rgba(75, 40, 20, 0.95) !important; /* Brun très foncé avec opacité quasi totale */
  padding: 15px !important;
  border-radius: 12px !important;
  box-shadow: 6px 6px 15px rgba(0, 0, 0, 0.6) !important; /* Ombre plus prononcée */
  border: 3px solid #D29B42 !important; /* Bordure dorée plus épaisse */
}

/* Style des titres et labels */
.sidebar h3, .sidebar label {
  color: #FFD700 !important; /* Texte doré pour bien ressortir */
  font-weight: bold !important;
}

/* Style des liens */
.sidebar .nav > li > a {
  color: white !important;
  font-weight: bold !important;
  font-size: 16px !important;
  padding: 12px !important;
  transition: all 0.3s ease-in-out;
}

/* Couleur active du lien */
.sidebar .nav > li.active > a {
  background-color: #D29B42 !important;
  color: white !important;
  border-radius: 8px !important;
}

/* Effet au survol */
.sidebar .nav > li > a:hover {
  background-color: #B87333 !important;
  color: white !important;
  border-radius: 8px !important;
}

/* ====== Tableaux ====== */
table.dataTable {
  background-color: rgba(255, 255, 255, 0.9) !important;
  border-radius: 10px !important;
  box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1) !important;
  overflow: hidden;
}

table.dataTable th {
  background-color: #D29B42 !important;
  color: white !important;
  font-weight: bold !important;
}

table.dataTable td {
  color: #333 !important;
  padding: 10px !important;
}

/* ====== Champs de saisie ====== */
label {
  color: black !important;
  font-weight: bold;
}

input, select, textarea {
  background-color: white !important;
  border-radius: 8px !important;
  border: 1px solid #DEE2E6 !important;
  padding: 8px !important;
  font-size: 14px !important;
  transition: border-color 0.3s ease-in-out;
}

input:focus, select:focus, textarea:focus {
  border-color: #D29B42 !important;
  outline: none !important;
  box-shadow: 0 0 5px rgba(210, 155, 66, 0.5) !important;
}

/* ====== Boutons ====== */
button {
  background-color: #D29B42 !important;
  color: white !important;
  font-weight: bold !important;
  padding: 10px 15px !important;
  border-radius: 8px !important;
  border: none !important;
  transition: background-color 0.3s ease-in-out;
}

button:hover {
  background-color: #B87333 !important;
  cursor: pointer !important;
}

/* ====== Amélioration de la section de filtrage ====== */
.filtrage-container {
  background-color: #8B5A2B !important; /* Brun foncé opaque */
  padding: 20px !important;
  border-radius: 12px !important;
  box-shadow: 8px 8px 20px rgba(0, 0, 0, 0.7) !important; /* Ombre plus marquée */
  border: 4px solid #D29B42 !important; /* Bordure dorée */
  width: 350px !important;
  color: white !important;
}

.filtrage-container h3, 
.filtrage-container label {
  color: #FFD700 !important; /* Texte doré */
  font-weight: bold !important;
  font-size: 1.2em !important;
}

.filtrage-container input, 
.filtrage-container select {
  background-color: white !important;
  color: #333 !important;
  border: 2px solid #D29B42 !important;
  padding: 10px !important;
  border-radius: 6px !important;
  width: 100% !important;
  font-size: 1em !important;
}

.filtrage-container button {
  background-color: #D29B42 !important;
  color: white !important;
  font-weight: bold !important;
  padding: 12px !important;
  border-radius: 8px !important;
  width: 100% !important;
  font-size: 1.1em !important;
  transition: all 0.3s ease-in-out !important;
}

.filtrage-container button:hover {
  background-color: #B87333 !important;
  transform: scale(1.05) !important;
}

/* ====== Optimisation pour mobile ====== */
@media screen and (max-width: 768px) {
  .main-container {
    flex-direction: column !important;
    align-items: center !important;
  }

  .filtrage-container {
    width: 100% !important;
    padding: 15px !important;
  }
}

/* ====== Style de la section 'À propos' ====== */
  .about-container {
    max-width: 800px;
    margin: auto;
    padding: 20px;
    background: rgba(255, 255, 255, 0.9);
    border-radius: 12px;
    text-align: center;
    box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2);
  }

  .about-container h2 {
    color: #D29B42;
    font-size: 22px;
    font-weight: bold;
  }
  
  

  .about-container p {
    font-size: 14px;
    color: #5A3E1B;
    line-height: 1.6;
  }

  .about-container .contact-info {
    font-size: 12px;
    margin-top: 20px;
    color: #444;
  }

  .about-container img {
    max-width: 150px;
    margin: 15px 0;
  }
  
.btn-toggle {
  background-color: #f2f2f2;
    color: black;
  font-weight: bold;
  padding: 10px;
  border-radius: 5px;
  border: 1px solid #ccc;
  cursor: pointer;
  transition: all 0.3s;
  margin: 5px;
}

.btn-selected {
  background-color: #D29B42 !important;  /* Orange */
    color: white !important;
  border: 1px solid #B87333 !important; /* Bordure marron */
}

.btn-cancel {
  background-color: #ccc;
    color: black;
  font-weight: bold;
  padding: 10px;
  border-radius: 5px;
  border: none;
}


")),
  tabsetPanel(
    id = "onglet",


    tabPanel("Accueil", 
             fluidPage(
               HTML("
    <div style='position: relative; text-align: center; padding-top: 50px;'>
      <div style='background: rgba(255, 255, 255, 0.85); padding: 40px 50px; border-radius: 15px; display: inline-block; max-width: 800px;'>
          <img src='logo.png' alt='Logo The Cooking Lab' style='width: 150px; display: block; margin: 0 auto 20px; opacity: 0;
              animation: fadeInZoom 2.5s ease-in-out forwards;'>
          
          <h1 style='color: #D29B42; font-weight: bold; font-size: 36px; 
              animation: fadeIn 2s ease-in-out;'>Bienvenue sur <span style='color: #8B5A2B;'>The Cooking Lab</span> !</h1>
          <h3 style='color: #5A3E1B; font-style: italic; margin-top: 10px;
              animation: fadeIn 3s ease-in-out;'>Recettes inspirées, cuisine optimisée</h3>
          
          <p style='font-size: 18px; color: #333; max-width: 700px; margin: auto;
             animation: fadeIn 3s ease-in-out;'>
            Notre site contient actuellement environ <strong>7000 recettes</strong> provenant de 
            <strong>61 pays</strong> différents. <br><br>
            En espérant que vous trouverez de quoi vous régaler !
          </p>
      </div>
    </div>
    "),
               
               # Bouton Démarrer en dehors du HTML pour qu'il soit interactif avec Shiny
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton("btn_explore", "Démarrer", class = "btn-explore")
               ),
               
               # Styles CSS
               tags$style(HTML("
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(-10px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes fadeInZoom {
        from { opacity: 0; transform: scale(0.8); }
        to { opacity: 1; transform: scale(1); }
      }

      .btn-explore {
        display: inline-block;
        background-color: #D29B42;
        color: white;
        font-size: 18px;
        font-weight: bold;
        padding: 12px 25px;
        border-radius: 8px;
        text-decoration: none;
        transition: all 0.3s ease-in-out;
        border: none;
      }

      .btn-explore:hover {
        background-color: #B87333;
        transform: scale(1.05);
        cursor: pointer;
      }
    "))
             )
    ),
    
    
    
    
    
    


    # ----- RECHERCHE PAR CARACTERISTIQUES -----
    tabPanel(
      "Recherche par caractéristiques",
      tabsetPanel(
        id = "carac_tabs",
        tabPanel(
          "Caractéristiques",
          sidebarLayout(
            sidebarPanel(
              h4("Choix du régime"),
              selectizeInput("diet", "Régime alimentaire :", 
                             choices = c("Tout sélectionner", regimes_disponibles), 
                             selected = "Tout sélectionner", 
                             multiple = TRUE,
                             options = list(plugins = list("remove_button"))),
              h4("Choix du repas"),
              selectizeInput("meal_type", "Type de repas :", 
                             choices = c("Tout sélectionner", sort(unique(na.omit(recette$course)))), 
                             selected = "Tout sélectionner", 
                             multiple = TRUE,
                             options = list(plugins = list("remove_button"))),
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
            )
          )
        ),
        tabPanel(
          "Recette",
          uiOutput("recette_details")
        )
      )
    ),



    # ----- RECHERCHE PAR CARTE -----
    tabPanel(
      "Recherche par pays",
      tabsetPanel(
        id = "carte_tabs",
        tabPanel(
          "Carte",
          sidebarLayout(
            sidebarPanel(
              h4("Choix du pays"),
              selectInput("region_select", "Sélectionnez un pays :",
                choices = c("Aucun", sort(unique(na.omit(recette$cuisine)))),
                selected = "Aucun"
              ),
              actionButton("reset_map", "Réinitialiser la carte")
            ),
            mainPanel(
              leafletOutput("map", height = "400px"), # Hauteur de la carte
              DTOutput("table_carte", width = "100%") # Largeur du tableau à 100%
            )
          )
        ),
        tabPanel(
          "Recette",
          uiOutput("recette_details_carte")
        )
      )
    ),







    # ----- FOND DE PLACARD -----
    tabPanel(
      "Fond de placard",
      tabsetPanel(
        id = "placard_tabs",
        tabPanel(
          "Ingrédients",
          sidebarLayout(
            sidebarPanel(
              h4("Sélection d'ingrédients (max 10)"),
              textInput("ing21", "Ingrédient 1 :"),
              textInput("ing22", "Ingrédient 2 :"),
              textInput("ing23", "Ingrédient 3 :"),
              textInput("ing24", "Ingrédient 4 :"),
              textInput("ing25", "Ingrédient 5 :"),
              textInput("ing26", "Ingrédient 6 :"),
              textInput("ing27", "Ingrédient 7 :"),
              textInput("ing28", "Ingrédient 8 :"),
              textInput("ing29", "Ingrédient 9 :"),
              textInput("ing210", "Ingrédient 10 :"),
              actionButton("search_by_ingredients", "Rechercher")
            ),
            mainPanel(
              DTOutput("recette_table_ingredients")
            )
          )
        ),
        tabPanel(
          "Recette",
          uiOutput("recette_details_placard")
        )
      )
    ),



    # ----- BARRE DE RECHERCHE -----
    tabPanel(
      "Recherche",
      tabsetPanel(
        id = "barre_tabs",
        tabPanel(
          "Nom de la recette",
          sidebarLayout(
            sidebarPanel(
              h4("Recherche par nom de recette"),
              textInput("recette_search", "Nom de la recette :"),
              actionButton("search_by_name", "Rechercher")
            ),
            mainPanel(
              DTOutput("recette_table_search")
            )
          )
        ),
        tabPanel(
          "Recette",
          uiOutput("recette_details_barre")
        )
      )
    ),

    #------ FAVORIS ------
    tabPanel(
      "Favoris",
      tabsetPanel(
        id = "favoris_tabs",
        tabPanel(
          "Liste des favoris",
          DTOutput("fav_table")
        ),
        tabPanel(
          "Recette",
          uiOutput("fav_details")
        )
      )
    ),
    
    
    #------ A PROPOS ------
    
    
    tabPanel("À propos",
             fluidPage(
               tags$head(
                 # JavaScript pour l'interaction des étoiles
                 tags$script(HTML("
        function setRating(rating) {
          var stars = document.querySelectorAll('.star');
          stars.forEach(function(star, index) {
            if (index < rating) {
              star.classList.add('selected');
            } else {
              star.classList.remove('selected');
            }
          });
          Shiny.setInputValue('rating', rating, {priority: 'event'});
        }
        
        function hoverRating(rating) {
          var stars = document.querySelectorAll('.star');
          stars.forEach(function(star, index) {
            if (index < rating) {
              star.classList.add('hovered');
            } else {
              star.classList.remove('hovered');
            }
          });
        }
        
        function resetHover() {
          var stars = document.querySelectorAll('.star');
          stars.forEach(function(star) {
            star.classList.remove('hovered');
          });
        }
        
        function resetStars() {
          var stars = document.querySelectorAll('.star');
          stars.forEach(function(star) {
            star.classList.remove('selected', 'hovered');
          });
          Shiny.setInputValue('rating', 0, {priority: 'event'});
        }
        
        document.addEventListener('DOMContentLoaded', function() {
          document.getElementById('submit_review').addEventListener('click', function() {
            setTimeout(function() {
              resetStars();
            }, 100);
          });
        });
      ")),
                 
                 # Styles CSS avec marges réduites et transparence uniforme
                 tags$style(HTML("
        .rating-stars {
          font-size: 50px;
          cursor: pointer;
          display: flex;
          gap: 5px;
        }
        .star {
          color: gray;
          transition: color 0.3s;
        }
        .star:hover, .star.hovered {
          color: gold;
        }
        .star.selected {
          color: gold;
        }
        
        /* Agrandissement des étoiles dans la zone d'avis */
        #rating_stars {
          font-size: 35px;
        }
        
        /* Uniformisation des styles pour tous les conteneurs */
        .left-container, .right-container, .posted-comments-container {
          background-color: rgba(255, 255, 255, 0.9);
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          padding: 15px;
          font-family: 'Arial', sans-serif;
          margin: 15px 30px 0 30px;  /* marge supérieure réduite */
          min-height: 400px;
        }
        
        .left-container {
          font-size: 16px;
          text-align: center;
        }
        
        .left-container img {
          display: block;
          margin: 0 auto 20px auto;
          max-width: 200px;
          height: auto;
        }
        
        /* Note moyenne à côté du titre */
        .average-rating {
          font-size: 20px;
          font-weight: bold;
          color: gold;
          margin-left: 15px;
        }
        
        /* Styles des commentaires */
        .comment {
          background: #f9f9f9;
          border: none;
          border-radius: 5px;
          padding: 10px;
          margin-bottom: 10px;
        }
        .comment .rating-stars {
          font-size: 40px;
          color: gold;
          margin-bottom: 5px;
        }
        .comment p {
          margin: 5px 0;
        }
      "))
               ),
               
               # Ligne supérieure : Section "À propos" (gauche) et saisie du commentaire (droite)
               fluidRow(style = "margin-top: 30px;",
                        column(8,
                               div(class = "left-container",
                                   # Ajout du titre avec note moyenne
                                   h2(style = "display:inline-block;", "À propos de The Cooking Lab"),
                                   span(uiOutput("average_rating"), class = "average-rating"),
                                   HTML("
            <p>
              The Cooking Lab, c'est une collection de <strong> 7000 recettes</strong> issues de <strong>61 pays différents</strong>.
            </p>
            <img src='logo.png' alt='Logo The Cooking Lab'>
            <p class='contact-info'>
              <strong>Depuis février 2025</strong><br>
              Cette application a été créée dans le cadre du cours <em>Dataviz : RShiny</em> du Master 1 ECAP.<br><br>
              📧 Pour toute question, contactez-nous :<br>
              <strong>Isaline HERVE</strong> - <a href='mailto:isalineherve@gmail.com'>isalineherve@gmail.com</a><br>
              <strong>Djayan DAERON</strong> - <a href='mailto:daeron.djayan@gmail.com'>daeron.djayan@gmail.com</a>
            </p>
          ")
                               )
                        ),
                        column(4,
                               div(class = "right-container",
                                   h3("Laissez un avis sur The Cooking Lab"),
                                   div(id = "rating_stars",
                                       span("★", class = "star", onclick = "setRating(1)", onmouseover = "hoverRating(1)", onmouseout = "resetHover()"),
                                       span("★", class = "star", onclick = "setRating(2)", onmouseover = "hoverRating(2)", onmouseout = "resetHover()"),
                                       span("★", class = "star", onclick = "setRating(3)", onmouseover = "hoverRating(3)", onmouseout = "resetHover()"),
                                       span("★", class = "star", onclick = "setRating(4)", onmouseover = "hoverRating(4)", onmouseout = "resetHover()"),
                                       span("★", class = "star", onclick = "setRating(5)", onmouseover = "hoverRating(5)", onmouseout = "resetHover()")
                                   ),
                                   textAreaInput("comment", "Votre commentaire :", "", rows = 3, width = "100%"),
                                   actionButton("submit_review", "Soumettre", icon = icon("paper-plane"), class = "btn-primary")
                               )
                        )
               ),
               
               # Ligne inférieure : Affichage des commentaires postés
               fluidRow(
                 column(12,
                        div(class = "posted-comments-container",
                            h3("Commentaires soumis"),
                            uiOutput("comments_ui")
                        )
                 )
               )
             )
    )))
    
  
  
  
  
  
  
  
  
  
  
  
  


    
    
    
    
    
    
    
    
    
    
    





#---------- 4. SERVEUR ----------

server <- function(input, output, session) {
  
  comments_data <- reactiveVal({
    if (file.exists("data/comments.csv")) {
      read.csv("data/comments.csv", stringsAsFactors = FALSE)
    } else {
      data.frame(user = character(), text = character(), rating = numeric(), stringsAsFactors = FALSE)
    }
  })
  
  
  if (file.exists("data/comments.csv")) {
    comments_data(read.csv("data/comments.csv", stringsAsFactors = FALSE))
  }
  

  
  
  first_load <- reactiveVal(TRUE)  # Variable qui empêche l'affichage initial du login
  
  
  user_logged <- reactiveVal(NULL)  # Stocke le nom d'utilisateur connecté
  
  observeEvent(input$btn_login, {
    req(input$user_id, input$password)  # Vérifie que les champs sont remplis
    
    users <- load_users()
    
    if (!(input$user_id %in% users$user)) {
      shinyjs::html("error_message", "❌ Utilisateur non trouvé !")
    } else {
      stored_password <- users$password[users$user == input$user_id]
      
      if (input$password == stored_password) {
        shinyjs::html("error_message", "")  # Efface les erreurs
        showNotification(paste("Bienvenue", input$user_id, "!"), type = "message")
        user_logged(input$user_id)  # Stocke l'utilisateur connecté
        removeModal()
        
        ### ✅ Mise à jour du bouton "Se connecter" → "Se déconnecter de ..."
        updateActionButton(session, "open_login", 
                           label = paste("Se déconnecter de", input$user_id), 
                           icon = icon("sign-out"))
        
        favorites(load_favorites(input$user_id))  # Charger les favoris après connexion
        
      } else {
        shinyjs::html("error_message", "❌ Mot de passe incorrect !")
      }
    }
  })
  
  
  
  
  observeEvent(input$btn_register, {
  req(input$user_id, input$password)  # Vérifie que les champs sont remplis

  users <- load_users()

  if (input$user_id %in% users$user) {
    shinyjs::html("error_message", "❌ Nom d'utilisateur déjà pris !")
  } else {
    success <- save_user(input$user_id, input$password)
    
    if (success) {
      shinyjs::html("error_message", "")  # Efface les erreurs
      showNotification("Compte créé avec succès !", type = "message")
      user_logged(input$user_id)  # Stocke l'utilisateur connecté
      removeModal()
    } else {
      shinyjs::html("error_message", "❌ Erreur lors de la création du compte !")
    }
  }
})

  
  observeEvent(input$cancel_login, {
    updateTextInput(session, "user_id", value = "")
    updateTextInput(session, "password", value = "")
    shinyjs::html("error_message", "")  # Effacer les messages d'erreur
    user_logged(NULL)  # Réinitialiser l'utilisateur connecté
    removeModal()  # Fermer la fenêtre modale proprement
    
    # Ajout d'un délai avant de rendre le bouton actif pour éviter un bug
    shinyjs::delay(500, shinyjs::runjs("document.getElementById('open_login').focus();"))
  })
  
  
  
  
  
  
  
  observeEvent(input$open_login, {
  if (!is.null(user_logged())) {
    showNotification(paste("Déconnexion de", user_logged()), type = "warning")
    
    # 🔥 Réinitialisation après déconnexion
    user_logged(NULL)
     # Vide les commentaires
    
    updateActionButton(session, "open_login", label = "Se connecter", icon = icon("user"))
  } else {
    shinyjs::delay(10, showModal(loginModal()))
  }
})

# 🔥 Efface les favoris seulement si l'application redémarre et personne n'est connecté
observe({
  if (is.null(user_logged())) {
    favorites(data.frame())  
  }
})

  
  
  
  
  
  
  
  
  
  
  
  
  
  #############
  
  load_users <- function() {
    file_path <- "data/users.csv"
    if (!file.exists(file_path)) {
      return(data.frame(user = character(), password = character(), stringsAsFactors = FALSE))
    }
    return(read.csv(file_path, stringsAsFactors = FALSE))
  }
  
  save_user <- function(user_id, password) {
    users <- load_users()
    
    if (user_id %in% users$user) {
      return(FALSE)  # Le nom d'utilisateur existe déjà
    }
    
    new_user <- data.frame(user = user_id, password = password, stringsAsFactors = FALSE)
    users <- rbind(users, new_user)
    write.csv(users, "data/users.csv", row.names = FALSE)
    
    return(TRUE)  # Inscription réussie
  }
  
  
  #############################################
  
  favorites <- reactiveVal(data.frame())  # Stocke les favoris de l'utilisateur
  
  load_favorites <- function(user_id) {
    file_path <- paste0("data/favorites_", user_id, ".csv")
    if (file.exists(file_path)) {
      return(read.csv(file_path, stringsAsFactors = FALSE))
    } else {
      return(data.frame())  # Retourne un tableau vide si pas de favoris
    }
  }
  
  save_favorites <- function(user_id, fav_data) {
    file_path <- paste0("data/favorites_", user_id, ".csv")
    write.csv(fav_data, file_path, row.names = FALSE)
  }
  
  
  output$fav_table <- renderDT({
    datatable(favorites(), selection = "single")
  })
  
  
  ##################################
  
  comments_data <- reactiveVal(data.frame(text = character(), rating = numeric(), stringsAsFactors = FALSE))
  
  
  
  
  observeEvent(input$validate_login, {
    req(input$user_id, input$password)  # Vérifie que les champs ne sont pas vides
    
    users <- load_users()  # Charge la liste des utilisateurs
    
    if (input$login_mode == "S'inscrire") {
      success <- save_user(input$user_id, input$password)
      
      if (success) {
        showNotification("Compte créé avec succès !", type = "message")
        removeModal()
      } else {
        showNotification("Nom d'utilisateur déjà pris !", type = "error")
      }
      
    } else {  # Mode connexion
      if (!(input$user_id %in% users$user)) {
        showNotification("Utilisateur non trouvé !", type = "error")
      } else {
        stored_password <- users$password[users$user == input$user_id]
        
        if (input$password == stored_password) {
          showNotification(paste("Bienvenue", input$user_id, "!"), type = "message")
          favorites(load_favorites(input$user_id))  # Charge les favoris de l'utilisateur
          removeModal()
        } else {
          showNotification("Mot de passe incorrect !", type = "error")
        }
      }
    }
  })
  
  
  
  
  
  #####################"
  
  # Fichier de sauvegarde des commentaires
  comments_file <- "data/comments.csv"
  
  # Vérifier si le fichier existe, sinon le créer
  if (!file.exists(comments_file)) {
    write.csv(data.frame(text = character(), rating = numeric()), comments_file, row.names = FALSE)
  }
  
  # Charger les commentaires dans une variable réactive
  comments_data <- reactiveVal(read.csv(comments_file, stringsAsFactors = FALSE))
  
  
  observeEvent(input$submit_review, {
    req(input$comment, input$rating) 
    
    if (is.null(user_logged())) {  
      showNotification("❌ Vous devez être connecté pour laisser un avis !", type = "error")
      return()
    }
    
    # 🔄 Vérifier que comments_data est bien une data frame
    if (!is.data.frame(comments_data())) {
      showNotification("⚠️ Erreur interne : comments_data corrompu. Réinitialisation...", type = "warning")
      comments_data(data.frame(user = character(), text = character(), rating = numeric(), stringsAsFactors = FALSE))
    }
    
    new_comment <- data.frame(
      user = user_logged(),  
      text = as.character(input$comment),  
      rating = as.numeric(input$rating),  
      stringsAsFactors = FALSE
    )
    
    existing_comments <- comments_data()
    updated_comments <- rbind(existing_comments, new_comment)
    
    # ✅ Sauvegarde propre des commentaires
    write.csv(updated_comments, "data/comments.csv", row.names = FALSE)
    # ⚠️ Assurez-vous que cette ligne met bien à jour une data frame
    
    # Mise à jour de l'affichage
    output$comments_ui <- renderUI({
      comments <- comments_data()
      
      if (nrow(comments) == 0) {
        return(h4("Aucun commentaire pour l’instant. Soyez le premier !"))
      }
      
      comment_list <- apply(comments, 1, function(row) {
        tagList(
          div(
            style = "background-color: #f9f9f9; padding: 10px; border-radius: 8px; margin-bottom: 10px;",
            h5(strong(row["user"])),  # 🔹 Affiche l’utilisateur qui a posté le commentaire
            div(
              style = "color: gold;",
              paste(rep("★", as.numeric(row["rating"])), collapse = "")  # 🔥 Convertit la note en étoiles
            ),
            p(row["text"])
          )
        )
      })
      
      do.call(tagList, comment_list)
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################"
  favorites <- reactiveVal(recette[0, ])

  observeEvent(input$btn_explore, {
    updateTabsetPanel(session, "onglet", selected = "Recherche par caractéristiques")
  })
  
  
  
  observeEvent(input$btn_explore, {
    selected_row <- input$recette_table_rows_selected
    if (length(selected_row) > 0) {
      selected_recipe(recettes_filtrees()[selected_row, ])
      updateTabsetPanel(session, "carac_tabs", selected = "Recette")
    }
  })
  
  


  #----- RECHERCHE CARACTERISTIQUES -----
  
  
  # Régime alimentaire
  observe({
    # Si l'utilisateur a sélectionné autre chose que "Tout sélectionner"
    if ("Tout sélectionner" %in% input$diet && length(input$diet) > 1) {
      updateSelectizeInput(session, "diet", selected = input$diet, choices = regimes_disponibles)
    }
  })
  
  # Type de repas
  observe({
    if ("Tout sélectionner" %in% input$meal_type && length(input$meal_type) > 1) {
      updateSelectizeInput(session, "meal_type", selected = input$meal_type, choices = sort(unique(na.omit(recette$course))))
    }
  })
  
  
  
  # Si l'utilisateur ne sélectionne rien, remettre "Tout sélectionner"
  observe({
    if (length(input$diet) == 0) {
      updateSelectizeInput(session, "diet", selected = "Tout sélectionner", choices = c("Tout sélectionner", regimes_disponibles))
    }
  })
  
  observe({
    if (length(input$meal_type) == 0) {
      updateSelectizeInput(session, "meal_type", selected = "Tout sélectionner", choices = c("Tout sélectionner", sort(unique(na.omit(recette$course)))))
    }
  })
  
  
  # Assurer qu'il y ait toujours au moins un choix sélectionné
  observe({
    if (length(input$diet) == 0) {
      updateSelectizeInput(session, "diet", selected = "Tout sélectionner", choices = c("Tout sélectionner", regimes_disponibles))
    }
  })
  
  observe({
    if (length(input$meal_type) == 0) {
      updateSelectizeInput(session, "meal_type", selected = "Tout sélectionner", choices = c("Tout sélectionner", sort(unique(na.omit(recette$course)))))
    }
  })
  
  
  

  recettes_filtrees <- reactiveVal(data.frame())
  selected_recipe <- reactiveVal(NULL)

  output$formatted_time <- renderText({
    label <- names(temps_labels)[temps_labels == input$max_prep_time]
    if (length(label) > 0) label else "Inconnu"
  })

  
  observeEvent(input$search, {
    ingredients <- c(input$ing1, input$ing2, input$ing3) |> 
      tolower() |> 
      trimws() |> 
      stri_trans_general("Latin-ASCII")  # Convertir les accents en caractères simples
    ingredients <- ingredients[ingredients != ""]
    
    allergenes <- tolower(input$allergie) |> trimws() |> 
      stri_trans_general("Latin-ASCII")  # Convertir les accents en caractères simples
    allergenes <- unlist(strsplit(allergenes, "[^a-zA-Z]+"))
    allergenes <- allergenes[allergenes != ""]
    
    diet_selected <- input$diet
    max_prep <- temps_labels[input$max_prep_time]
    meal_selected <- input$meal_type
    
    recettes_filtrees_data <- recette
    
    # ⚡ Appliquer la normalisation aux noms d'ingrédients de la base de données
    recettes_filtrees_data$ingr_name <- stri_trans_general(tolower(recettes_filtrees_data$ingr_name), "Latin-ASCII")
    

    if (length(ingredients) > 0) {
      recettes_filtrees_data <- recettes_filtrees_data |>
        filter(sapply(tolower(ingr_name), function(ing) any(sapply(ingredients, grepl, ing, ignore.case = TRUE))))
    }

    if (length(allergenes) > 0) {
      recettes_filtrees_data <- recettes_filtrees_data |>
        filter(!sapply(tolower(ingr_name), function(ing) any(sapply(allergenes, grepl, ing, ignore.case = TRUE))))
    }

    if ("Tout sélectionner" %in% diet_selected) {
      # Si "Tout sélectionner" est choisi, on ne filtre pas par régime
      recettes_filtrees_data <- recettes_filtrees_data
    } else if (length(diet_selected) > 0) {
      # Filtrer les recettes par les régimes sélectionnés
      recettes_filtrees_data <- recettes_filtrees_data %>%
        filter(diet %in% diet_selected)
    }
    

    if (!is.null(max_prep) && !is.na(max_prep)) {
      recettes_filtrees_data <- recettes_filtrees_data |> filter(total_time <= max_prep)
    }
    
    if ("Tout sélectionner" %in% meal_selected) {
      # Si "Tout sélectionner" est choisi, on ne filtre pas par type de repas
      recettes_filtrees_data <- recettes_filtrees_data
    } else if (length(meal_selected) > 0) {
      # Filtrer les recettes par les types de repas sélectionnés
      recettes_filtrees_data <- recettes_filtrees_data %>%
        filter(course %in% meal_selected)
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
      options = list(pageLength = 5),
      colnames = c("Nom", "Description", "Temps de préparation")
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
      div(
        style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
        actionButton("add_to_fav_carac", " Favoris ",
          icon = icon("heart"),
          style = "position: absolute; top: 5px; right: 62px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"
        ),
        downloadButton("download_recipe", shiny::HTML("<span style='font-weight: bold;'>Télécharger en PDF</span>"),
                       style = "position: absolute; top: 5px; right: 180px; width: 200px; height: 47px; background: #D29B42; color: white; padding: 8px 12px; border-radius: 8px; border: none; font-size: 18px; cursor: pointer; text-align: center;"),
        actionButton("close_recipe", "✖",
          style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"
        ),
        fluidRow(
          column(
            4,
            p(strong("Régime : "), recipe$diet),
            p(strong("Temps de préparation : "), recipe$prep_time, " min"),
            p(strong("Temps de cuisson : "), recipe$cook_time, " min")
          ),
          column(
            8,
            h3(style = "margin-top: 40px;", recipe$name)
            ,
            img(
              src = recipe$image_url, width = "100%",
              style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;"
            )
          )
        ),
        h4("Ingrédients"),
        HTML(paste0("<ul>", ingredients_html, "</ul>")),
        h4("Instructions"),
        p(recipe$instructions)
      )
    )
  })

  
  observeEvent(input$close_recipe, {
    selected_recipe(NULL)  # Réinitialiser la recette sélectionnée
    
    # Définir les sous-onglets des différents onglets principaux
    sous_onglets <- list(
      "carac_tabs" = "Caractéristiques",
      "carte_tabs" = "Carte",
      "placard_tabs" = "Ingrédients",
      "barre_tabs" = "Nom de la recette"
    )
    
    # Vérifier si l'onglet actuel a un sous-onglet défini et le mettre à jour
    for (id in names(sous_onglets)) {
      if (!is.null(input[[id]])) {  
        updateTabsetPanel(session, id, selected = sous_onglets[[id]])
      }
    }
    
    # 🔹 Cas particulier pour Favoris
    if (input$onglet == "Favoris") {
      updateTabsetPanel(session, "favoris_tabs", selected = "Liste des favoris")
    }
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
        fillColor = ~ colorFactor("viridis", world_with_recipes$region_un)(world_with_recipes$region_un),
        fillOpacity = 0.6,
        weight = 1,
        highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.8),
        label = ~name_fr, # Afficher les noms en français
        layerId = ~name
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
  })

  # ---- Mise à jour du zoom sur sélection ----
  observeEvent(input$region_select, {
    region_fr <- input$region_select # Récupère la région sélectionnée en français
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
    clicked_country <- input$map_shape_click$id # Récupère le pays cliqué
    if (!is.null(clicked_country) && clicked_country %in% names(country_mapping_en_to_fr)) {
      french_country_name <- country_mapping_en_to_fr[clicked_country]
      updateSelectInput(session, "region_select", selected = french_country_name)
    }
  })

  # ---- Filtrage des recettes selon la région ou le pays sélectionné ----
  recettes_par_carte <- reactive({
    selected_region <- input$region_select
    if (selected_region == "Aucun") {
      return(data.frame()) # Retourne un tableau vide si "Aucun" est sélectionné
    } else {
      return(recette %>% filter(cuisine == selected_region))
    }
  })

  # ---- Mise à jour du tableau en fonction du pays sélectionné ----
  output$table_carte <- renderDT({
    data <- recettes_par_carte()
    if (nrow(data) == 0) {
      return(NULL)
    }

    
    data$description <- ifelse(nchar(data$description) > 150, 
                               paste0(word(data$description, 1, str_count(substr(data$description, 1, 160), "\\S+")), "..."), 
                               data$description)
    
    # Affiche uniquement les 100 premiers caractères

    datatable(data[, c("name", "description", "prep_time")],
      selection = "single",
      options = list(pageLength = 5),
      colnames = c("Nom", "Description", "Temps de préparation")
    )
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
      div(
        style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
        actionButton("add_to_fav_carte", " Favoris ",
          icon = icon("heart"),
          style = "position: absolute; top: 5px; right: 62px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"
        ),
        downloadButton("download_recipe", shiny::HTML("<span style='font-weight: bold;'>Télécharger en PDF</span>"),
                       style = "position: absolute; top: 5px; right: 180px; width: 200px; height: 47px; background: #D29B42; color: white; padding: 8px 12px; border-radius: 8px; border: none; font-size: 18px; cursor: pointer; text-align: center;"),
        actionButton("close_recipe_carte", "✖",
          style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"
        ),
        fluidRow(
          column(
            4,
            p(strong("Régime : "), recipe$diet),
            p(strong("Temps de préparation : "), recipe$prep_time, " min"),
            p(strong("Temps de cuisson : "), recipe$cook_time, " min")
          ),
          column(
            8,
            h3(style = "margin-top: 40px;", recipe$name)
            ,
            img(
              src = recipe$image_url, width = "100%",
              style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;"
            )
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
      paste0(gsub(" ", "_", selected_recipe()$name), "_recette.pdf")
    },
    content = function(file) {
      recipe <- selected_recipe()
      
      # Télécharger l'image dans un fichier temporaire
      image_path <- tempfile(fileext = ".jpg")
      tryCatch({
        download.file(recipe$image_url, image_path, mode = "wb")
      }, error = function(e) {
        image_path <- "placeholder.jpg"  # Veillez à ce que ce chemin soit correct
      })
      
      # Préparer les paramètres pour le template
      params <- list(
        nom = recipe$name,
        regime = recipe$diet,
        prep_time = recipe$prep_time,
        cook_time = recipe$cook_time,
        ingredients = paste0("- ", strsplit(recipe$ingr_name, ",")[[1]], collapse = "\n"),
        instructions = recipe$instructions,
        image_path = image_path
      )
      
      # Rendre le document et capturer le chemin du PDF généré
      out_pdf <- rmarkdown::render(
        "recette_template.Rmd",
        output_format = "pdf_document",
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      # Vérifier que le PDF existe (pour le débuggage)
      if (!file.exists(out_pdf)) {
        stop("Le PDF n'a pas été créé.")
      }
      
      # Copier le PDF généré vers le chemin fourni par Shiny
      file.copy(out_pdf, file, overwrite = TRUE)
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
    ingredients <- c(
      input$ing21, input$ing22, input$ing23, input$ing24, input$ing25,
      input$ing26, input$ing27, input$ing28, input$ing29, input$ing210
    )
    ingredients <- tolower(trimws(ingredients))
    ingredients <- stri_trans_general(ingredients, "Latin-ASCII")  # 🔥 Supprime les accents
    ingredients <- ingredients[ingredients != ""]
    
    req(length(ingredients) > 0)
    
    result <- recette |>
      mutate(
        ingr_lower = stri_trans_general(tolower(ingr_name), "Latin-ASCII"),  # 🔥 Supprime les accents dans la base
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
        selection = "single",
        colnames = c("Nom", "Description", "Temps de préparation", "Score")
      )
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
      div(
        style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
        actionButton("add_to_fav_placard", " Favoris ",
          icon = icon("heart"),
          style = "position: absolute; top: 5px; right: 62px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"
        ),
        downloadButton("download_recipe", shiny::HTML("<span style='font-weight: bold;'>Télécharger en PDF</span>"),
                       style = "position: absolute; top: 5px; right: 180px; width: 200px; height: 47px; background: #D29B42; color: white; padding: 8px 12px; border-radius: 8px; border: none; font-size: 18px; cursor: pointer; text-align: center;"),
        actionButton("close_recipe_placard", "✖",
          style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"
        ),
        fluidRow(
          column(
            4,
            p(strong("Régime : "), recipe$diet),
            p(strong("Temps de préparation : "), recipe$prep_time, " min"),
            p(strong("Temps de cuisson : "), recipe$cook_time, " min")
          ),
          column(
            8,
            h3(style = "margin-top: 40px;", recipe$name)
            ,
            img(
              src = recipe$image_url, width = "100%",
              style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;"
            )
          )
        ),
        h4("Ingrédients"),
        HTML(paste0("<ul>", ingredients_html, "</ul>")),
        h4("Instructions"),
        p(recipe$instructions)
      )
    )
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
    query <- stri_trans_general(query, "Latin-ASCII")  # 🔥 Supprime les accents
    mots_recherche <- unlist(strsplit(query, "\\s+"))
    
    result <- recette |>
      mutate(
        name_lower = stri_trans_general(tolower(name), "Latin-ASCII"),  # 🔥 Supprime les accents dans la base
        score = sapply(name_lower, function(x) {
          sum(sapply(mots_recherche, function(mot) as.numeric(grepl(mot, x, ignore.case = TRUE))))
        })
      ) |>
      filter(score > 0) |>
      arrange(desc(score))
    
    recettes_found_name(result)
  

    output$recette_table_search <- renderDT({
      datatable(result[, c("name", "description", "prep_time")],
        options = list(pageLength = 5),
        selection = "single",
        colnames = c("Nom", "Description", "Temps de préparation")
      )
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
      div(
        style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
        actionButton("add_to_fav_barre", " Favoris ",
          icon = icon("heart"),
          style = "position: absolute; top: 5px; right: 62px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"
        ),
        downloadButton("download_recipe", shiny::HTML("<span style='font-weight: bold;'>Télécharger en PDF</span>"),
                       style = "position: absolute; top: 5px; right: 180px; width: 200px; height: 47px; background: #D29B42; color: white; padding: 8px 12px; border-radius: 8px; border: none; font-size: 18px; cursor: pointer; text-align: center;"),
        actionButton("close_recipe_barre", "✖",
          style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"
        ),
        fluidRow(
          column(
            4,
            p(strong("Régime : "), recipe$diet),
            p(strong("Temps de préparation : "), recipe$prep_time, " min"),
            p(strong("Temps de cuisson : "), recipe$cook_time, " min")
          ),
          column(
            8,
            h3(style = "margin-top: 40px;", recipe$name)
            ,
            img(
              src = recipe$image_url, width = "100%",
              style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;"
            )
          )
        ),
        h4("Ingrédients"),
        HTML(paste0("<ul>", ingredients_html, "</ul>")),
        h4("Instructions"),
        p(recipe$instructions)
      )
    )
  })

  observeEvent(input$close_recipe_barre, {
    selected_recipe(NULL)
    updateTabsetPanel(session, "barre_tabs", selected = "Nom de la recette")
  })


  #----- FAVORIS -----
  manage_favorites <- function(button_id) {
    observeEvent(input[[button_id]], {
      req(selected_recipe())  # 🔥 Vérifie qu'une recette est sélectionnée
      
      new_recipe <- selected_recipe()
      new_recipe <- new_recipe[, !colnames(new_recipe) %in% c("name_lower", "ingr_lower", "score")]
      current_fav <- favorites()
      
      if (nrow(current_fav) > 0 && new_recipe$name %in% current_fav$name) {
        updated_fav <- current_fav[current_fav$name != new_recipe$name, ]
        favorites(updated_fav)
        
        # 🔥 Sauvegarde seulement si l'utilisateur est connecté
        if (!is.null(user_logged())) {
          save_favorites(user_logged(), updated_fav)
        }
        
        shinyjs::runjs(paste0("$('#", button_id, "').css('color', 'grey');"))
        showNotification("Recette retirée des favoris", type = "warning")
        
        updateTabsetPanel(session, "favoris_tabs", selected = "Liste des favoris")
        
      } else {
        if (nrow(current_fav) == 0) {
          updated_fav <- new_recipe
        } else {
          all_columns <- union(colnames(current_fav), colnames(new_recipe))
          new_recipe <- new_recipe[, all_columns, drop = FALSE]
          current_fav <- current_fav[, all_columns, drop = FALSE]
          updated_fav <- rbind(current_fav, new_recipe)
        }
        
        favorites(updated_fav)
        
        # 🔥 Sauvegarde seulement si l'utilisateur est connecté
        if (!is.null(user_logged())) {
          save_favorites(user_logged(), updated_fav)
        }
        
        shinyjs::runjs(paste0("$('#", button_id, "').css('color', 'red');"))
        showNotification("Recette ajoutée aux favoris", type = "message")
      }
    })
  }
  
  
  
  
  manage_favorites("add_to_fav_carac")
  manage_favorites("add_to_fav_carte")
  manage_favorites("add_to_fav_placard")
  manage_favorites("add_to_fav_barre")
  

  output$fav_table <- renderDT({
    fav_data <- favorites()
    if (nrow(fav_data) == 0) {
      return(NULL)
    }
    fav_data <- fav_data[, !colnames(fav_data) %in% c("ingr_lower", "score")]
    datatable(fav_data[, c("name", "description", "prep_time")],
      selection = "single",
      options = list(pageLength = 5),
      colnames = c("Nom", "Description", "Temps de préparation")
    )
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
      div(
        style = "border: 2px solid #ccc; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: relative;",
        actionButton("add_to_fav_barre", " Favoris ",
                     icon = icon("heart"),
                     style = "position: absolute; top: 5px; right: 62px; background: none; border: none; font-size: 18px; color: grey; cursor: pointer;"
        ),
        downloadButton("download_recipe", shiny::HTML("<span style='font-weight: bold;'>Télécharger en PDF</span>"),
                       style = "position: absolute; top: 5px; right: 180px; width: 200px; height: 47px; background: #D29B42; color: white; padding: 8px 12px; border-radius: 8px; border: none; font-size: 18px; cursor: pointer; text-align: center;"),
        actionButton("close_recipe", "✖",
                     style = "position: absolute; top: 5px; right: 10px; background: none; border: none; font-size: 18px; color: red; cursor: pointer;"
        ),
        fluidRow(
          column(
            4,
            p(strong("Régime : "), recipe$diet),
            p(strong("Temps de préparation : "), recipe$prep_time, " min"),
            p(strong("Temps de cuisson : "), recipe$cook_time, " min")
          ),
          column(
            8,
            h3(style = "margin-top: 40px;", recipe$name),
            img(
              src = recipe$image_url, width = "100%",
              style = "max-height: 300px; object-fit: cover; display: block; margin: 0 auto;"
            )
          )
        ),
        h4("Ingrédients"),
        HTML(paste0("<ul>", ingredients_html, "</ul>")),
        h4("Instructions"),
        p(recipe$instructions)
      )
    )
  })

  observeEvent(input$close_recipe, {
    selected_recipe(NULL)  # Réinitialiser la recette sélectionnée
    
    # Vérifier si on est bien dans l'onglet "Favoris" avant de changer
    if (input$onglet == "Favoris") {
      updateTabsetPanel(session, "favoris_tabs", selected = "Liste des favoris")  
    }
  })
  
  
  
  #-----
  

  # Liste réactive pour stocker les commentaires
  comments_data <- reactiveVal(read.csv("data/comments.csv", stringsAsFactors = FALSE))
  
  
  # Affichage des commentaires
  output$comments_ui <- renderUI({
    comments <- comments_data()
    
    if (nrow(comments) == 0) {
      return(h4("Aucun commentaire pour l’instant. Soyez le premier !"))
    }
    
    comment_list <- apply(comments, 1, function(row) {
      tagList(
        div(
          style = "background-color: #f9f9f9; padding: 10px; border-radius: 8px; margin-bottom: 10px;",
          h5(strong(row["user"])),  # 🔹 Affiche l’utilisateur
          div(
            style = "color: gold;",
            paste(rep("★", as.numeric(row["rating"])), collapse = "")  # 🔥 Convertit en numérique pour éviter des erreurs
          ),
          p(row["text"])
        )
      )
    })
    
    do.call(tagList, comment_list)
  })
  
  
  
  
  # Gestion du bouton de soumission
  observeEvent(input$submit_review, {
    if (is.null(user_logged()) || user_logged() == "") {  
      return()  # Stoppe ici si l'utilisateur n'est pas connecté
    }
    
    if (is.null(input$rating) || input$rating == 0) {
      showNotification("Veuillez sélectionner une note.", type = "warning")
      return()
    }
    
    if (input$comment == "") {
      showNotification("Veuillez entrer un commentaire.", type = "warning")
      return()
    }
    
    # 🔄 Vérifier si `comments_data()` a bien les 3 colonnes attendues
    if (!is.data.frame(comments_data()) || !all(c("user", "text", "rating") %in% colnames(comments_data()))) {
      showNotification("⚠️ Erreur interne : comments_data corrompu. Réinitialisation...", type = "warning")
      comments_data(data.frame(user = character(), text = character(), rating = numeric(), stringsAsFactors = FALSE))
    }
    
    # 🔥 Création du nouveau commentaire avec les 3 colonnes
    new_comment <- data.frame(
      user = as.character(user_logged()),  # Ajout du nom de l'utilisateur
      text = as.character(input$comment),
      rating = as.numeric(input$rating),
      stringsAsFactors = FALSE
    )
    
    # 🔍 Vérifier que `existing_comments` a bien les 3 colonnes
    existing_comments <- comments_data()[, c("user", "text", "rating"), drop = FALSE]  
    
    # 🔗 Fusionner les commentaires existants avec le nouveau
    updated_comments <- rbind(existing_comments, new_comment)
    comments_data(updated_comments)  # ✅ Mise à jour propre
    
    # ✅ Sauvegarde des commentaires dans `data/comments.csv`
    write.csv(updated_comments, "data/comments.csv", row.names = FALSE)

    # 🔄 Recharger les commentaires depuis le fichier pour assurer l'affichage à tous
    comments_data(read.csv("data/comments.csv", stringsAsFactors = FALSE))
    
    # 🧹 Réinitialisation des champs
    updateTextAreaInput(session, "comment", value = "")
    
    showNotification("Merci pour votre avis !", type = "message")  
})

  
  
  
  
  
  
  
  ratings <- reactiveVal(c())
  
  # Lors de la soumission, ajouter la note seulement si un commentaire a été entré
  observeEvent(input$submit_review, {
    if (!is.null(input$comment) && trimws(input$comment) != "" &&
        !is.null(input$rating) && input$rating > 0) {
      ratings(c(ratings(), input$rating))
    }
  })
  
  # Calcul et affichage de la note moyenne
  output$average_rating <- renderUI({
  comments <- comments_data()  # 🔥 Récupère tous les commentaires sauvegardés
  
  if (nrow(comments) > 0 && "rating" %in% colnames(comments)) {
    avg <- round(mean(comments$rating, na.rm = TRUE), 1)  # 🟢 Calcule la moyenne avec toutes les notes
    HTML(paste0("<span>⭐ ", avg, "/5</span>"))
  } else {
    HTML("<span>Pas encore de note</span>")
  }
})

  
}
#---------- 5. LANCER L'APPLICATION ----------
shinyApp(ui = ui, server = server)