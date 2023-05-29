library(shiny)
library(BelgiumMaps.StatBel)
library(sp)
library(sf)
library(leaflet)
library(mongolite)
library(bslib)
library(slickR)

ui <- navbarPage(
  # logo
  title = div(
    a(
      href = "https://www.ulb.be/", target = "_blank",
      img(
        src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d5/ULB_logo3lp.svg/langfr-1024px-ULB_logo3lp.svg.png",
        height = "40px", style = "max-height: 100%;"
      ),
    ),
    a(
      href = "https://bioing.ulb.be/", target = "_blank",
      img(
        src = "BIOING-logo.webp",
        height = "40px", style = "max-height: 100%;"
      ),
    ),
    style = "margin-left: 10px; margin-right: 10px; display: flex; align-items: center;"
  ),

  # Add responsive styling for small screens
  tags$head(tags$style(HTML("@media (max-width: 768px) {
  .navbar-brand img {
    height: 30px;
  }
}"))),
  id = "navbar",
  # navbar theme
  theme = bs_theme(version = 5, bootswatch = "superhero"),
  # addapt navbar to screen size
  collapsible = TRUE,
  # color
  inverse = TRUE,
  # window title
  windowTitle = "Wildlife in Belgium",
  # navbar items to end
  tags$style(
    ".navbar-nav {
                justify-content: flex-end;
            }
          .navbar-brand {
                display: flex;
            }"
  ),
  # get geolocation
  tags$script('
              window.onblur = function () { document.title = "The beaver dance!"; };
              window.onfocus = function () { document.title = "Wildlife in Belgium"; };
              $(document).ready(function () {
                function getLocation(callback){
                var options = {
                  enableHighAccuracy: true,
                  timeout: 5000,
                  maximumAge: 0
                };
                navigator.geolocation.getCurrentPosition(onSuccess, onError);
                function onError (err) {
                  Shiny.onInputChange("geolocation", false);
                }
                function onSuccess (position) {
                  setTimeout(function () {
                    var coords = position.coords;
                    Shiny.onInputChange("geolocation", true);
                    Shiny.onInputChange("user_lat", coords.latitude);
                    Shiny.onInputChange("user_long", coords.longitude);
                    Shiny.onInputChange("accuracy", coords.accuracy);
                    if (callback) {
                      callback();
                    }
                  }, 1100)
                }
              }
              var TIMEOUT = 10000; //SPECIFY
              var started = false;
              function getLocationRepeat(){
                //first time only - no delay needed
                if (!started) {
                  started = true;
                  getLocation(getLocationRepeat);
                  return;
                }
                setTimeout(function () {
                  getLocation(getLocationRepeat);
                }, TIMEOUT);
              };
              getLocationRepeat();
            });
        '),
  # Main page
  tabPanel("Home",
    value = "home",
    shinyjs::useShinyjs(),
    tags$head(tags$script(HTML('
             var fakeClick = function(tabName) {
             var dropdownList = document.getElementsByTagName("a");
             for (var i = 0; i < dropdownList.length; i++) {
             var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
                         };
                    }
               };
        '))),
    fluidRow(
      column(3),
      column(
        6,
        shiny::HTML("<br><br><center> <h1>Bienvenue</h1> </center><br>"),
        shiny::HTML("<h5>Ce site internet est un outil de suivi de la distribution du castor en Wallonie. Il a été développé par un groupe d’étudiant de l’Université Libre de Bruxelles dans le but d’améliorer la qualité de la conservation de l’espèce dans la région ainsi que d’aider à la gestion des problèmes de cohabitation avec cette dernière.
                                        Pour reporter une observation ou pour voir la distribution actuelle, rendez-vous sur l’onglet ‘Map’. Pour plus d’informations sur le site,
                                        rendez-vous sur l’onglet ‘About’.</h5>")
      ),
      column(3)
    ),
    fluidRow(
      style = "height:50px;"
    ),

    # PAGE BREAK
    tags$hr(),

    # HOW
    mawWidth = 120,
    fluidRow(
      column(
        6,
        shiny::HTML("<br><br><center><h1>Pourquoi protéger le castor ?</h1></center><br>"),
        shiny::HTML("<h5>Longtemps absent du paysage wallon, le castor a été réintroduit dans les années 90. Il est aujourd’hui un acteur majeur de la régulation des écosystèmes Belges. L’espèce est précieuse et intégralement protégée.
Le castor est un allié de la biodiversité. Par sa faculté à aménager son environnement, il recrée des zones propices aux déploiements d’espèces en déclins et contrôle le reboisement naturel des rives. De plus, il joue un rôle dans la régulation des cours d’eau, dans la protection contre l’érosion et dans l’épuration de nos eaux.
Cependant, cette faculté d’aménagement du territoire mène parfois à des problèmes. En effet, des transformations, parfois de grande ampleur, peuvent affecter des zones résidentielles, agricoles ou industrielles. Il existe de nombreux moyens de gérer ces problèmes de cohabitation sans trop perturber l'existence du castor. Rappelons par ailleurs que la protection de l’espèce inclut l’animal lui-même, ses constructions et son habitat.
On peut estimer la population actuelle en Wallonie entre 3200 et 3500 individus. La plupart des bassins versants sont aujourd’hui complètement ou partiellement recolonisés.</h5>")
      ),
      column(
        6,
        br(),
        br(),
        img(
          src = "home1.jpeg",
          height = "auto",
          width = "80%",
          style = "display: block; margin: 0 auto; "
        )
      ),
    ),
    fluidRow(
      style = "height:50px;"
    ),

    # PAGE BREAK
    tags$hr(),

    # WHERE
    fluidRow(
      column(
        6,
        br(),
        br(),
        img(
          src = "home2.jpeg",
          height = "auto",
          width = "80%",
          style = "display: block; margin: 0 auto;"
        )
      ),
      column(
        6,
        shiny::HTML("<br><br><center> <h1>Comment participer ?</h1> </center><br>"),
        shiny::HTML("<h5>Vous observez ou suspectez la présence de castors dans les environs ? Il est très utile pour les professionnels
                                    de l’environnement d’avoir le plus de données possibles sur la présence de l’espèce.
                                    Pour un rapport d’observation de qualité, documentez-vous sur le castor en parcourant <a href=http://biodiversite.wallonie.be/servlet/Repository/brochure-castor-mai-2015.pdf?ID=33061&saveFile=true>la brochure du Service Public de Wallonie.</a>
                                    Puis, remplissez tous les champs requis sur l’onglet ‘Map’. Vous pouvez ajouter toute information complémentaire dans le champ ‘Commentaires’.
                                    Si vous n’êtes pas certain de l’exactitude de votre observation,
                                    mentionnez-le dans le champ ‘Fiabilité de l’observation’.</h5>")
      )
    ),
    fluidRow(
      style = "height:50px;"
    ),
    tags$hr(),
    fluidRow(
      style = "height:50px;"
    ),

    # INSTRUCTIONAL SECTION
    fluidRow(
      shiny::HTML("<br><br><center> <h1>Commencez dès à présent!</h1> </center>
                                            <br>")
    ),
    fluidRow(
      column(3),
      column(
        2,
        div(
          class = "panel panel-default",
          div(
            class = "panel-body", width = "600px",
            align = "center",
            div(
              tags$img(
                src = "https://th.bing.com/th/id/R.afc23fd4da55535d9466835c601e8dd7?rik=MgZviMqstJGKOQ&pid=ImgRaw&r=0",
                width = "50px", height = "50px"
              )
            ),
            div(
              h5(
                "Baladez-vous"
              )
            )
          )
        )
      ),
      column(
        2,
        div(
          class = "panel panel-default",
          div(
            class = "panel-body", width = "600px",
            align = "center",
            div(
              tags$img(
                src = "https://th.bing.com/th/id/R.ba1ba277f2bffbd70ff6a34c53a1fa23?rik=2W%2bbnE6x%2bwEW9g&pid=ImgRaw&r=0",
                width = "50px", height = "50px"
              )
            ),
            div(
              h5(
                "Vous pensez avoir repéré des castors ou des indices laissant penser à la présence de castors ?"
              )
            )
          )
        )
      ),
      column(
        2,
        div(
          class = "panel panel-default",
          div(
            class = "panel-body", width = "600px",
            align = "center",
            div(
              tags$img(
                src = "https://media.cdnws.com/_i/101847/m1440-866/655/69/3169v-chiffre-3.jpeg",
                width = "50px", height = "50px"
              )
            ),
            div(
              h5(
                "Reportez vos observations sur l'onglet «Map»"
              )
            )
          )
        )
      ),
      column(3)
    ),
    fluidRow(
      br(),
      div(
        style = "text-align:center;",
        actionButton("go_to_maps", "Allez vers la carte",
          style = "font-size: 18px; color: white; background-color: green; border-color: green; border-radius: 50px; width: 200px;"
        )
      )
    ),
    fluidRow(
      style = "height:25px;"
    ),
  
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    fluidRow(
      tags$div(
        p(em("Developpé par"),
          br("Ahmad Nabil, Boeckx Benjamin, Hermans Kerryan, Van Malderen Carolina, Vasey Mathieu"),
          style = "text-align:center; font-family: times"
        ),
        style = "border: 1px solid black; padding: 10px;"
      )
    )
  ),
  # Map page
  tabPanel("Map",
    # input style
    tags$style(
      ".form-control {
            background-color: #f8f9fa;
            border-radius: 0.25rem;
            }
            #imgOut img {
            border-radius: 0.25rem;
            width: 100%;
            height: 100%;
            }
        "
    ),
    icon = icon("map"), value = "map",
    # main flex box
    fluidRow(
      # column : setting
      sidebarPanel(
        # navbar : setting
        tabsetPanel(
          id = "tabs",
          # setting : Add Marker
          tabPanel(
            "Ajouter",
            # Add marker input
            h3("Ajouter un point"),
            selectInput("category_input", "Profil",
              choices = c(
                "Professionel",
                "Naturaliste amateur",
                "Autre"
              ),
              selected = "Naturaliste amateur"
            ),
            selectInput("Confirmed", "Fiabilité de l'observation",
              choices = c(
                "Fiable",
                "Incertain"
              ),
              selected = "Incertain"
            ),
            textInput("name", "Nom"),
            textInput("Phone", "Numéro de téléphone"),
            textInput("Mail", "Mail"),
            fluidRow(
              column(
                6,
                numericInput("lat", "Latitude (WGS84)",
                  value = 50.5
                ),
                HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title='les coordonnées GPS de l’observation sont transmises automatiquement si vous acceptez de partager votre localisation. Autrement, cliquez sur la carte à l’endroit désiré après avoir rempli tous les champs. Vous pouvez également remplir les champs ‘Latitude’ et ‘Longitude’ manuellement.'></span>"))
              ),
              column(
                6,
                numericInput("lng", "Longitude (WGS84)",
                  value = 4.5
                ),
                HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title='Les coordonnées GPS de l’observation sont transmises automatiquement si vous acceptez de partager votre localisation. Autrement, cliquez sur la carte à l’endroit désiré après avoir rempli tous les champs. Vous pouvez également remplir les champs ‘Latitude’ et ‘Longitude’ manuellement. '></span>"))
              )
            ),
            checkboxGroupInput("Observation", "Observation(s)",
              choices = list(
                "Barrage",
                "Hutte/Terrier",
                "Individu(s)",
                "Zone inondée",
                "Rongeage",
                "Trace de pas",
                "Autre"
              )
            ),
            radioButtons("Coeur", "Coeur de territoire",
              choices = c(
                "Oui",
                "Non",
                "Incertain"
              ),
              selected = "Incertain"
            ),
            HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title=' On reconnait notamment le cœur de territoire à l’abondance de traces récentes ainsi qu’à la proximité du gite principal (Hutte ou Terrier).'></span>")),
            dateInput("date", "Date"),
            radioButtons("Issues", "Problèmes de cohabitation",
              choices = c(
                "Oui",
                "Non",
                "À risque",
                "Incertain"
              ),
              selected = "Incertain"
              # inline = TRUE
            ),
            fileInput("imgInput", "Partagez une photo de vos observations", accept = c("image/png", "image/jpeg")),
            # imageOutput("imgOut", height = 300),
            textInput("comments", "Commentaire"),
            actionButton("add", "Ajouter point", class = "btn btn-outline-success")
          ),
          # setting : Remove Marker
          tabPanel(
            "Supprimer",
            # Remove marker input
            fluidRow(
              column(
                6,
                textInput("id", "ID")
              ),
              column(
                6,
                passwordInput("password", "Mot de passe")
              )
            ),
            actionButton("delete", "Supprimer un point", class = "btn btn-outline-danger"),
            HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title='Seul un modérateur à le pouvoir de supprimer un point. Cliquez sur un point à supprimer puis entrer le mot de passe.'></span>"))
          ),
          # setting : Filter Marker
          tabPanel(
            "Filtre",
            # Filter marker input
            h3("Filtre"),
            checkboxGroupInput("marker_color", "Observation",
              choices = list(
                "Barrage",
                "Hutte/Terrier",
                "Individu(s)",
                "Zone inondée",
                "Rongeage",
                "Trace de pas",
                "Autre"
              ),
              selected = c("Barrage", "Hutte/Terrier", "Individu(s)", "Zone inondée", "Rongeage", "Trace de pas", "Autre")
            ),
            sliderInput("filtre1", "Selectionner une année :",
              min = 2020,
              max = 2030, value = c(2022, 2023), step = 1, sep = ""
            ),
            selectInput("filtre2", "Problèmes de cohabitation",
              choices = c(
                "Oui",
                "Non",
                "Incertain",
                "À risque",
                "All"
              ),
              selected = "All"
            ),
            selectInput("filtre3", "Fiabilité de l'observation",
              choices = c(
                "Fiable",
                "Incertain",
                "All"
              ),
              selected = "All"
            ),
            selectInput("filtre4", "Coeur de territoire",
              choices = c(
                "Oui",
                "Non",
                "Incertain",
                "All"
              ),
              selected = "All"
            )
          ),
          # setting : Edit Marker SAMPLE !!
          tabPanel(
            "Editer",
            h3("Editer des points"),
            selectInput("category_input_edit", "Profil",
              choices = c(
                "Professionel",
                "Naturaliste amateur",
                "Autre"
              ),
              selected = "Naturaliste amateur"
            ),
            selectInput("confirmed_edit", "Fiabilité de l'observation",
              choices = c(
                "Fiable",
                "Incertain"
              ),
              selected = "Incertain"
            ),
            textInput("name_edit", "Nom"),
            textInput("Phone_edit", "Numéro de téléphone"),
            textInput("Mail_edit", "Mail"),
            fluidRow(
              column(
                6,
                numericInput("latitude_edit", "Latitude (WGS84)",
                  value = 50.5
                ),
                HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title='Les coordonnées GPS de l’observation sont transmises automatiquement si vous acceptez de partager votre localisation. Autrement, cliquez sur la carte à l’endroit désiré après avoir rempli tous les champs. Vous pouvez également remplir les champs ‘Latitude’ et ‘Longitude’ manuellement'></span>"))
              ),
              column(
                6,
                numericInput("longitude_edit", "Longitude (WGS84)",
                  value = 4.5
                ),
                HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title='es coordonnées GPS de l’observation sont transmises automatiquement si vous acceptez de partager votre localisation. Autrement, cliquez sur la carte à l’endroit désiré après avoir rempli tous les champs. Vous pouvez également remplir les champs ‘Latitude’ et ‘Longitude’ manuellement'></span>"))
              )
            ),
            checkboxGroupInput("Observation_edit", "Observation(s)",
              choices = list(
                "Barrage",
                "Hutte/Terrier",
                "Individu(s)",
                "Zone inondée",
                "Rongeage",
                "Trace de pas",
                "Autre"
              )
            ),
            radioButtons("Coeur_edit", "Coeur de territoire",
              choices = c(
                "Oui",
                "Non",
                "Incertain"
              ),
              selected = "Incertain"
            ),
            dateInput("date_edit", "Date"),
            radioButtons("Issues_edit", "Problèmes de cohabitation",
              choices = c(
                "Oui",
                "Non",
                "Incertain",
                "À risque"
              ),
              selected = "Incertain"
              # inline = TRUE
            ),
            # fileInput("imgInput_edit", "Image", accept = c("image/png", "image/jpeg")),
            # imageOutput("imgOut", height = 300),
            textInput("comments_edit", "Commentaires"),
            fluidRow(
              column(
                6,
                textInput("id_edit", "ID")
              ),
              column(
                6,
                passwordInput("password_edit", "Mot de passe")
              )
            ),
            actionButton("edit", "Editer un point", class = "btn btn-outline-primary"),
            HTML(paste0("<span class='glyphicon glyphicon-question-sign' data-toggle='tooltip' data-placement='top' title=\"Seul un modérateur à le pouvoir d'éditer un point. Cliquez sur un point à supprimer puis entrer le mot de passe.\"></span>"))
          )
        )
      ),
      # column : map
      mainPanel(
        leafletOutput("map",
          height = "100vh",
          width = "100%"
        )
      ),
      div(style = "background-color:black;padding:1px;margin-top:5px")
      #  h3("Data :"),
      # tableOutput("dataTable")
    )
  ),
  # About page
  tabPanel("About",
    icon = icon("info"), value = "about",
    fluidRow(
      column(2),
      column(
        8,
        h1("A propos"),
        p(
          "Ce site a été développé par un groupe d’étudiants de l’ULB en bachelier en sciences de l’ingénieur, orientation bioingénieur, dans le cadre d’un projet visant à créer une plateforme de monitoring de la distribution du castor en Wallonie à l’aide d’une approche citoyenne. Ce projet a été supervisé par",
          a("le Laboratoire d’Epidémiologie Spatiale.", href = "https://spell.ulb.be/"),
          "Pour plus d’informations sur le castor et l’amélioration de sa cohabitation avec l’humain, téléchargez",
          a("la brochure du Service Public de Wallonie", href = "http://biodiversite.wallonie.be/servlet/Repository/brochure-castor-mai-2015.pdf?ID=33061&saveFile=true"), "ou visiter le site",
          a("du Groupe de Travail Castors de l’association Natagora.", href = "https://castor.natagora.be/"),
          HTML("Si vous rencontrez des difficultés ou si vous avez des questions, n'hésitez pas à nous contacter par e-mail à l'adresse suivante : <a href='mailto:help.contact.castor@gmail.com'>help.contact.castor@gmail.com</a>. Nous serons heureux de vous aider dans les meilleurs délais.")
        )
      )
    )
  )
)
