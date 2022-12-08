install.packages("bslib")
library(bslib)

ui <- fluidPage(theme = bs_theme(version = 5, bootswatch = "pulse"),
    navbarPage("Castor", id = "navbar", theme = bs_theme(version = 5, bootswatch = "pulse"),
        # Main page
        tabPanel("Home", icone = NA, value = "home",
            fluidRow(
                column(2, h2("Home")),
                column(10, h2("Welcome to the Castor app"))
            )
        ),
        # Map page
        tabPanel("Map", icon = icon("map"), value = "map",
            fluidRow(
                column(2, 
                    h2("Map of Belgium"),
                    selectInput("color", "Color", choices = c("Red", "Orange", "Green", "All"), selected = "Red"),
                    checkboxInput("legend", "Show legend", value = TRUE),
                    textInput("name", "Name"),
                    fluidRow(
                        column(6, numericInput("lat", "Latitude", value = 50.5, min = 49,5 , max = 51,55)),
                        column(6, numericInput("lng", "Longitude", value = 4.5, min = 2.55, max = 6.5))
                    ),
                    textInput("comments", "Comments"),
                    dateInput("date", "Date"),
                    actionButton("add", "Add marker")
                ),
                column(10, leafletOutput("map", height = "100vh", width = "100%"))
            )
        ),
        # About page
        tabPanel("About", icon = icon("info"), value = "about",
            h2("About"),
            h3("This app was made by:")
        )
    )
)
