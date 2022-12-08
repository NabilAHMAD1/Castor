
# Create a function to show the map
show_map <- function(belgique, cordinates, icons_list){
    renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(data = belgique, color = "black", weight = 1, fillOpacity = 0.2) %>%
                addMarkers(lng = cordinates$Longitude, lat = cordinates$Latitude, popup = cordinates$Nom, icon = icons_list) 
        })
}

# Create the server
server <- function(input, output, session) {
    #------------------------------------------------------------------------------
    # Make the map of Belgium
    data('BE_ADMIN_BELGIUM')
    belgique <- st_as_sf(BE_ADMIN_BELGIUM)
    data("BE_ADMIN_PROVINCE")
    provinces <- st_as_sf(BE_ADMIN_PROVINCE) 
    data("BE_ADMIN_MUNTY")
    communes <- st_as_sf(BE_ADMIN_MUNTY)
    # Rivers
    #data("BE_OSM_WATERWAYS")
    #waterways <- st_as_sf(BE_OSM_WATERWAYS)

    #-------------------------------------------------------------------------------
    # Show the map
    output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(data = belgique, color = "black", weight = 1, fillOpacity = 0.2) #%>%
                #addPolygons(data = provinces, color = "black", weight = 1, fillOpacity = 0.2) %>%
                #addPolygons(data = communes, color = "black", weight = 1, fillOpacity = 0.2) %>%
                #addPolygons(data = waterways, color = "blue", weight = 1, fillOpacity = 0.2)
    })

    #-------------------------------------------------------------------------------
    # Get input from the user
    # Show specific markers
    observe({
        proxy <- leafletProxy("map")
        color <- input$color
        cordinates <- read.csv("coordonnée.csv")
        # take only the points with the color selected by the user
        if (color != "All")
            cordinates <- cordinates[cordinates$Couleur == color,]
        # Make a data frame with the color of the points
        df1 <- data.frame(col = cordinates$Couleur)
        # Make a list of the icons
        icons_list <- icons(iconUrl = ifelse(df1$col == "Red",'https://www.clker.com/cliparts/z/G/t/d/x/o/google-maps-marker-for-residencelamontagne-md.png',
                                        ifelse(df1$col == "Orange", "http://www.clker.com/cliparts/U/9/x/u/V/b/orange-marker-black-border-md.png", 
                                        ifelse(df1$col == "Green","http://www.clker.com/cliparts/F/w/l/C/e/W/map-marker-md.png",NA))),
                        iconWidth = cordinates$Width, iconHeight = cordinates$Heigth)
        # clear the markers add the new
        proxy %>% clearMarkers() %>% addMarkers(lng = cordinates$Longitude, lat = cordinates$Latitude, popup = cordinates$Comments, icon = icons_list)
    })
    # Show legend
    observe({
        proxy <- leafletProxy("map")
        if (input$legend) {
            proxy %>% addLegend(position = "bottomright", colors = c("red", "orange", "green"), labels = c("Rouge", "Orange", "Vert"))
        } else {
            proxy %>% clearControls()
        }
    })
    # Add Markers
    observeEvent(input$add, {
        ID <- input$name
        Latitude <- input$lat
        Longitude <- input$lng
        Couleur <- "Red"
        Comments <- input$comments
        Date <- input$date
        Heigth <- 30
        Width <- 20
        data <- data.frame(ID,Latitude,Longitude,Couleur,Heigth,Width,Date,Comments)
        
        write.table(data,
                "coordonnée.csv",
                append = TRUE,
                quote = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = FALSE)
    })

    observeEvent(input$map_click, {
        ID <- input$name
        Latitude <- input$map_click$lat
        Longitude <- input$map_click$lng
        Couleur <- "Red"
        Comments <- input$comments
        Date <- input$date
        Heigth <- 30
        Width <- 20
        data <- data.frame(ID,Latitude,Longitude,Couleur,Heigth,Width,Date,Comments)
        
        write.table(data,
                "coordonnée.csv",
                append = TRUE,
                quote = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = FALSE)
    })
}
