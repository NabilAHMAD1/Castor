
#------------Load libraries----------------
library(shiny)
library(BelgiumMaps.StatBel)
library(sp)
library(sf)
library(leaflet)
library(mongolite)
library(bslib)
library(base64enc)
library(slickR)
#------------Set  Function ----------------
# Create a function to get icon list
get_icon_list <- function(color, coordinates, issue, year, confirmed, coeur) {
    # take only the points with the color selected by the user
    if (!is.null(color)) {
        coordinates <- coordinates[sapply(strsplit(coordinates$Observation, ","), function(x) any(x %in% color)), ]
    }
    # filtre selon problème de cohabitation ou pas
    if (issue != "All") {
        coordinates <- coordinates[coordinates$Issues == issue, ]
    }
     if (confirmed != "All") {
        coordinates <- coordinates[coordinates$Confirmed == confirmed, ]
    }
    if (coeur != "All") {
        coordinates <- coordinates[coordinates$Coeur == coeur, ]
    }
                                          
    # filtre selon année entre min et max
    coordinates <- coordinates[format(as.Date(coordinates$Date, ), "%Y") >= as.character(year[1]) & format(as.Date(coordinates$Date, ), "%Y") <= as.character(year[2]), ]
    # if no points are selected, create a fake point to avoid error
    if (nrow(coordinates) < 1) {
        coordinates <- data.frame(
            Longitude = 0,
            Latitude = 0,
            Category = "Autre",
            Date = "01/01/2020",
            Issues = "No",
            Observation = "No",
            Comments = "No",
            Mail = "No",
            Phone = "No",
            Name = "No",
            Confirmed = "Not sure"
        )
    }
    df1 <- data.frame(col = coordinates$Confirmed)
    icons_list <- icons(
        iconUrl = ifelse(df1$col == "Incertain",
            "http://www.clker.com/cliparts/U/9/x/u/V/b/orange-marker-black-border-md.png",
            ifelse(df1$col == "Fiable",
                        "http://www.clker.com/cliparts/F/w/l/C/e/W/map-marker-md.png", NA
                    )
        ),
        iconWidth = 20, iconHeight = 30
    )
    return(list("icon" = icons_list, "coo" = coordinates))
}
# Create a function to reset markers
reset_markers <- function(input, castor_coordinates) {
    # Get the icons list and the filtered coordinates
    ret <- get_icon_list(input$marker_color, castor_coordinates, input$filtre2, input$filtre1, input$filtre3, input$filtre4)
    icons_list <- ret$icon
    filtered_coordinates <- ret$coo
    proxy <- leafletProxy("map")
    # refresh the markers
    proxy %>%
        clearMarkers() %>%
        addMarkers(
            lng = as.numeric(filtered_coordinates$Longitude),
            lat = as.numeric(filtered_coordinates$Latitude),
            icon = icons_list,
            label = filtered_coordinates$Date,
            layerId = filtered_coordinates[[1]],
            popup = paste(
                "<img src='", paste0("data:image/jpeg;base64,", filtered_coordinates$Img_encoded), "' width='200'/>", br(),
                "<b>Profil:</b>", filtered_coordinates$Category,
                "<br><b>Localisation:</b>", br(), paste("Latitude:", filtered_coordinates$Latitude, br(), "Longitude:", filtered_coordinates$Longitude), br(),
                "<br><b>Observation(s):</b>", filtered_coordinates$Observation,
                "<br><b>Problèmes de cohabitation :</b>", br(), filtered_coordinates$Issues,
                "<br><b>Coeur de territoire:</b>", br(), filtered_coordinates$Coeur,
                "<br><b>Description:</b>", filtered_coordinates$Comments
            ),
            popupOptions = ""
        )
}
# Create a function to check image ext
valid_ext <- function(ext) {
  valid_exts <- c("bmp", "gif", "jpeg", "jpg", "pbm", "pgm", "png", "ppm", "psd", "svg", "tiff", "webp", "xbm", "xpm")
  return(ext %in% valid_exts)
}
# Create the server
server <- function(input, output, session) {
    #------------Db  Connect------------
    # Get the connection string from the environment variable
    connection_string <<- Sys.getenv("MONGO_USER_URI")
    tryCatch(
        {
            # Get the collection from the database
            castor_collection <<- mongo(collection = "coordonnée", db = "castor", url = connection_string)
            # Get the coordinates from the collection
            castor_coordinates <<- castor_collection$find(fields = '{"_id": true, "Confirmed": true, "Category": true, "Date": true, "Issues": true, "Latitude": true, "Longitude": true, "Number": true, "Observation": true, "Comments": true, "Mail": true, "Phone": true, "Name": true, "Img_encoded": true, "Coeur": true}')
        },
        # Show an error message if the points could not be retrieved
        error = function(e) {
            showNotification("Les points ne peuvent pas être retrouvés", type = "error")
        }
    )
    #------------Make  Map  ------------
    # Get the data
    data("BE_ADMIN_BELGIUM")
    # Convert foreign object to an sf object
    belgique <- st_as_sf(BE_ADMIN_BELGIUM)
    # Get the icons list and the filtered coordinates
    ret <- get_icon_list(NULL, castor_coordinates, "All", c(2022, 2023), "All", "All")
    icons_list <- ret$icon
    coordinates <- ret$coo
    # Show the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = belgique, color = "black", weight = 1, fillOpacity = 0.2) %>%
            addMarkers(
            lng = as.numeric(coordinates$Longitude),
            lat = as.numeric(coordinates$Latitude),
            icon = icons_list,
            label = coordinates$Date,
            layerId = coordinates[[1]],
            popup = paste(
                "<img src='", paste0("data:image/jpeg;base64,", coordinates$Img_encoded), "' width='200'/>", br(),
                "<b>Profil:</b>", coordinates$Category,
                "<br><b>Localisation:</b>", br(), paste("Latitude:", coordinates$Latitude, br(), "Longitude:", coordinates$Longitude), br(),
                "<br><b>Observation(s):</b>", coordinates$Observation,
                "<br><b>Problèmes de cohabitation:</b>", br(), coordinates$Issues,
                "<br><b>Description:</b>", coordinates$Comments
            ),
            popupOptions = ""
        ) %>%
        addLegend(
            position = "bottomright",
            colors = c("orange", "green"),
            labels = c("Observation incertaine", "Observation fiable")
        )
    })
    #----------------------------------------
     output$my_carousel <- renderSlickR({
     imgs <- list("https://media.istockphoto.com/id/1460463892/fr/photo/barrage-de-castors-sur-une-petite-rivi%C3%A8re-foresti%C3%A8re-dans-le-moyen-oural.jpg?s=612x612&w=0&k=20&c=EIL4EktWBh-t5e-l7VFVW2TEvMwznyAsFF_S2S15ckc=",
                  "https://media.istockphoto.com/id/1445641712/fr/photo/barrage-beaver-dans-les-badlands.jpg?s=612x612&w=0&k=20&c=LH19xEKF-AVk0S4XcZ7k0G5v8IpyPJUj3EnHZzN467k=",
                  "https://media.istockphoto.com/id/157375045/fr/photo/barrage-de-castor.jpg?s=612x612&w=0&k=20&c=nU1rM-JuCRhBgUdb_bSB5r4Bk1FWAVu3BTdb__lMHLc="
                 )
     slickR(imgs)
   })

   observeEvent(input$go_to_maps, {
        print("go to maps")
     updateNavbarPage(session, "navbar", selected = "map")
   })
    #------------Event Lists------------
    # Filter Markers
    observeEvent(c(input$marker_color, input$filtre1, input$filtre2, input$filtre3, input$filtre4), {
        reset_markers(input, castor_coordinates)
    })
    # Position
    observe({
        updateTextInput(session, "lat", value = input$user_lat)
        updateTextInput(session, "lng", value = input$user_long)
        proxy <- leafletProxy("map")
        if (!is.null(input$geolocation) && input$geolocation != FALSE && !is.null(input$user_lat) && !is.null(input$user_long)) {
            #proxy %>% setView(lng = input$long, lat = input$lat, zoom = 15)
            removeMarker(proxy, "user_location")
            # Add a new marker
            addMarkers(
                proxy,
                lng = input$user_long,
                lat = input$user_lat,
                popup = "Vous êtes ici!",
                popupOptions = "",
                layerId = "user_location"
            ) #%>% setView(lng = input$user_long, lat = input$user_lat, zoom = 15)
        }
    })
    # Show image input
    observeEvent(input$imgInput, {
        # Get the extension of the file
        ext <- tools::file_ext(input$imgInput$datapath)
        # Check if the extension is valid
        if (valid_ext(ext)) {
            # Make the modal box
            showModal(modalDialog(
                title = "Upload an image",
                h3("Image uploaded"),
                imageOutput("imgOut", height = 300),
            ))
            # Show the image in the modal box
            output$imgOut <- renderImage(
                {
                    list(src = input$imgInput$datapath)
                },
                deleteFile = FALSE
            )
        } else {
            # Make the modal box
            showModal(modalDialog(
                title = "Upload an image",
                h3("Veuillez télécharger une image de la forme jpg ou png."),
            ))
        }
    })
    # Add Markers
    observeEvent(input$add, {
        # Check if an image is uploaded
        if (!is.null(input$imgInput)) {
            # check extension
            ext <- tools::file_ext(input$imgInput$datapath)
            if (!valid_ext(ext)) {
                showNotification("Le type de fichier d'image n'est pas valide. Les types de fichier d'image valides sont: bmp, gif, jpeg, jpg, pbm, pgm, png, ppm, psd, svg, tiff, webp, xbm et xpm.", type = "error", duration = 5)
                return()
            }
            # read the binary data of the image
            img_bin <- readBin(input$imgInput$datapath, "raw", n = file.info(input$imgInput$datapath)$size)
            # encode the image to base64
            Img_encoded <- base64encode(img_bin)
        }
        else {
            Img_encoded <- ""
        }
        # Get the data
        Name <- input$name
        Phone <- input$Phone
        Mail <- input$Mail
        Latitude <- input$lat
        Longitude <- input$lng
        Category <- input$category_input
        Confirmed <- input$Confirmed
        Observation <- paste(input$Observation, collapse = ",")
        Comments <- input$comments
        Date <- input$date
        Issues <- input$Issues
        Coeur <- input$Coeur
        # Check if the required fields are filled
        if (Mail == "" || Name == "" || Phone == "" || Observation == "") {
            # Show a notification if not
            showNotification("Veuillez remplir tous les champs.", type = "error", duration = 5)
            return()
        }
        if (!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}", Mail)) {
            showNotification("Veuillez entrer une adresse mail valide.", type = "error", duration = 5)
            return()
        }
        if (Phone != "" && grepl("[0-9]{7,13}", Phone) == FALSE) {
            showNotification("Veuillez entrer un numéro de téléphone valide.", type = "error", duration = 5)
            return()
        }
        if (!grepl("^[A-Za-z ]+$", Name)) {
            showNotification("Veuillez entrer un nom valide.", type = "error", duration = 5)
            return()
        }
        # Create the data frame
        data <- data.frame(
            Img_encoded = Img_encoded,
            Name = Name,
            Phone = Phone,
            Mail =  Mail,
            Latitude = Latitude,
            Longitude = Longitude,
            Category = Category,
            Confirmed = Confirmed,
            Observation = Observation,
            Comments = Comments,
            Date = Date,
            Issues = Issues,
            Coeur = Coeur
        )
        # Insert the data in the database
        tryCatch(
            {
                # Insert the data
                castor_collection$insert(data)
                # Get the last inserted row for the _id
                new_row <- castor_collection$find(fields = '{"_id": true, "Confirmed": true, "Category": true, "Date": true, "Issues": true, "Latitude": true, "Longitude": true, "Number": true, "Observation": true, "Comments": true, "Mail": true, "Phone": true, "Name": true, "Img_encoded": true, "Coeur": true}',limit = 1, sort = '{"_id": -1}')
                # Show a notification if the insertion is successful
                showNotification("Point créé avec succès", type = "message", duration = 5)
            },
            error = function(e) {
                # Show a notification if the insertion is not successful
                showNotification("An error occured!", type = "error", duration = 5)
                return()
            }
        )
        # Update the coordinates list with the new row
        castor_coordinates <<- rbind(castor_coordinates, new_row)
        # Update the map
        reset_markers(input, castor_coordinates)
        # Reset the inputs
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "Phone", value = "")
        updateTextInput(session, "Mail", value = "")
        updateTextInput(session, "comments", value = "")
    })
    # Add Markers on click
    observeEvent(input$map_click, {
        # Check if an image is uploaded
        if (!is.null(input$imgInput)) {
            # check extension
            ext <- tools::file_ext(input$imgInput$datapath)
            if (!valid_ext(ext)) {
                showNotification("Le type de fichier d'image n'est pas valide. Les types de fichier d'image valides sont: bmp, gif, jpeg, jpg, pbm, pgm, png, ppm, psd, svg, tiff, webp, xbm et xpm.", type = "error", duration = 5)
                return()
            }
            # read the binary data of the image
            img_bin <- readBin(input$imgInput$datapath, "raw", n = file.info(input$imgInput$datapath)$size)
            # encode the image to base64
            Img_encoded <- base64encode(img_bin)
        }
        else {
            Img_encoded <- ""
        }
        # Get the data
        Name <- input$name
        Phone <- input$Phone
        Mail <- input$Mail
        Latitude <- input$map_click$lat
        Longitude <- input$map_click$lng
        Category <- input$category_input
        Confirmed <- input$Confirmed
        Observation <- paste(input$Observation, collapse = ",")
        Comments <- input$comments
        Date <- input$date
        Issues <- input$Issues
        Coeur <- input$Coeur
        # Check if the required fields are filled
        if (Mail == "" || Name == "" || Observation == "") {
            # Show a notification if not
            showNotification("Veuillez remplir tous les champs.", type = "error", duration = 5)
            return()
        }
        if (!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}", Mail)) {
            showNotification("Veuillez entrer une adresse mail valide.", type = "error", duration = 5)
            return()
        }
        if (Phone != "" && grepl("[0-9]{10}", Phone) == FALSE) {
            showNotification("Veuillez entrer un numéro de téléphone valide.", type = "error", duration = 5)
            return()
        }
        if (!grepl("^[A-Za-z ]+$", Name)) {
            showNotification("Veuillez entrer un nom valide.", type = "error", duration = 5)
            return()
        }
        # Create the data frame
        data <- data.frame(
            Img_encoded = Img_encoded,
            Name = Name,
            Phone = Phone,
            Mail =  Mail,
            Latitude = Latitude,
            Longitude = Longitude,
            Category = Category,
            Confirmed = Confirmed,
            Observation = Observation,
            Comments = Comments,
            Date = Date,
            Issues = Issues,
            Coeur = Coeur
        )
        # Insert the data in the database
        tryCatch(
            {
                # Insert the data
                castor_collection$insert(data)
                # Get the last inserted row
                new_row <- castor_collection$find(fields = '{"_id": true, "Confirmed": true, "Category": true, "Date": true, "Issues": true, "Latitude": true, "Longitude": true, "Number": true, "Observation": true, "Comments": true, "Mail": true, "Phone": true, "Name": true, "Img_encoded": true, "Coeur": true}',limit = 1, sort = '{"_id": -1}')
                # Show a notification if the insertion is successful
                showNotification("Point crée avec succès!", type = "message", duration = 5)
            },
            error = function(e) {
                # Show a notification if the insertion is not successful
                showNotification("An error occured!", type = "error", duration = 5)
                return()
            }
        )
        # Update the coordinates list with the new row
        castor_coordinates <<- rbind(castor_coordinates, new_row)
        # Update the map
        reset_markers(input, castor_coordinates)
        # Reset the inputs
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "Phone", value = "")
        updateTextInput(session, "Mail", value = "")
        updateTextInput(session, "comments", value = "")
    })
    # Delete Markers
    observeEvent(input$delete, {
        # Get the ID of the selected row
        id <- input$id
        # Make a string that can be used to query the MongoDB collection
        str <- paste0('{"_id": {"$oid":"', id, '"}}')
        # Get the password for the MongoDB connection
        pass <- input$password
        # Update MongoDB connection
        tryCatch(
            {
                # Check if the connection string is already set
                if (connection_string == Sys.getenv("MONGO_ADD_URI")) {
                    # Check if the password is correct
                } else if (pass == Sys.getenv("MONGO_ADD_URI")) {
                    # Set the connection string
                    connection_string <<- pass
                    # Get the MongoDB collection
                    castor_collection <<- mongo(collection = "coordonnée", db = "castor", url = connection_string)
                    showNotification("Password set!", type = "default")
                    # Show an error message if the password is incorrect
                } else {
                    showNotification("Mot de passe incorrect!", type = "error")
                    return()
                }
            },
            # Show an error message if the user does not have the rights to delete the point
            error = function(e) {
                showNotification("Point non supprimé", type = "error")
                return()
            }
        )
        # Catch errors if point could not be deleted
        tryCatch(
            {
                # Remove the point from the MongoDB collection
                castor_collection$remove(str)
                # Show a success message if the point was deleted
                showNotification("Point supprimé!", type = "message")
            },
            # Show an error message if the point could not be deleted
            error = function(e) {
                showNotification("Point non supprimé!", type = "error")
                return()
            }
        )
        # Remove the point from the data frame
        castor_coordinates <<- castor_coordinates[castor_coordinates$`_id` != id,]
        # Update the map
        leafletProxy("map") %>% removeMarker(id)
        # Update input tab
        isolate(input$password)
    })
    # Get Markers ID on click
    observeEvent(input$map_marker_click, {
        # Get the ID of the selected row
        id <- input$map_marker_click$id
        # Update the text input for removing the point
        updateTextInput(session, "id", value = id)
        # Get the data of the selected point
        data <- castor_coordinates[castor_coordinates$`_id` == id,]
        # Update the text input for editing the point SAMPLE !!
        updateTextInput(session, "id_edit", value = id)
        if (connection_string == Sys.getenv("MONGO_ADD_URI")) {
            updateTextInput(session, "name_edit", value = data$Name)
            updateTextInput(session, "Phone_edit", value = data$Phone)
            updateTextInput(session, "Mail_edit", value = data$Mail)
        }
        updateSelectInput(session, "confirmed_edit", selected = data$Confirmed)
        updateSelectInput(session, "category_input_edit", selected = data$Category)
        updateRadioButtons(session, "Issues_edit", selected = data$Issues)
        updateTextInput(session, "comments_edit", value = data$Comments)
        updateSelectInput(session, "Observation_edit", selected = unlist(strsplit(data$Observation, ",")))
        updateDateInput(session, "date_edit", value = data$Date)
        updateNumericInput(session, "latitude_edit", value = data$Latitude)
        updateNumericInput(session, "longitude_edit", value = data$Longitude)
        updateRadioButtons(session, "Coeur_edit", selected = data$Coeur)
    })
    # Edit Markers SAMPLE !!
    observeEvent(input$edit, {
        # Get the ID of the selected row
        id <- input$id_edit
        name_edit <- input$name_edit
        Phone_edit <- input$Phone_edit
        Mail_edit <- input$Mail_edit
        # Make a string that can be used to query the MongoDB collection
        str <- paste0('{"_id": {"$oid":"', id, '"}}')
        # Get the password for the MongoDB connection
        pass <- input$password_edit
        # Update MongoDB connection
        tryCatch(
            {
                # Check if the connection string is already set
                if (connection_string == Sys.getenv("MONGO_ADD_URI")) {
                    # Check if the password is correct
                } else if (pass == Sys.getenv("MONGO_ADD_URI")) {
                    # Check empty fields
                    if (name_edit == "")
                        name_edit <- castor_coordinates[castor_coordinates$`_id` == id,]$Name
                    if (Phone_edit == "")
                        Phone_edit <- castor_coordinates[castor_coordinates$`_id` == id,]$Phone
                    if (Mail_edit == "")
                        Mail_edit <- castor_coordinates[castor_coordinates$`_id` == id,]$Mail
                    # Set the connection string
                    connection_string <<- pass
                    # Get the MongoDB collection
                    castor_collection <<- mongo(collection = "coordonnée", db = "castor", url = connection_string)
                    showNotification("Password set!", type = "default")
                    # Show an error message if the password is incorrect
                } else {
                    showNotification("Password incorrect!", type = "error")
                    return()
                }
            },
            # Show an error message if the user does not have the rights to edit the point
            error = function(e) {
                showNotification("Point non edité, vous n'avez pas les droits!", type = "error")
                return()
            }
        )
        # Catch errors if point could not be deleted
        tryCatch(
            {
                # Update the point in the MongoDB collection
                new <- paste0('{"$set": {"Name": "', name_edit, '", "Phone": "', Phone_edit, '", "Mail": "', Mail_edit, '", "Confirmed": "', input$confirmed_edit, '", "Category": "', input$category_input_edit, '", "Issues": "', input$Issues_edit, '", "Comments": "', input$comments_edit, '", "Observation": "', paste(input$Observation_edit, collapse = ','), '", "Date": "', input$date_edit, '", "Latitude": "', input$latitude_edit, '", "Longitude": "', input$longitude_edit, '", "Coeur": "', input$Coeur_edit, '"}}')
                castor_collection$update(str, new)
                updated_row <- castor_collection$find(str, fields = '{"_id": true, "Confirmed": true, "Category": true, "Date": true, "Issues": true, "Latitude": true, "Longitude": true, "Number": true, "Observation": true, "Comments": true, "Mail": true, "Phone": true, "Name": true, "Img_encoded": true, "Coeur": true}',limit = 1)
                # Show a success message if the point was deleted
                showNotification("Point edité!", type = "message")
                # Update input tab
                isolate(input$password_edit)
                # Update the map
                castor_coordinates <<- castor_coordinates[castor_coordinates$`_id` != id,]
                castor_coordinates <<- rbind(castor_coordinates, updated_row)
                reset_markers(input, castor_coordinates)
            },
            # Show an error message if the point could not be deleted
            error = function(e) {
                showNotification("Point non edité!", type = "error")
                return()
            }
        )
    })
}
# data castor cours zoologie
