library(shiny)

# check if required files exist; if not, download the files and save locally

# King County census tract GIS data
if (!file.exists("./data/kc_tract.json")) {
    download_kc_tract()
    while (!file.exists("./data/kc_tract.json")) {
        Sys.sleep(1)
    }
}

# King County public clinics GIS data
if (!file.exists("./data/kc_public_clinics.json")) {
    download_kc_public_clinics()
    while (!file.exists("./data/kc_public_clinics.json")) {
        Sys.sleep(1)
    }
    
}

# King County school sites GIS data
if (!file.exists("./data/kc_schools.json")) {
    download_kc_schools()
    while (!file.exists("./data/kc_schools.json")) {
        Sys.sleep(1)
    }
    
}

# load census tract GIS data
kc_tract_spdf <- readOGR("./data/kc_tract.json")

# load health reporting area GIS data
kc_hra_spdf <- readOGR("./data/kc_hra.json")

# load transit line data
kc_tl_2040 <- readOGR("./data/kc_tl_2040.json")
while (!exists("kc_tl_2040")) {
    Sys.sleep(1)
}

# load tract-level projections
tract_proj <- read.csv(
    file = "./data/tract_age5_race_sex_proj_2000_2045.csv",
    colClasses = c("GEOID" = "character")
)
while (!exists("tract_proj")) {
    Sys.sleep(1)
}

# load HRA-level projections
hra_proj <- read.csv(
    file = "./data/hra_age5_race_sex_proj_2000_2045.csv"
)
while (!exists("hra_proj")) {
    Sys.sleep(1)
}

server <- function(input, output, session) {
    year_reactive <- reactive({
        input$year
    })
    
    race_reactive <- reactive({
        if (input$race == "American Indian and Alaska Native (AIAN)") {
            "AIAN"
        } else if (input$race == "Native Hawaiian or Other Pacific Islander (NHOPI)") {
            "NHOPI"
        } else if (input$race == "All") {
            c("AIAN", "Asian", "Black", "Hispanic", "NHOPI", "Two or More Races", "White")
        } else {
            input$race
        }
    })
    
    sex_reactive <- reactive({
        if (input$sex == "Both") {
            c("Female", "Male")
        } else {
            input$sex
        }
    })
    
    age_reactive <- reactive({
        upper <- 90
        
        if (input$age[2] != "85+") {
            upper <- as.integer(input$age[2])
        }
        
        if (input$age[1] == "85+") {
            c()
        } else {
            lower <- as.integer(input$age[1])
            age_list <- c(
                "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                "70-74", "75-79", "80-84", "85+"
            )
            
            if (upper != lower) {
                age_list[(lower / 5 + 1):(upper / 5)]
            } else {
                c()
            }
        }
    })
    
    # return TRUE when census tract level is selected
    geo_reactive <- reactive({
        input$geo_level == "Census Tract"
    })
    
    measure_reactive <- reactive({
        input$measure_type
    })
    
    # return TRUE if "percentage" and the whole population are selected; this is for generating the warning message
    all_selected <- reactive({
        measure_reactive() == "Percentage" &&
            length(sex_reactive()) == 2 &&
            length(age_reactive()) == 18 &&
            length(race_reactive()) == 7
    })
    
    # warning message when "percentage" and the whole population are selected
    warning_text_reactive <- reactive({
        if (all_selected()) {
            "Please change the \"Measure\" option to \"Count\" when the whole population is selected!"
        } else {
            ""
        }
    })
    
    # warning message when the no age interval is selected
    age_warning_reactive <- reactive({
        if (length(age_reactive()) == 0) {
            "Please select a VALID age range!"
        }
    })
    
    # generate an sp object with population data and related geographic data based on user input
    sp_reactive <- reactive({
        # selected geographic level and then selected age groups
        if (geo_reactive()) {
            selected_df <- tract_proj %>%
                filter(Year %in% year_reactive())
        } else {
            selected_df <- hra_proj %>%
                filter(Year %in% year_reactive())
        }
        
        # if percentage is selected, calculate the percentage values and save in the "value" column
        if (measure_reactive() == "Percentage") {
            selected_df <- selected_df %>%
                group_by(GEOID) %>%
                mutate(
                    percentage = round(value / sum(value) * 10^4) / 10^2
                ) %>%
                ungroup() %>%
                select(-value)
            
            colnames(selected_df)[6] <- "value"
        }
        
        # selected sex, race, and age range
        selected_df <- selected_df %>%
            filter(
                Sex %in% sex_reactive(),
                Race %in% race_reactive(),
                Age5 %in% age_reactive()
            ) %>%
            group_by(GEOID) %>%
            summarize(value = sum(value))
        
        # if "percentage" and the whole population are selected, the calculation
        # will not always be 100% but have some small variances (around 0.5%)
        # the code belowe changes them all to 100% to avoid confusion
        if (measure_reactive() == "Percentage") {
            if (all_selected()) {
                selected_df <- selected_df %>%
                    mutate(value = 100)
            }
        }
        
        # merge the dataframe with the corresponding geographic level GIS data and return
        if (geo_reactive()) {
            merge_df_spdf(selected_df, kc_tract_spdf)
        } else {
            merge_df_spdf(selected_df, kc_hra_spdf)
        }
    })
    
    legend_title_reactive <- reactive({
        if (input$measure_type == "Count") {
            "Population Count"
        } else {
            "Population Percentage (%)"
        }
    })
    
    popup_text_reactive <- reactive({
        paste0(
            ifelse(
                geo_reactive(),
                "Tract GEOID: ",
                "HRA Name: "
            ),
            "<strong>%s</strong><br/>",
                ifelse (
                    input$measure_type == "Count",
                    "Population: <strong>%g</strong>",
                    "Population: <strong>%g %%</strong>"
                )
        )
    })
    
    output$warning <- renderText({
        warning_text_reactive()
    })
    
    output$age_warning <- renderText({
        age_warning_reactive()
    })
    
    # render the initial basemap and the public facility layers
    output$map <- renderLeaflet({
        # load community rail station GIS data
        kc_cr_station_2040 <- readOGR("./data/kc_cr_station_2040.json")
        
        # load light rail station GIS data
        kc_lr_station_2040 <- readOGR("./data/kc_lr_station_2040.json")
        
        # load public clinics GIS data
        kc_public_clinics <- readOGR("./data/kc_public_clinics.json")
        
        # load Women, Infant and Children Services GIS data
        kc_wic <- readOGR("./data/kc_wic.json")
        
        # load Community Health Centers GIS data
        kc_chc <- readOGR("./data/kc_chc.json")
        
        # load school sites data
        kc_schools <- readOGR("./data/kc_schools.json")
        
        # replace code with the corresponding site type name
        kc_schools <- list(
            "School - Elementary" = kc_schools[kc_schools$CODE == "School - Elementary", ],
            "School - Junior High or Middle" = kc_schools[kc_schools$CODE == "School - Junior High or Middle", ],
            "School - High" = kc_schools[kc_schools$CODE == "School - High", ],
            "School - College or University" = kc_schools[kc_schools$CODE == "School - College or University", ],
            "School - Alternative" = kc_schools[kc_schools$CODE == "School - Alternative", ],
            "School - Other facility" = kc_schools[kc_schools$CODE == "School - Other facility", ],
            "School - K thru 12" = kc_schools[kc_schools$CODE == "School - K thru 12", ]
        )
        
        # remove the loading page and show the main content
        shinyjs::hideElement(id = "initializing_page")
        shinyjs::showElement(id = "main_content")
        
        leaflet() %>%
            addProviderTiles(
                providers$CartoDB.Positron,
                options = providerTileOptions(
                    minZoom = 9,
                    maxZoom = 15
                )
            ) %>%
            setView(
                lng = -121.810721,
                lat = 47.412716,
                zoom = 9
            ) %>%
            setMaxBounds(
                -123.222921, 48.300822,
                -120.383728, 46.652146
            ) %>%
            # define z-indexes manually for the polygons, markers, lines added to the map
            # polygon(bottom), line(middle), marker(top)
            addMapPane(
                name = "layer1",
                zIndex = "411"
            ) %>%
            addMapPane(
                name = "layer2",
                zIndex = "412"
            ) %>%
            addMapPane(
                name = "layer3",
                zIndex = "413"
            ) %>%
            addMapPane(
                name = "layer4",
                zIndex = "414"
            ) %>%
            addMapPane(
                name = "layer5",
                zIndex = "415"
            ) %>%
            addMapPane(
                name = "layer6",
                zIndex = "416"
            ) %>%
            addMapPane(
                name = "layer7",
                zIndex = "417"
            ) %>%
            addMapPane(
                name = "layer8",
                zIndex = "418"
            ) %>%
            addMapPane(
                name = "layer9",
                zIndex = "419"
            ) %>%
            addMapPane(
                name = "layer10",
                zIndex = "420"
            ) %>%
            addMapPane(
                name = "layer11",
                zIndex = "421"
            ) %>%
            addMapPane(
                name = "layer12",
                zIndex = "422"
            ) %>%
            addMapPane(
                name = "layer13",
                zIndex = "423"
            ) %>%
            addMapPane(
                name = "layer14",
                zIndex = "424"
            ) %>%
            # add the public facility layers
            addMarkers(
                data = kc_public_clinics,
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = "Public Health Clinics (2018)",
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Address: %s</br>Zip Code: %s",
                    kc_public_clinics$NAME,
                    kc_public_clinics$ADDRESS,
                    kc_public_clinics$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer3")
            ) %>%
            addMarkers(
                data = kc_wic,
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = "Women, Infant and Children Services (2020)",
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Address: %s</br>",
                    kc_wic$Name,
                    kc_wic$Address
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer4")
            ) %>%
            addMarkers(
                data = kc_chc,
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/hospital.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = "Community Health Centers (2020)",
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Address: %s</br>",
                    kc_chc$Name,
                    kc_chc$Address
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer5")
            ) %>%
            addMarkers(
                data = kc_schools[[1]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[1], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[1]]$NAME,
                    kc_schools[[1]]$CODE,
                    kc_schools[[1]]$DISTRICT,
                    kc_schools[[1]]$ADDRESS,
                    kc_schools[[1]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer6")
            ) %>%
            addMarkers(
                data = kc_schools[[2]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[2], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[2]]$NAME,
                    kc_schools[[2]]$CODE,
                    kc_schools[[2]]$DISTRICT,
                    kc_schools[[2]]$ADDRESS,
                    kc_schools[[2]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer7")
            ) %>%
            addMarkers(
                data = kc_schools[[3]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[3], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[3]]$NAME,
                    kc_schools[[3]]$CODE,
                    kc_schools[[3]]$DISTRICT,
                    kc_schools[[3]]$ADDRESS,
                    kc_schools[[3]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer8")
            ) %>%
            addMarkers(
                data = kc_schools[[4]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[4], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[4]]$NAME,
                    kc_schools[[4]]$CODE,
                    kc_schools[[4]]$DISTRICT,
                    kc_schools[[4]]$ADDRESS,
                    kc_schools[[4]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer9")
            ) %>%
            addMarkers(
                data = kc_schools[[5]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[5], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[5]]$NAME,
                    kc_schools[[5]]$CODE,
                    kc_schools[[5]]$DISTRICT,
                    kc_schools[[5]]$ADDRESS,
                    kc_schools[[5]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer10")
            ) %>%
            addMarkers(
                data = kc_schools[[6]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[6], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[6]]$NAME,
                    kc_schools[[6]]$CODE,
                    kc_schools[[6]]$DISTRICT,
                    kc_schools[[6]]$ADDRESS,
                    kc_schools[[6]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                clusterOptions = TRUE,
                options = pathOptions(pane = "layer11")
            ) %>%
            addMarkers(
                data = kc_schools[[7]],
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/metro/26/000000/school.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                group = paste(names(kc_schools)[7], "(2018)"),
                popup = sprintf(
                    "Name: <strong>%s</strong></br>Type: %s</br>District: %s</br>Address: %s</br>Zip Code: %s",
                    kc_schools[[7]]$NAME,
                    kc_schools[[7]]$CODE,
                    kc_schools[[7]]$DISTRICT,
                    kc_schools[[7]]$ADDRESS,
                    kc_schools[[7]]$ZIPCODE
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer12")
            ) %>%
            addMarkers(
                data = kc_cr_station_2040,
                group = "Commuter Rail Stations (2040)",
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/windows/32/000000/city-railway-station.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                popup = sprintf(
                    "Station Name: <strong>%s</strong>",
                    kc_cr_station_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer13")
            ) %>%
            addMarkers(
                data = kc_lr_station_2040,
                group = "Light Rail Stations (2040)",
                icon = makeIcon(
                    iconUrl = "https://img.icons8.com/windows/32/000000/city-railway-station.png",
                    iconWidth = 15,
                    iconHeight = 15
                ),
                popup = sprintf(
                    "Station Name: <strong>%s</strong>",
                    kc_lr_station_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer14")
            ) %>%
            addPolylines(
                data = kc_tl_2040,
                group = "Transit Lines (2040)",
                color = "#62AC55",
                weight = 3,
                opacity = 0.2,
                popup = sprintf(
                    "Transit Line Name: <strong>%s</strong>",
                    kc_tl_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer2")
            ) %>%
            addLayersControl(
                overlayGroups = c(
                    "Public Health Clinics (2018)",
                    "Community Health Centers (2020)",
                    "Women, Infant and Children Services (2020)",
                    paste(names(kc_schools), "(2018)"),
                    "Commuter Rail Stations (2040)",
                    "Light Rail Stations (2040)",
                    "Transit Lines (2040)"
                ),
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            hideGroup(
                c(
                    "Public Health Clinics (2018)",
                    "Community Health Centers (2020)",
                    "Women, Infant and Children Services (2020)",
                    paste(names(kc_schools), "(2018)"),
                    "Commuter Rail Stations (2040)",
                    "Light Rail Stations (2040)",
                    "Transit Lines (2040)"
                )
            )
    })
    
    # this function updates the map based on user input
    observe({
        shinyjs::showElement(id = 'loading')
        
        sp <- sp_reactive()
        
        proxy_map <- leafletProxy(
            "map",
            data = sp
        ) %>%
            # clear the existing shapes and legend
            clearShapes() %>%
            clearControls() %>%
            # since polyline will also be removed by the clearShape() function
            # add the layer again here
            addPolylines(
                data = kc_tl_2040,
                group = "Transit Lines (2040)",
                color = "#62AC55",
                weight = 3,
                opacity = 0.2,
                popup = sprintf(
                    "Transit Line Name: <strong>%s</strong>",
                    kc_tl_2040$Name
                ) %>%
                    lapply(htmltools::HTML),
                options = pathOptions(pane = "layer2")
            )
        
        if (all_selected()) {
            proxy_map <- proxy_map %>%
                addPolygons(
                    layerId = ~GEOID,
                    color = "#606060",
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 0.9,
                    fillOpacity = 0.6,
                    fillColor = ~ colorNumeric(
                        palette = "#08519C",
                        domain = c(100)
                    )(value),
                    highlightOptions = highlightOptions(
                        color = "white", weight = 2,
                        bringToFront = TRUE
                    ),
                    label = sprintf(
                        popup_text_reactive(),
                        sp$GEOID,
                        sp$value
                    ) %>%
                        lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"
                    ),
                    options = pathOptions(pane = "layer1")
                ) %>%
                addLegend(
                    pal = colorNumeric(
                        palette = "#08519C",
                        domain = c(100)
                    ),
                    values = c(100),
                    opacity = 0.7,
                    title = legend_title_reactive(),
                    position = "bottomright"
                )
        } else {
            # define the color palette for filling the polygons based on population
            col_pal <- colorQuantile(
                palette = "Blues",
                domain = sp@data$value,
                n = 5,
                na.color = NA
            )
            
            # calculate the values displayed in the legend
            legend_values <- quantile(sp@data$value, type = 5, names = FALSE, na.rm = TRUE)
            
            # add the new population data to the map
            proxy_map <- proxy_map %>%
                addPolygons(
                    layerId = ~GEOID,
                    color = "#606060",
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 0.9,
                    fillOpacity = 0.6,
                    fillColor = ~ col_pal(value),
                    highlightOptions = highlightOptions(
                        color = "white", weight = 2,
                        bringToFront = TRUE
                    ),
                    label = sprintf(
                        popup_text_reactive(),
                        sp$GEOID,
                        sp$value
                    ) %>%
                        lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"
                    ),
                    options = pathOptions(pane = "layer1")
                ) %>%
                addLegend(
                    pal = col_pal,
                    values = ~value,
                    opacity = 0.7,
                    labFormat = {
                        function(type, cuts, p) {
                            n <- length(cuts)
                            lower <- as.integer(cuts)[-n]
                            if (-n != 1) {
                                lower <- as.integer(cuts)[-n] + 1
                            }
                            upper <- as.integer(cuts)[-1]
                            paste0(lower, " - ", upper, " (", seq(20, 100, 20)[-n], "th PCTL)")
                        }
                    },
                    title = legend_title_reactive(),
                    position = "bottomright"
                )
        }
        
        Sys.sleep(1)
        
        shinyjs::hideElement(id = 'loading')
    })
    
    
    #-----------------plot----------
    clicked_geo <- reactiveValues(df = NULL)
    
    # if not clicking on a polygon, remove the clicked polygon info
    observeEvent(input$map_click,
                 {
                     print(paste("MAP", input$map_click))
                     clicked_geo$df <- NULL
                 },
                 priority = 100
    )
    
    # if clicking on a polygon, save the clicked polygon info
    observeEvent(input$map_shape_click,
                 {
                     print(paste("MAP SHAPE", input$map_shape_click))
                     if (!is.null(input$map_shape_click$id)) {
                        if (geo_reactive()) {
                            clicked_geo$df <- tract_proj %>%
                                filter(GEOID == input$map_shape_click$id)
                        } else {
                            clicked_geo$df <- hra_proj %>%
                                filter(GEOID == input$map_shape_click$id)
                        }
                    }
                 },
                 priority = 99
    )
    
    # generate a dataframe for the line chart
    df_reactive <- reactive({
        # used the clicked polygon
        df <- clicked_geo$df
        
        # if no polygon is clicked, use the county total data
        if (is.null(df)) {
            if (geo_reactive()) {
                df <- tract_proj
            } else {
                df <- hra_proj
            }
        }
        
        df <- df %>%
            select(-GEOID)
        
        # calculate the data based on user input; similar to the process in sp_reactive()
        if (measure_reactive() == "Count") {
            df <- df %>%
                group_by(Year, Age5, Sex, Race) %>%
                summarize(value = sum(value))
        } else {
            df <- df %>%
                group_by(Year, Age5, Sex, Race) %>%
                summarize(value = sum(value)) %>%
                group_by(Year) %>%
                mutate(
                    percentage = round(value / sum(value) * 100, 2)
                ) %>%
                select(-value)
            colnames(df)[5] <- "value"
        }
        
        df <- df %>%
            filter(
                Age5 %in% age_reactive(),
                Sex %in% sex_reactive()
            ) %>%
            group_by(Year, Race) %>%
            summarize(value = sum(value))
        
        # add rows for the total population of all race and ethnicity categories
        df <- rbind(
            df %>%
                mutate(Race = "Total") %>%
                group_by(Year, Race) %>%
                summarize(value = sum(value)) %>%
                arrange(Year),
            df %>%
                arrange(Race, Year)
        )
        
        df
    })
    
    
    output$plot <- renderPlotly({
        df <- df_reactive()
        
        yr = as.integer(year_reactive())
        
        
        P <- plot_ly(
            type = "scatter",
            mode = "lines"
        ) %>%
            # add the vertical dash line at 2020
            layout(
                yaxis = list(rangemode = "tozero"),
                shapes = list(
                    list(
                        type = "line",
                        y0 = 0,
                        y1 = 1,
                        yref = "paper",
                        x0 = 2020,
                        x1 = 2020,
                        line = list(
                            dash = "dash",
                            width = 2,
                            color = "black"
                        )
                    )
                )
            )
        
        # pre-define the colors for drawing lines for different race and ethnicity categories
        col_pal <- c(
            c(
                "rgba(1,1,1,1)",
                "rgba(127,201,127,1)",
                "rgba(190,174,212,1)",
                "rgba(253,192,134,1)",
                "rgba(255,255,153,1",
                "rgba(56,108,176,1)",
                "rgba(240,2,127,1)",
                "rgba(191,91,23,1)"
            )
        )
        
        index <- NULL
        races <- unique(df$Race)
        
        selected_race <- "Total"
        
        if (length(race_reactive()) != 7) {
            selected_race <- race_reactive()
        }
        
        # draw the lines for the unselected race and ethnicity categories and hide them first
        for (i in 1:length(races)) {
            curr_race <- races[i]
            
            if (curr_race != selected_race) {
                pop <- filter(df, Race == curr_race)$value
                
                P <- add_trace(
                    P,
                    x = ~ unique(df$Year),
                    y = pop,
                    name = curr_race,
                    line = list(
                        color = col_pal[i],
                        width = 2
                    ),
                    visible = "legendonly"
                )
            } else {
                index <- i
            }
        }
        
        temp_yval <- filter(filter(df, Race == selected_race), Year == yr)[["value"]]
        
        # draw the line for the selected race/ethnicity
        P <- add_trace(
            P,
            x = ~ unique(df$Year),
            y = ~ filter(df, Race == selected_race)$value,
            name = selected_race,
            line = list(
                color = col_pal[index],
                width = 4
            )
        )
        
        # add titles
        P <- layout(
            P,
            title = ifelse(
                is.null(clicked_geo$df),
                "County-Level Population of the Selected Groups",
                paste0(
                    "Population of the Selected Groups, Selected ",
                    ifelse(
                        geo_reactive(),
                        "Tract (GEOID: ",
                        "HRA ("
                    ),
                    clicked_geo$df$GEOID[1],
                    ")"
                )
            ),
            xaxis = list(
                title = "Year",
                tickformat = "K"
            ),
            yaxis = list(
                title = ifelse(
                    measure_reactive() == "Count",
                    "Population",
                    "Population (%)"
                )
            )
        )
        
        P
    })
    
    # generate a button for going back to the county-level data if a polygon is clicked
    output$reset_chart_button <- renderUI(
        if (!is.null(clicked_geo$df)) {
            actionButton(
                inputId = "reset_line_chart",
                label = "Go Back to County-Level Data"
            )
        }
    )
    
    # generate a button for quickly selecting all age groups when not all age groups are selected
    output$all_age_button <- renderUI(
        if (length(age_reactive()) != 18) {
            actionButton(
                inputId = "all_age",
                label = "Select All Age Groups"
            )
        }
    )
    
    # if "Select All Age Groups" button is clicked, update the input
    observeEvent(input$all_age, {
        updateSliderTextInput(
            session,
            inputId = "age",
            label = "Age Range",
            choices = c(seq(0, 85, 5), "85+"),
            selected = c("0", "85+")
        )
    })
    
    # if "Go Back to County-Level Data" button is clicked, set the clicked polygon to NULL
    observeEvent(input$reset_line_chart, {
        clicked_geo$df <- NULL
    })
    
    # define the helper text the visualizations
    selected_charac_html_text <- reactiveValues()
        
    observe({
        sex <- "female and male"
        if (length(sex_reactive()) == 1) {
            sex <- tolower(sex_reactive())
        }
        
        race <- "population of all racial and ethnic groups"
        
        if (length(race_reactive()) == 1 ) {
            race <- paste0(race_reactive(), " population")
        }
        
        age <- "NA"
        lower <- "NA"
        upper <- "NA"
        
        l = length(age_reactive())
        if (l != 0) {
            lower <- str_split(age_reactive()[1], "-")[[1]][1]
            upper <- age_reactive()[l]
            if (upper == "85+") {
                age <- paste0(
                    "[",
                    lower,
                    "-85+)"
                )
            } else {
                upper <- as.integer(str_split(age_reactive()[l], "-")[[1]][2]) + 1
                age <- paste0(
                    "[",
                    lower,
                    "-",
                    upper,
                    ")"
                )
            }
        }
        
        
        selected_charac_html_text$map <- paste0(
            "Currently displaying <strong>population ",
            tolower(measure_reactive()), "s",
            "</strong> for the <strong>",
            sex, " ", race,
            "</strong> aged <strong>",
            age,
            "</strong> for the year <strong>",
            year_reactive(),
            "</strong> at the <strong>",
            ifelse(
                geo_reactive(),
                "census tract",
                "HRA"
            ),
            "</strong> level."
        )
        
        selected_charac_html_text$plot <- paste0(
            "Currently displaying <strong>population ",
            tolower(measure_reactive()), "s",
            "</strong> for the <strong>",
            sex, " ", race,
            "</strong> aged <strong>",
            age,
            "</strong>."
        )
        
        if (length(age_reactive()) == 18) {
            selected_charac_html_text$age <- "All age groups are selected"
        } else {
            if (upper == "85+") {
                selected_charac_html_text$age <- paste0(
                    "Selected Age Range: ",
                    age,
                    "</br>(i.e. ",
                    lower,
                    " ≤ Selected Ages)"
                )
            } else {
                selected_charac_html_text$age <- paste0(
                    "Selected Age Range: ",
                    age,
                    "</br>(i.e. ",
                    lower,
                    " ≤ Selected Ages < ",
                    upper,
                    ")"
                )
            }
            
        }
    })
    
    observe({
        addTooltip(
            session,
            id = "age",
            title = selected_charac_html_text$age,
            placement = "top",
            options = list(html = TRUE)
        )
    })
    
    observe({
        addTooltip(
            session,
            id = "map",
            title = selected_charac_html_text$map,
            placement = "right",
            options = list(html = TRUE)
        )
    })
    
    observe({
        addTooltip(
            session,
            id = "plot",
            title = selected_charac_html_text$plot,
            placement = "right",
            options = list(html = TRUE)
        )
    })
    
    # preload the visualization once the website is opened
    outputOptions(output, "map", suspendWhenHidden = FALSE)
    outputOptions(output, "plot", suspendWhenHidden = FALSE)
}