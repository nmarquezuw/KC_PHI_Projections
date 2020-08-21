library(shiny)

hp_proj <- read.csv(
    file = "./data/tract_age5_race_sex_proj_2000_2045.csv",
    colClasses = c("GEOID" = "character")
)

kc_geo_spdf <- readOGR("./data/kc_tract.json")

kc_public_clinics <- readOGR("./data/kc_public_clinics.json")

kc_schools <- readOGR("./data/kc_schools.json")

kc_schools <- list(
    "School - Elementary" = kc_schools[kc_schools$CODE == "School - Elementary", ],
    "School - Junior High or Middle" = kc_schools[kc_schools$CODE == "School - Junior High or Middle", ],
    "School - High" = kc_schools[kc_schools$CODE == "School - High", ],
    "School - College or University" = kc_schools[kc_schools$CODE == "School - College or University", ],
    "School - Alternative" = kc_schools[kc_schools$CODE == "School - Alternative", ],
    "School - Other facility" = kc_schools[kc_schools$CODE == "School - Other facility", ],
    "School - K thru 12" = kc_schools[kc_schools$CODE == "School - K thru 12", ]
)

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
    
    measure_reactive <- reactive({
        input$measure_type
    })
    
    all_selected <- reactive({
        measure_reactive() == "Percentage" &&
            length(sex_reactive()) == 2 &&
            length(age_reactive()) == 18 &&
            length(race_reactive()) == 7
    })
    
    warning_text_reactive <- reactive({
        if (all_selected()) {
            "Please change the \"Measure\" option to \"Count\" when the whole population is selected!"
        } else {
            ""
        }
    })
    
    age_warning_reactive <- reactive({
        if (length(age_reactive()) == 0) {
            "Please select a VALID age range!"
        }
    })
    
    observeEvent(input$all_age, {
        updateSliderTextInput(
            session,
            inputId = "age",
            label = "Age Range",
            choices = c(seq(0, 85, 5), "85+"),
            selected = c("0", "85+")
        )
    })
    
    sp_reactive <- reactive({
        selected_df <- hp_proj %>%
            filter(Year %in% year_reactive())
        
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
        
        selected_df <- selected_df %>%
            filter(
                Sex %in% sex_reactive(),
                Race %in% race_reactive(),
                Age5 %in% age_reactive()
            ) %>%
            group_by(GEOID) %>%
            summarize(value = sum(value))
        
        if (measure_reactive() == "Percentage") {
            if (all_selected()) {
                selected_df <- selected_df %>%
                    mutate(value = 100)
            }
        }
        
        merge_df_spdf(selected_df, kc_geo_spdf)
    })
    
    legend_title_reactive <- reactive({
        if (input$measure_type == "Count") {
            "Population Count"
        } else {
            "Population Percentage (%)"
        }
    })
    
    popup_text_reactive <- reactive({
        paste(
            "GEOID: <strong>%s</strong><br/>",
            {
                if (input$measure_type == "Count") {
                    "Population: <strong>%g</strong>"
                } else {
                    "Population: <strong>%g %%</strong>"
                }
            },
            sep = ""
        )
    })
    
    output$warning <- renderText({
        warning_text_reactive()
    })
    
    output$age_warning <- renderText({
        age_warning_reactive()
    })
    
    output$map <- renderLeaflet({
        selected_df <- hp_proj %>%
            filter(Year %in% 2020)
        
        selected_df <- selected_df %>%
            filter(
                Sex %in% c("Female", "Male"),
                Race %in% c("AIAN", "Asian", "Black", "Hispanic", "NHOPI", "Two or More Races", "White"),
                Age5 %in% c(
                    "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44"
                )
            ) %>%
            group_by(GEOID) %>%
            summarize(value = sum(value))
        
        sp <- merge_df_spdf(selected_df, kc_geo_spdf)
        
        col_pal <- colorQuantile(
            palette = "Blues",
            domain = sp@data$value,
            n = 5,
            na.color = NA
        )
        
        legend_values <- quantile(sp@data$value, type = 5, names = FALSE, na.rm = TRUE)
        
        leaflet(sp) %>%
            addTiles(attribution = HTML("<a href='https://icons8.com/icon/633/hospital'>Hospital icon</a> and <a href='https://icons8.com/icon/1953/school'>School icon</a> by Icons8")) %>%
            addProviderTiles(
                providers$CartoDB.Positron,
                options = providerTileOptions(
                    minZoom = 9,
                    maxZoom = 15
                )
            ) %>%
            setMaxBounds(
                -123.222921, 48.300822,
                -120.383728, 46.652146
            ) %>%
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
                    paste0(
                        "GEOID: <strong>%s</strong><br/>",
                        "Population: <strong>%g</strong>"
                    ),
                    sp$GEOID,
                    sp$value
                ) %>%
                    lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
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
                        paste0(lower, " - ", upper, "(", seq(20, 100, 20)[-n], "th PCTL)")
                    }
                },
                title = "Population Count",
                position = "bottomright"
            ) %>%
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
                    lapply(htmltools::HTML)
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
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
                clusterOptions = TRUE
            ) %>%
            addLayersControl(
                overlayGroups = c(
                    "Public Health Clinics (2018)",
                    paste(names(kc_schools), "(2018)")
                ),
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            hideGroup(
                c(
                    "Public Health Clinics (2018)",
                    paste(names(kc_schools), "(2018)")
                )
            )
    })
    
    outputOptions(output, "map", suspendWhenHidden = FALSE)
    
    observe({
        shinyjs::showElement(id = 'loading')
        
        sp <- sp_reactive()
        
        proxy_map <- leafletProxy(
            "map",
            data = sp
        ) %>%
            clearShapes() %>%
            clearControls()
        
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
                    )
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
            col_pal <- colorQuantile(
                palette = "Blues",
                domain = sp@data$value,
                n = 5,
                na.color = NA
            )
            
            legend_values <- quantile(sp@data$value, type = 5, names = FALSE, na.rm = TRUE)
            
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
                    )
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
    clicked_tract <- reactiveValues(df = NULL)
    
    observeEvent(input$map_click,
                 {
                     clicked_tract$df <- NULL
                 },
                 priority = 100
    )
    
    
    observeEvent(input$map_shape_click,
                 {
                     clicked_tract$df <- hp_proj %>%
                         filter(GEOID == input$map_shape_click$id)
                 },
                 priority = 99
    )
    
    df_reactive <- reactive({
        df <- clicked_tract$df
        
        if (is.null(df)) {
            df <- hp_proj
        }
        
        df <- df %>%
            select(-GEOID)
        
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
        
        P <- layout(
            P,
            title = ifelse(
                is.null(clicked_tract$df),
                "County-Level Population of the Selected Groups",
                paste0("Population of the Selected Groups, Selected Tract (GEOID: ", clicked_tract$df$GEOID[1], ")")
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
    
    output$reset_chart_button <- renderUI(
        if (!is.null(clicked_tract$df)) {
            actionButton(
                inputId = "reset_line_chart",
                label = "Go Back to County-Level Data"
            )
        }
    )
    
    output$all_age_button <- renderUI(
        if (length(age_reactive()) != 18) {
            actionButton(
                inputId = "all_age",
                label = "Select All Age Groups"
            )
        }
    )
    
    observeEvent(input$reset_line_chart, {
        clicked_tract$df <- NULL
    })
    
    outputOptions(output, "plot", suspendWhenHidden = FALSE)
    
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
        
        l = length(age_reactive())
        if (l != 0) {
            if (age_reactive()[l] == "85+") {
                age <- paste0(
                    str_split(age_reactive()[1], "-")[[1]][1],
                    "-85+"
                )
            } else {
                age <- paste0(
                    str_split(age_reactive()[1], "-")[[1]][1],
                    "-",
                    str_split(age_reactive()[l], "-")[[1]][2]
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
            "</strong>."
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
    
}