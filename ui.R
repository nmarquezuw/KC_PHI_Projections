library(tidyverse)
library(readxl)
library(sf)
library(viridisLite)
library(jsonlite)
library(plotly)
library(dplyr)
library(tidyr)
library(rgdal)
library(geojsonio)
library(sp)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(leaflet)
source("./00_utils.R")

# Source of the function
# https://stackoverflow.com/a/37165597
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL) {
  options <- shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options <- paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

#------------shiny------------------

#------------shiny ui--------
ui <- dashboardPage(
  dashboardHeader(title = "PHI INTERACTIVE VIZ"),

  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "User Guide / Intro Page",
        tabName = "intro"
        # icon = icon("dashboard")
      ),
      menuItem(
        "Interactive Map",
        tabName = "viz_tab"
        # icon = icon("dashboard")
      ),
      menuItem(
        "Acknowledgement",
        tabName = "ack"
        # icon = icon("dashboard")
      )
    ),

    img(
      src = "uw_logo.png",
      height = "45px",
      style = "position: absolute; left: 10px; bottom: 13px;"
    ),

    img(
      src = "csde_logo.gif",
      height = "50px",
      style = "position: absolute; left: 100px; bottom: 10px;"
    ),
    collapsed = FALSE
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",

        h1(
          HTML("Welcome to the Interactive Visualization Tool<br/>For UW’s 2020 Population Health Applied Research Project"),
          style = "text-align: center;"
        )
      ),


      tabItem(
        tabName = "viz_tab",

        textOutput("warning"),

        fluidRow(
          #----------viz------
          column(
            width = 9,

            box(
              width = NULL,

              leafletOutput(
                "map",
                height = 480
              )
            ),

            box(
              width = NULL,
              plotlyOutput(
                "plot",
                height = 230
              ),
              helpText("Click on a tract on the map above to see tract-level population")
            )
          ),
          #----------options--------
          column(
            width = 3,

            box(
              width = NULL,
              height = 90,

              radioButtons(
                inputId = "measure_type",
                label = "Measure",
                choices = c("Count", "Percentage"),
                selected = "Count"
              ),

              radioTooltip(
                id = "measure_type",
                choice = "Percentage",
                title = "<img src=\"percentage_explanation.png\"/>",
                placement = "left",
                options = list(
                  html = TRUE
                )
              )
            ),

            box(
              width = NULL,
              height = 90,

              selectInput(
                inputId = "year",
                label = "Year",
                choices = seq(2020, 2045, 5),
                selected = 2020
              )
            ),

            box(
              width = NULL,
              height = 120,

              radioButtons(
                inputId = "sex",
                label = "Sex",
                choices = c("Female", "Male", "Both"),
                selected = "Both"
              )
            ),

            box(
              width = NULL,

              sliderTextInput(
                inputId = "age",
                label = "Age",
                choices = c(seq(0, 85, 5), "85+"),
                selected = c("15", "45"),
                grid = TRUE
              ),

              actionButton(
                inputId = "all_age",
                label = "Select All"
              )
            ),

            box(
              width = NULL,

              radioButtons(
                inputId = "race",
                label = "Race/Ethnicity",
                choices = c("All", "AIAN", "Asian", "Black", "Hispanic", "NHOPI", "Two or More Races", "White"),
                selected = "All"
              ),

              radioTooltip(
                id = "race",
                choice = "AIAN",
                title = "American Indian and Alaska Native",
                placement = "bottom"
              ),

              radioTooltip(
                id = "race",
                choice = "NHOPI",
                title = "Native Hawaiian or Other Pacific Islander",
                placement = "bottom"
              )
            )
          )
        )
      ),

      tabItem(
        tabName = "ack"
      )
    )
  )
)
