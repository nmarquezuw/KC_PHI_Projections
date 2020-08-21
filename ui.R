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
library(shinycssloaders)
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

ui <- dashboardPage(
  title = "Exploring Our Future - King County Forecasts to 2045",
  
  header = dashboardHeader(
    title = "King County Forecasts to 2045",
    titleWidth = 330
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Introduction",
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
      style = "position: fixed; left: 10px; bottom: 13px;"
    ),
    
    img(
      src = "csde_logo.gif",
      height = "50px",
      style = "position: fixed; left: 100px; bottom: 10px;"
    ),
    collapsed = FALSE
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(
        tabName = "intro",
        
        tags$style(type='text/css', "p {font-size: 15px !important} "),
        
        h1(
          "Exploring our Future - King County Forecasts to 2045",
          align = "center"
        ),
        br(),
        
        
        h2("King County Population Projections Overview"),
        p(HTML("With support from the <a href='https://www.kingcounty.gov/depts/health.aspx'>Public Health Services Division of Seattle & King County</a> and in partnership with the  <a href='https://csde.washington.edu/'>University of Washington’s Center for Studies in Demography and Ecology</a>, the 2020 <a href='https://www.washington.edu/populationhealth/'>Population Health Initiative</a> Applied Research Fellowship Program developed small area population projections at the Census tract and Health Reporting Area (<a href='https://www.kingcounty.gov/depts/health/data/community-health-indicators/definitions.aspx'>HRA</a>) levels for King County by age, race and ethnicity in 5 year intervals from 2020 to 2045. These projections use demographic data from the <a href='https://www.census.gov/programs-surveys/acs'>American Community Survey</a> and the <a href='https://www.census.gov/'>Decennial Census</a>, along with existing population projections from the <a href='https://www.ofm.wa.gov/'>Washington Office of Financial Management (OFM)</a> and  <a href='https://www.psrc.org/'>Puget Sound Regional Council (PSRC)</a>.")),
        br(),
        
        h2("User Guide"),
        p("The interactive mapping tool can be used to explore the changing dynamics of King County’s population over time by age, race and ethnicity, within census tracts and health reporting areas. For example, the [name of tool] let’s you see how King County’s population is expected to grow until 2045 if current trends in fertility, mortality, and migration continue. To explore the [name of tool], click the Interactive Map tab on the left side of this page."),
        br(),
        
        h4(tags$i("Measure")),
        p("To begin, select the measure by which you want to view the population totals by tract. Selecting the count measure will provide a projected population count within each census tract. This population count will be based on the selection of year, sex, age, and race groups chosen, as described below. "),
        p("Selecting the percentage measure will provide the percentage of the selected group’s population divided by the total population of the corresponding geography. Note the legend icon in the bottom right corner of the map matches your selection accordingly, as does the y-axis of the line graph."),
        br(),
        
        h4(tags$i("Year and Sex")),
        p("To display projections by year, use the drop down menu to select the year of interest. To explore projections by sex choose the female, male or both option."),
        br(),
        
        h4(tags$i("Age Range")),
        p("The age feature allows users to display projections by specific age ranges in 5 year increments. Use the toggle feature to set a minimum and maximum age range (0 to 85+), or click the select all option to display projections across all ages. "),
        br(),
        
        h4(tags$i("Race and Ethnicity")),
        p("Projections can be displayed by specific race and ethnic groups by selecting from the provided race and ethnic categories. Selecting the all option provides projections across all races and ethnicities."),
        br(),
        
        h4(tags$i("Line Graph")),
        p("The line graph feature provides an additional visual for understanding how different race and ethnic groups are changing over time. You may select multiple race and ethnic groups to view at once. Be sure to deselect when you no longer want to view a particular group in the line graph visual. "),
        br(),
        
        h4(tags$i("Map")),
        p("All of the above selections are reflected in the map feature. Click on the map for detailed information by census tract. Additionally, the layers icon in the top right corner of the map allows the user to overlay multiple, current county facilities across all projections over time. "),
        br(),
        
        h2("Assumptions"),
        p("Although population projections aim to provide important estimates for planners, service providers, researchers, and the general public, they are only a reflection of what the population could look like if current population trends continue. They are not determinative of the future. . Further, small area projections can be more uncertain and so less predictive than projections for larger areas and populations, and less accurate for the distant future. Like all forecasts, our projections reflect a number of assumptions about expected populations dynamics in King County over the forecast period."),
        br(),
        
        h2("Methods"),
        p("The projections presented here use the Hamilton Perry method with smoothing (a variant of the cohort-component method) based on OFM’s 5-year age data by race, ethnicity, and sex at state, county, & tract level from [dates] as observed in data from the [date] American Community Survey and 2010 decennial census data. Projections were compared with OFM state-level population projections by sex, age, race and ethnicity from 2020 to 2045 and OFM county-level population projections by sex and age from 2020 to 2045."),
        br()
        
      ),
      
      
      tabItem(
        tabName = "viz_tab",
        
        tags$head(tags$style("#warning{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
        )
        ),
        
        textOutput("warning"),
        
        singleton(tags$head(tags$script(src = "pop_patch.js"))),
        tags$style(".tooltip{width: 200px; font-size: 14px; }"),
        
        fluidRow(
          #----------viz------
          column(
            width = 9,
            
            id = "viz_col",
            
            box(
              width = NULL,
              
              leafletOutput(
                "map",
                height = 500
              ),
              
              shinyjs::useShinyjs(),
              shinyjs::hidden(div(id = 'loading', style = "position: absolute; left: 50%; top: 240px;", addSpinner(div(), spin = "circle", color = "black")))
              
            ),
            
            box(
              width = NULL,
              plotlyOutput(
                "plot",
                height = 230
              ),
              helpText(
                "* Click on a tract on the map above to see tract-level population.",
                br(),
                "** Click on a name in the legend on the right to show the line for the corresponding race/ethnicity.",
                br(),
                "*** \"M\" indicates millions (‘000,000s); \"k\" indicates thousands (‘000s)."
              ),
              
              uiOutput("reset_chart_button")
              
            )
          ),
          #----------options--------
          column(
            width = 3,
            
            box(
              width = NULL,
              
              radioButtons(
                inputId = "race",
                label = "Race and Ethnicity",
                choices = c("All", "American Indian and Alaska Native (AIAN)", "Asian", "Black", "Hispanic", "Native Hawaiian or Other Pacific Islander (NHOPI)", "Two or More Races", "White"),
                selected = "All"
              )
            ),
            
            
            box(
              width = NULL,
              
              sliderTextInput(
                inputId = "age",
                label = "Age Range",
                choices = c(seq(0, 85, 5), "85+"),
                selected = c("0", "85+"),
                grid = TRUE
              ),
              
              tags$head(tags$style("#age_warning{color: red;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
              )
              ),
              
              textOutput("age_warning"),
              
              uiOutput("all_age_button")
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
              
              selectInput(
                inputId = "year",
                label = "Year",
                choices = seq(2000,2045,5),
                selected = 2020
              ),
              
              bsTooltip(
                id = "year",
                title = "<strong>Our population forecasts start from 2020</strong>; population data before 2020 are OFM estimates.",
                placement = "top",
                options = list(
                  html = TRUE
                )
              )
            ),
            
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
                placement = "top",
                options = list(
                  html = TRUE
                )
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