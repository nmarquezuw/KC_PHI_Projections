rm(list=ls())
library(tidyverse)
library(leaflet)
library(plotly)
library(dplyr)
library(sp)
library(rgdal)
library(geojsonio)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyBS)
source("./00_utils.R")

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
        "Interactive Visualization",
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
      # Introduction tab
      tabItem(
        tabName = "intro",
        
        tags$style(type='text/css', "p {font-size: 16px !important} li {font-size: 16px !important}"),
        
        h1(
          HTML("Exploring our Future - King County Forecasts to 2045"),
          align = "center"
        ),
        br(),
        
        
        h2(HTML("King County Population Projections Overview")),
        p(HTML("With support from the <a href='https://www.kingcounty.gov/depts/health.aspx'>Public Health Services Division of Seattle & King County</a> and in partnership with the  <a href='https://csde.washington.edu/'>University of Washington’s Center for Studies in Demography and Ecology</a>, the 2020 <a href='https://www.washington.edu/populationhealth/'>Population Health Initiative</a> Applied Research Fellowship Program developed small area population projections at the <a href='https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_13'>Census tract</a> and Health Reporting Area (<a href='https://www.kingcounty.gov/depts/health/data/community-health-indicators/definitions.aspx'>HRA</a>) levels for King County by age, race and ethnicity in 5 year intervals from 2020 to 2045. These UW projections use <a href='https://www.ofm.wa.gov/'>Washington State Office of Financial Management (OFM)</a> estimates of the King County population by sex, race, ethnicity, and 5-year age groups from 2010 and 2015 at the tract-, HRA-, and county-levels. The projections are assessed in relation to existing projections created by OFM and <a href='https://www.psrc.org/'>Puget Sound Regional Council (PSRC)</a>.")),
        br(),
        
        h2(HTML("User Guide")),
        p(HTML("The interactive mapping tool can be used to explore anticipated changes in King County’s population over time by sex age, race and ethnicity, within census tracts and health reporting areas. For example, this tool allows you to see how King County’s population is expected to grow until 2045 if current trends in fertility, mortality, and migration continue. To explore this tool, click the <i>Interactive Map</i> tab on the left side of this page.")),
        br(),
        
        
        h3(tags$i("Walkthrough Video")),
        tags$video(src = 'http://population-dynamics-lab.csde.washington.edu:8080/phi_viz_guide.mp4',  type = 'video/mp', controls = 'controls', onloadstart="this.volume=0.3", width="100%"),
        br(),
        br(),
        br(),
        
        
        h4(tags$i("Geographic Level")),
        p(HTML("To begin, select the geographic level you are interested in examining. The projections are available at both the <i>census tract level</i> and the <i>HRA level</i>.")),
        br(),
        
        h4(tags$i("Race and Ethnicity")),
        p(HTML("Projections can be displayed by specific race and ethnic groups by selecting from the provided race and ethnic categories. Selecting the <i>all</i> option provides projections across all races and ethnicities.")),
        br(),

        h4(tags$i("Year")),
        p("To display King County’s population composition by year, use the drop down menu to select the year of interest. Selecting years before 2020 will display King County’s population composition as it is estimated based on pre-existing data. Selecting 2020 or subsequent years will display what King County’s population is projected to look like if current trends in population change continue. These projections are therefore not certain."),
        br(),
        
        h4(tags$i("Sex")),
        p(HTML("To explore projections by sex choose the <i>female</i>, <i>male</i> or <i>both</i> option.")),
        br(),
        
        h4(tags$i("Age")),
        p(HTML("The age feature allows users to display projections by specific age ranges in 5 year increments. Use the toggle feature to set a minimum and maximum age range (0 to 85+), or click the <i>select all</i> option to display projections across all ages. Note that because the age ranges are measured in 5-year increments, selecting 0-5 will actually reflect the 0-4-year-old population, and selecting 30-35 will show you the 30-34-year-old population. ")),
        br(),
        
        h4(tags$i("Measure")),
        p(HTML("Selecting the <i>count</i> measure will provide a projected population count within each census tract. This population count will be based on the selection of year, sex, age, and race groups chosen, as described above. For example, if you select \"Black\" \"Female,\" \"15-45,\" and year \"2030,\" the population count will show you the number of Black females aged 15-45 who are expected to live in a given tract or HRA in 2030 based on current population trends.")),
        p(HTML("Selecting the <i>percentage</i> measure will provide the percentage of the selected group’s population divided by the total population of the tract or HRA. For example, if you select \"Asian\" \"Male\" \"65-75\" and year \"2040\", the population percentage will show you the percentage of the tract’s or HRA’s population that is expected to fall into the category \"Asian males aged 65 to 75\" in 2040.")),
        p("Note the legend icon in the bottom right corner of the map matches your selection accordingly, as does the y-axis of the line graph."),
        br(),
        
        h4(tags$i("Map")),
        p("All of the above selections are reflected in the map feature. Click on the map for detailed information by census tract or HRA. Additionally, the layers icon in the top right corner of the map allows the user to overlay the locations of multiple, current county facilities across all projections over time. Note that for the \"Transit Lines (2040)\" layer, the darker color indicates that there are multiple transit lines covering the same route."),
        br(),
        
        h4(tags$i("Line Graph")),
        p("The line graph feature provides an additional visual for understanding how different race and ethnic groups are changing over time by census tract or HRA, and reflects the age, race, ethnicity, and sex selections you’ve made above. Selecting a specific tract or HRA in the map will display the population of your selections above in the line graph visual. You may also select multiple race and ethnic groups from the line graph race and ethnicity categories to view at once. Be sure to deselect when you no longer want to view a particular group in the line graph visual."),
        br(),
        
        h2("Limitations"),
        p(HTML("Although population projections aim to provide important estimates for planners, service providers, researchers, and the general public, they are only a reflection of what the population <i>could</i> look like if current population trends continue. They are not determinative of the future. Further, small area projections can be more uncertain and so less predictive than projections for larger areas and populations, and less accurate for the distant future. Like all forecasts, our projections reflect a number of assumptions about expected populations dynamics in King County over the forecast period.")),
        br(),
        
        h2("Methods"),
        p(HTML("The projections presented here use the <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2822904/'>Hamilton Perry method</a> with smoothing (a variant of the cohort-component method) based on OFM’s 5-year age data by race, ethnicity, and sex at state, county, & tract level for 2015 and 2010. Projections were compared with OFM state-level population projections by sex, age, race and ethnicity from 2020 to 2045 and OFM county-level population projections by sex and age from 2020 to 2045. For more information on the methods and assumptions that are used to create these projections, please refer to the <a href='http://population-dynamics-lab.csde.washington.edu:8080/uw_forecast_2045_technical_report.pdf'>Technical Report</a>.")),
        br(),
        
        p(HTML("Please contact <a href='mailto:csde@uw.edu'>csde@uw.edu</a> if you have any questions."))
        
      ),
      
      # Interactive Visualization tab
      tabItem(
        tabName = "viz_tab",
        
        # use shinyjs to control the loading page
        useShinyjs(),
        
        # show loading page first when loading the data
        div(
          id = "initializing_page",
          div(
            h1("Initializing", align = "center"),
            br(),
            br(),
            div(addSpinner(div(), spin = "circle", color = "black"))
          )
        ),
        
        # the actual visualization tool page is hidden when the data are not loaded
        hidden(
          div(
            id = "main_content",
            
            tags$head(tags$style("#warning{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
            )
            ),
            
            # warning text for selecting "percentage" and the whole population, since the result would always be 100%
            textOutput("warning"),
            
            singleton(tags$head(tags$script(src = "pop_patch.js"))),
            tags$style(".tooltip{width: 200px; font-size: 14px; }"),
            
            fluidRow(
              #----------viz------
              column(
                width = 9,
                
                tabBox(
                  width = NULL,
                  height = 560,
                  
                  # tab for the map
                  tabPanel(
                    # loading spinner when loading new data based on user input
                    shinyjs::useShinyjs(),
                    shinyjs::hidden(div(id = 'loading', style = "position: absolute; left: 50%; top: 240px; z-index: 1000;", addSpinner(div(), spin = "circle", color = "black"))),
                    
                    title = "Map",
                    id = "map_tab",
                    
                    leafletOutput(
                      "map",
                      height = 500
                    )
                    
                  ),
                  
                  # tab for the data attriubtion info
                  tabPanel(
                    title = "Data Attribution",
                    id = "data_attribution_tab",
                    
                    tags$style(type='text/css', "li {font-size: 15px !important}"),
                    
                    tags$h3("Data and Resources Used for Creating the Map:"),
                    tags$ul(
                      tags$li(HTML("<a href='https://gis-kingcounty.opendata.arcgis.com/datasets/2010-census-tracts-for-king-county-conflated-to-parcels-major-waterbodies-erased-tracts10-shore-area'>2010 Census Tracts Data by King County GIS Open Data</a>")),
                      tags$li("2010 Health Reporting Area (HRA) data by King County Public Health"),
                      tags$li(HTML("<a href='https://gis-kingcounty.opendata.arcgis.com/datasets/public-health-clinics-ph-clinics-point'>King County Public Health Clinics Data by King County GIS Open Data</a>")),
                      tags$li(HTML("<a href='https://gis-kingcounty.opendata.arcgis.com/datasets/school-sites-in-king-county-schsite-point'>King County School Sites Data by King County GIS Open Data</a>")),
                      tags$li(HTML("The list of Community Health Centers is retrieved from <a href='https://www.kingcounty.gov/depts/health/locations/community-health-centers.aspx'>the King County Public Health website</a>")),
                      tags$li(HTML("The list of Women, Infant and Children Services is retrieved from <a href='https://www.kingcounty.gov/depts/health/locations/wic-first-steps.aspx'>the King County Public Health website</a>")),
                      tags$li("The 2040 Commuter Rail Station, Light Rail Station, and Transit Line data by Puget Sound Regional Council (PSRC)"),
                      tags$li(HTML("<a href='https://icons8.com/icon/18112/hospital'>Hospital icon by Icons8</a>")),
                      tags$li(HTML("<a href='https://icons8.com/icon/18744/school'>School icon by Icons8</a>")),
                      tags$li(HTML("<a href='https://icons8.com/icon/17941/city-railway-station'>City Railway Station icon by Icons8</a>"))
                    )
                  )
                ),
                
                # line chart
                box(
                  width = NULL,
                  plotlyOutput(
                    "plot",
                    height = 230
                  ),
                  helpText(
                    "* Click on a tract on the map above to see tract/HRA-level population.",
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
                    inputId = "geo_level",
                    label = "Geographic Level",
                    choices = c("Census Tract", "Health Reporting Area (HRA)"),
                    selected = "Census Tract"
                  )
                ),
                
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
                  
                  # bstooltip for the helper window
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
          )
        )
        
      ),
      
      # Acknowledgement tab
      tabItem(
        tabName = "ack",
        
        h1(
          "Acknowledgements",
          align = "center"
        ),
        br(),
        
        
        p(HTML("This research was funded by the Population Health Initiative at the University of Washington. Partial support for this research also came from Shanahan Endowment Fellowship and a Eunice Kennedy Shriver National Institute of Child Health and Human Development research infrastructure grant, P2C HD042828, to the Center for Studies in Demography & Ecology at the University of Washington."),
        br(),
        br(),
        
        p(
          "This research was supported by the following individuals:",
          tags$ul(
            tags$li("Xiaoqi (Steven) Bao"),
            tags$li("Eileen Kazura"),
            tags$li("Jessica Lapham"),
            tags$li("Priya Sarma"),
            tags$li("Crystal Yu"),
            tags$li("Eva Wong"),
            tags$li("Rebeccah Maskin"),
            tags$li("Neal Marquez"),
            tags$li("Christine Leibbrand"),
            tags$li("Sara Curran"),
            tags$li("Meher Antia"),
            tags$li("Derek Fulwiler"),
            tags$li("Takashi Inoue")
          )
        ),
        
        br(),
        p(
          "The following R packages are used:",
          tags$ul(
            tags$li("tidyverse"),
            tags$li("leaflet"),
            tags$li("plotly"),
            tags$li("dplyr"),
            tags$li("sp"),
            tags$li("rgdal"),
            tags$li("geojsonio"),
            tags$li("shiny"),
            tags$li("shinydashboard"),
            tags$li("shinyWidgets"),
            tags$li("shinycssloaders"),
            tags$li("shinyBS")
          )
        )
      )
    )
  )
)
)