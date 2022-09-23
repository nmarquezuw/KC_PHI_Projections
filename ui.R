rm(list=ls())
# install newest version of isoband (a dependency package)
# because versions <= 0.2.4 will cause error during installation
if (!require(isoband)) {
  remotes::install_github("wilkelab/isoband")
}
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
        "Project Overview",
        tabName = "intro"
        # icon = icon("dashboard")
      ),
      menuItem(
        "Interactive Forecast Map",
        tabName = "viz_tab"
        # icon = icon("dashboard")
      ),
      menuItem(
        "User Guide",
        tabName = "guide_tab"
      ),
      menuItem(
        "Acknowledgement",
        tabName = "ack"
        # icon = icon("dashboard")
      )
    ),
    
    br(),
    br(),
    
    div(
      id = "attr_info",
      
      tags$style(type='text/css', ".linked_text:hover { text-decoration: underline !important; }"),
      
      hr(
        style="border-top: 1px solid #5c6367;"
      ),
      HTML("For more information contact:<br><a class='linked_text' href='mailto:csde@uw.edu'>csde@uw.edu</a>"),
      br(),
      br(),
      HTML("Funding for this project provided by: <br> <ul style='list-style-position: outside; padding-inline-start: 20px; padding-inline-end: 15px;'><li style='font-size:13px !important;'><a class='linked_text' target='_blank' href='https://www.washington.edu/populationhealth/'>UW Population Health Initiative</a></li><li style='font-size:13px !important;'><a class='linked_text' target='_blank' href='https://csde.washington.edu'>Center for Studies in Demography & Ecology</a></li></ul>"),
      br(),
      
      hr(
        style="border-top: 1px solid #5c6367;"
      ),
      div(
        style="width:230px;padding-bottom:10px;",
        img(
          src = "uw_logo.png",
          height = "45px",
          style = "position: relative;"
        ),
        
        img(
          src = "csde_logo.gif",
          height = "50px",
          style = "position: relative;"
        )
      ),
      
      HTML(
        "<i style='position:relative; font-size:12px; bottom:0px; width:230px; height: 210px; padding: 0px 30px 12px 0px;'><a class='linked_text' target='_blank' href='https://csde.washington.edu'>CSDE</a> acknowledges we are on Coast Salish territory, the traditional homeland of the Duwamish, Suquamish, Tulalip, and Muckleshoot nations and other Native peoples.<br><a class='linked_text' target='_blank' href='https://csde.washington.edu'>CSDE</a>’s commissioned logo from Native artist, UW Professor Marvin Oliver acknowledges these vital connections to native peoples and territories.</i>"
      )
    ),
    
    
    collapsed = FALSE
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    
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
        
        h2(HTML("Project Overview")),
        p(HTML("Exploring our Future – King County Forecasts to 2045 provides small area population projections at the Census tract and Health Reporting Area levels for King County by age, race and ethnicity, and sex. These projections are for each five-year interval from 2020-2045. To view our estimates, visit our interactive map.")),
        br(),
        
        p(HTML("For an overview of our project, download this <a target='_blank' href='./docs/uw_forecast_2045_slide_deck.pdf'>slide deck</a>. For more information about our methodology, please read our  <a target='_blank' href='http://population-dynamics-lab.csde.washington.edu:8080/kc_forecast_2045/docs/uw_forecast_2045_technical_report.pdf'>technical report</a>. Visit our <a class='clickable_text' onclick='openTab(\"guide_tab\");' style='cursor:pointer;'>User Guide</a> for directions on using our <a class='clickable_text' onclick='openTab(\"viz_tab\");' style='cursor:pointer;'>interactive map</a>.")),
        br(),
  
        p(HTML("Please contact <a target='_blank' href='mailto:csde@uw.edu'>csde@uw.edu</a> if you have any questions."))
        
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
                      tags$li(HTML("<a target='_blank' href='https://gis-kingcounty.opendata.arcgis.com/datasets/2010-census-tracts-for-king-county-conflated-to-parcels-major-waterbodies-erased-tracts10-shore-area'>2010 Census Tracts Data by King County GIS Open Data</a>")),
                      tags$li("2010 Health Reporting Area (HRA) data by King County Public Health"),
                      tags$li(HTML("<a target='_blank' href='https://gis-kingcounty.opendata.arcgis.com/datasets/public-health-clinics-ph-clinics-point'>King County Public Health Clinics Data by King County GIS Open Data</a>")),
                      tags$li(HTML("<a target='_blank' href='https://gis-kingcounty.opendata.arcgis.com/datasets/school-sites-in-king-county-schsite-point'>King County School Sites Data by King County GIS Open Data</a>")),
                      tags$li(HTML("The list of Community Health Centers is retrieved from <a target='_blank' href='https://www.kingcounty.gov/depts/health/locations/community-health-centers.aspx'>the King County Public Health website</a>")),
                      tags$li(HTML("The list of Women, Infant and Children Services is retrieved from <a target='_blank' href='https://www.kingcounty.gov/depts/health/locations/wic-first-steps.aspx'>the King County Public Health website</a>")),
                      tags$li("The 2040 Commuter Rail Station, Light Rail Station, and Transit Line data by Puget Sound Regional Council (PSRC)"),
                      tags$li(HTML("<a target='_blank' href='https://icons8.com/icon/18112/hospital'>Hospital icon by Icons8</a>")),
                      tags$li(HTML("<a target='_blank' href='https://icons8.com/icon/18744/school'>School icon by Icons8</a>")),
                      tags$li(HTML("<a target='_blank' href='https://icons8.com/icon/17941/city-railway-station'>City Railway Station icon by Icons8</a>"))
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
      
      tabItem(
        tabName = "guide_tab",
        # id = "guide_tab",
        # value = "guide_tab",
        
        h1(
          HTML("User Guide"),
          align = "center"
        ),
        br(),
        p(HTML("The interactive mapping tool can be used to explore anticipated changes in King County’s population over time by sex age, race and ethnicity, within census tracts and health reporting areas. For example, this tool allows you to see how King County’s population is expected to grow until 2045 if current trends in fertility, mortality, and migration continue. To explore this tool, click the <i>Interactive Map</i> tab on the left side of this page.")),
        br(),
        
        p(HTML("<a target='_blank' href='./docs/uw_forecast_2045_user_guide.pdf'>Click here to view the user guide in PDF</a>")),
        
        
        h3("Walkthrough Video"),
        tags$video(id = "video", src = 'phi_viz_guide.mp4',  type = 'video/mp4', controls = 'controls', onloadstart="this.volume=0.3", width="100%"),
        br(),
        br(),
        br(),

        h3("Limitations"),
        p(HTML("Although population projections aim to provide important estimates for planners, service providers, researchers, and the general public, they are only a reflection of what the population <i>could</i> look like if current population trends continue. They are not determinative of the future. Further, small area projections can be more uncertain and so less predictive than projections for larger areas and populations, and less accurate for the distant future. Like all forecasts, our projections reflect a number of assumptions about expected populations dynamics in King County over the forecast period.")),
        br(),
        
        h3("Methods"),
        p(HTML("The projections presented here use the <a target='_blank' href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2822904/'>Hamilton Perry method</a> with smoothing (a variant of the cohort-component method) based on OFM’s 5-year age data by race, ethnicity, and sex at state, county, & tract level for 2015 and 2010. Projections were compared with OFM state-level population projections by sex, age, race and ethnicity from 2020 to 2045 and OFM county-level population projections by sex and age from 2020 to 2045. For more information on the methods and assumptions that are used to create these projections, please refer to the <a target='_blank' href='http://population-dynamics-lab.csde.washington.edu:8080/kc_forecast_2045/docs/uw_forecast_2045_technical_report.pdf'>Technical Report</a>.")),
        br()
        
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