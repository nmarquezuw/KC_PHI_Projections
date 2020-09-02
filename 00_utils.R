# functions in R help to clean up processes that we may need to do several
# times its also helpful to give your function a useful

#' Takes a compiled data object with 5 year windows and returns the mid-year
#' 
#' @description alters the year column of a data frame that has a form similar
#' to '2009-2013' and returns the mid year for that range as a numeric column.
#' Also optionally removes the acs census  year 
#' 
#' @param DF data frame like object with a character column named YEAR
#' @param remove_acs_overlap logical if 2008-2012 should be filtered out
#' @return data frame with YEAR converted to numeric and midpoints extracted

clean_years <- function(DF, remove_acs_overlap = TRUE){
    if(!is.data.frame(DF)){
        stop("DF must be a dataframe that has a character YEAR variable")
    }
    if(!is.character(DF$YEAR)){
        stop("DF must be a dataframe that has a character YEAR variable")
    }
    DF %>%
        filter(YEAR != "2008-2012" | rep(!remove_acs_overlap, nrow(.))) %>%
        mutate(YEAR = ifelse(
            str_length(YEAR) == 4, 
            as.numeric(str_sub(YEAR, 1, 4)),
            as.numeric(str_sub(YEAR, 1, 4)) + 2))
}

#' Collapses ACS ages into more coarse groupings
#' 
#' @description alters the age column of a data frame that has 18 5 year age
#' groups and returns a data frame with only 4 age groups in a newly renamed
#' column.
#' 
#' @param DF data frame like object with a character column named Age5
#' @return data frame with collapsed age groups

collapse_ages <- function(DF){
    if(!is.data.frame(DF)){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
    if(length(levels(DF$Age5)) != 18){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
    
    DF %>%
        mutate(Age5 = as.numeric(Age5)) %>%
        mutate(Age = case_when(
            Age5 %in% 1:3 ~ "0-14",
            Age5 %in% 14:18 ~ "65+",
            Age5 %in% 10:13 ~ "45-64",
            TRUE ~ "15-44")) %>%
        select(-Age5) %>%
        group_by_at(vars(-value)) %>%
        summarise_all(sum) %>%
        ungroup()
}

#' Aggregates Ages and Population totals to calculate mean age of population
#' 
#' @description uses the age column of a data frame that has 18 5 year age
#' groups and the population values and returns a data frame with an estimated
#' mean age of the remaining identifying variables.
#' 
#' @param DF data frame like object with a character column named Age5
#' @return data frame with estimated
calc_mean_age <- function(DF){
    if(!is.data.frame(DF)){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
    if(length(levels(DF$Age5)) != 18){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
    
    DF %>%
        mutate(Age5 = (as.numeric(Age5) * 5) - 2.5) %>%
        group_by_at(vars(-Age5, -value)) %>%
        summarize(mean_age = sum(Age5 * value) / sum(value)) %>%
        ungroup()
}

#' Factors 5 year ages in the appropriate order
#' 
#' @description Factors 5 year ages in the appropriate order when labels
#' appear in the format "0-4", "5-9", "10-14", "15-19", etc.
#' 
#' @param x character vector of ages
#' @return factor vector
factor_ages <- function(x){
    factor(
        x,
        c(str_c(seq(0, 80, by = 5), "-", seq(4, 84, by = 5)), "85+")
    )
}

#'
#'
#'

download_kc_race_data <- function(tract = TRUE, collapse_hispanic = TRUE){
    
    options(tigris_use_cache=TRUE)
    
    tf <- tempfile(fileext = ".xlsx")
    
    lks <- "https://www.ofm.wa.gov/sites/default/files/public/" %>%
        str_c("dataresearch/pop/") %>%
        str_c(c(
            "asr/sade/ofm_pop_sade_county_2010_to_2019.xlsx",
            "asr/sade/ofm_pop_sade_county_2000_to_2010.xlsx"))
    
    if(tract){
        lks <- "https://www.ofm.wa.gov/sites/default/files/public/" %>%
            str_c(c(
                "dataresearch/pop/asr/sade/ofm_pop_sade_tract_2015_to_2019",
                "dataresearch/pop/asr/sade/ofm_pop_sade_tract_2010_to_2014",
                "legacy/pop/asr/sade/ofm_pop_sade_tract_2000_to_2010")) %>%
            str_c(".xlsx")
    }
    
    # sheet 1 is a description of the data
    # sheet 2 is broken down by age race and sex
    # sheet 3 is the same but only for the Hispanic population
    # sheet 4 is again the same but only for the non-Hispanic population
    
    re_kc_df <- bind_rows(lapply(lks, function(i){
        
        download.file(i, tf)
        # loop through sheet 3 and 4 to get both Hispanic/non Hispanic pop
        sub_df <- bind_rows(lapply(3:4, function(x){
            read_excel(tf, sheet = x) %>%
                # filter only to king county
                filter(str_starts(`Area ID`, "53033")) %>%
                # remove some unwanted columns
                select(-`Area Name`) %>%
                # make data long for ggplot and ease of analysis
                pivot_longer(-(`Area ID`:`Age Group`)) %>%
                # remove totals we only want sex specific data
                filter(!str_detect(name, "Total") & `Age Group` != "Total") %>%
                # remove the sex totals
                filter(str_split_fixed(name, " ", 2)[,2] != "Male") %>%
                filter(str_split_fixed(name, " ", 2)[,2] != "Female") %>%
                # detect sex information
                mutate(Sex = ifelse(
                    str_detect(name, "Female"), "Female", "Male")) %>%
                # extract race information
                mutate(HRace = str_remove(name, " Male| Female")) %>%
                mutate(Race = str_split_fixed(HRace, " ", 2)[,2]) %>%
                mutate(Ethnicity = str_split_fixed(HRace, " ", 2)[,1]) %>%
                # redo the columns so it works with other functions
                mutate(Age5 = factor_ages(`Age Group`)) %>%
                mutate(value = as.numeric(value), Year = as.numeric(Year)) %>%
                rename(GEOID = `Area ID`) %>%
                # no longer need the name column
                select(-name, -`Age Group`, -HRace)
        }))
        
        if(endsWith(i, "2010.xlsx")){
            sub_df <- filter(sub_df, Year != 2010)
        }
        
        sub_df})) %>%
        arrange(Year, desc(Ethnicity), Race, Age5, Sex)
    
    if(collapse_hispanic){
        re_kc_df <- re_kc_df %>%
            mutate(Race = ifelse(Ethnicity == "Hispanic", "Hispanic", Race)) %>%
            group_by(GEOID, Year, Sex, Race, Age5) %>%
            summarize(value = sum(value)) %>%
            ungroup()
    }

    re_kc_df
}


#' required packages: tidyverse, readxl

#' Download the most recent King County population forecast from WA OFM
#' 
#' @description download the most recent population forecast data (2017 Growth
#' Management Act county projections) and clean up the data for furthur use
#' 
#' @param None
#' @return data frame with columns for Year, Sex, Age5

download_kc_age_sex_projections <- function() {
    # data source
    # Projections of the Population by Age and Sex for Growth Management
    # 2017 GMA Projections - Medium Series
    # https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/GMA/projections17/gma_2017_age_sex_med.xlsx
    
    tf <- tempfile(fileext = ".xlsx")
    "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/GMA/projections17/gma_2017_age_sex_med.xlsx" %>%
        download.file(tf, mode="wb")
    
    # read data from the .xlsx file
    DF <- read_excel(tf)
    
    # data cleaning
    
    # select the data only for King County
    kc_df <- DF %>%
        filter(DF[,1]=="King") %>%
        select(-1) # exclude the last row, which is the total for the county
    
    # remove empty columns
    kc_df <- kc_df[,-c(5,9)]
    
    
    # initialize an empty list for dataframes of different years
    kc_dfs <- list()
    
    # a list of dataframe names
    df_names <- c("kc_2010", "kc_2015", "kc_2020", "kc_2025", "kc_2030", "kc_2035", "kc_2040")
    
    
    n <- 2 # helper index variable for selecting the right columns for different years in the for loop
    
    # for loop to create a list of dataframes of different years
    for (i in 1:length(df_names)) {
        # create a dataframe with selected data for one year, excluding 'total' (last row)
        df <- kc_df[1:nrow(kc_df)-1,c(1, n, n+1)]
        # rename the column names
        colnames(df)[1:3] <- c("Age5","Female", "Male")
        #change the data frame to a desired format
        df <-  df %>%
            pivot_longer(`Female`:`Male`) %>%
            mutate(
                Year = as.integer(substr(df_names[i], 4, 7)),
                Age5 = factor_ages(`Age5`), 
                Sex = ifelse(str_detect(name, "Female"), "Female", "Male"),
                value = as.numeric(value)
            ) %>%
            select(-name)
        
        # add the data frame to the list
        kc_dfs[[df_names[i]]] <- df
        
        #update the helper variable
        n <- n + 3
    }
    
    # combine the data frames in the list as one data frame
    clean_kc_df <- bind_rows(kc_dfs)
    
    # return the clean data frame
    clean_kc_df
}


#' required packages: sf, tidycensus

#' Download the Median Household Income data from ACS
#' 
#' @description download the Median Household Income data from ACS
#' 
#' @param years required years in a list
#' @param geography_level geography of the data: "tract", "county", "state"
#' @param state state name or code for income data to pull
#' @param county county name or code for income data to pull
#' @param ... other parameters passed to get_acs
#' @return clean data frame
download_median_household_income <- function(
    years,
    geography_level,
    state = "WA",
    county = "King",
    ...
){
    
    var_selection <- c(
        Overall = "B19013_001",
        White = "B19013A_001",
        Black = "B19013B_001",
        AIAN = "B19013C_001",
        Asian = "B19013D_001",
        NHOPI = "B19013E_001",
        Other = "B19013F_001",
        'Two or More Races' = "B19013G_001"
    )
    
    df <- get_acs(
        geography_level,
        variables = var_selection,
        year = years[1],
        state = state,
        county = county,
        ...
    ) %>%
        mutate(Year=years[1]) %>%
        select(-moe)
    
    if (!is.na(years[2])) {
        for (yr in years[2:length(years)]) {
            df <- rbind(
                df,
                get_acs(
                    geography_level,
                    variables = var_selection,
                    year = yr,
                    state = state,
                    county = county,
                    ...
                ) %>%
                    mutate(Year=yr) %>%
                    select(-moe)
            )
        }
    }
    
    colnames(df)[3] = "Race"
    
    clean_df <- df %>%
        rename('Median Household Income'=estimate) %>%
        mutate(Year=as.integer(Year))
    
    clean_df
}

#' Required package: sp
#' 
#' Add a data frame of population stats to a SpatialPolygonsDataFrame
#' @description merge a data frame of populations stats to a SpatialPolygonDataFrame
#' based on GEOID
#' 
#' @param df data frame like object with a character column named "GEOID"
#' @param spdf SpatialPolygonsDataFrame like object with a character column in its" data" field named "GEOID"
#' @return SpatialPolygonsDataFrame whose "data" field added with the columns in "df"

merge_df_spdf <- function(df, spdf) {
    sp::merge(spdf, df, by = "GEOID", duplicateGeoms = TRUE)
}

#' Required package: rgdal
#' 
#' Download King County census tract data from King Conty GIS Open Data Hub
#' 
#' @description create a SpatialPolygonsDataFrame by using the given
#' data frame and the census tract geographies downloaded from King County
#' GIS Open Data API.
#' 
#' @return SpatialPolygonsDataFrame with census tract geographies

download_kc_tract <- function() {
    # Census Tract data source from King County GIS Open Data API
    # https://gis-kingcounty.opendata.arcgis.com/datasets/2010-census-tracts-for-king-county-conflated-to-parcels-major-waterbodies-erased-tracts10-shore-area?geometry=-125.956%2C46.738%2C-118.151%2C48.040
    kc_tract_spdf <- readLines("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/census___base/MapServer/887/query?where=1%3D1&outFields=GEO_ID_TRT&outSR=4326&f=json")
    kc_tract_spdf <- readOGR(kc_tract_spdf, verbose = FALSE)
    colnames(kc_tract_spdf@data)[1] <- "GEOID"
    writeOGR(kc_tract_spdf, dsn = "./data/kc_tract.json", layer = "kc_tract", driver="GeoJSON", verbose = FALSE)
    
}

download_kc_public_clinics <- function() {
    # Public Health Clinics data source from King County GIS Open Data API
    # https://gis-kingcounty.opendata.arcgis.com/datasets/public-health-clinics-ph-clinics-point?geometry=-124.257%2C47.186%2C-120.241%2C47.835
    kc_public_clinics <- readLines("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/pubsafe___base/MapServer/178/query?where=1%3D1&outFields=NAME,ADDRESS,ZIPCODE&outSR=4326&f=json")
    write(kc_public_clinics, file = "./data/kc_public_clinics.json")
}

download_kc_schools <- function() {
    # School Sites in King County / schsite point data source from King County GIS Open Data API
    # https://gis-kingcounty.opendata.arcgis.com/datasets/school-sites-in-king-county-schsite-point?geometry=-123.799%2C47.157%2C-120.017%2C47.807
    kc_schools <- readLines("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/admin___base/MapServer/107/query?where=1%3D1&outFields=CODE,NAME,ADDRESS,ZIPCODE,DISTRICT&outSR=4326&f=json")
    kc_schools <- readOGR(kc_schools, verbose = FALSE)
    kc_schools@data <- kc_schools@data %>%
        mutate(
            CODE = case_when(
                CODE==660 ~ "School - Elementary",
                CODE==661 ~ "School - Junior High or Middle",
                CODE==662 ~ "School - High",
                CODE==663 ~ "School - College or University",
                CODE==664 ~ "School - Alternative",
                CODE==665 ~ "School - Other facility",
                CODE==666 ~ "School - K thru 12"
            )
        )
    writeOGR(kc_schools, dsn = "./data/kc_schools.json", layer = "kc_schools", driver="GeoJSON", verbose = FALSE)
}

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

