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
