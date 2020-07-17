#' Check if df has all necessary components
#' 
#' @description checks columns of df for use with HP functions
#' 
#' @param DF data frame like object
#' @return None

validate_age_df <- function(DF){
    if(!is.data.frame(DF)){
        stop("Object is not a valid data frame.")
    }
    
    if(!all(c("Sex", "Age5", "value", "Year") %in% names(DF))){
        stop("Missing either Sex, Age5, Year, or value column in data frame")
    }
    
    if(length(levels(DF$Age5)) != 18){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
}

#' Calculates Cohort Change Ratios across two time periods 5 years apart
#' 
#' @description Calculates Cohort Change Ratios across two time periods 5
#' years apart
#' 
#' @param df_pre data frame like object at time t
#' @param df_post data frame like object at time t+5
#' @return data frame object with CCR calculate by age and sex
 
calc_CCR <- function(df_pre, df_post){
    lapply(list(df_pre, df_post), validate_age_df)
    if(length(unique(c(df_pre$Year, df_post$Year))) != 2){
        if((df_post$Year[1] - df_pre$Year[1]) != 5){
            stop("Year columns not properly organized.")
        }
    }
    
    lages <- levels(df_pre$Age5)
    
    left_join(
        df_pre %>%
            mutate(Agen = as.numeric(Age5) + 1) %>%
            select(-Age5, -Year) %>%
            rename(pre = value) %>%
            mutate(Agen = ifelse(Agen == 19, 18, Agen)) %>%
            group_by(Sex, Agen) %>%
            summarize(pre = sum(pre)) %>%
            ungroup(),
        df_post %>%
            mutate(Agen = as.numeric(Age5)) %>%
            select(-Age5, -Year) %>% 
            rename(post = value) %>%
            filter(Agen != 1),
        by = c("Agen", "Sex")) %>%
        mutate(CCR = post / pre) %>%
        mutate(Age5 = factor(lages[Agen-1], lages)) %>%
        mutate(Age5_post = factor(lages[Agen], lages)) %>%
        select(-Agen, -pre, -post) %>%
        bind_rows(
            filter(., Age5 == "80-84") %>%
                mutate(Age5 = Age5_post))
}

#' Calculates Child Woman Ratios within a single time period
#' 
#' @description Calculates Child Woman Ratios within a single time period
#' by sex
#' 
#' @param DF data frame like object
#' @return data frame object with CWR calculated by sex

calc_CWR <- function(DF){
    validate_age_df(DF)
    
    if(length(unique(DF$Year)) != 1){
        if(nrow(DF) != 36){
            stop("Must have exactly 36 rows (18 ages x 2 sexes x 1 year).")
        }
    }
    
    woman_count <- DF %>%
        mutate(Agen = as.numeric(Age5)) %>%
        filter(Sex == "Female" & Agen >= 4 & Agen <= 9) %>%
        pull(value) %>%
        sum()
    
    DF %>%
        filter(Age5 == "0-4") %>%
        mutate(CWR = value/ woman_count) %>%
        select(Sex, CWR)
}

#' Produces forecasts using the Hamilton-Perry Method
#' 
#' @description Produces 5 year forecasts out to specified year
#' 
#' @param df_pre data frame like object at time t
#' @param df_post data frame like object at time t+5
#' @param years_out int how many years out to forecast from t+5
#' @param cwr_df df precalculated CWR values
#' @param ccr_df df precalculated CCR values
#' @return data frame object with projected values

hp_project <- function(
    df_pre, df_post, years_out = 20, cwr_df = NULL, ccr_df = NULL){
    if((years_out %% 5) != 0){
        stop("Years out must be divisible by 5")
    }
    
    forecast_years <- seq(
        df_post$Year[1] + 5, df_post$Year[1] + years_out, by = 5)
    if(is.null(cwr_df)){
        cwr_df <- calc_CWR(df_post)
    }
    if(is.null(ccr_df)){
        ccr_df <- calc_CCR(df_pre, df_post)
    }
    
    final_df <- bind_rows(df_pre, df_post)
    
    for(y in forecast_years){
        sub_tp5_df <- final_df %>%
            filter(Year == (y - 5)) %>%
            left_join(ccr_df, by = c("Sex", "Age5")) %>%
            mutate(value = value * CCR) %>%
            mutate(Age5 = Age5_post) %>%
            select(-Age5_post, -CCR) %>%
            mutate(Year = Year + 5) %>%
            group_by_at(vars(-value)) %>%
            summarize(value = sum(value)) %>%
            ungroup()
        
        woman_count <- sub_tp5_df %>%
            mutate(Agen = as.numeric(Age5)) %>%
            filter(Sex == "Female" & Agen >= 4 & Agen <= 9) %>%
            pull(value) %>%
            sum()
        
        tp5_df <- sub_tp5_df %>%
            # this way we retain all original columns
            filter(Age5 == "5-9") %>%
            mutate(Age5 = factor(rep("0-4", 2), levels(.$Age5))) %>%
            left_join(cwr_df, by = "Sex") %>%
            mutate(value = woman_count * CWR) %>%
            select(-CWR) %>%
            bind_rows(sub_tp5_df)
        
        final_df <- bind_rows(final_df, tp5_df)
    }
    
    return(arrange(final_df, Year, Sex, Age5))
}
