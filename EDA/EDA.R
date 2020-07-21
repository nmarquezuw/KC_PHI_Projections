rm(list=ls())
library(tidyverse)
library(readxl)

source("00_utils.R")
source("00a_hp_funcs.R")

# For the data acquired using this function,
# the 2010 data are the census data,
# the 2015 data are the estimates made by OFM,
# the 2020-2040 data are the projections made by OFM
OFM_data <- download_kc_age_sex_projections()

yrs <- c(2015, 2020, 2025, 2030, 2035)

# prepare the 2010 & 2015 data for making forecasts
df <- OFM_data %>%
    filter(
        Year == 2010 |
        Year == 2015
    )


# prepare the parameters for the hp_project function
df_pre <- filter(df, Year == 2010)
df_post <- filter(df, Year == 2015)
# cwr <- calc_CWR(df_post)
# ccr <- calc_CCR(df_pre, df_post)

# make projections until 2040
df <- hp_project(df_pre, df_post, years_out = 25)

# make faceted line charts by Age5
bind_rows(OFM_data, df, .id="source") %>%
    mutate(source = ifelse(source=="1", "OFM", "HP Projection")) %>%
    group_by(Year, Age5, source) %>%
    summarize(Population = sum(value)) %>%
    ggplot(aes(x=Year, y=Population, color=source, linetype=source)) +
    geom_line() +
    ylim(c(0, 200000)) +
    facet_wrap(~Age5)


# make a line chart for overall population projections
bind_rows(OFM_data, df, .id="source") %>%
    mutate(source = ifelse(source=="1", "OFM", "HP Projection")) %>%
    group_by(Year, source) %>%
    summarize(Population = sum(value)) %>%
    ggplot(aes(x=Year, y=Population, color=source, linetype=source))  +
    geom_line() +
    scale_x_continuous(breaks = c(2010,2015,2020,2025,2030,2035,2040)) +
    ylim(c(0, 3000000))


# Mean age overtime (faceted by sex)
bind_rows(OFM_data, df, .id="source") %>%
    mutate(source = ifelse(source=="1", "OFM", "HP Projection")) %>%
    calc_mean_age() %>%
    ggplot(aes(x=Year, y=mean_age, color=source, linetype=source)) +
    geom_line() +
    scale_x_continuous(breaks = c(2010,2015,2020,2025,2030,2035,2040)) +
    scale_y_continuous(limits=c(30,43), breaks = seq(0,43, by = 1)) +
    labs(y="Mean Age") +
    theme_bw() +
    facet_wrap(~Sex)

# Mean age overtime (faceted by data source)
bind_rows(OFM_data, df, .id="source") %>%
    mutate(source = ifelse(source=="1", "OFM", "HP Projection")) %>%
    calc_mean_age() %>%
    ggplot(aes(x=Year, y=mean_age, color=Sex, linetype=Sex)) +
    geom_line() +
    scale_x_continuous(breaks = c(2010,2015,2020,2025,2030,2035,2040)) +
    scale_y_continuous(limits=c(30,43), breaks = seq(0,43, by = 1)) +
    labs(y="Mean Age") +
    theme_bw() +
    facet_wrap(~source)

    
# Mean age overtime by sex (HP projection)
df %>%
    calc_mean_age() %>%
    ggplot(aes(x=Year, y=mean_age, color=Sex, linetype=Sex)) +
    geom_line() +
    scale_x_continuous(breaks = c(2010,2015,2020,2025,2030,2035,2040)) +
    scale_y_continuous(limits=c(30,42), breaks = seq(0,42, by = 1)) +
    labs(y="Mean Age") +
    theme_bw() 


# HP projection with collapsed ages by sex
collapse_ages(df) %>%
    ggplot(aes(x=Year, y=round(value), fill=forcats::fct_rev(Age), label=round(value))) +
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(breaks = c(2010,2015,2020,2025,2030,2035,2040)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    labs(fill="Age Group", y="Population") +
    theme_bw() +
    facet_wrap(~Sex)


