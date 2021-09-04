
## Load libraries ----

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinySearchbar)
library(dipsaus)
library(WDI)
library(dplyr)
library(RColorBrewer)
library(tools)
library(readxl)
library(haven)
library(tidyr)
library(bsplus)
library(DT)
library(summarytools)
library(reshape2)
library(ExPanDaR)
library(zip)
library(purrr)
library(ggplot2)
library(scales)
library(rlist)
library(grid)
library(gridExtra)
library(gtable)
library(extrafont)
library(Jmisc)
library(ggrepel)
library(ggh4x)
library(forcats)
library(lattice)
library(zoo)
library(rasterVis)
library(stringr)
library(conflicted)
library(vctrs)
library(bslib)

print("*******************************")
print("**RUNNING CODE OUTSIDE SERVER**")
print("*******************************")
cat("\n")

# Solve conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("renderDataTable", "DT")
conflict_prefer("levelplot", "rasterVis")
conflict_prefer("zip", "zip")

## Fix variables ----

# Load dataframes from WDI package to get countries
ctry_df <- as.data.frame(WDI_data$country)
ctry_vec <- sort(ctry_df[ctry_df[ , "region"] != "Aggregates", "country"])
reg_vec <- sort(ctry_df[ctry_df[ , "region"] == "Aggregates", "country"])

# Create dataframe with flags
flags_df <- filter(ctry_df, !region %in% c("Aggregates")) %>% select(country, iso2c, iso3c)
flags_df$URL <- paste( "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", tolower(flags_df$iso2c), ".svg", sep = "")
flags_df$URL[flags_df$iso2c == "JG"] <- "https://upload.wikimedia.org/wikipedia/commons/f/f5/Flag_of_the_Channel_Islands_1.png"

# Load dataframes from WDI package to get variables
var_df <- as.data.frame(WDI_data$series)
var_df$name_indicator <- paste(var_df$name, var_df$indicator, sep = " | ")
wdi_vars_aux <- c("GDP growth (annual %) | NY.GDP.MKTP.KD.ZG",
        "GDP per capita, PPP (constant 2017 international $) | NY.GDP.PCAP.PP.KD",
        "Inflation, consumer prices (annual %) | FP.CPI.TOTL.ZG",
        "Population, total | SP.POP.TOTL",
        "Exports of goods and services (% of GDP) | NE.EXP.GNFS.ZS",
        "Imports of goods and services (% of GDP) | NE.IMP.GNFS.ZS")
var_df <- var_df %>% arrange(factor(name_indicator, levels = wdi_vars_aux))
var_vec <- var_df$name_indicator

# Country lists
ctry_df_aux <- ctry_df[ctry_df[ , "region"] != "Aggregates", ]
inc_group_list <- unique(ctry_df_aux$income)
reg_group_list <- unique(ctry_df_aux$region)

inc_group_list <- factor(as.character(inc_group_list), levels=c("Low income", "Lower middle income", "Upper middle income", "High income"))
inc_group_list <- as.character(sort(inc_group_list))
inc_group_list_name <- paste0("Income: ", inc_group_list)
inc_group_list <- as.list(inc_group_list)
names(inc_group_list) <- inc_group_list_name

reg_group_list <- sort(reg_group_list)
reg_group_list_name <- paste0("Region: ", reg_group_list)
reg_group_list <- as.list(reg_group_list)
names(reg_group_list) <- reg_group_list_name

group_list <- c(inc_group_list, reg_group_list)

# Color palettes vector
palette_vec <- row.names(filter(brewer.pal.info, colorblind == TRUE))

# Font list
font_list <- list(Calibri = "Calibri", Arial = "sans", Courier = "mono", TimesNewRoman = "serif")

# Positions list
position_list <- list(Top = "top", Bottom = "bottom", Right = "right", Left = "left")

# Statistics vector
stats_vec <- c("Average", "Median", "Maximum", "Minimum", "Standard deviation", "Most recent value", "None (raw annual data)")

# Units transformation zeros
units_zeros <- c("Trillions", "Billions", "Millions", "Thousands")

# Empty title vectors
title_text <- character(0)
subtitle_text <- character(0)

# Empty dataframes
empty_data_df <- data.frame(
        Var_name = character(0),
        Var_code = character(0),
        Units = character(0),
        Country = character(0),
        Ctry_iso = character(0),
        Ctry_group = character(0),
        Ctry_group_num = numeric(length = 0L),
        Year = numeric(length = 0L),
        Period = character(0),
        Period_num = numeric(length = 0L),
        Value = numeric(length = 0L),
        Database = character(0),
        Source = character(0))

empty_metadata_df <- data.frame(
        Var_name = character(0),
        Var_code = character(0),
        Description = character(0),
        Units = character(0),
        Source_data = character(0),
        Source_org = character(0),
        Database = character(0),
        Source = character(0))

# Functions

# Load slider function
MYsliderInput <- function(inputId, label, min, max, value, step=NULL, sep = "", from_min, from_max){
        
        x <- sliderInput(inputId = inputId, 
                label = label, 
                min = min, 
                max = max,
                sep = "" , 
                value = value, 
                step = step)
        
        x$children[[2]]$attribs <- c(x$children[[2]]$attribs,
                "data-from-min" = as.character(from_min),
                "data-from-max" = as.character(from_max),
                "data-from-shadow" = 'true',
                "data-type" = "single",
                "data-data-type" = "number")

        return(x)
}


# Max and min functions when all values are missing
my_max_fun <- function(x) ifelse( !all(is.na(x)), max(x, na.rm = TRUE), NA)
my_min_fun <- function(x) ifelse( !all(is.na(x)), min(x, na.rm = TRUE), NA)

empty_panel_fun <-  function(df, country_vec, year_vec){
        
        # variable_df: dataframe containing Var_name (column 1) and units (column 2)
        # country_vec: vector containing country_iso (3 digits)
        # year_vec: vector containing years
        var_headers <- names(df) %in% c("Var_name", "Var_code", "Units", "Database")

        proc_var <- df[ , var_headers]
        proc_var <- proc_var[!duplicated(proc_var), ]
        proc_var <- proc_var[order(proc_var[ , "Var_name"], proc_var[ , "Units"] ), ]
        proc_var_names <- names(proc_var)
        
        num_var <- nrow(proc_var)
        num_ctry <- length(unique(country_vec))
        num_year <- length(unique(year_vec))
        
        print(paste0("Unique variable-units: ", num_var))
        print(proc_var)
        
        proc_var <- data.frame(proc_var[rep(seq_len(nrow(proc_var)), each = num_ctry * num_year), ])
        colnames(proc_var) <- proc_var_names
      
        proc_ctry <- data.frame(sort(unique(country_vec)))
        colnames(proc_ctry)[1] <- "Ctry_iso"
        
        print(paste0("Unique countries: ", num_ctry))
        
        proc_ctry <- data.frame(proc_ctry[rep(seq_len(nrow(proc_ctry)), each = num_year), ])
        proc_ctry <- do.call("rbind", replicate(num_var, proc_ctry, simplify = FALSE))
        colnames(proc_ctry)[1] <- "Ctry_iso"
        
        proc_year <- data.frame(sort(unique(year_vec)))
        colnames(proc_year)[1] <- "Year"

        print(paste0("Unique years: ", num_year))
        print(paste0(year_vec[1], " - ", year_vec[num_year]))
        
        proc_year <- do.call("rbind", replicate(num_var * num_ctry, proc_year, simplify = FALSE))

        proc <- data.frame(proc_var, proc_ctry, proc_year)
        print(paste0("Empty dataframe ready to merge with external data. Number of rows in empty dataframe: ", nrow(proc)))
        
        return(proc)
        
}

# Function "nice_wdi_fun": Downloads WDI data and creates "nice" dataframe
nice_wdi_fun <- function(countries_ctry, 
        countries_str, 
        countries_asp, 
        countries_reg,
        countries_group = NULL,
        countries_all = FALSE,
        regions_all = FALSE,
        variables, 
        start_year, 
        end_year){
        
        # Print messages indicating selected parameters
        print("*****Starting WDI download process*****")
        print("(1/3) Number of selected countries/aggregates:")
        
        if(countries_all == TRUE & regions_all == TRUE){
                countries <- c(ctry_vec, reg_vec)
                ctries_select_iso2 <- c("all")
        }
        
        if(countries_all == FALSE & regions_all == FALSE){
                countries <- c(countries_ctry, countries_str, countries_asp, countries_reg)
                ctries_select_iso2 <- ctry_df[ctry_df[ , "country"] %in% c(countries_ctry, countries_str, countries_asp, countries_reg), "iso2c"]
        }
        
        if(countries_all == TRUE & regions_all == FALSE){
                countries <- c(ctry_vec, countries_reg)
                ctries_select_iso2 <- ctry_df[ctry_df[ , "country"] %in% c(ctry_vec, countries_reg), "iso2c"]
        }
        
        if(countries_all == FALSE & regions_all == TRUE){
                countries <- c(countries_ctry, countries_str, countries_asp, reg_vec)
                # For some reason I can't understand, the following does not work (WDI function does not download the data). I set it to "all" and then filter after data is downloaded
                # ctries_select_iso2 <- ctry_df[ctry_df[ , "country"] %in% c(countries_ctry, countries_str, countries_asp, reg_vec), "iso2c"]
                ctries_select_iso2 <- c("all")
        }
        
        # Add groups
        if(length(countries_group) > 0){
                group_selection <- ctry_df[ctry_df[ , "income"] %in% countries_group | ctry_df[ , "region"] %in% countries_group, "country"]
                add <- setdiff(group_selection, countries)
                countries <- c(countries, add)

                
                # For the same reason stated above I set it to "all" and then filter after data is downloaded
                ctries_select_iso2 <- c("all")
        }
        
        print(length(countries))
        print("(2/3) Selected variables (code):")
        print(variables)
        print("(3/3) Year range:")
        print(paste0(start_year, " - ", end_year))
        
        if((length(ctries_select_iso2) == 0 & countries_all == FALSE & regions_all == FALSE) | length(variables) == 0){
                print("A WDI download parameter (countries or variable) is missing: no WDI data download")
                return(empty_data_df)
        }
        
        # Download data from WDI
        raw_wdi <- WDI(
                country = ctries_select_iso2,
                indicator = variables,
                start = start_year,
                end = end_year,
                extra = FALSE,
                cache = NULL,
                latest = NULL,
                language = "en")
        
        # Filter in case of regions_all selected but not ctries_all
        if(!(countries_all == TRUE & regions_all == TRUE)){
                raw_wdi <- raw_wdi[raw_wdi$country %in% countries, ]
        }
        
        
        
        # Store which variables were downloaded and which weren't
        vars_downloaded <- variables[variables %in% names(raw_wdi)]
        print(paste0(c("Variables successfully downloaded: ", variables)))
        vars_not_downloaded <- variables[!variables %in% names(raw_wdi)]
        if(length(vars_not_downloaded)>0){
                print(paste0(c("Variables not downloaded: ", vars_not_downloaded)))
        }

        
        # Return if download is empty
        if(class(raw_wdi)=="NULL") {
                print("No data was downloaded for the selected countries/years/variables")
                return(empty_data_df)
        }
        
        # Create "empty" dataframe (only country and year)
        num_years <- end_year - start_year + 1
        proc_wdi <- rep(countries, each = num_years)
        proc_wdi <- data.frame(proc_wdi)
        names(proc_wdi)[names(proc_wdi) == "proc_wdi"] <- "country"
        proc_wdi$year <- start_year:end_year
        
        # Complete "empty" dataframe with downloaded data
        for (i in vars_downloaded){
                aux <- raw_wdi[ , c("country", "year", i)]
                aux <- aux %>% drop_na(all_of(i))
                proc_wdi <- merge(
                        x = proc_wdi,
                        y = aux,
                        by = c("country", "year"),
                        all.x = TRUE
                )
        }
        
        # Set data to long format
        proc_wdi <- proc_wdi %>% 
                gather(key = "Var_code", 
                        value = "Value",
                        all_of(vars_downloaded))
        
        # Include iso3c code
        proc_wdi <- merge(
                x = proc_wdi, 
                y = ctry_df[ , c("country", "iso3c")], 
                by = c("country")
        )
        
        # Include variable name
        proc_wdi <- merge(x = proc_wdi, 
                y = var_df[ ,c("name", "indicator")], 
                by.x = c("Var_code"),
                by.y = c("indicator"))
        
        # Include country group column 
        #(Analized/Structural/Aspirational)
        proc_wdi$Ctry_group <- ifelse(
                proc_wdi$country %in% countries_ctry , "Analized",
                ifelse(proc_wdi$country %in% countries_str, "Structural",
                        ifelse(proc_wdi$country %in% countries_asp, "Aspirational",
                                ifelse(proc_wdi$country %in% countries_reg, "Aggregate",
                                        ifelse((!proc_wdi$country %in% countries_reg) & (proc_wdi$country %in% reg_vec),"Rest (aggregate)" , "Rest")
                                )
                        )
                )
        )
        
        # Create variable used for sorting by country group
        proc_wdi$Ctry_group_num <- ifelse(
                proc_wdi$country %in% countries_ctry, 1,
                ifelse(proc_wdi$country %in% countries_str, 2,
                        ifelse(proc_wdi$country %in% countries_asp, 3,
                                ifelse(proc_wdi$country %in% countries_reg, 4,
                                        ifelse((!proc_wdi$country %in% countries_reg) & (proc_wdi$country %in% reg_vec), 6, 5)
                                )
                        )
                )
        )
        
        # Generate "Units" variable (whatever is 
        # between brackets in Name will be taken as units)
        proc_wdi <- proc_wdi %>% separate(
                name,  
                c(NA, "Units"), 
                sep = "([\\(\\)])", 
                remove = FALSE,
                extra = "drop",
                fill = "right"
        )
        proc_wdi$Units <- lapply(proc_wdi$Units , function(x) {
                if(!is.na(x)){gsub("%", "percent", x)}
        })
        capFirst <- function(s) {
                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
        }
        proc_wdi$Units <- capFirst(proc_wdi$Units)
        proc_wdi$Units[proc_wdi$Units == "NULL"] <- NA
        
        # Rename columns
        names(proc_wdi)[names(proc_wdi) == "name"] <- "Var_name"
        names(proc_wdi)[names(proc_wdi) == "iso3c"] <- "Ctry_iso"
        names(proc_wdi)[names(proc_wdi) == "country"] <- "Country"
        names(proc_wdi)[names(proc_wdi) == "year"] <- "Year"
        
        # Add variable indicating that data was downloaded from DWI
        proc_wdi$Database <- "WDI"
        proc_wdi$Source <- "WDI"
        
        # Add empty variables
        proc_wdi$Period <- as.character(NA)
        proc_wdi$Period_num <- as.numeric(NA)
        
        # Make sure Year and Value variables are numeric
        proc_wdi <- transform(proc_wdi, Year = as.numeric(Year), Value = as.numeric(Value))
        
        # Order columns and sort 
        proc_wdi <- proc_wdi %>% select(
                "Var_name",
                "Var_code",
                "Units",
                "Country",
                "Ctry_iso",
                "Ctry_group",
                "Ctry_group_num",
                "Year",
                "Period",
                "Period_num",
                "Value",
                "Database",
                everything()
        )
        
        proc_wdi <- proc_wdi[order(
                proc_wdi$Var_name,
                proc_wdi$Var_code,
                proc_wdi$Ctry_group_num,
                proc_wdi$Ctry_iso,
                proc_wdi$Year), ]
        
        # Store data in reactiveValues
        print("*****WDI data download was successful*****")
        cat("\n")
        return(proc_wdi)
}

#Function "nice_wdi_fun_meta": creates nice WDI metadata dataframe

nice_wdi_fun_meta <- function(dataframe){
        
        var_headers <- names(dataframe) %in% c("Var_name", "Var_code", "Units", "Database", "Source")
        
        proc <- dataframe[ , var_headers]
        proc <- proc[!duplicated(proc), ]

        # Merge data for WDI variables
        if(!plyr::empty(dataframe)){
                
                proc_wdi <- proc[proc$Source == "WDI", ]  
                
                metadata_table_wdi <- var_df
                
                names(metadata_table_wdi)[names(metadata_table_wdi)==
                                "indicator"]<- "Var_code"
                names(metadata_table_wdi)[names(metadata_table_wdi)==
                                "name"]<- "Var_name"
                names(metadata_table_wdi)[names(metadata_table_wdi)==
                                "description"] <- "Description"
                names(metadata_table_wdi)[names(metadata_table_wdi)==
                                "sourceDatabase"]<- "Source_data"
                names(metadata_table_wdi)[names(metadata_table_wdi)==
                                "sourceOrganization"]<- "Source_org" 
                
                proc_wdi <- merge(
                        x = proc_wdi,
                        y = metadata_table_wdi[ , c("Var_name", "Var_code", "Description", "Source_data", "Source_org")],
                        by = c("Var_name", "Var_code"),
                        all.x = TRUE)
                
                proc_wdi <- proc_wdi %>% select("Var_name",
                        "Var_code",
                        "Description",
                        "Units",
                        "Source_data",
                        "Source_org",
                        "Database",
                        "Source"
                )
                return(proc_wdi)
        }else{
                
                empty_metadata_df <- data.frame(
                        Var_name = character(0),
                        Var_code = character(0),
                        Description = character(0),
                        Units = character(0),
                        Source_data = character(0),
                        Source_org = character(0),
                        Database = character(0),
                        Source = character(0))
                return(empty_metadata_df)
        }
}

# Prepare data for dynamic graphs
prep_data_for_graphs <- function(
        df,
        vars,
        ctries,
        t_start,                              
        t_end){
        if(!is.null(vars)){
                datfra <- data.frame(vars)
                colnames(datfra)[1] <- "VARS"
                datfra <- separate(data = datfra, 
                        col = VARS,
                        into = c("A", "Var_code"), 
                        sep  =  " \\| ",
                        extra = "merge",
                        fill = "right")
                plot_input <- df %>%
                        as_tibble() %>%
                        filter(Country %in% ctries & 
                                        Var_code %in% datfra$Var_code &
                                        Year >= t_start &
                                        Year <= t_end) %>%
                        group_split(Var_name)
                
                return(plot_input)
        }
}

# Create dataframe with subperiod divisions and lines 
subper_rectangles_fun <- function(
        subper_num,
        time_range,
        time_lim_1,
        time_lim_2,
        time_lim_3,
        time_name_1,
        time_name_2,
        time_name_3,
        time_name_4
){
        
        # If 2 subperiods, then there are 3 cases
        if(subper_num == 2){
                
                # Case 1
                if(time_range[1] < time_lim_1 & time_range[2] > time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2)
                        )
                }
                # Case 2
                if(time_range[2] <= time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_1)
                        )
                }
                # Case 3
                if(time_range[1] >= time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_2)
                        )
                }
        }
        # If 3 subperiods, then there are 6 cases
        if(subper_num == 3){
                
                # Case 1
                if(time_range[1] < time_lim_1 & time_range[2] > time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5, time_lim_2 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_lim_2 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2, time_name_3)
                        )
                }
                # Case 2
                if(time_range[1] < time_lim_1 & time_range[2] <= time_lim_2 & time_range[2] > time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2)
                        )
                }
                # Case 3
                if(time_range[1] >= time_lim_1 & time_range[1] < time_lim_2 & time_range[2] > time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_2 + 0.5),
                                Year_end = c(time_lim_2 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_2, time_name_3)
                        )
                }
                # Case 4
                if(time_range[2] <= time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_1)
                        )
                }
                # Case 5
                if(time_range[1] >= time_lim_1 & time_range[2] <= time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_2)
                        )
                }
                # Case 6
                if(time_range[1] >= time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_3)
                        )
                }
        }
        # If 4 subperiods, then there are 10 cases
        if(subper_num == 4){
                # Case 1
                if(time_range[1] < time_lim_1 & time_range[2] > time_lim_3){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5, time_lim_2 + 0.5, time_lim_3 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_lim_2 + 0.5, time_lim_3 + 0.5,time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2, time_name_3, time_name_4)
                        )
                }
                # Case 2
                if(time_range[1] < time_lim_1 & time_range[2] <= time_lim_3 & time_range[2] > time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5, time_lim_2 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_lim_2 + 0.5,time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2, time_name_3)
                        )
                }
                # Case 3
                if(time_range[1] >= time_lim_1 & time_range[1] < time_lim_2 & time_range[2] > time_lim_3){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_2 + 0.5, time_lim_3 + 0.5),
                                Year_end = c(time_lim_2 + 0.5, time_lim_3 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_2, time_name_3, time_name_4)
                        )
                }
                # Case 4
                if(time_range[1] < time_lim_1 & time_range[2] <= time_lim_2 & time_range[2] > time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_1 + 0.5),
                                Year_end = c(time_lim_1 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_1, time_name_2)
                        )
                }
                # Case 5
                if(time_range[1] >= time_lim_1 & time_range[1] < time_lim_2 & time_range[2] <= time_lim_3 & time_range[2] > time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_2 + 0.5),
                                Year_end = c(time_lim_2 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_2, time_name_3)
                        )
                }
                # Case 6
                if(time_range[1] >= time_lim_2 & time_range[1] < time_lim_3 & time_range[2] > time_lim_3){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5, time_lim_3 + 0.5),
                                Year_end = c(time_lim_3 + 0.5, time_range[2] + 0.5),
                                Period = c(time_name_3, time_name_4)
                        )
                }
                # Case 7
                if(time_range[2] <= time_lim_1){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_1)
                        )
                }
                # Case 8
                if(time_range[1] >= time_lim_1 & time_range[2] <= time_lim_2){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_2)
                        )
                }
                # Case 9
                if(time_range[1] >= time_lim_2 & time_range[2] <= time_lim_3){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_3)
                        )
                }
                # Case 10
                if(time_range[1] >= time_lim_3){
                        rectangle_text <- data.frame(
                                Year_start = c(time_range[1] - 0.5),
                                Year_end = c(time_range[2] + 0.5),
                                Period = c(time_name_4)
                        )
                }
        }
        
        vertical_lines <- rectangle_text$Year_end
        vertical_lines <- vertical_lines[1:(length(vertical_lines) - 1)]
        
        return(list(rect = rectangle_text, vert = vertical_lines))
}

# Default data input parameters
def_list_data <- list(
        # Pakistan
        ctries_ctry = "Pakistan",
        ctries_str = c("Bangladesh", "Egypt, Arab Rep.", "Ethiopia", "India"),
        ctries_asp = c("Indonesia", "Mexico", "Turkey", "Vietnam"),
        # Bhutan
        # ctries_ctry = "Bhutan",
        # ctries_str = c("Benin", "Bolivia", "Kyrgyz Republic", "Lao PDR"),
        # ctries_asp = c("Lesotho", "Mongolia", "Eswatini", "Tajikistan"),
        ctries_reg = "South Asia",
        ctries_group = "Lower middle income",
        ctries_all = FALSE,
        regs_all = FALSE,
        time_range_start = 2000,
        time_range_end = 2020,
        time_subper = TRUE,
        time_subper_num = 3,
        time_limit_1 = as.integer(2008),
        time_limit_2 = as.integer(2014),
        time_limit_3 = as.integer(2018),
        time_name_1 = "Historical",
        time_name_2 = "Recent past",
        time_name_3 = "Present",
        time_name_4 = "Period 4",
        wdi_vars = wdi_vars_aux,
        # imf_vars = NULL,
        update_button_show = FALSE,
        
        # Fixed
        time_range_min = 1960,
        time_range_max = 2030,
        col_spinner = "#009FDA"
)

# Initial WDI dataframe
ctries_select <- c(def_list_data$ctries_ctry ,
        def_list_data$ctries_str,
        def_list_data$ctries_asp,
        def_list_data$ctries_reg)
ctries_select_iso2 <- ctry_df[ctry_df[ , "country"] %in% ctries_select, "iso2c"]
datfra <- data.frame(def_list_data$wdi_vars)
datfra <- separate(data = datfra, 
        col = def_list_data.wdi_vars,
        into = c("A", "Var_code"), 
        sep  =  " \\| ",
        extra = "merge",
        fill = "right")
variables <- sort(datfra[ ,2])

print("***Download initial WDI data***")
initial_wdi_df <- nice_wdi_fun(
        countries_ctry = def_list_data$ctries_ctry,
        countries_str = def_list_data$ctries_str,
        countries_asp = def_list_data$ctries_asp,
        countries_reg = def_list_data$ctries_reg,
        countries_group = def_list_data$ctries_group,
        countries_all = def_list_data$ctries_all,
        regions_all = def_list_data$regs_all,
        variables = variables,
        start_year = def_list_data$time_range_start, 
        end_year = def_list_data$time_range_end)

initial_wdi_df_meta <- nice_wdi_fun_meta(initial_wdi_df)

# Small country list
print("***Download population 2019 data for small country filter***")
small_country_filter_df <- nice_wdi_fun(
        countries_ctry = "",
        countries_str = "",
        countries_asp = "",
        countries_reg = "",
        countries_group = NULL,
        countries_all = TRUE,
        regions_all = FALSE,
        variables = c("SP.POP.TOTL"),
        start_year = 2019, 
        end_year = 2019)

small_country_filter_df <- small_country_filter_df[small_country_filter_df$Value >= 1000000 & (!is.na(small_country_filter_df$Value)), c("Country", "Ctry_iso")]
rownames(small_country_filter_df) <- 1:nrow(small_country_filter_df)

# Default bar plot parameters

def_list_bar <- list(
        
        # Plot parameters
        vars = def_list_data$wdi_vars,
        ctries_ctry = def_list_data$ctries_ctry,
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        
        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = TRUE,
        time_subper = TRUE,
        time_subper_avg = TRUE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        digits = 1,
        digits_y = 0,
        color = "#EC7063",
        font = "Calibri"
        
)

def_list_mult <- list(
        
        # Plot parameters
        ctries = c(def_list_data$ctries_ctry , def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg),
        vars = def_list_data$wdi_vars,
        stat = stats_vec[1],
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        
        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = TRUE,
        time_subper = TRUE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        ctry_short = FALSE,
        digits = 1,
        digits_y = 0,
        font = "Calibri",
        color_pal = "Oranges",
        color = "#EC7063",
        legend_pos = position_list

)

def_list_stas <- list(
        
        # Plot parameters
        ctries_ctry = def_list_data$ctries_ctry,
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        
        # Individual plot parameters
        plot_num = 1,
        vars = c("Exports of goods and services (% of GDP) | NE.EXP.GNFS.ZS",
                "Imports of goods and services (% of GDP) | NE.IMP.GNFS.ZS"),
        title_text = "Trade",
        yaxis_text = "Percent of GDP",
        stacked_100 = FALSE,

        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = TRUE,
        time_subper = TRUE,
        time_subper_avg = FALSE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        digits = 1,
        digits_y = 0,
        color = "RdPu",
        font = "Calibri",
        legend_pos = "right"

)

def_list_stam <- list(
        
        # Plot parameters
        ctries = c(def_list_data$ctries_ctry , def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg),
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        stat = stats_vec[1],
        
        # Individual plot parameters
        plot_num = 1,
        vars = c("Exports of goods and services (% of GDP) | NE.EXP.GNFS.ZS",
                "Imports of goods and services (% of GDP) | NE.IMP.GNFS.ZS"),
        title_text = "Trade",
        yaxis_text = "Percent of GDP", 
        stacked_100 = FALSE,
        
        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = TRUE,
        time_subper = TRUE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        ctry_short = FALSE,
        digits = 0,
        digits_y = 0,
        color = "Set2",
        font = "Calibri",
        legend_pos = "right"

)

def_list_line <- list(
        
        # Plot parameters
        ctries = c(def_list_data$ctries_ctry, def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg),
        vars = def_list_data$wdi_vars,
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        
        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = FALSE,
        time_subper = TRUE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        ctry_short = FALSE,
        digits = 1,
        digits_y = 0,
        font = "Calibri",
        highlight = FALSE,
        highlight_ctry = def_list_data$ctries_ctry,
        highlight_color = "#B02307"

)

def_list_linm <- list(
        
        # Plot parameters
        ctries = c(def_list_data$ctries_ctry , def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg)[c(1,10)],
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),

        # Individual plot parameters
        plot_num = 1,
        vars = c("Exports of goods and services (% of GDP) | NE.EXP.GNFS.ZS",
                "Imports of goods and services (% of GDP) | NE.IMP.GNFS.ZS"),
        title_text = "Trade",
        yaxis_text = "Percent of GDP", 

        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = FALSE,
        time_subper = TRUE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        ctry_short = FALSE,
        digits = 0,
        digits_y = 0,
        font = "Calibri",
        legend_pos = "right"
        
)

def_list_rnkt <- list(
        
        # Plot parameters
        ctries = c(def_list_data$ctries_ctry, def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg),
        vars = c("GDP per capita, PPP (constant 2017 international $) | NY.GDP.PCAP.PP.KD",
                "Population, total | SP.POP.TOTL",
                "Exports of goods and services (% of GDP) | NE.EXP.GNFS.ZS",
                "Imports of goods and services (% of GDP) | NE.IMP.GNFS.ZS"),
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        incomplete = TRUE,
        descending = TRUE,
        
        # Display parameters
        ctries_disp = c(def_list_data$ctries_ctry, def_list_data$ctries_str , def_list_data$ctries_asp , def_list_data$ctries_reg),
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        time_subper = TRUE,
        ctry_short = FALSE,
        font = "Calibri",
        highlight = FALSE,
        highlight_ctry = def_list_data$ctries_ctry,
        highlight_color = "#B02307"
        
)

rnks_aux_ctries <- unique(initial_wdi_df$Country)

def_list_rnks <- list(
        
        # Plot parameters
        ctries = rnks_aux_ctries,
        vars = def_list_data$wdi_vars,
        stat = stats_vec[1],
        time_range = c(def_list_data$time_range_start, def_list_data$time_range_end),
        
        # Display parameters
        title = TRUE,
        yaxis = TRUE,
        source = TRUE,
        data_labels = TRUE,
        horizontal = FALSE,
        transform_zeros = TRUE,
        transform_log = FALSE,
        ctry_short = FALSE,
        digits = 1,
        digits_y = 0,
        font = "Calibri",
        color = "#EC7063",
        highlight = TRUE,
        highlight_ctry = def_list_data$ctries_ctry
)


scat_aux_ctries <- unique(initial_wdi_df$Country)
# scat_aux_ctries <- scat_aux_ctries[as.logical(!scat_aux_ctries %in% c("South Sudan", "Libya", "Equatorial Guinea", "Iraq"))]
scat_aux_ctries <- scat_aux_ctries[as.logical(scat_aux_ctries %in% small_country_filter_df$Country)]

def_list_scat <- list(
        
        # Plot parameters
        ctries = scat_aux_ctries,
        ctries_small = TRUE,
        # ctries_excluded = c("South Sudan", "Libya", "Equatorial Guinea", "Iraq"),
        x_var = wdi_vars_aux[2],

        # Individual plot parameters
        plot_num = 1,
        stat_x = stats_vec[1],            # Average
        stat_y = stats_vec[1],            # Average

        # Display parameters
        title_text = "Exports and per capita income",
        title = TRUE,
        xaxis = TRUE,
        yaxis = TRUE,
        source = TRUE,
        note = FALSE,
        transform_zeros = FALSE,
        transform_log = FALSE,
        
        scat_regline = TRUE,
        scat_regline_ci = FALSE,
        scat_45line = FALSE,
        scat_quad_avg = FALSE,
        scat_quad_med = FALSE,
        
        ctry_short = TRUE,
        digits_x = 0,
        digits_y = 0,
        font = "Calibri",
        highlight = FALSE,
        highlight_ctry = def_list_data$ctries_ctry,
        highlight_color = "#B02307",
        highlight_group = TRUE

)

# Clear input parameters
reset_list <- list(
        ctries_ctry = character(0),
        ctries_str = character(0),
        ctries_asp = character(0),
        ctries_reg = character(0),
        ctries_group = NULL,
        ctries_all = FALSE,
        regs_all = FALSE,
        ctries_select = character(0),
        ctries_select_iso2 = character(0),
        time_range_start = 2000,
        time_range_end = 2020,
        time_subper = FALSE,
        time_subper_num = 2,
        time_limit_1 = as.integer(2008),
        time_limit_2 = as.integer(2014),
        time_limit_3 = as.integer(2018),
        time_name_1 = "Period 1",
        time_name_2 = "Period 2",
        time_name_3 = "Period 3",
        time_name_4 = "Period 4",
        wdi_vars = character(0),
        wdi_vars_code = character(0),
        # imf_vars = character(0),
        update_button_show = FALSE
)

# Add period and period number variables function
add_periods_fun <- function(dataframe,
        time_range_start = def_list_data$time_range_start,
        time_range_end = def_list_data$time_range_end,
        time_subper_num = def_list_data$time_subper_num,
        time_limit_1 = def_list_data$time_limit_1,
        time_limit_2 = def_list_data$time_limit_2,
        time_limit_3 = def_list_data$time_limit_3,
        time_name_1 = def_list_data$time_name_1,
        time_name_2 = def_list_data$time_name_2,
        time_name_3 = def_list_data$time_name_3,
        time_name_4 = def_list_data$time_name_4
        ){
        
        if(time_range_end-time_range_start >= 2){
                if(time_subper_num == 2){
                        dataframe$Period[dataframe$Year <= time_limit_1] <- time_name_1
                        dataframe$Period_num[dataframe$Year <= time_limit_1] <- 1
                        dataframe$Period[dataframe$Year > time_limit_1] <- time_name_2
                        dataframe$Period_num[dataframe$Year > time_limit_1] <- 2
                }
                if(time_subper_num == 3){
                        dataframe$Period[dataframe$Year <= time_limit_1] <- time_name_1
                        dataframe$Period_num[dataframe$Year <= time_limit_1] <- 1
                        dataframe$Period[dataframe$Year > time_limit_1 & 
                                        dataframe$Year <= time_limit_2] <- time_name_2
                        dataframe$Period_num[dataframe$Year > time_limit_1 & 
                                        dataframe$Year <= time_limit_2] <- 2
                        dataframe$Period[dataframe$Year > time_limit_2] <- time_name_3
                        dataframe$Period_num[dataframe$Year > time_limit_2] <- 3
                }
                if(time_subper_num == 4){
                        dataframe$Period[dataframe$Year <= time_limit_1] <- time_name_1
                        dataframe$Period_num[dataframe$Year <= time_limit_1] <- 1
                        dataframe$Period[dataframe$Year > time_limit_1 & 
                                        dataframe$Year <= time_limit_2] <- time_name_2
                        dataframe$Period_num[dataframe$Year > time_limit_1 & 
                                        dataframe$Year <= time_limit_2] <- 2
                        dataframe$Period[dataframe$Year > time_limit_2 & 
                                        dataframe$Year <= time_limit_3] <- time_name_3
                        dataframe$Period_num[dataframe$Year > time_limit_2 & 
                                        dataframe$Year <= time_limit_3] <- 3
                        dataframe$Period[dataframe$Year > time_limit_3 ] <- time_name_4
                        dataframe$Period_num[dataframe$Year > time_limit_3 ] <- 4
                }
        }
        
        return(dataframe)
}

initial_wdi_df <- add_periods_fun(dataframe = initial_wdi_df)

# Transform text from one line to multiple lines
multiline_text_fun <- function(
        text_vector,
        number_characters){
        
        for (i in 1:length(text_vector)){
                if(nchar(text_vector[i])>number_characters){
                        character_count <- 0
                        new_legend <- ""
                        legend_aux <- unlist(strsplit(text_vector[i], " "))
                        for (j in 1:length(legend_aux)){
                                if(j==1){
                                        new_legend <- paste0(new_legend, legend_aux[j])
                                        character_count <- character_count + nchar(legend_aux[j])
                                }else{
                                        if(character_count + nchar(legend_aux[j]) < number_characters){
                                                new_legend <- paste0(new_legend, " ", legend_aux[j])
                                                character_count <- character_count + nchar(legend_aux[j]) + 1
                                        }else{
                                                new_legend <- paste0(new_legend, "\n", legend_aux[j])
                                                character_count <- 0
                                        }
                                }
                        }
                        text_vector[i] <- new_legend
                }
        }
        
        return(text_vector)
}


# Help text for country groups
load_help_text_group <- bs_modal(
        id = "help_text_group",
        title = "Country group selection",
        body = tags$p("All countries belonging to at least one of the groups selected will be included. You can check the classifications in the 'Countries/aggregates WDI' tab."),
        size = "medium")

# Help text for country groups
load_help_text_var <- bs_modal(
        id = "help_text_var",
        title = "WDI variables",
        body = tags$div("WDI variables are retrieved using the", tags$a(href="https://vincentarelbundock.github.io/WDI/", "WDI package"), "developed by Vincent Arel-Bundock. This package allows to search and download data from over 40 databases hosted by the World Bank, including the World Development Indicators ('WDI'), International Debt Statistics, Doing Business, Human Capital Index, and Sub-national Poverty indicators. You can ckeck the over 17,000 variables available in the 'WDI variables' tab."),
        size = "medium")


# Help text for upload data
load_help_text <- bs_modal(
        id = "help_text",
        title = "Instructions to upload external data",
        body = includeHTML("help_upload.html"),
        size = "medium")

# Color palette info
load_palette <- bs_modal(
        id = "help_palette",
        title = "Color palettes",
        body = HTML('<img src="https://www.r-graph-gallery.com/38-rcolorbrewers-palettes_files/figure-html/thecode-1.png" width="500" hight="200" >'),
        size = "medium")

print("********************************")
print("***END OF CODE OUTSIDE SERVER***")
print("********************************")
cat("\n")
cat("\n")


# jsc <- '
# $(document).ready(function () {
#   $(".sidebar-menu").children("li").on("click", function() {
#     $("#mult, #single").toggle();
#   });
# });
# '


# ui.r ----

ui <- shinyUI(
                fluidPage(
                
                # Load useShinyjs 
                shinyjs::useShinyjs(),
                
                # Load modals
                load_help_text_group,
                load_help_text_var,
                load_help_text,
                load_palette,
                
                # Theme
                theme = shinytheme("cerulean"),
                        
                        # Navigation bar style
                        tags$style(HTML("
                        
                                .navbar1 .navbar{
                                height: 70px;
                                line-height: 70px;
                                }
                                .navbar1 .navbar-brand{
                                width: 200px !important;
                                line-height: 35px
                                }
                                .navbar1 .navbar a{
                                text-align: center;
                                width: 160px;
                                height: 70px;
                                line-height: 35px
                                }
                                .navbar1 .dropdown-menu>li>a{
                                text-align: left;
                                height: 30px;
                                line-height:25px
                                }"
                                )
                        ),
                        
                # Navigation bar
                div(class = "navbar1", 
                navbarPage(title = "Plot Data Tool",
                        position = "fixed-top",
                        
                        # tabPanel(title = "About",
                        #         
                        #         
                        # 
                        # ),

                        tabPanel(title = "Data",
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),

                                sidebarLayout(
                                
                                        sidebarPanel(title = "Data inputs",
                                                
                                                # Clear data
                                                actionButtonStyled(inputId = "in_id_reset",
                                                        label = "Clear inputs",
                                                        icon = NULL,
                                                        width = NULL,
                                                        btn_type = "btn-sm",
                                                        type = "danger"),
                                                
                                                # Update countries button
                                                shinyjs::hidden(
                                                        actionButtonStyled(
                                                                inputId = "in_id_update",
                                                                label = "Apply changes",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success")
                                                        ),
                                                
                                                tags$br(),
                                                tags$p(),
                                                
                                                # Select countries
                                                wellPanel(
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Step 1: Select countries"),
                                                        
                                                        # Input: Country of analysis
                                                        pickerInput(
                                                                inputId = "in_id_ctries_ctry",
                                                                label = "Select country of analysis",
                                                                choices = flags_df$country,
                                                                choicesOpt = list(content =  
                                                                                mapply(flags_df$country, flags_df$URL, FUN = function(country, flagUrl) {
                                                                                        HTML(paste(
                                                                                                tags$img(src = flagUrl, width = 20, height = 15),
                                                                                                country
                                                                                        ))
                                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                                ),
                                                                selected = def_list_data$ctries_ctry,
                                                                options = list(size = 15, `live-search` = TRUE)
                                                        ),
                                                        
                                                        # Input: Comparators - Structural
                                                        selectizeInput(
                                                                inputId = "in_id_ctries_str",
                                                                label = "Structural comparators",
                                                                choices = ctry_vec[!ctry_vec %in% c(def_list_data$ctries_ctry, def_list_data$ctries_asp)],
                                                                multiple = TRUE,
                                                                selected = def_list_data$ctries_str),
                                                        
                                                        # Input: Comparators - Aspirational
                                                        selectizeInput(
                                                                inputId = "in_id_ctries_asp",
                                                                label = "Aspirational comparators",
                                                                choices = ctry_vec[!ctry_vec %in% c(def_list_data$ctries_ctry, def_list_data$ctries_str)],
                                                                multiple = TRUE,
                                                                selected = def_list_data$ctries_asp),
                                                        
                                                        # Input: Comparators - Regions
                                                        uiOutput('in_id_ctries_group_ui'),

                                                        # Input: Comparators - Regions
                                                        selectizeInput(
                                                                inputId = "in_id_ctries_reg",
                                                                label = "Aggregates",
                                                                choices = reg_vec,
                                                                multiple = TRUE,
                                                                selected = def_list_data$ctries_reg),
                                                        
                                                        # Download all countries
                                                        materialSwitch(
                                                                inputId = "in_id_ctries_all",
                                                                label = "Include all countries",
                                                                value = def_list_data$ctries_all,
                                                                status = "primary",
                                                                right = TRUE),
                                                        
                                                        # Download all regions
                                                        materialSwitch(
                                                                inputId = "in_id_regs_all",
                                                                label = "Include all aggregates",
                                                                value = def_list_data$regs_all,
                                                                status = "primary",
                                                                right = TRUE)
                                                        
                                                ),
                                                
                                                # Select time period
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Step 2: Select time period"),
                                                        
                                                        # Input: Slider for the year span
                                                        sliderInput(
                                                                inputId = "in_id_time_range",
                                                                label = "Select year range",
                                                                sep = "",
                                                                min = def_list_data$time_range_min,
                                                                max = def_list_data$time_range_max,
                                                                step = 1,
                                                                value = c(def_list_data$time_range_start, def_list_data$time_range_end)),
                                                        
                                                        # Input: Subperiods
                                                        conditionalPanel(
                                                                
                                                                # Subperiod switch only appears if there are 2 or more years in range
                                                                condition = "input.in_id_time_range[1]-input.in_id_time_range[0]>=1",
                                                                materialSwitch (
                                                                        inputId = "in_id_time_subper",
                                                                        label = "Divide range in subperiods",
                                                                        value = def_list_data$time_subper,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                                
                                                                
                                                                conditionalPanel(
                                                                        
                                                                        # Number of subperiods only appears if subperiods switch is on
                                                                        condition = "input.in_id_time_subper == true",
                                                                        
                                                                        numericInput(
                                                                                inputId = "in_id_time_subper_num",
                                                                                label = "Number of subperiods (min=2; max=4)",
                                                                                min = 2,
                                                                                max = 4,
                                                                                value = def_list_data$time_subper_num),
                                                                        
                                                                        tags$hr(style="border-color: #99a3a4;"),
                                                                        
                                                                        textInput(
                                                                                inputId = "in_id_time_name_1",
                                                                                label="Period 1 name",
                                                                                value = def_list_data$time_name_1),
                                                                        
                                                                        textInput(
                                                                                inputId = "in_id_time_name_2",
                                                                                label="Period 2 name",
                                                                                value = def_list_data$time_name_2),

                                                                        uiOutput("in_id_time_limit_1_ui"),
                                                                        
                                                                        conditionalPanel(
                                                                                
                                                                                # Subperiod 3's name and the end of period 2 appear only if number of subperiods selected is 3 or 4
                                                                                condition = "input.in_id_time_subper_num == 3 | input.in_id_time_subper_num == 4",
                                                                                
                                                                                tags$hr(style="border-color: #99a3a4;"),
                                                                                
                                                                                textInput(inputId = "in_id_time_name_3",
                                                                                        label="Period 3 name",
                                                                                        value = def_list_data$time_name_3),
                                                                                
                                                                                uiOutput("in_id_time_limit_2_ui"),
                                                                                
                                                                        ),
                                                                        
                                                                        conditionalPanel(
                                                                                
                                                                                # Subperiod 4's name and the end of period 3 appear only if number of subperiods selected is 4
                                                                                condition = "input.in_id_time_subper_num == 4",
                                                                                
                                                                                tags$hr(style="border-color: #99a3a4;"),
                                                                                
                                                                                textInput(inputId = "in_id_time_name_4",
                                                                                        label="Period 4 name",
                                                                                        value = def_list_data$time_name_4),

                                                                                uiOutput("in_id_time_limit_3_ui")
                                                                                
                                                                        )
                                                                )
                                                        )
                                                ),
                                                
                                                # Select variables from pre-loaded databases (optional)
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Step 3: Select variables from pre-loaded databases"),
                                                        
                                                        # Input: WDI variables
                                                        selectizeInput(
                                                                inputId = "in_id_wdi_vars",
                                                                label = "WDI variables (Name | Code)",
                                                                choices = NULL,
                                                                multiple = TRUE,
                                                                options = list(maxOptions = 100))%>%
                                                                shinyInput_label_embed(
                                                                        shiny_iconlink() %>%
                                                                                bs_attach_modal(id_modal = "help_text_var")
                                                                )
                                                        
                                                        # Input: IMF variables
                                                        # selectizeInput(
                                                        #         inputId = "in_id_imf_vars",
                                                        #         label = "IMF variables (Name | Code)",
                                                        #         choices = NULL,
                                                        #         multiple = TRUE)
                                                        
                                                ),
                                                
                                                # Upload data from external file
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Step 4: Upload data from external file"),
                
                                                        uiOutput('in_id_ext_ui'),
                                                        
                                                        # Messages
                                                        uiOutput("in_id_ext_mes_ui")
                                                
                                                )
                                        ),
                                        
                                        mainPanel(
                                                
                                                tabsetPanel(type = "tabs",
                                                        
                                                        # Output: Table
                                                        tabPanel("Table",
                                                                
                                                                # Data download button
                                                                tags$br(),
                                                                downloadButton("out_download_data", 
                                                                        "Download data"),
                                                                
                                                                # Table 1: data
                                                                tags$br(),
                                                                tags$br(),
                                                                dataTableOutput("out_data_table") %>% withSpinner(
                                                                        type = 3, 
                                                                        color = def_list_data$col_spinner, 
                                                                        color.background = "white")
                                                                
                                                        ),
                                                        
                                                        # Output: Metadata
                                                        tabPanel("Metadata",
                                                                
                                                                # Data download button
                                                                tags$br(),
                                                                downloadButton("out_download_metadata", 
                                                                        "Download metadata"),
                                                                
                                                                # Table 2: metadata
                                                                tags$br(),
                                                                tags$br(),
                                                                dataTableOutput("out_metadata_table") %>% withSpinner(
                                                                        type = 3, 
                                                                        color = def_list_data$col_spinner, 
                                                                        color.background = "white")

                                                        ),
                                                        
                                                        # Output: Summary
                                                        tabPanel("Summary",
                                                                tags$br(),
                                                                
                                                                uiOutput("out_summary_title"),
                                                                htmlOutput("out_summary")%>% withSpinner(type = 3, 
                                                                        color = def_list_data$col_spinner, 
                                                                        color.background = "white"),
                                                                
                                                                uiOutput("out_summary_missing_title"),
                                                                plotOutput("out_summary_missing")%>% withSpinner(type = 3, 
                                                                        color = def_list_data$col_spinner, 
                                                                        color.background = "white")
                                                        
                                                        ),
                                                        
                                                        # Output: Country table
                                                        tabPanel("Countries/aggregates WDI",
                                                                
                                                                # Download button
                                                                tags$br(),
                                                                downloadButton("out_download_ctry_df", 
                                                                        "Download countries/aggregates"),
                                                                
                                                                # Countries' table
                                                                tags$br(),
                                                                h4("Countries"),
                                                                dataTableOutput("out_ctry_df"),
                                                                
                                                                # Aggregates' table
                                                                tags$br(),
                                                                h4("Aggregates"),
                                                                dataTableOutput("out_ctry_df_agg")
                                                                

                                                        ),
                                                        
                                                        # Output: WDI variables
                                                        tabPanel("WDI variables",
                                                                
                                                                # Download button
                                                                tags$br(),
                                                                downloadButton("out_download_var_df", 
                                                                        "Download variables"),
                                                                
                                                                # Variables' table
                                                                tags$br(),
                                                                tags$br(),
                                                                dataTableOutput("out_var_df")
                                                                
                                                        )
                                                )
                                        )
                                )
                        ),
                        
                        navbarMenu(title = "Bar plots",
        
                                tabPanel(title = "Single Country",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_bar_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_bar_id_ctries_ctry",
                                                                        label = "Select country",
                                                                        choices = c(""),
                                                                        selected = c(""),
                                                                        options = list(size = 15,
                                                                                `live-search` = TRUE)
                                                                ),
                                                                
                                                                # Select variables
                                                                pickerInput(
                                                                        inputId = "gr_bar_id_vars",
                                                                        label = "Select variables", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_bar_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_bar$time_range[1],
                                                                        max = def_list_bar$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_bar$time_range[1], def_list_bar$time_range[2])
                                                                )
                                                        ),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                # Display options
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_bar$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_bar$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_bar$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_bar$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_bar_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_bar$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE),
                                                                ),
                                                                
                                                                # Include period/subperiod averages
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_time_subper_avg",
                                                                        label = "Include period/subperiod averages",
                                                                        value = def_list_bar$time_subper_avg,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_bar$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_bar$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_bar_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_bar$digits
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_bar_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_bar$digits_y
                                                                ),
                                                                
                                                                # Select color and transparency of bars
                                                                colourpicker::colourInput(
                                                                        inputId = "gr_bar_id_color",
                                                                        label = "Select color and transparency of bars",
                                                                        value = def_list_bar$color,
                                                                        allowTransparent = TRUE,
                                                                        closeOnClick = TRUE
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_bar_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_bar$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                        
                                                                )
                                                        )
                                                ),
                                                
                                                mainPanel(
                                                        
                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_bar_download_large", "Long Plots"),
                                                        downloadButton("gr_bar_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_bar_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        
                                                )
                                        )
                                ),
        
                                tabPanel(title = "Multiple Countries",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_mult_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_mult_id_ctries",
                                                                        label = "Select countries/aggregates", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select variables
                                                                pickerInput(
                                                                        inputId = "gr_mult_id_vars",
                                                                        label = "Select variables", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select statistic
                                                                pickerInput(
                                                                        inputId = "gr_mult_id_stat",
                                                                        label = "Method of aggregation by period", 
                                                                        choices = stats_vec,
                                                                        selected = def_list_mult$stat,
                                                                        multiple = FALSE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_mult_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_mult$time_range[1],
                                                                        max = def_list_mult$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_mult$time_range[1], def_list_mult$time_range[2])
                                                                )
                                                                        
                                                        ),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_mult$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_mult$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_mult$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_mult$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (inputId = "gr_mult_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_mult$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE
                                                                        )
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_mult$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_mult$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_mult_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_mult$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_mult_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_mult$digits
                                                                ),
                                                                
                                                                # Number of digits Y-axis
                                                                numericInput(
                                                                        inputId = "gr_mult_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_mult$digits_y
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_mult_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_mult$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),
                                                                
                                                                # Select color palette
                                                                conditionalPanel(
                                                                        condition = "input.gr_mult_id_time_subper == true",
                                                                        uiOutput('gr_mult_id_color_pal_ui')
                                                                ),
                                                                
                                                                conditionalPanel(
                                                                        condition = "input.gr_mult_id_time_subper == false",
                                                                        colourpicker::colourInput(
                                                                                inputId = "gr_mult_id_color",
                                                                                label = "Select color and transparency of bars",
                                                                                value = def_list_mult$color,
                                                                                allowTransparent = TRUE,
                                                                                closeOnClick = TRUE)
                                                                ),
                                                                
                                                                # Select legend position
                                                                conditionalPanel(
                                                                        condition = "input.gr_mult_id_time_subper == true",
                                                                        pickerInput(
                                                                                inputId = "gr_mult_id_legend_pos",
                                                                                label = "Select legends position", 
                                                                                choices = def_list_mult$legend_pos,
                                                                                selected = "top",
                                                                                multiple = FALSE
                                                                        )
                                                                )
                                                        )
                                                        
                                                ),
                                                mainPanel(

                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_mult_download_large", "Long Plots"),
                                                        downloadButton("gr_mult_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_mult_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        

                                                )
                                        )
                                )
                        ),
                        
                        navbarMenu(title = "Stacked bar plots",
                                
                                tabPanel(title = "Single Country",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_stas_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_stas_id_ctries_ctry",
                                                                        label = "Select country",
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `live-search` = TRUE)
                                                                ),

                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_stas_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_stas$time_range[1],
                                                                        max = def_list_stas$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_stas$time_range[1], def_list_stas$time_range[2])
                                                                ),

                                                                # Define number of plots
                                                                numericInput(
                                                                        inputId = "gr_stas_id_plot_num",
                                                                        label = "Number of plots",
                                                                        min = 1,
                                                                        value = def_list_stas$plot_num
                                                                )
                                                                
                                                        ),
                                                        
                                                        # Individual plot panels
                                                        uiOutput("gr_stas_id_input_panels"),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                # Display options
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_stas$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_stas$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_stas$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_stas$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_stas_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_stas$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE),
                                                                ),
                                                                
                                                                # Include period/subperiod averages
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_time_subper_avg",
                                                                        label = "Include period/subperiod averages",
                                                                        value = def_list_stas$time_subper_avg,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_stas$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_stas_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_stas$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_stas_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_stas$digits
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_stas_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_stas$digits_y
                                                                ),
                                                                
                                                                # Select color and transparency of bars
                                                                uiOutput('gr_stas_id_color_ui'),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_stas_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_stas$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                        
                                                                ),
                                                                
                                                                # Select legend position
                                                                pickerInput(
                                                                        inputId = "gr_stas_id_legend_pos",
                                                                        label = "Select legends position", 
                                                                        choices = position_list,
                                                                        selected = def_list_stas$legend_pos,
                                                                        multiple = FALSE
                                                                )
                                                                
                                                        )
                                                ),
                                                
                                                mainPanel(

                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_stas_download_large", "Long Plots"),
                                                        downloadButton("gr_stas_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_stas_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        
                                                )
                                        )
                                ),
                                
                                tabPanel(title = "Multiple Countries",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_stam_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_stam_id_ctries",
                                                                        label = "Select countries/aggregates", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_stam_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_stam$time_range[1],
                                                                        max = def_list_stam$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_stam$time_range[1], def_list_stam$time_range[2])
                                                                ),
                                                                
                                                                # Select statistic
                                                                pickerInput(
                                                                        inputId = "gr_stam_id_stat",
                                                                        label = "Method of aggregation by period", 
                                                                        choices = stats_vec,
                                                                        selected = def_list_stam$stat,
                                                                        multiple = FALSE
                                                                ),
                                                                
                                                                # Define number of plots
                                                                numericInput(
                                                                        inputId = "gr_stam_id_plot_num",
                                                                        label = "Number of plots",
                                                                        min = 1,
                                                                        value = def_list_stam$plot_num
                                                                )

                                                        ),
                                                        
                                                        # Individual plot panels
                                                        uiOutput("gr_stam_id_input_panels"),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_stam$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_stam$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_stam$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_stam$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_stam_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_stam$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE),
                                                                ),

                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_stam$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_stam$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_stam_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_stam$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_stam_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_stam$digits
                                                                ),
                                                                
                                                                # Number of digits Y-axis
                                                                numericInput(
                                                                        inputId = "gr_stam_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_stam$digits_y
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_stam_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_stam$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),
                                                                
                                                                # Select color palette
                                                                uiOutput('gr_stam_id_color_ui'),
                                                                
                                                                # Select legend position
                                                                pickerInput(
                                                                        inputId = "gr_stam_id_legend_pos",
                                                                        label = "Select legends position", 
                                                                        choices = position_list,
                                                                        selected = def_list_stam$legend_pos,
                                                                        multiple = FALSE
                                                                )
                                                        )
                                                        
                                                ),
                                                mainPanel(

                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_stam_download_large", "Long Plots"),
                                                        downloadButton("gr_stam_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_stam_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        

                                                )
                                        )
                                )
                        ),
                        
                        navbarMenu(title = "Line plots",
                                
                                tabPanel(title = "Single Variable",
                                                
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),        
                                
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_line_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel(
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries/aggregates
                                                                pickerInput(
                                                                        inputId = "gr_line_id_ctries",
                                                                        label = "Select countries/aggregates", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select variables
                                                                pickerInput(
                                                                        inputId = "gr_line_id_vars",
                                                                        label = "Select variables", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_line_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_line$time_range[1],
                                                                        max = def_list_line$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_line$time_range[1], def_list_line$time_range[2])
                                                                )
                                                                
                                                        ),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_line$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_line$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_line$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_line$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_line_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_line$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE)
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_line$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_line$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_line$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_line_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_line$digits
                                                                ),
                                                                
                                                                # Number of digits Y-axis
                                                                numericInput(
                                                                        inputId = "gr_line_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_line$digits_y
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_line_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_line$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),
                                                                
                                                                # Select highlighted country/aggregate and color
                                                                materialSwitch(
                                                                        inputId = "gr_line_id_highlight",
                                                                        label = "Highlight a country/aggregate",
                                                                        value = def_list_line$highlight,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                                
                                                                conditionalPanel(
                                                                        condition = "input.gr_line_id_highlight == true",
                                                                        
                                                                        # Input: Country of analysis
                                                                        selectInput(
                                                                                inputId = "gr_line_id_highlight_ctry",
                                                                                label = "Select highlighted country/aggregate",
                                                                                choices = c(""),
                                                                                selected = "input.ctry"),
                                                                        
                                                                        # Input: Line plot color
                                                                        colourpicker::colourInput(
                                                                                inputId ="gr_line_id_highlight_color", 
                                                                                label = "Select colors of highlighted line", 
                                                                                value = def_list_line$highlight_color,
                                                                                allowTransparent = TRUE,
                                                                                closeOnClick = TRUE),
                                                                )
                                                        )
                                                ),
                                                
                                                mainPanel(
                                                        
                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_line_download_large", "Long Plots"),
                                                        downloadButton("gr_line_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_line_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                )
                                        )
                                ),
                                tabPanel(title = "Multiple Variables",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_linm_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_linm_id_ctries",
                                                                        label = "Select countries/aggregates", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_linm_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_linm$time_range[1],
                                                                        max = def_list_linm$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_linm$time_range[1], def_list_linm$time_range[2])
                                                                ),

                                                                # Define number of plots
                                                                numericInput(
                                                                        inputId = "gr_linm_id_plot_num",
                                                                        label = "Number of plots",
                                                                        min = 1,
                                                                        value = def_list_linm$plot_num
                                                                )
                                                                
                                                        ),
                                                        
                                                        # Individual plot panels
                                                        uiOutput("gr_linm_id_input_panels"),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_linm$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_linm$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_linm$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_linm$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_linm_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_linm$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE),
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_linm$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_linm$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_linm_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_linm$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_linm_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_linm$digits
                                                                ),
                                                                
                                                                # Number of digits Y-axis
                                                                numericInput(
                                                                        inputId = "gr_linm_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_linm$digits_y
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_linm_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_linm$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),

                                                                # Select legend position
                                                                pickerInput(
                                                                        inputId = "gr_linm_id_legend_pos",
                                                                        label = "Select legends position", 
                                                                        choices = position_list,
                                                                        selected = def_list_linm$legend_pos,
                                                                        multiple = FALSE
                                                                )
                                                        )
                                                        
                                                ),
                                                mainPanel(
                                                        
                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_linm_download_large", "Long Plots"),
                                                        downloadButton("gr_linm_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_linm_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        
                                                )
                                        )
                                        
                                )
                        ),
                        
                        navbarMenu(title = "Ranking plots",
                                
                                tabPanel(title = "Across Time",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_rnkt_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel(
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries/aggregates
                                                                pickerInput(
                                                                        inputId = "gr_rnkt_id_ctries",
                                                                        label = "Select countries/aggregates (defines ranking)", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select variables
                                                                pickerInput(
                                                                        inputId = "gr_rnkt_id_vars",
                                                                        label = "Select variables", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_rnkt_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_rnkt$time_range[1],
                                                                        max = def_list_rnkt$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_rnkt$time_range[1], def_list_rnkt$time_range[2])
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_incomplete",
                                                                        label = "Exclude countries with incomplete data for the selected period",
                                                                        value = def_list_rnkt$incomplete,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Rank in descending order (Max ranked 1st)
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_descending",
                                                                        label = "Rank in descending order (max ranked 1)",
                                                                        value = def_list_rnkt$descending,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                        ),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Select display countries/aggregates
                                                                pickerInput(
                                                                        inputId = "gr_rnkt_id_ctries_disp",
                                                                        label = "Select countries/aggregates to display", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_rnkt$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_yaxis",
                                                                        label = "Include Y-axis units",
                                                                        value = def_list_rnkt$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_rnkt$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include subperiods
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true",
                                                                        materialSwitch (
                                                                                inputId = "gr_rnkt_id_time_subper",
                                                                                label = "Include subperiods",
                                                                                value = def_list_rnkt$time_subper,
                                                                                status = "primary",
                                                                                right = TRUE)
                                                                ),

                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_rnkt$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_rnkt_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_rnkt$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),
                                                                
                                                                # Select highlighted country/aggregate and color
                                                                materialSwitch(
                                                                        inputId = "gr_rnkt_id_highlight",
                                                                        label = "Highlight a country/aggregate",
                                                                        value = def_list_rnkt$highlight,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                                
                                                                conditionalPanel(
                                                                        condition = "input.gr_rnkt_id_highlight == true",
                                                                        
                                                                        # Input: Country of analysis
                                                                        selectInput(
                                                                                inputId = "gr_rnkt_id_highlight_ctry",
                                                                                label = "Select highlighted country/aggregate",
                                                                                choices = c(""),
                                                                                selected = "input.ctry"),
                                                                        
                                                                        # Input: Line plot color
                                                                        colourpicker::colourInput(
                                                                                inputId ="gr_rnkt_id_highlight_color", 
                                                                                label = "Select colors of highlighted line", 
                                                                                value = def_list_rnkt$highlight_color,
                                                                                allowTransparent = TRUE,
                                                                                closeOnClick = TRUE),
                                                                )
                                                        )
                                                ),
                                                        
                                                mainPanel(
                                                        
                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_rnkt_download_large", "Long Plots"),
                                                        downloadButton("gr_rnkt_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_rnkt_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),

                                                )
                                        )
                                ),
                                
                                tabPanel(title = "Static",
                                        
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        
                                        sidebarLayout(
                                                
                                                sidebarPanel(
                                                        
                                                        actionButtonStyled(
                                                                inputId = "gr_rnks_id_update",
                                                                label = "Update plots",
                                                                icon = NULL,
                                                                width = NULL,
                                                                btn_type = "btn-sm",
                                                                type = "success"),
                                                        
                                                        tags$br(),
                                                        tags$p(),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Plot parameters"),
                                                                
                                                                # Select countries
                                                                pickerInput(
                                                                        inputId = "gr_rnks_id_ctries",
                                                                        label = "Select countries/aggregates", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select variables
                                                                pickerInput(
                                                                        inputId = "gr_rnks_id_vars",
                                                                        label = "Select variables", 
                                                                        choices = c(""),
                                                                        options = list(size = 15,
                                                                                `actions-box` = TRUE),
                                                                        multiple = TRUE
                                                                ),
                                                                
                                                                # Select statistic
                                                                pickerInput(
                                                                        inputId = "gr_rnks_id_stat",
                                                                        label = "Method of aggregation by period", 
                                                                        choices = stats_vec[1:6],
                                                                        selected = def_list_rnks$stat,
                                                                        multiple = FALSE
                                                                ),
                                                                
                                                                # Select range
                                                                sliderInput(
                                                                        inputId = "gr_rnks_id_time_range",
                                                                        label = "Select range",
                                                                        sep = "",
                                                                        min = def_list_rnks$time_range[1],
                                                                        max = def_list_rnks$time_range[2],
                                                                        step = 1,
                                                                        value = c(def_list_rnks$time_range[1], def_list_rnks$time_range[2])
                                                                )
                                                                
                                                        ),
                                                        
                                                        wellPanel( 
                                                                
                                                                style = "background-color: #ebf5fb",
                                                                
                                                                h4("Display options"),
                                                                
                                                                # Include title
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_title",
                                                                        label = "Include title",
                                                                        value = def_list_rnks$title,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include Y-axis units
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_yaxis",
                                                                        label = "Include axis units",
                                                                        value = def_list_rnks$yaxis,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include source
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_source",
                                                                        label = "Include source",
                                                                        value = def_list_rnks$source,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Include data labels
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_data_labels",
                                                                        label = "Include data labels",
                                                                        value = def_list_rnks$data_labels,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Plot horizontally
                                                                materialSwitch (inputId = "gr_rnks_id_horizontal",
                                                                        label = "Plot horizontally",
                                                                        value = def_list_rnks$horizontal,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to Trillion/Billion/Million/Thousands
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_transform_zeros",
                                                                        label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                        value = def_list_rnks$transform_zeros,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Transform data to logs (applies only to positive series)
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_transform_log",
                                                                        label = "Transform data to logs (applies only to positive series)",
                                                                        value = def_list_rnks$transform_log,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Change country names to short
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_ctry_short",
                                                                        label = "Short country names (ISO 3)",
                                                                        value = def_list_rnks$ctry_short,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                                
                                                                # Number of digits
                                                                numericInput(
                                                                        inputId = "gr_rnks_id_digits",
                                                                        label = "Number of digits in labels",
                                                                        min = 0,
                                                                        value = def_list_rnks$digits
                                                                ),
                                                                
                                                                # Number of digits Y-axis
                                                                numericInput(
                                                                        inputId = "gr_rnks_id_digits_y",
                                                                        label = "Number of digits in Y-axis",
                                                                        min = 0,
                                                                        value = def_list_rnks$digits_y
                                                                ),
                                                                
                                                                # Select font
                                                                pickerInput(
                                                                        inputId = "gr_rnks_id_font",
                                                                        label = "Select font",
                                                                        choices = font_list,
                                                                        selected = def_list_rnks$font,
                                                                        choicesOpt = list(
                                                                                content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                        "<div style='font-family: sans'>Arial</div>", 
                                                                                        "<div style='font-family: mono'>Courier</div>", 
                                                                                        "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                ),
                                                                
                                                                # Select highlighted country/aggregate and color
                                                                materialSwitch(
                                                                        inputId = "gr_rnks_id_highlight",
                                                                        label = "Highlight a country/aggregate",
                                                                        value = def_list_rnks$highlight,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                                
                                                                conditionalPanel(
                                                                        condition = "input.gr_rnks_id_highlight == true",
                                                                        
                                                                        # Input: Country of analysis
                                                                        selectInput(
                                                                                inputId = "gr_rnks_id_highlight_ctry",
                                                                                label = "Select highlighted country/aggregate",
                                                                                choices = c(""),
                                                                                selected = "input.ctry"),
                                                                        
                                                                )
                                                                
                                                        )
                                                        
                                                ),
                                                mainPanel(
                                                        
                                                        # Download graphs options
                                                        helpText("Download zip file with plots and underlying data (select plot size)"),
                                                        downloadButton("gr_rnks_download_large", "Long Plots"),
                                                        downloadButton("gr_rnks_download_small", "Small Plots"),
                                                        tags$br(),
                                                        tags$br(),
                                                        
                                                        # Plots' dynamic UI
                                                        tags$br(),
                                                        uiOutput("gr_rnks_out_plots") %>% withSpinner(type = 3, 
                                                                color = def_list_data$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        
                                                )
                                        )
                                )
                        ),
                        
                        tabPanel(title = "Scatter plots",
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                sidebarLayout(
                                        
                                        sidebarPanel(width = 4,
                                                
                                                actionButtonStyled(
                                                        inputId = "gr_scat_id_update",
                                                        label = "Update plots",
                                                        icon = NULL,
                                                        width = NULL,
                                                        btn_type = "btn-sm",
                                                        type = "success"),
        
                                                tags$br(),
                                                tags$p(),
                                                 
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Plot parameters"),
                                                        
                                                        # Select countries/aggregates
                                                        pickerInput(
                                                                inputId = "gr_scat_id_ctries",
                                                                label = "Select countries/aggregates", 
                                                                choices = c(""),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        # Filter small countries
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_ctries_small",
                                                                label = "Filter small countries (Population < 1M)",
                                                                value = def_list_scat$ctries_small,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Define number of plots
                                                        numericInput(
                                                                inputId = "gr_scat_id_plot_num",
                                                                label = "Number of plots",
                                                                min = 1,
                                                                value = def_list_scat$plot_num
                                                        )
                                                        
                                                ),
                                                
                                                # Individual plot panels
                                                uiOutput("gr_scat_id_input_panels"),
                                                
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Display options"),
                                                        
                                                        # Include title
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_title",
                                                                label = "Include title",
                                                                value = def_list_scat$title,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include X-axis title
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_xaxis",
                                                                label = "Include X-axis title",
                                                                value = def_list_scat$xaxis,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_yaxis",
                                                                label = "Include Y-axis title",
                                                                value = def_list_scat$yaxis,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include source
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_source",
                                                                label = "Include source",
                                                                value = def_list_scat$source,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include source
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_note",
                                                                label = "Include notes",
                                                                value = def_list_scat$note,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = def_list_scat$transform_zeros,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Change country names to short
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_ctry_short",
                                                                label = "Short country names (ISO 3)",
                                                                value = def_list_scat$ctry_short,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Number of digits X-axis
                                                        numericInput(
                                                                inputId = "gr_scat_id_digits_x",
                                                                label = "Number of digits in X-axis",
                                                                min = 0,
                                                                value = def_list_scat$digits_x
                                                        ),
                                                        
                                                        # Number of digits Y-axis
                                                        numericInput(
                                                                inputId = "gr_scat_id_digits_y",
                                                                label = "Number of digits in Y-axis",
                                                                min = 0,
                                                                value = def_list_scat$digits_y
                                                        ),
                                                        
                                                        # Select font
                                                        pickerInput(
                                                                inputId = "gr_scat_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = def_list_scat$font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                        ),
                                                        
                                                        # Select highlighted country/aggregate and color
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_highlight",
                                                                label = "Highlight a country/aggregate",
                                                                value = def_list_scat$highlight,
                                                                status = "primary",
                                                                right = TRUE),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_scat_id_highlight == true",
                                                                
                                                                # Input: Country of analysis
                                                                selectInput(
                                                                        inputId = "gr_scat_id_highlight_ctry",
                                                                        label = "Select highlighted country/aggregate",
                                                                        choices = c(""),
                                                                        selected = "input.ctry"),
                                                                
                                                                # Input: Line plot color
                                                                colourpicker::colourInput(
                                                                        inputId ="gr_scat_id_highlight_color", 
                                                                        label = "Select colors of highlighted country/aggregate", 
                                                                        value = def_list_scat$highlight_color,
                                                                        allowTransparent = TRUE,
                                                                        closeOnClick = TRUE),
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_scat_id_highlight == false",
        
                                                                materialSwitch(
                                                                        inputId = "gr_scat_id_highlight_group",
                                                                        label = "Highlight analyzed country and comparators",
                                                                        value = def_list_scat$highlight_group,
                                                                        status = "primary",
                                                                        right = TRUE)
                                                        ),
                                                        
                                                        hr(style = "border-top: 1px solid #a6acaf;"),
                                                        
                                                        h5("Include lines"),
                                                        
                                                        # Regression line
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_regline",
                                                                label = "Regression line",
                                                                value =  def_list_scat$scat_regline,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Regression line with confidence interval
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_regline_ci",
                                                                label = "Regression line with 95% confidence intervals",
                                                                value =  def_list_scat$scat_regline_ci,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # 45 degree line
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_45line",
                                                                label = "45 degree line",
                                                                value =  def_list_scat$scat_45line,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Mean quadrants
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_quad_avg",
                                                                label = "Mean quadrants",
                                                                value =  def_list_scat$scat_quad_avg,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Median quadrants
                                                        materialSwitch(
                                                                inputId = "gr_scat_id_quad_med",
                                                                label = "Median quadrants",
                                                                value =  def_list_scat$scat_quad_med,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                )
                                                
                                                
                                        ),
                                        
                                        mainPanel(width = 8,
                                                
                                                # Download graphs options
                                                helpText("Download zip file with plots and underlying data (select plot size)"),
                                                downloadButton("gr_scat_download_large", "Long Plots"),
                                                downloadButton("gr_scat_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br(),
                                                
                                                # Plots' dynamic UI
                                                tags$br(),
                                                uiOutput("gr_scat_out_plots") %>% withSpinner(type = 3, 
                                                        color = def_list_data$col_spinner, 
                                                        color.background = "white"),


        
                                        )
                                )
                        )
                )),

                # Footer
                hr(),
                tags$p("Developed by Federico Ganz | federicoganz@gmail.com")
        )
)

# server.r ----
server <- function(input, output, session) {
        
##************************************************************************************************** ----
        
        ## Data tab ----
        
        ### Inputs, reactive values, reset and clear buttons ---- 

        # Update inputs based on other inputs
        
        # Update structural comparators available choices
        observeEvent(c(input$in_id_ctries_ctry, input$in_id_ctries_asp) ,{
                
                print("Data - Inputs [1/5]: Stuctural comparators (observeEvent)")

                updateSelectizeInput(
                        inputId = 'in_id_ctries_str',
                        choices = ctry_vec[!ctry_vec %in% c(input$in_id_ctries_ctry , input$in_id_ctries_asp)],
                        selected = input$in_id_ctries_str)
                
        })
        
        # Update aspirational comparators available choices
        observeEvent(c(input$in_id_ctries_ctry, input$in_id_ctries_str) ,{
                
                print("Data - Inputs [1/5]: Aspirational comparators (observeEvent)")
                
                updateSelectizeInput(
                        inputId = 'in_id_ctries_asp',
                        choices = ctry_vec[!ctry_vec %in% c(input$in_id_ctries_ctry , input$in_id_ctries_str)],
                        selected = input$in_id_ctries_asp)
                
        })
        
        # Update country groups
        output$in_id_ctries_group_ui <- renderUI({
                
                print("Data - Inputs [1/5]: Groups of countries (renderUI)")
                
                selectizeInput(
                        inputId = "in_id_ctries_group",
                        label = "Groups of countries",
                        choices = group_list,
                        multiple = TRUE,
                        selected = def_list_data$ctries_group) %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_text_group")
                        )
                
        })
        
        
        # Update subperiod inputs as range changes
        observeEvent(input$in_id_time_range, {
                
                print("Data - Inputs [1/5]: Number of subperiods (observeEvent)")
                
                updateNumericInput(
                        session = session,
                        inputId = 'in_id_time_subper_num',
                        min = 2,
                        max = min(4, input$in_id_time_range[2] - input$in_id_time_range[1] + 1),
                        value = min(input$in_id_time_range[2] - input$in_id_time_range[1] + 1, input$in_id_time_subper_num))
                
        })
        
        output$in_id_time_limit_1_ui <- renderUI({
                
                print("Data - Inputs [1/5]: Subperiod limit 1")
                
                if(is.null(input$in_id_time_limit_1)){
                        value <- def_list_data$time_limit_1 } else{
                                value <- input$in_id_time_limit_1
                        }
                
                if(input$in_id_time_subper_num == 2){
                        slider1limit_max <- input$in_id_time_range[2] - 1
                        slider1limit_min <- input$in_id_time_range[1]
                }
                
                if(input$in_id_time_subper_num >= 3){
                        
                        slider1limit_max <- input$in_id_time_limit_2 - 1
                        slider1limit_min <- input$in_id_time_range[1]
                        
                }
                
                MYsliderInput(
                        inputId = "in_id_time_limit_1",
                        label = "Select last year of Period 1",
                        sep = "",
                        min = input$in_id_time_range[1],
                        max = input$in_id_time_range[2],
                        from_min = slider1limit_min,
                        from_max = slider1limit_max,
                        step = 1,
                        value = value)
                
        })
        
        output$in_id_time_limit_2_ui <- renderUI({
                
                print("Data - Inputs [1/5]: Subperiod limit 2")
                
                if(is.null(input$in_id_time_limit_2)){
                        value <- def_list_data$time_limit_2 } else{
                                value <- input$in_id_time_limit_2
                        }
                
                if(input$in_id_time_subper_num == 3){
                        
                        slider2limit_max <- input$in_id_time_range[2] - 1
                        slider2limit_min <- input$in_id_time_limit_1 + 1
                        
                }        
                
                if(input$in_id_time_subper_num == 4){
                        
                        slider2limit_max <- input$in_id_time_limit_3 - 1
                        slider2limit_min <- input$in_id_time_limit_1 + 1
                        
                }
                
                MYsliderInput(
                        inputId = "in_id_time_limit_2",
                        label = "Select last year of Period 2",
                        sep = "",
                        min = input$in_id_time_range[1],
                        max = input$in_id_time_range[2],
                        from_min = slider2limit_min,
                        from_max = slider2limit_max,
                        step = 1,
                        value = value)
                
        })
        
        output$in_id_time_limit_3_ui <- renderUI({
                
                print("Data - Inputs [1/5]: Subperiod limit 3")
                
                if(is.null(input$in_id_time_limit_3)){
                        value <- def_list_data$time_limit_3 } else{
                                value <- input$in_id_time_limit_3
                        }
                
                slider3limit_max <- input$in_id_time_range[2] - 1
                slider3limit_min <- input$in_id_time_limit_2 + 1

                MYsliderInput(
                        inputId = "in_id_time_limit_3",
                        label = "Select last year of Period 3",
                        sep = "",
                        min = input$in_id_time_range[1],
                        max = input$in_id_time_range[2],
                        from_min = slider3limit_min,
                        from_max = slider3limit_max,
                        step = 1,
                        value = value)
                
        })
        
        # Update WDI variables
        observe({
                
                print("Data - Inputs [1/5]: Variables (observe)")
                
                updateSelectizeInput(
                        session = session,
                        inputId = 'in_id_wdi_vars',
                        selected = def_list_data$wdi_vars,
                        choices = var_vec,
                        server = FALSE)
        })


        # Upload external data
        output$in_id_ext_ui <- renderUI({
                
                print("Data - Inputs [1/5]: External data widget (renderUI)")
                
                ctry_aux <- isolate(
                        c(input$in_id_ctries_ctry, 
                        input$in_id_ctries_str,
                        input$in_id_ctries_asp,
                        input$in_id_ctries_reg)
                )

                isolate(
                        validate(
                        need(length(ctry_aux)>0 | length(input$in_id_ctries_group)>0 | input$in_id_ctries_all == TRUE | input$in_id_regs_all == TRUE, 
                                'Select at least one country and the time period before loading external data')
                        )
                )
                
                fileInput('in_id_ext', label = "Select file")  %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_text")
                        )
        })
        
        # Inputs reactive values
        
        # Create reactive values initially containing list with default inputs
        rv_input <- reactiveValues(
                
                # Country inputs
                ctries_ctry = def_list_data$ctries_ctry,
                ctries_str = def_list_data$ctries_str,
                ctries_asp = def_list_data$ctries_asp,
                ctries_reg = def_list_data$ctries_reg,
                ctries_group = def_list_data$ctries_group,
                ctries_all = def_list_data$ctries_all,
                regs_all = def_list_data$regs_all,
                
                # The following 2 inputs are a function of the above. They will be calculated later.
                ctries_select = character(0),
                ctries_select_iso2 = character(0), 
                
                # Time inputs
                time_range_start = def_list_data$time_range_start,
                time_range_end = def_list_data$time_range_end,
                time_subper = def_list_data$time_subper,
                time_subper_num = def_list_data$time_subper_num,
                time_limit_1 = def_list_data$time_limit_1,
                time_limit_2 = def_list_data$time_limit_2,
                time_limit_3 = def_list_data$time_limit_3,
                time_name_1 = def_list_data$time_name_1,
                time_name_2 = def_list_data$time_name_2,
                time_name_3 = def_list_data$time_name_3,
                time_name_4 = def_list_data$time_name_4,
                
                # Variable inputs
                wdi_vars = def_list_data$wdi_vars,
                # wdi_vars = NULL,
                # The following input is a function of the above. They will be calculated later.
                wdi_vars_code = character(0),
                
                # External files
                ext = NULL,
                
                # imf_vars = def_list_data$imf_vars,
                
                update_button_show = FALSE
        )
        
        # Create reactive values initially containing default dataframes
        
        rv_df <- reactiveValues(

                dat_wdi = initial_wdi_df,
                # dat_imf = empty_data_df,
                dat_ext = empty_data_df,
                dat_ext_last = empty_data_df,
                dat_all = initial_wdi_df,
                
                met_wdi = initial_wdi_df_meta,
                # met_imf = empty_metadata_df,
                met_ext = empty_metadata_df,
                met_ext_last = empty_metadata_df,
                met_all = initial_wdi_df_meta
                
        )

        # Load external data
        observeEvent(input$in_id_ext, {
                
                # If there is no file loaded, then stop
                req(input$in_id_ext)
                
                # Print messages with filter parameters
                cat("\n")
                print("******************************")
                print("*****EXTERNAL FILE LOADED*****")
                print("******************************")
                cat("\n")
                
                print("[1/7] External: upload parameters")
                print("Selected countries/aggregates (ISO-2):")
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == TRUE){
                        message_ctries <- c("All countries and aggregates")
                } 
                
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == FALSE){
                        message_ctries <- ctry_df[ctry_df[ , "country"] %in% c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, input$in_id_ctries_reg), "iso2c"]
                } 
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == FALSE){
                        message_ctries <- ctry_df[ctry_df[ , "country"] %in% c(ctry_vec, input$in_id_ctries_reg), "iso2c"]
                } 
                
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == TRUE){
                        message_ctries <- ctry_df[ctry_df[ , "country"] %in% c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, reg_vec), "iso2c"]
                }
                
                if(length(input$in_id_ctries_group)>0){
                        group_selection <- ctry_df[ctry_df[ , "income"] %in% input$in_id_ctries_group | ctry_df[ , "region"] %in% input$in_id_ctries_group, "iso2c"]
                        add <- setdiff(group_selection, message_ctries)
                        message_ctries <- c(message_ctries, add)
                }
                
                print(message_ctries)
                print(paste0("Selected year range: ", input$in_id_time_range[1], " - ", input$in_id_time_range[2]))

                
                # Read file
                file_extension <- file_ext(input$in_id_ext$name)
                print(paste0("Extension of the loaded file:", file_extension))
                
                if(file_extension == "txt"){
                        df_external <- read.table(input$in_id_ext$datapath, header = TRUE, fill = TRUE)
                }
                
                if(file_extension == "csv"){
                        df_external <- read.csv(input$in_id_ext$datapath, header = TRUE, fill = TRUE)
                        if(ncol(df_external)==1){
                                read.csv2(input$in_id_ext$datapath, header = TRUE, fill = TRUE)
                        }
                }
                
                if(file_extension == "xls" | file_extension == "xlsx"){
                        df_external <- read_excel(input$in_id_ext$datapath, col_names = TRUE)
                }
                
                if(file_extension == "dta") {df_external <- as.data.frame(read_stata(input$in_id_ext$datapath))}
                
                if (!(file_extension == "txt" | file_extension == "csv" | file_extension == "xls" | file_extension == "xlsx" | file_extension == "dta")){
                        print("Wrong file extension. The file must be one of the following formats: .txt, .xls, .xlsx, .csv, .dta")
                        output$in_id_ext_mes_ui <- renderUI({
                                return(tags$p("Wrong file extension. The file must be one of the following formats: .txt, .xls, .xlsx, .csv, .dta", 
                                        style = "color:red"))
                        })
                        return()
                }
                
                # Keep only the following variables
                print("[2/7] External: checking that the loaded file has all the required variables")
                headers <- names(df_external) %in% c("Var_name", "Var_code", "Description", "Units", "Ctry_iso", "Year", "Value", "Source", "Database", "Source_data", "Source_org")
                merge_variables <- names(df_external)[names(df_external) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Database")]
                df_external <- df_external[ , headers]
                
                # Check all necessary variables were included
                if((!"Var_name" %in% names(df_external)) |
                                (!"Units" %in% names(df_external)) |
                                (!"Ctry_iso" %in% names(df_external)) |
                                (!"Year" %in% names(df_external)) |
                                (!"Value" %in% names(df_external))
                ){
                        print("One of the following variables is missing from the uploaded dataset: Var_name, Units, Ctry_iso, Year, Value.")
                        print("*****************************")
                        cat("\n")
                        output$in_id_ext_mes_ui <- renderUI({
                                vec <- c("Var_name", "Units", "Ctry_iso", "Year", "Value")
                                missing_variables <- vec[!vec %in% names(df_external)]
                                text <- paste(missing_variables, collapse =", ")
                                text <- paste("Error in file upload. The following required variables are missing from the last uploaded dataset: ", 
                                        text, ". Data has not been included.",
                                        collapse ="")
                                return(tags$p(text, style = "color:red"))
                        })
                        return()}
                print("Loaded file has all the required variables")
                
                # To ensure we have the same number of rows per variable
                # create a dataset with num_ctries * num_years rows * num_variable
                # that includes only "Country", "Year" and "Variable" 
                
                print("[3/7] External: checking Country and Year parameters are defined")
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == TRUE){
                        ctries_select_iso3 <- ctry_df[ctry_df[ , "country"], "iso3c"]
                }
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == FALSE){
                        ctries_select <- c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, input$in_id_ctries_reg)
                        if(is.null(ctries_select)){
                                print("No countries selected")
                                print("*****************************")
                                cat("\n")
                                return()}
                        ctries_select_iso3 <- ctry_df[ctry_df[ , "country"] %in% ctries_select, "iso3c"]
                }
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == FALSE){
                        ctries_select <- c(ctry_vec, input$in_id_ctries_reg)
                        ctries_select_iso3 <- ctry_df[ctry_df[ , "country"] %in% ctries_select, "iso3c"]
                }
                
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == TRUE){
                        ctries_select <- c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, reg_vec)
                        ctries_select_iso3 <- ctry_df[ctry_df[ , "country"] %in% ctries_select, "iso3c"]
                }
                
                # Add countries from groups
                if(length(input$in_id_ctries_group)>0){
                        group_selection <- ctry_df[ctry_df[ , "income"] %in% input$in_id_ctries_group | ctry_df[ , "region"] %in% input$in_id_ctries_group, "iso3c"]
                        add <- setdiff(group_selection, ctries_select_iso3)
                        ctries_select_iso3 <- c(ctries_select_iso3, add)
                }
                
                years <- input$in_id_time_range[1]:input$in_id_time_range[2]
                
                # If no defined parameters for time range or countries, then return

                if(length(ctries_select_iso3)==0 | length(years)==0){
                        print("Missing Year or Country parameter")
                        print("*****************************")
                        cat("\n")
                        return()
                }
                
                print("[4/7] External: creating an enpty dataset with variables, years and countries")
                cat("\n")
                
                empty <- empty_panel_fun(df_external, ctries_select_iso3, years)
                
                cat("\n")
                print("[5/7] External: merging empty dataset with uploaded external data")
                
                # Include country names
                proc <- merge(x = empty,
                        y = ctry_df[ , c("country", "iso3c")],
                        by.x="Ctry_iso", 
                        by.y="iso3c",
                        all.x = TRUE)
                
                names(proc)[names(proc) == "country"] <- "Country"
                
                # Merge empty dataframe with uploaded data
                df_external <- merge(
                        x = proc,
                        y = df_external,
                        by = merge_variables,
                        all.x = TRUE)
                
                # If no source Dataset information, then add file name as dataset variable
                if(!"Database" %in% names(df_external)) {
                        df_external$Database <- input$in_id_ext$name
                } else {df_external$Database[is.na(df_external$Database)] <- input$in_id_ext$name}
                
                # Source (file name)
                df_external$Source <- input$in_id_ext$name
                
                # If no Var_code, then concatenate variable name and dataset variable and use as var_code
                if(!"Var_code" %in% names(df_external)) {
                        df_external$Var_code <- paste(df_external$Var_name, df_external$Database, sep="_")
                } else {
                        df_external$Var_code[is.na(df_external$Var_code)] <- paste(df_external$Var_name, df_external$Database, sep="_")
                }
                
                # If no Description, include columns with NA
                if(!"Description" %in% names(df_external)) {
                        df_external$Description <- as.character(NA)
                }
                
                # If no Source_data, include columns with NA
                if(!"Source_data" %in% names(df_external)) {
                        df_external$Source_data <- as.character(NA)
                }
                
                # If no Source_org, include columns with NA
                if(!"Source_org" %in% names(df_external)) {
                        df_external$Source_org <- as.character(NA)
                }
                
                # Generate country group string variable and country group number variable to sort
                df_external$Ctry_group_num[df_external$Country %in% input$in_id_ctries_ctry ] <- 1
                df_external$Ctry_group[df_external$Country %in% input$in_id_ctries_ctry ] <- "Analized"
                df_external$Ctry_group_num[df_external$Country %in% input$in_id_ctries_str] <- 2
                df_external$Ctry_group[df_external$Country %in% input$in_id_ctries_str] <- "Structural"
                df_external$Ctry_group_num[df_external$Country %in% input$in_id_ctries_asp] <- 3
                df_external$Ctry_group[df_external$Country %in% input$in_id_ctries_asp] <- "Aspirational"
                df_external$Ctry_group_num[df_external$Country %in% input$in_id_ctries_reg] <- 4
                df_external$Ctry_group[df_external$Country %in% input$in_id_ctries_reg] <- "Aggregate"
                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_ctries_ctry) & 
                                !(df_external$Country %in% input$in_id_ctries_str) & 
                                !(df_external$Country %in% input$in_id_ctries_asp) &
                                !(df_external$Country %in% input$in_id_ctries_reg) & 
                                !(df_external$Country %in% reg_vec)] <- 5
                df_external$Ctry_group[!(df_external$Country %in% input$in_id_ctries_ctry) & 
                                !(df_external$Country %in% input$in_id_ctries_str) & 
                                !(df_external$Country %in% input$in_id_ctries_asp) &
                                !(df_external$Country %in% input$in_id_ctries_reg) &
                                !(df_external$Country %in% reg_vec)] <- "Rest"
                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_ctries_ctry) & 
                                !(df_external$Country %in% input$in_id_ctries_str) & 
                                !(df_external$Country %in% input$in_id_ctries_asp) &
                                !(df_external$Country %in% input$in_id_ctries_reg) &
                                (df_external$Country %in% reg_vec)] <- 6
                df_external$Ctry_group[!(df_external$Country %in% input$in_id_ctries_ctry) & 
                                !(df_external$Country %in% input$in_id_ctries_str) & 
                                !(df_external$Country %in% input$in_id_ctries_asp) &
                                !(df_external$Country %in% input$in_id_ctries_reg) &
                                (df_external$Country %in% reg_vec)] <- "Rest (aggregate)"
                
                df_external <- df_external[order(
                        df_external$Var_name,
                        df_external$Var_code,
                        df_external$Ctry_group_num,
                        df_external$Ctry_iso,
                        df_external$Year), ]
                
                # Make sure Year and Value variables are numeric
                df_external <- transform(df_external, Year = as.numeric(Year), Value = as.numeric(Value))
                
                # Add empty variable Period 
                df_external$Period <- as.character(NA)
                df_external$Period_num <- as.numeric(NA)
                
                # Order columns and sort 
                df_external <- df_external %>% select("Var_name",
                        "Var_code",
                        "Units",
                        "Country",
                        "Ctry_iso",
                        "Ctry_group",
                        "Year",
                        "Period",
                        "Value",
                        "Database",
                        everything()
                )
                
                df_external <- df_external[order(
                        df_external$Var_name,
                        df_external$Var_code,
                        df_external$Ctry_group_num,
                        df_external$Ctry_iso,
                        df_external$Year), ]
                
                # Save dataframe
                rv_df$dat_ext_last <- df_external[ , c("Var_name",
                        "Var_code",
                        "Units",
                        "Country",
                        "Ctry_iso",
                        "Ctry_group",
                        "Year",
                        "Period",
                        "Value",
                        "Database",
                        "Period_num",
                        "Ctry_group_num",
                        "Source"
                )]
                
                print("[6/7] External: dataframe for uploaded external data has been created successfully")
                
                aux_met_ext_last <- df_external[ , c("Var_name",
                        "Var_code",
                        "Description",
                        "Units",
                        "Source_data",
                        "Source_org",
                        "Database",
                        "Source")]
                
                rv_df$met_ext_last <- aux_met_ext_last[!duplicated(aux_met_ext_last), ]
                
                print("[7/7] External: dataframe for metadata of uploaded external data has been created successfully")
                
                rv_input$ext <- input$in_id_ext$name
                
                print("*****************************")
                cat("\n")
                
        })
        
        # Create reactive values for plot lists
        rv_plots <- reactiveValues()
        
        # Listen to any changes in inputs
        # This reactive function will be invalidated (and invalidate everything that depends on it) 
        # when at least one input changes (it will not invalidate if, after changes, the input is returned to 
        # its original position), but it will not react if we press clear or update.
        # It will return one logical value (TRUE/FALSE), depending on whether any inputs changed 
        # with respect to what is stored in reactive values 
        listen_inputs <- eventReactive(
                
                # Invalidating vector
                c(
                        # Country inputs
                        input$in_id_ctries_ctry,
                        input$in_id_ctries_str,
                        input$in_id_ctries_asp,
                        input$in_id_ctries_reg,
                        input$in_id_ctries_group,
                        input$in_id_ctries_all,
                        input$in_id_regs_all,
                        
                        # Time inputs
                        input$in_id_time_range,
                        input$in_id_time_subper,
                        input$in_id_time_subper_num,
                        input$in_id_time_limit_1,
                        input$in_id_time_limit_2,
                        input$in_id_time_limit_3,
                        input$in_id_time_name_1,
                        input$in_id_time_name_2,
                        input$in_id_time_name_3,
                        input$in_id_time_name_4,
                        
                        # Variable inputs
                        input$in_id_wdi_vars,
                        # input$in_id_imf_vars,
                        
                        # External
                        input$in_id_ext
                        
                ) , {
                        
                        # Vector of changes                
                        no_changes_vec <- c(
                                
                                # Country inputs
                                setequal(rv_input$ctries_ctry, input$in_id_ctries_ctry),
                                setequal(rv_input$ctries_str, input$in_id_ctries_str),
                                setequal(rv_input$ctries_asp, input$in_id_ctries_asp),
                                setequal(rv_input$ctries_reg, input$in_id_ctries_reg),
                                setequal(rv_input$ctries_group, input$in_id_ctries_group),
                                setequal(rv_input$ctries_all, input$in_id_ctries_all),
                                setequal(rv_input$regs_all, input$in_id_regs_all),
                                
                                # Time inputs
                                setequal(rv_input$time_range_start, input$in_id_time_range[1]),
                                setequal(rv_input$time_range_end, input$in_id_time_range[2]),
                                setequal(rv_input$time_subper, input$in_id_time_subper),
                                setequal(rv_input$time_subper_num, input$in_id_time_subper_num),
                                setequal(rv_input$time_limit_1, input$in_id_time_limit_1),
                                setequal(rv_input$time_limit_2, input$in_id_time_limit_2),
                                setequal(rv_input$time_limit_3, input$in_id_time_limit_3),
                                setequal(rv_input$time_name_1, input$in_id_time_name_1),
                                setequal(rv_input$time_name_2, input$in_id_time_name_2),
                                setequal(rv_input$time_name_3, input$in_id_time_name_3),
                                setequal(rv_input$time_name_4, input$in_id_time_name_4),
                                
                                # Variable inputs
                                setequal(rv_input$wdi_vars, input$in_id_wdi_vars),
                                # setequal(rv_input$imf_vars, input$in_id_imf_vars),
                                
                                # External data
                                is.null(rv_input$ext)

                        )

                        # If all elements in vector no_changes_vec are TRUE, then return FALSE, else TRUE

                        return(!all(no_changes_vec))
                        
                }, ignoreInit = TRUE)
        
        # Show update button
        observeEvent(listen_inputs(), {
                
                rv_input$update_button_show <- FALSE
                if(listen_inputs()){
                        rv_input$update_button_show <- TRUE
                }

                # If at least one input changed, show update button
                if(rv_input$update_button_show){
                        print("Data - Inputs [2/5]: A data input has been modified: show update button")
                        shinyjs::show("in_id_update")
                } else {
                        print("Data - Inputs [2/5]: Data inputs are updated: hide update button")        
                        shinyjs::hide("in_id_update")}
                
                
        }, ignoreInit = TRUE)
        
        # Print in console when data_update button is pressed
        observeEvent(input$in_id_update,{
                
                cat("\n")
                print("*********************")
                print("*****UPDATE DATA*****")
                print("*********************")
                cat("\n")
        })
        
        # Print in console when reset button is pressed
        observeEvent(input$in_id_reset_confirm,{
                
                cat("\n")
                print("*********************")
                print("*****RESET DATA*****")
                print("*********************")
                cat("\n")
        })
        
        # Update reactive values list when observing input$in_id_update 
        observeEvent(input$in_id_update,{

                print("Data - Inputs [3/5]: updating data input reactive values - all but variable (observeEvent)")
                
                # Country inputs
                rv_input$ctries_ctry <- input$in_id_ctries_ctry
                rv_input$ctries_str <- input$in_id_ctries_str
                rv_input$ctries_asp <- input$in_id_ctries_asp
                rv_input$ctries_reg <- input$in_id_ctries_reg
                rv_input$ctries_group <- input$in_id_ctries_group
                rv_input$ctries_all <- input$in_id_ctries_all
                rv_input$regs_all <- input$in_id_regs_all
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == TRUE){
                        rv_input$ctries_selected <- c(ctry_vec, reg_vec)
                } 
                
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == FALSE){
                        rv_input$ctries_select <- c(input$in_id_ctries_ctry ,
                                input$in_id_ctries_str,
                                input$in_id_ctries_asp,
                                input$in_id_ctries_reg)
                } 
                
                if(input$in_id_ctries_all == TRUE & input$in_id_regs_all == FALSE){
                        rv_input$ctries_select <- c(ctry_vec,
                                input$in_id_ctries_reg)
                }
                
                if(input$in_id_ctries_all == FALSE & input$in_id_regs_all == TRUE){
                        rv_input$ctries_select <- c(input$in_id_ctries_ctry ,
                                input$in_id_ctries_str,
                                input$in_id_ctries_asp,
                                reg_vec)
                } 
                
                # Add countries from groups
                if(length(input$in_id_ctries_group)>0){
                        group_selection <- ctry_df[ctry_df[ , "income"] %in% input$in_id_ctries_group | ctry_df[ , "region"] %in% input$in_id_ctries_group, "country"]
                        add <- setdiff(group_selection, rv_input$ctries_select)
                        rv_input$ctries_select <- c(rv_input$ctries_select, add)
                }
                
                rv_input$ctries_select_iso2 <- ctry_df[ctry_df[ , "country"] %in% rv_input$ctries_select, "iso2c"]
                
                
                # Time inputs
                rv_input$time_range_start <- input$in_id_time_range[1]
                rv_input$time_range_end <- input$in_id_time_range[2]
                rv_input$time_subper <- input$in_id_time_subper
                rv_input$time_subper_num <- input$in_id_time_subper_num
                rv_input$time_limit_1 <- input$in_id_time_limit_1
                rv_input$time_limit_2 <- input$in_id_time_limit_2
                rv_input$time_limit_3 <- input$in_id_time_limit_3
                rv_input$time_name_1 <- input$in_id_time_name_1
                rv_input$time_name_2 <- input$in_id_time_name_2
                rv_input$time_name_3 <- input$in_id_time_name_3
                rv_input$time_name_4 <- input$in_id_time_name_4
                
        })
        
        observeEvent(input$in_id_update,{
                
                print("Data - Inputs [3/5]: updating data input reactive values - variables (observeEvent)")
                
                # Variable inputs
                rv_input$wdi_vars <- input$in_id_wdi_vars
                rv_input$wdi_vars_code <- filter(var_df, name_indicator %in% input$in_id_wdi_vars) %>% pull(indicator)
                
                # rv_input$imf_vars <- input$in_id_imf_vars
        
        }, ignoreInit = TRUE)
        
        # Hide update button after updating
        observeEvent(input$in_id_update, {
                
                print("Data - Inputs [3/5]: data inputs are updated: hide update button")
                shinyjs::hide("in_id_update")
                
                # External data (reset appearance)
                output$in_id_ext_ui <- renderUI({
                        fileInput('in_id_ext', label = "Select file") %>%
                                shinyInput_label_embed(
                                        shiny_iconlink() %>%
                                                bs_attach_modal(id_modal = "help_text")
                                )
                })
                
        })
        
        # Warning asking if you want to delete data
        observeEvent(input$in_id_reset, {
                confirmSweetAlert(
                        session = session,
                        inputId = "in_id_reset_confirm",
                        type = "warning",
                        title = "Reset all inputs and delete all loaded data?",
                        closeOnClickOutside = TRUE
                )
        })
        
        # Reseting
        observeEvent(input$in_id_reset_confirm, {
                
                if(input$in_id_reset_confirm){
                        
                        print("Data - Inputs [3/5]: reseting data input reactive values and input widgets")
                
                        # Reset reactive value list
                        
                                # Country inputs
                                rv_input$ctries_ctry <- reset_list$ctries_ctry
                                rv_input$ctries_str <- reset_list$ctries_str
                                rv_input$ctries_asp <- reset_list$ctries_asp
                                rv_input$ctries_reg <- reset_list$ctries_reg
                                rv_input$ctries_group <- reset_list$ctries_group
                                rv_input$ctries_all <- reset_list$ctries_all
                                rv_input$regs_all <- reset_list$regs_all
                                rv_input$ctries_select <- reset_list$ctries_select
                                rv_input$ctries_select_iso2 <- reset_list$ctries_select_iso2
                                
                                # Time inputs
                                rv_input$time_range_start <- reset_list$time_range_start
                                rv_input$time_range_end <- reset_list$time_range_end
                                rv_input$time_subper <- reset_list$time_subper
                                rv_input$time_subper_num <- reset_list$time_subper_num
                                rv_input$time_limit_1 <- reset_list$time_limit_1
                                rv_input$time_limit_2 <- reset_list$time_limit_2
                                rv_input$time_limit_3 <- reset_list$time_limit_3
                                rv_input$time_name_1 <- reset_list$time_name_1
                                rv_input$time_name_2 <- reset_list$time_name_2
                                rv_input$time_name_3 <- reset_list$time_name_3
                                rv_input$time_name_4 <- reset_list$time_name_4
                                
                                # Variable inputs
                                rv_input$wdi_vars <- reset_list$wdi_vars
                                rv_input$wdi_vars_code <- reset_list$wdi_vars_code
                                # rv_input$imf_vars <- reset_list$imf_vars
                                
                                # Show update button
                                rv_input$update_button_show <- reset_list$update_button_show
                                
                                # Dataframes
                                rv_df$dat_wdi <- empty_data_df
                                # rv_df$dat_imf <- empty_data_df
                                rv_df$dat_ext <- empty_data_df
                                rv_df$dat_ext_last <- empty_data_df
                                rv_df$dat_all <- empty_data_df
                                rv_df$met_wdi <- empty_metadata_df
                                # rv_df$met_imf <- empty_metadata_df
                                rv_df$met_ext <- empty_metadata_df
                                rv_df$met_ext_last <- empty_metadata_df
                                rv_df$met_all <- empty_metadata_df
                        
                        # Reset input widgets
                        
                                # Country variables
                                
                                # Country of analysis
                                updatePickerInput(
                                        session = session,
                                        inputId = 'in_id_ctries_ctry',
                                        label = "Select country of analysis",
                                        choices = flags_df$country,
                                        choicesOpt = list(content =  
                                                        mapply(flags_df$country, flags_df$URL, FUN = function(country, flagUrl) {
                                                                HTML(paste(
                                                                        tags$img(src = flagUrl, width = 20, height = 15),
                                                                        country
                                                                ))
                                                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                        ),
                                        selected = reset_list$ctries_ctry,
                                        options = list(size = 15, `live-search` = TRUE))
                                
                                # Structural comparators
                                updateSelectizeInput(
                                        session = session,
                                        inputId = 'in_id_ctries_str',
                                        choices = ctry_vec[!ctry_vec %in% c(reset_list$ctries_ctry , reset_list$ctries_asp)],
                                        selected = reset_list$ctries_str)
                                
                                # Aspirational comparators
                                updateSelectizeInput(
                                        session = session,
                                        inputId = 'in_id_ctries_asp',
                                        choices = ctry_vec[!ctry_vec %in% c(reset_list$ctries_ctry , reset_list$ctries_str)],
                                        selected = reset_list$ctries_asp)
                                
                                # Regions
                                updateSelectizeInput(
                                        session = session,
                                        inputId = 'in_id_ctries_reg',
                                        choices = reg_vec,
                                        selected = reset_list$ctries_reg)
                                
                                # Update country groups
                                output$in_id_ctries_group_ui <- renderUI({
                                        
                                        selectizeInput(
                                                inputId = "in_id_ctries_group",
                                                label = "Groups of countries",
                                                choices = group_list,
                                                multiple = TRUE,
                                                selected = def_list_data$ctries_asp) %>%
                                                shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                                bs_attach_modal(id_modal = "help_text_group")
                                                )
                                        
                                })

                                # All countries
                                updateMaterialSwitch(
                                        session = session,
                                        inputId = "in_id_ctries_all",
                                        value = reset_list$ctries_all)
                                
                                # All aggregates
                                updateMaterialSwitch(
                                        session = session,
                                        inputId = "in_id_regs_all",
                                        value = reset_list$regs_all)
                                
                                # Time range
                                updateSliderInput(
                                        session = session,
                                        inputId = "in_id_time_range",
                                        min = def_list_data$time_range_min,
                                        max = def_list_data$time_range_max,
                                        step = 1,
                                        value = c(reset_list$time_range_start, reset_list$time_range_end))
                                
                                # Time subperiods
                                updateMaterialSwitch(
                                        session = session,
                                        inputId = "in_id_time_subper",
                                        value = reset_list$time_subper)
                                
                                # Time subperiods number
                                updateNumericInput(
                                        session = session,
                                        inputId = "in_id_time_subper_num",
                                        label = "Number of subperiods (min=2; max=4)",
                                        min = 2,
                                        max = 4,
                                        value = reset_list$time_subper_num)
                                
                                # Time subperiods name 1
                                updateTextInput(
                                        session = session,
                                        inputId = "in_id_time_name_1",
                                        label="Period 1 name",
                                        value = reset_list$time_name_1)
                                
                                # Time subperiods name 2
                                updateTextInput(
                                        session = session,
                                        inputId = "in_id_time_name_2",
                                        label="Period 2 name",
                                        value = reset_list$time_name_2)
        
                                # Time limit 1
                                updateSliderInput(
                                        session = session,
                                        inputId = "in_id_time_limit_1",
                                        min = reset_list$time_range_start,
                                        max = reset_list$time_limit_2 - 1,
                                        step = 1,
                                        value = reset_list$time_limit_1)
                                
                                
                                # Time subperiods name 3
                                updateTextInput(
                                        session = session,
                                        inputId = "in_id_time_name_3",
                                        label="Period 2 name",
                                        value = reset_list$time_name_3)
                                
                                
                                # Time limit 2
                                updateSliderInput(
                                        session = session,
                                        inputId = "in_id_time_limit_2",
                                        min = reset_list$time_limit_1 + 1,
                                        max = reset_list$time_limit_3 - 1,
                                        step = 1,
                                        value = reset_list$time_limit_2)
                                
                                # Time subperiods name 4
                                updateTextInput(
                                        session = session,
                                        inputId = "in_id_time_name_4",
                                        label="Period 4 name",
                                        value = reset_list$time_name_4)
                                
                                # Time limit 3
                                updateSliderInput(
                                        session = session,
                                        inputId = "in_id_time_limit_3",
                                        min = reset_list$time_limit_2,
                                        max = reset_list$time_range_end - 1,
                                        step = 1,
                                        value = reset_list$time_limit_3)
                                
                                # WDI variables
                                updateSelectizeInput(
                                        session = session,
                                        inputId = "in_id_wdi_vars",
                                        choices = var_vec,
                                        server = FALSE,
                                        selected = reset_list$wdi_vars)
                                
                                # IMF variables
                                # updateSelectizeInput(
                                #         session = session,
                                #         inputId = "in_id_imf_vars",
                                #         choices = NULL,
                                #         server = TRUE,
                                #         selected = reset_list$imf_vars)
                                

                                # External data
                                output$in_id_ext_ui <- renderUI({
                                        
                                        ctry_aux <- c(input$in_id_ctries_ctry, 
                                                input$in_id_ctries_str,
                                                input$in_id_ctries_asp,
                                                input$in_id_ctries_reg)
                                        
                                        validate(
                                                need(length(ctry_aux)>0 | length(input$in_id_ctries_group)>0 | input$in_id_ctries_all == TRUE | input$in_id_regs_all == TRUE, 
                                                        'Select at least one country and the time period before loading external data')
                                        )
                                        
                                        fileInput('in_id_ext', label = "Select file") %>%
                                                shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                                bs_attach_modal(id_modal = "help_text")
                                                )
                                        
                                })
                                
                                rv_input$ext <- NULL
                }
        })

        # Reactive data_wdi
        dat_wdi <- eventReactive(input$in_id_update,{
                
                print("Data - Inputs [4/5]: downloading WDI data")

                # Download data from WDI
                data <- nice_wdi_fun(
                        countries_ctry = rv_input$ctries_ctry,
                        countries_str = rv_input$ctries_str,
                        countries_asp = rv_input$ctries_asp,
                        countries_reg = rv_input$ctries_reg,
                        countries_group = rv_input$ctries_group,
                        countries_all = rv_input$ctries_all,
                        regions_all = rv_input$regs_all,
                        variables = rv_input$wdi_vars_code,
                        start_year = rv_input$time_range_start, 
                        end_year = rv_input$time_range_end)
                
                return(data)
                
        }, ignoreNULL = TRUE)                          # IgnoreNULL TRUE to avoid this running when the app starts
        

        
        # Merge datasets
        observeEvent(input$in_id_update,{
                
                print("Data - Inputs [5/5]: appending WDI and external data...")
                
                # Append last external dataset to all loaded external datasets
                rv_df$dat_ext <- plyr::rbind.fill(rv_df$dat_ext, rv_df$dat_ext_last)
                rv_df$dat_ext <- rv_df$dat_ext[!duplicated(rv_df$dat_ext), ]
                rv_df$dat_ext_last <- empty_data_df
                
                rv_df$met_ext <- plyr::rbind.fill(rv_df$met_ext, rv_df$met_ext_last)
                rv_df$met_ext <- rv_df$met_ext[!duplicated(rv_df$met_ext), ]
                rv_df$dat_ext_last <- empty_metadata_df
                
                # Update WDI dataset
                rv_df$dat_wdi <- dat_wdi()

                # Merge WDI data with all external data
                rv_df$dat_all <- plyr::rbind.fill(rv_df$dat_wdi, rv_df$dat_ext)
                
                # Add period and period number variables
                if(rv_input$time_subper){
                        rv_df$dat_all <- add_periods_fun(
                                dataframe = rv_df$dat_all,
                                time_range_start = rv_input$time_range_start,
                                time_range_end = rv_input$time_range_end,
                                time_subper_num = rv_input$time_subper_num,
                                time_limit_1 = rv_input$time_limit_1,
                                time_limit_2 = rv_input$time_limit_2,
                                time_limit_3 = rv_input$time_limit_3,
                                time_name_1 = rv_input$time_name_1,
                                time_name_2 = rv_input$time_name_2,
                                time_name_3 = rv_input$time_name_3,
                                time_name_4 = rv_input$time_name_4
                                )
                }
                
                # Order columns and sort 
                rv_df$dat_all <- rv_df$dat_all %>% select("Var_name",
                        "Var_code",
                        "Units",
                        "Country",
                        "Ctry_iso",
                        "Ctry_group",
                        "Year",
                        "Period",
                        "Value",
                        "Database",
                        everything()
                )
                
                rv_df$dat_all <- rv_df$dat_all[order(
                        rv_df$dat_all$Var_name,
                        rv_df$dat_all$Var_code,
                        rv_df$dat_all$Ctry_group_num,
                        rv_df$dat_all$Ctry_iso,
                        rv_df$dat_all$Year), ]
                
        }, ignoreNULL = TRUE)                          # IgnoreNULL to get it running when the app first starts
        
        ### Render table ----     
        output$out_data_table <- renderDataTable({
                
                cat("\n")
                print("****************************")
                print("Data - table: start renderUI")
                
                input$in_id_update
                rendered_table <- rv_df$dat_all

                # Eliminate variables we don't want to present
                
                rendered_table <- rendered_table %>% select("Var_name",
                        "Var_code",
                        "Units",
                        "Country",
                        "Ctry_iso",
                        "Ctry_group",
                        "Year",
                        "Period",
                        "Value",
                        "Database")
                
                print("Data - table: end renderUI")
                print("****************************")
                cat("\n")

                # Datatable
                datatable(rendered_table,
                        rownames = FALSE,
                        options = list(pageLength = 25,
                                columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )%>%
                        formatCurrency(columns = "Value",
                                currency = "", 
                                interval = 3, 
                                mark = ",",
                                digits = 1)
        })
        
        # Hide update button
        observeEvent(input$in_id_reset_confirm, {
                
                print("Data - Inputs [3/5]: data inputs are updated (empty): hide update button")
                
                shinyjs::hide("in_id_update")
        
        })
        
        # Download data handler
        output$out_download_data <- downloadHandler(
                
                filename = function() {
                        paste('raw_data', '.csv', sep='')
                },
                
                content = function(file) {
                        write.csv(rv_df$dat_all, file, row.names = FALSE)
                }
        )
        
        # Event reactive metadata
        metadata <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm),{
                
                print("Metadata - table: retrieve data")
                
                if(plyr::empty(rv_df$dat_all)){
                        print("Empty dataset. Return empty metadata")
                        rv_df$met_all <- empty_metadata_df
                        print("Data - table: end renderUI")
                        print("****************************")
                        cat("\n")
                        return(rv_df$met_all)}

                # Merge data for WDI variables
                
                rv_df$met_wdi <- nice_wdi_fun_meta(rv_df$dat_wdi)
                rv_df$met_all <- rbind(rv_df$met_wdi, rv_df$met_ext)

                print("Metadata - table: end retrieve data")

                return(rv_df$met_all)
                
        })
        
        ### Render metadata table ----        
        output$out_metadata_table <- renderDataTable({
                
                cat("\n")
                print("****************************")
                print("Metadata - table: start renderUI")
                
                torender <- datatable(metadata(), rownames = FALSE, options = list(
                        autoWidth = TRUE,
                        columnDefs = list(list(targets = c(2,5), width = '300px'),
                                list(targets = c(0,1), width = '50px')
                                ),
                        scrollX = TRUE)
                        )
                
                print("Metadata - table: end renderUI")
                print("****************************")
                cat("\n")
                
                return(torender)
                
        })
        
        # Download metadata handler
        output$out_download_metadata <- downloadHandler(
                
                filename = function() {
                        paste('metadata', '.csv', sep='')
                },
                
                content = function(file) {
                        write.csv(rv_df$met_all, file, row.names = FALSE)
                }
        
        )
        
        ### Render summary table ---- 
        output$out_summary_title <- renderUI({
                
                if(nrow(rv_df$dat_all)==0){return()}
                return(h4("Summary"))
                
        })
        
        
        # Render summary table
        output$out_summary <- renderUI({
                
                cat("\n")
                print("****************************")
                print("Summary - table: start renderUI")
                
                if(nrow(rv_df$dat_all)==0){
                        print("Summary - table: end renderUI")
                        print("****************************")
                        cat("\n")
                        return()}
                
                # Reshape and select data
                aux <- reshape2::dcast(rv_df$dat_all, Ctry_iso + Year ~ Var_name, value.var="Value")
                
                print("Summary - table: end renderUI")
                print("****************************")
                cat("\n")
                
                # Print summary
                return(
                        
                        print(dfSummary(aux), 
                                headings = FALSE, 
                                method = 'render', 
                                bootstrap.css = FALSE,
                                graph.magnif = 0.75, 
                                valid.col = FALSE, 
                                style = "grid")
                        
                )

                
        })
        
        # Render summary missing values plot title
        output$out_summary_missing_title <- renderUI({
                
                if(nrow(rv_df$dat_all)==0){return()}
                return(h4("Missing values"))
                
        })
        
        # Render summary missing values plot
        output$out_summary_missing <- renderPlot({
                
                cat("\n")
                print("****************************")
                print("Summary - plot: start renderUI")
                
                if(nrow(rv_df$dat_all)==0){
                        print("Summary - plot: end renderUI")
                        print("****************************")
                        cat("\n")
                        return()}
                
                
                testing.NA <- matrix(ncol= rv_input$time_range_end - rv_input$time_range_start+1, 
                        nrow=length(unique(rv_df$dat_all$Var_code)))
                
                # Reshape and select data
                aux <- reshape2::dcast(rv_df$dat_all, Ctry_iso + Year ~ Var_name, value.var="Value")
                
                for (i in 3:dim(aux)[2])
                {
                        testing.NA[i-2,] <- tapply(aux[[i]], aux$Year, function(x) 100* sum(is.na(x)) / length(x))
                }
                
                dimnames(testing.NA) <- list(
                        str_wrap(names(aux)[3:length(names(aux))], width = 20),
                        sort(unique(aux$Year)))
                
                testing.NA <- t(testing.NA)
                testing.NA <- testing.NA[ , ncol(testing.NA):1] # Change order of variable so that they appear alphabetically from A to Z

                my.theme <- rasterTheme(region=rev(brewer.pal(10,'RdYlGn')))

                my.theme.2 <- rev(brewer.pal(10, "RdYlGn"))
                
                my.at <- seq(0, 100, by=length(my.theme.2))
                my.ckey <- list(at=my.at, 
                        col=my.theme.2,
                        space="bottom",
                        title = list(label = "Percentage of missing values", cex=1),
                        labels=list(at=my.at, labels=my.at)
                        )
                
                lattice.options(axis.padding=list(factor=0.5)) # Eliminate outside border
                
                p <- levelplot(testing.NA, 
                        scales=list(x=list(rot=90), 
                                tck = c(0,0)),
                        main = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        axis.padding = 0.5,
                        border = "black",
                        aspect="fill",
                        par.settings=my.theme,
                        at=my.at, 
                        colorkey=my.ckey)

                print("Summary - plot: end renderUI")
                print("****************************")
                cat("\n")
                
                p
        })
        
        ### Render country table ----      
        output$out_ctry_df <- renderDataTable({
                
                cat("\n")
                print("****************************")
                print("Countries - table: start renderUI")
                
                ctry_df <- ctry_df %>% arrange(factor(country, levels = c(ctry_vec, reg_vec)))
                ctry_df <- subset(ctry_df, select=-c(capital, longitude, latitude, lending))
                
                names(ctry_df)[names(ctry_df) == "iso3c"] <- "Iso3c"
                names(ctry_df)[names(ctry_df) == "iso2c"] <- "Iso2c"
                names(ctry_df)[names(ctry_df) == "country"] <- "Country/Region"
                names(ctry_df)[names(ctry_df) == "region"] <- "Region"
                names(ctry_df)[names(ctry_df) == "income"] <- "Income"
                
                rv_df$country_list <- ctry_df 
                
                aux <- ctry_df[ctry_df$Region != "Aggregates", ]
                
                torender <- datatable(aux,
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )
                
                print("Countries - table: end renderUI")
                print("****************************")
                cat("\n")
                
                return(torender)
                
        })
        
        # Render countries table        
        output$out_ctry_df_agg <- renderDataTable({
                
                cat("\n")
                print("****************************")
                print("Aggregates - table: start renderUI")
                
                aux <- rv_df$country_list[rv_df$country_list$Region == "Aggregates", ]
                
                torender <- datatable(aux,
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )
                
                print("Aggregates - table: end renderUI")
                print("****************************")
                cat("\n")
                
                return(torender)
        })
        
        # Download countries
        output$out_download_ctry_df <- downloadHandler(
                filename = function() {
                        paste('country_list', '.csv', sep='')
                },
                content = function(file) {
                        write.csv(rv_df$country_list, file, row.names = FALSE)
                }
        )
        
        # Render variables table        
        output$out_var_df <- renderDataTable({
                
                cat("\n")
                print("****************************")
                print("Variables - table: start renderUI")
                
                aux <- var_df
                names(aux)[names(aux) == 'indicator'] <- 'Var_code'
                names(aux)[names(aux) == 'name'] <- 'Var_name'
                names(aux)[names(aux) == 'description'] <- 'Description'
                names(aux)[names(aux) == 'sourceDatabase'] <- 'Source_data'
                names(aux)[names(aux) == 'sourceOrganization'] <- 'Source_org'
                
                aux <- aux %>% select(Var_name, Var_code, Description, Source_data, Source_org)
                rv_df$vars_WDI <- aux
                
                torender <- datatable(rv_df$vars_WDI,
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )
                
                print("Variables - table: end renderUI")
                print("****************************")
                cat("\n")
                
                return(torender)
        })
        
        # Download countries
        output$out_download_var_df <- downloadHandler(
                filename = function() {
                        paste('variables_WDI', '.csv', sep='')
                },
                content = function(file) {
                        write.csv(rv_df$vars_WDI, file, row.names = FALSE)
                }
        )

        
##************************************************************************************************** ----
        
        ## Bar plots - Single country tab ----
        
        ### Reactive values ----
        rv_bar <- reactiveValues(
                
                # Plot parameters
                vars = def_list_bar$vars,
                ctries_ctry = def_list_bar$ctries_ctry,
                time_range = def_list_bar$time_range,
                
                # Display parameters
                title = def_list_bar$title,
                yaxis = def_list_bar$yaxis,
                source = def_list_bar$source,
                data_labels = def_list_bar$data_labels,
                time_subper = def_list_bar$time_subper,
                time_subper_avg = def_list_bar$time_subper_avg,
                transform_zeros = def_list_bar$transform_zeros,
                transform_log = def_list_bar$transform_log,
                digits = def_list_bar$digits,
                digits_y = def_list_bar$digits_y,
                color = def_list_bar$color,
                font = def_list_bar$font

        )

        ### Update inputs ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {
                
                print("Plots - Bar [1]: Updating inputs")

                # Country input
                aux_ctry <- unique(rv_df$dat_all$Country)
                flags_df <- filter(flags_df, country %in% aux_ctry)
                aux_ctry_group <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                flags_df <- merge(
                        x = flags_df,
                        y = aux_ctry_group,
                        by.x = c("country"),
                        by.y = c("Country"),
                        all.x = TRUE)

                flags_df <- flags_df[order(
                        flags_df$Ctry_group_num,
                        flags_df$country), ]

                flags_df$Ctry_slash_Group <- paste(flags_df$country, flags_df$Ctry_group, sep = " | ")

                updatePickerInput(
                        session = session,
                        inputId = "gr_bar_id_ctries_ctry",
                        choices = flags_df$country,
                        choicesOpt = list(content =
                                        mapply(flags_df$Ctry_slash_Group, flags_df$URL, FUN = function(x, y) {
                                                HTML(paste(
                                                        tags$img(src = y, width = 20, height = 15),
                                                        x
                                                ))
                                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                        ),
                        selected = aux_ctry[1],
                        options = list(size = 15,
                                `live-search` = TRUE)
                )

                # Variable input
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                updatePickerInput(
                        session = session,
                        inputId = 'gr_bar_id_vars',
                        label = "Select variables",
                        choices = aux_var,
                        selected = aux_var,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Time input
                updateSliderInput(
                        session = session,
                        inputId = "gr_bar_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end))

        })
        
        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
                
                print("Plots - Bar [2]: Updating reactive values [changes in raw data]")
                
                # Country (if previously selected country is in new dataset, keep selection, else select first country from vector)
                aux_ctry <- unique(rv_df$dat_all$Country)
                rv_bar$ctries_ctry <- aux_ctry[1]

                # Variables (select all variables from new dataset)
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                rv_bar$vars <- aux_var
                
                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_bar$time_range <- aux_year

        }, ignoreInit = TRUE)
        
        ###  Update reactive values to changes in bar plot parameters ----
        observeEvent(c(input$gr_bar_id_update),{
                
                print("Plots - Bar [2]: Updating reactive values [update bar plot parameters]")
                
                # Plot parameters
                rv_bar$vars <- input$gr_bar_id_vars
                rv_bar$ctries_ctry <- input$gr_bar_id_ctries_ctry
                rv_bar$time_range <- input$gr_bar_id_time_range
                
                # Display parameters
                rv_bar$title <- input$gr_bar_id_title
                rv_bar$yaxis <- input$gr_bar_id_yaxis
                rv_bar$source <- input$gr_bar_id_source
                rv_bar$data_labels <- input$gr_bar_id_data_labels
                rv_bar$time_subper <- input$gr_bar_id_time_subper
                rv_bar$time_subper_avg <- input$gr_bar_id_time_subper_avg
                rv_bar$transform_zeros <- input$gr_bar_id_transform_zeros
                rv_bar$transform_log <- input$gr_bar_id_transform_log
                rv_bar$digits <- input$gr_bar_id_digits
                rv_bar$digits_y <- input$gr_bar_id_digits_y
                rv_bar$color <- input$gr_bar_id_color
                rv_bar$font <- input$gr_bar_id_font
        
        }, ignoreInit = TRUE)

        ### Plots ----

        # Prep data
        prepped_data_bar <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_bar_id_update),{
                
                print("Plots - Bar [3]: preparing data")
                        
                if(nrow(rv_df$dat_all)==0){return()}        

                graph_input <- prep_data_for_graphs(
                        df = rv_df$dat_all,
                        vars = rv_bar$vars,
                        ctries = rv_bar$ctries_ctry,
                        t_start = rv_bar$time_range[1],
                        t_end = rv_bar$time_range[2]
                )
                
                # Create subperiod variables to include rectangles
                if(rv_input$time_subper & rv_bar$time_subper){
                        subper_list <- subper_rectangles_fun(
                                subper_num = rv_input$time_subper_num,
                                time_range = rv_bar$time_range,
                                time_lim_1 = rv_input$time_limit_1,
                                time_lim_2 = rv_input$time_limit_2,
                                time_lim_3 = rv_input$time_limit_3,
                                time_name_1 = rv_input$time_name_1,
                                time_name_2 = rv_input$time_name_2,
                                time_name_3 = rv_input$time_name_3,
                                time_name_4 = rv_input$time_name_4
                        )
                        
                        rv_bar$rectangle_text <- subper_list[[1]]
                        rv_bar$vertical_lines <- subper_list[[2]]
                }
                
                return(graph_input)

        })

        # Create plots
        createUI_bar <- function(table) {
                
                rv_plots$bar_counter <- rv_plots$bar_counter + 1
                print(paste0("Render bar plot ", rv_plots$bar_counter, "/", rv_plots$bar_counter_total))

                rectangle_text <- rv_bar$rectangle_text
                vertical_lines <- rv_bar$vertical_lines

                if(all(is.na(table$Value))){return()}
                
                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""

                # Input for title, subtitle Y-axis and source
                title_text <- if(rv_bar$title){
                        paste0(unique(table$Var_name), " - ", unique(table$Country))}

                yaxis_units <- if(rv_bar$yaxis){
                                if(is.na(unique(table$Units))){NULL} else {unique(table$Units)}
                        } else {NULL}

                if(rv_bar$source){
                        graph_source <- unique(table$Database)
                        if(graph_source=="WDI"){
                                graph_source <- "World Development Indicators"}
                        }

                # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                intervals <- ifelse(rv_bar$time_range[2] - rv_bar$time_range[1] < 30, 1, 2)

                # Transform data: divide by trillions/billions/millions/thousands
                for (i in 4:1){
                        if(rv_bar$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                                if(rv_bar$title) {
                                        subtitle_text <- paste(
                                                c(subtitle_text, units_zeros[5 - i]),
                                                collapse = "")
                                        separator <- if(subtitle_text == ""){""} else {", "}
                                        if(rv_bar$yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, units_zeros[5 - i]),
                                                        collapse = separator)
                                        }
                                }
                                table$Value <- table$Value/(10^(3*i))
                        }
                }

                # Log transform
                if(rv_bar$transform_log & min(table$Value, na.rm = TRUE)>0){
                        subtitle_text <- paste(
                                c(subtitle_text, " (Log transformation)"),
                                collapse = "")
                        if(rv_bar$yaxis){
                                yaxis_units <- paste(
                                        c(yaxis_units, " (Log transformation)"),
                                        collapse = "")
                        }
                        table$Value <- log(table$Value)
                }
                
                # Title paragraph
                title_text_paragraph <- str_wrap(title_text, width = 130)

                # Actually create plot
                p <- ggplot(table, aes(x = Year, y = Value)) +
                        # Define type of graph: bar
                        geom_bar(stat = "identity",
                                fill = rv_bar$color,
                                colour = "black", 
                                na.rm = TRUE
                        )+
                        # Include title, subtitle, source and Y-axis title
                        labs(title = title_text_paragraph,
                                subtitle = subtitle_text,
                                caption = if(rv_bar$source){paste("Source: ", graph_source, ".", sep = "")},
                                y = if(rv_bar$yaxis){yaxis_units} else {NULL}
                        )+
                        # Aesthetics
                        theme_minimal() +
                        theme(panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(linetype = "dashed"),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12,  family = rv_bar$font)
                        )+
                        # Define intervals of Year axis
                        scale_x_continuous(name = "",
                                breaks = seq(rv_bar$time_range[1],
                                        rv_bar$time_range[2],
                                        by = intervals)
                        )+
                        coord_cartesian(xlim = c(rv_bar$time_range[1], rv_bar$time_range[2]))


                # Include data labels
                if(rv_bar$data_labels){p <- p + geom_text(
                        aes(
                                x = Year,
                                y = Value,
                                label = format(round(as.numeric(Value), rv_bar$digits),
                                        nsmall = rv_bar$digits, big.mark = ",")),
                        vjust = ifelse(table$Value < 0 , 1.5, -0.5),
                        hjust = 0.5,
                        size = 3,
                        family = rv_bar$font,
                        na.rm = TRUE)
                }

                # Increase range of Y axis to make room for the box indicating subperiod
                y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range <- y_max - y_min

                # Include thousands separating comma in Y-axis
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(rv_bar$digits_y), big.mark = ","),
                        breaks = pretty_breaks(),
                        limits = c(y_min, y_max + (y_range*0.1))
                )

                # Get new axis limits that will be used as inputs to define the size of the subperiod rectangles
                y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range_new <- y_max - y_min

                #Define other parameters that will be used as inputs to define the size of the subperiod rectangles
                size_factor <- 0.08
                ALPHA <- 0.15

                # Subperiod rectangles, their labels and the dotted lines separating
                if(rv_input$time_subper & rv_bar$time_subper){
                        p <- p +
                                geom_rect(data = rectangle_text,
                                        aes(NULL, NULL, xmin = my_min_fun(Year_start), xmax = my_max_fun(Year_end)),
                                        ymin = y_max_new - y_range_new * size_factor ,
                                        ymax = y_max_new,
                                        colour = NA,
                                        fill="grey",
                                        alpha = 0.5
                                )+
                                geom_label(data = rectangle_text,
                                        aes(x = Year_start + (Year_end - Year_start) / 2,
                                                y = y_max_new * ALPHA + (y_max_new - y_range_new * size_factor) * (1 - ALPHA),
                                                label = Period,
                                                family = rv_bar$font
                                        ),
                                        size = 3.3,
                                        fill = "grey",
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom")

                        for (i in vertical_lines) {
                                p <- p + geom_segment(x = i,
                                        y = y_max_new,
                                        xend = i,
                                        yend = y_max_new - y_range_new * size_factor,
                                        colour = "white",
                                        size = 1,
                                        alpha = 1) +
                                        geom_segment(x = i,
                                                y = y_max_new - y_range_new * size_factor,
                                                xend = i,
                                                yend = -Inf,
                                                colour = "grey",
                                                linetype = "dotted")
                        }
                }

                # Period average lines
                if(rv_bar$time_subper_avg){
                        if(rv_input$time_subper & rv_bar$time_subper){
                                vec_average <- table %>%
                                        group_by(Period) %>%
                                        summarise_at(vars(Value),
                                                list(Value_avg = mean),
                                                na.rm = TRUE)
                                vec_average <- as.data.frame(vec_average)

                                rectangle_text <- merge(x = rectangle_text,
                                        y = as.data.frame(vec_average),
                                        by = "Period")

                                for(i in 1:nrow(rectangle_text)){
                                        yvalue <- vec_average %>% filter(Period == rectangle_text$Period[i]) %>% select(Value_avg)
                                        period_average <- yvalue[[1]]
                                        p <- p + geom_segment(x = rectangle_text$Year_start[i],
                                                y = period_average,
                                                xend = rectangle_text$Year_end[i],
                                                yend = period_average)
                                }
                                p <- p + geom_label(
                                        data = rectangle_text,
                                        aes(x = Year_start+(Year_end-Year_start) / 2,
                                                y = (y_max_new - y_range_new * size_factor) - (y_max_new - (y_max_new * ALPHA + (y_max_new - y_range_new * size_factor) * (1 - ALPHA))),
                                                label = paste("Avg. (", round((Year_start + 0.5), 0), "-", round((Year_end - 0.5), 0) , "): ", format(round(as.numeric(Value_avg), rv_bar$digits), nsmall = rv_bar$digits, big.mark = ","), sep = ""),
                                                family = rv_bar$font
                                        ),
                                        size = 3.3,
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom")

                        } else {
                                # Full period average
                                vec_average <- table %>%
                                        summarise_at(vars(Value),
                                                list(Value_avg = mean),
                                                na.rm = TRUE)
                                yvalue <- vec_average %>% select(Value_avg)
                                yvalue <- yvalue[[1]]
                                p <- p + geom_segment(
                                        x = rv_bar$time_range[1],
                                        y = yvalue,
                                        xend = rv_bar$time_range[2],
                                        yend = yvalue) +

                                        geom_label(
                                        aes(x = rv_bar$time_range[1] + (rv_bar$time_range[2] - rv_bar$time_range[1]) / 2,
                                                y = (y_max_new - y_range_new * size_factor) - (y_max_new - (y_max_new * ALPHA + (y_max_new - y_range_new * size_factor)*(1 - ALPHA))),
                                                label = paste("Avg. (", rv_bar$time_range[1], "-", rv_bar$time_range[2], "): ", format(round(as.numeric(yvalue), rv_bar$digits), nsmall = rv_bar$digits, big.mark = ","), sep = ""),
                                                family = rv_bar$font
                                        ),
                                        size = 3.3,
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom")
                        }
                }

                # Correct digits if numbers are repeated in Y-axis
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, rv_bar$digits_y)))
                digits <- rv_bar$digits_y

                while(yaxis_number_labels > yaxis_number_labels_obs){
                        digits <- digits + 1
                        p <- p + scale_y_continuous(name = yaxis_units,
                                labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                                breaks = pretty_breaks())

                        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                        x <- x[!is.na(x)]
                        yaxis_number_labels <- length(x)
                        yaxis_number_labels_obs <- length(unique(round(x, digits)))
                }

                # Append plot to the list of plots
                rv_plots$bar <- isolate(list.append(rv_plots$bar, p))
                
                # Append data
                table$Title <- if(rv_bar$title){title_text}else{""} 
                table$Subtitle <- if(rv_bar$title){subtitle_text}else{""} 
                table$Y_axis <- if(rv_bar$yaxis){yaxis_units}else{""}
                table$Source <- if(rv_bar$source){graph_source}else{""}

                # Select columns
                table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
                
                rv_plots$bar_data <- isolate(list.append(rv_plots$bar_data, table))

                # Show plot
                renderPlot(p)

        }

        # Call prep data and create plots
        output$gr_bar_out_plots <- renderUI({
                
                print("****************************")
                print("Plots - Bar: start renderUI")

                rv_plots$bar <- list()
                rv_plots$bar_data <- list()

                pd <- req(prepped_data_bar())
                
                rv_plots$bar_counter <- 0
                rv_plots$bar_counter_total <- length(isolate(pd))

                final <- isolate(tagList(map(pd, ~ createUI_bar(.))))
                
                print("Plots - Bar: finish renderUI")
                print("****************************")
                cat("\n")
                
                return(final)
                
        })

        ### Plots download handlers ----

        # Download plots as png zipped - large
        output$gr_bar_download_large <- downloadHandler(
                filename = 'gr_bar_out_plots_large.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()

                        for (i in 1:length(rv_plots$bar)){
                                name <- paste("barplot_large", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$bar[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("barplot_large", i, ".png", sep = ""))

                                name_data <- paste("barplot_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$bar_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("barplot_large_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

        # Download plots as png zipped - small
        output$gr_bar_download_small <- downloadHandler(
                filename = 'gr_bar_out_plots_small.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$bar)){
                                name <- paste("barplot_small", i, ".png", sep = "")

                                # Increase intervals in X axis in small plots
                                intervals <- ifelse(max(rv_bar$time_range) - min(rv_bar$time_range) < 30, 2, 4)
                                rv_plots$bar[[i]] <- rv_plots$bar[[i]] +
                                        scale_x_continuous(name = "",
                                                breaks = seq(min(rv_bar$time_range),
                                                        max(rv_bar$time_range),
                                                        by = intervals))

                                ggsave(name,
                                        plot = rv_plots$bar[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("barplot_small", i, ".png", sep = ""))

                                name_data <- paste("barplot_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$bar_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("barplot_small_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

##************************************************************************************************** ----
        
        ## Bar plots - Multiple country tab ----
        
        ### Reactive values ----
        rv_mult <- reactiveValues(
                
                # Plot parameters
                ctries = def_list_mult$ctries,
                vars = def_list_mult$vars,
                stat = def_list_mult$stat,
                time_range = def_list_mult$time_range,
                
                # Display parameters
                title = def_list_mult$title,
                yaxis = def_list_mult$yaxis,
                source = def_list_mult$source,
                data_labels = def_list_mult$data_labels,
                time_subper = def_list_mult$time_subper,
                transform_zeros = def_list_mult$transform_zeros,
                transform_log = def_list_mult$transform_log,
                ctry_short = def_list_mult$ctry_short,
                digits = def_list_mult$digits,
                digits_y = def_list_mult$digits_y,
                font = def_list_mult$font,
                color_pal = def_list_mult$color_pal,
                color = def_list_mult$color,
                legend_pos = def_list_mult$legend_pos
                
        )

        ### Update inputs ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {
                
                print("Plots - Mult [1]: Updating inputs")

                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
                aux_ctry <- aux_ctry[order(
                        aux_ctry$Ctry_group_num,
                        aux_ctry$Country), ]
                ctry_choices <- as.list(aux_ctry$Country)
                names(ctry_choices) <- aux_ctry$Ctry_slash_Group
                ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)

                updatePickerInput(
                        session = session,
                        inputId = 'gr_mult_id_ctries',
                        choices = ctry_choices,
                        selected = ctry_select,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Variable input
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                updatePickerInput(
                        session = session,
                        inputId = 'gr_mult_id_vars',
                        label = "Select variables",
                        choices = aux_var,
                        selected = aux_var,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Time input
                updateSliderInput(
                        session = session,
                        inputId = "gr_mult_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end)
                )

        })
        
        # Color palette
        output$gr_mult_id_color_pal_ui <- renderUI({
                pickerInput(
                        inputId = "gr_mult_id_color_pal",
                        label = "Select color palette",
                        choices = palette_vec,
                        selected = def_list_mult$color_pal,
                        multiple = FALSE
                ) %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_palette")
                        )
        })
        
        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
                
                print("Plots - Mult [2]: Updating reactive values [changes in raw data]")
                
                # Country 
                aux_ctries <- unique(rv_df$dat_all$Country)
                rv_mult$ctries <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]

                # Variables (select all variables from new dataset)
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                rv_mult$vars <- aux_var
                
                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_mult$time_range <- aux_year
                
        }, ignoreInit = TRUE)
        
        ###  Update reactive values to changes in mult plot parameters ----
        
        observeEvent(c(input$gr_mult_id_update),{
                
                print("Plots - Mult [2]: Updating reactive values [update mult plot parameters]")
                
                # Plot parameters
                rv_mult$ctries <- input$gr_mult_id_ctries
                rv_mult$vars <- input$gr_mult_id_vars
                rv_mult$stat <- input$gr_mult_id_stat
                rv_mult$time_range <- input$gr_mult_id_time_range
                
                # Display parameters
                rv_mult$title <- input$gr_mult_id_title
                rv_mult$yaxis <- input$gr_mult_id_yaxis
                rv_mult$source <- input$gr_mult_id_source
                rv_mult$data_labels <- input$gr_mult_id_data_labels
                rv_mult$time_subper <- input$gr_mult_id_time_subper
                rv_mult$transform_zeros <- input$gr_mult_id_transform_zeros
                rv_mult$transform_log <- input$gr_mult_id_transform_log
                rv_mult$ctry_short <- input$gr_mult_id_ctry_short
                rv_mult$digits <- input$gr_mult_id_digits
                rv_mult$digits_y <- input$gr_mult_id_digits_y
                rv_mult$font <- input$gr_mult_id_font
                rv_mult$color_pal <- input$gr_mult_id_color_pal
                rv_mult$color <- input$gr_mult_id_color
                rv_mult$legend_pos <- input$gr_mult_id_legend_pos
                
        }, ignoreInit = TRUE)

        ### Plots ----
        
        # Prep data
        prepped_data_mult <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_mult_id_update),{
                
                print("Plots - Mult [3]: preparing data")
                
                if(nrow(rv_df$dat_all)==0){return()}

                graph_input <- prep_data_for_graphs(
                        df = rv_df$dat_all,
                        vars = rv_mult$vars,
                        ctries = rv_mult$ctries,
                        t_start = rv_mult$time_range[1],
                        t_end = rv_mult$time_range[2]
                )

                return(graph_input)
        })
        
        # Create plots
        createUI_mult <- function(table) {

                rv_plots$mult_counter <- rv_plots$mult_counter +1
                print(paste0("Render mult plot ", rv_plots$mult_counter, "/", rv_plots$mult_counter_total))
                
                if(all(is.na(table$Value))){return()}
                
                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""

                # Input for title, subtitle Y-axis and source
                title_text <- if(rv_mult$title){unique(table$Var_name)}

                yaxis_units <- if(rv_mult$yaxis){
                        if(is.na(unique(table$Units))){NULL} else {unique(table$Units)}
                }else {NULL}

                if(rv_mult$source){
                        graph_source <- unique(table$Database)
                        if(graph_source == "WDI"){
                                graph_source <- "World Development Indicators"}
                }

                # Transform data: divide by trillions/billions/millions/thousands
                for (i in 4:1){
                        if(rv_mult$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                                if(rv_mult$title) {
                                        subtitle_text <- paste(
                                                c(subtitle_text, units_zeros[5 - i]),
                                                collapse = "")
                                        separator <- if(subtitle_text == ""){""} else {", "}
                                        if(rv_mult$yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, units_zeros[5 - i]),
                                                        collapse = separator)
                                        }
                                }
                                table$Value <- table$Value/(10^(3*i))
                        }
                }
                
                # Log transform
                if(rv_mult$transform_log & min(table$Value, na.rm = TRUE)>0){
                        subtitle_text <- paste(
                                c(subtitle_text, " (Log transformation)"),
                                collapse = "")
                        if(rv_mult$yaxis){
                                yaxis_units <- paste(
                                        c(yaxis_units, " (Log transformation)"),
                                        collapse = "")
                        }
                        table$Value <- log(table$Value)
                }
                
                table$Period_aux <- table$Period

                # Add years to Period variable
                if(rv_mult$time_subper & rv_input$time_subper){

                        # Period 1
                        if(rv_mult$time_range[1] > rv_input$time_range_start){
                                x <- paste(rv_input$time_name_1, " (", rv_mult$time_range[1], sep = "")
                                table$Period[table$Period_num == 1] <- x
                        }else{
                                x <- paste(rv_input$time_name_1, " (", as.character(rv_input$time_range_start), sep = "")
                                table$Period[table$Period_num == 1] <- x
                        }
                        if(rv_mult$time_range[2] < rv_input$time_limit_1){
                                table$Period[table$Period_num == 1] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                        }else{
                                table$Period[table$Period_num == 1] <- paste(x, "-", as.character(rv_input$time_limit_1), ")",sep = "")
                        }

                        # Period 2
                        if(rv_input$time_subper_num == 2){
                                if(rv_mult$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste(rv_input$time_name_2, " (", rv_mult$time_range[1], sep = "")
                                        table$Period[table$Period_num == 2] <- x
                                }else{
                                        x <- paste(rv_input$time_name_2, " (", as.character(rv_input$time_limit_1+1), sep = "")
                                        table$Period[table$Period_num == 2] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Period[table$Period_num == 2] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                                }else{
                                        table$Period[table$Period_num == 2] <- paste(x, "-", as.character(rv_input$time_range_end), ")", sep = "")
                                }
                        }else{
                                if(rv_mult$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste(rv_input$time_name_2, " (", rv_mult$time_range[1], sep = "")
                                        table$Period[table$Period_num == 2] <- x
                                }else{
                                        x <- paste(rv_input$time_name_2, " (", as.character(rv_input$time_limit_1+1), sep = "")
                                        table$Period[table$Period_num == 2] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_limit_2){
                                        table$Period[table$Period_num == 2] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                                }else{
                                        table$Period[table$Period_num == 2] <- paste(x, "-", as.character(rv_input$time_limit_2), ")", sep = "")
                                }
                        }

                        # Period 3
                        if(rv_input$time_subper_num == 3){
                                if(rv_mult$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste(rv_input$time_name_3, " (", rv_mult$time_range[1], sep = "")
                                        table$Period[table$Period_num == 3] <- x
                                }else{
                                        x <- paste(rv_input$time_name_3, " (", as.character(rv_input$time_limit_2+1), sep = "")
                                        table$Period[table$Period_num == 3] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Period[table$Period_num == 3] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                                }else{
                                        table$Period[table$Period_num == 3] <- paste(x, "-", as.character(rv_input$time_range_end), ")", sep = "")
                                }
                        }else{
                                if(rv_mult$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste(rv_input$time_name_3, " (", rv_mult$time_range[1], sep = "")
                                        table$Period[table$Period_num == 3] <- x
                                }else{
                                        x <- paste(rv_input$time_name_3, " (", as.character(rv_input$time_limit_2+1), sep = "")
                                        table$Period[table$Period_num == 3] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_limit_3){
                                        table$Period[table$Period_num == 3] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                                }else{
                                        table$Period[table$Period_num == 3] <- paste(x, "-", as.character(rv_input$time_limit_3), ")", sep = "")
                                }
                        }

                        # Period 4
                        if(rv_input$time_subper_num == 4){
                                if(rv_mult$time_range[1] > rv_input$time_limit_3 + 1){
                                        x <- paste(rv_input$time_name_4, " (", rv_mult$time_range[1], sep = "")
                                        table$Period[table$Period_num == 4] <- x
                                }else{
                                        x <- paste(rv_input$time_name_4, " (", as.character(rv_input$time_limit_3+1), sep = "")
                                        table$Period[table$Period_num == 4] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Period[table$Period_num == 4] <- paste(x, "-", rv_mult$time_range[2], ")", sep = "")
                                }else{
                                        table$Period[table$Period_num == 4] <- paste(x, "-", as.character(rv_input$time_range_end), ")", sep = "")
                                }
                        }
                } else {
                        table$Period <- NA
                        table$Period_num <- NA
                }

                # Calculate variable to plot based on selected stat

                if(rv_mult$stat == "Average"){
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value = mean(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period average"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period average"),
                                collapse = "")}
                }

                if(rv_mult$stat == "Median"){
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value = median(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period median"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period median"),
                                collapse = "")}
                }

                if(rv_mult$stat == "Standard deviation"){
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value = sd(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period standard deviation"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period standard deviation"),
                                collapse = "")}
                }

                if(rv_mult$stat == "Maximum"){
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value = my_max_fun(Value)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period maximum"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period maximum"),
                                collapse = "")}
                }

                if(rv_mult$stat == "Minimum"){
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value =  my_min_fun(Value)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                        if(rv_mult$title){
                                if(subtitle_text == ""){subtitle_text <- paste(
                                        c(subtitle_text, "Period minimum"),
                                        collapse = "")}
                                else{subtitle_text <- paste(
                                        c(subtitle_text, ", period minimum"),
                                        collapse = "")}}
                }

                if(rv_mult$stat == "Most recent value"){
                        table <- table[!is.na(table$Value), ]
                        table <- table %>%
                                group_by(Country, Period) %>%
                                mutate(sum_value = Value[which.max(Year)]) %>%
                                ungroup()
                        if(rv_mult$title){
                                if(subtitle_text == ""){subtitle_text <- paste(
                                        c(subtitle_text, "Period most recent value"),
                                        collapse = "")}
                                else{subtitle_text <- paste(
                                        c(subtitle_text, ", period most recent value"),
                                        collapse = "")}}
                }
                
                if(rv_mult$stat == "None (raw annual data)"){
                        table$sum_value <- table$Value
                }

                # Include years in subtitle if no sub-periods
                if(rv_mult$title & (!rv_mult$time_subper | !rv_input$time_subper)){
                        subtitle_text <- paste(
                                c(subtitle_text, " (", rv_mult$time_range[1], "-", rv_mult$time_range[2], ")"),
                                collapse = "")
                }

                # Create factor variables
                table$Period_num <- factor(table$Period_num,
                        levels = unique(table$Period_num),
                        labels = unique(table$Period)
                )

                table$Ctry_group_num <- factor(table$Ctry_group_num,
                        levels = unique(table$Ctry_group_num),
                        labels = unique(table$Ctry_group))

                # Short country names
                if(rv_mult$ctry_short){table$Country_aux <- table$Ctry_iso}else{table$Country_aux <- table$Country}
                
                # Title
                title_text_paragraph <- str_wrap(title_text, width = 130)

                # Actually create plot
                p <- ggplot(data = table,
                        aes(x = Country_aux,
                                y = sum_value,
                                fill = if(rv_mult$stat != "None (raw annual data)"){if(!rv_input$time_subper | !rv_mult$time_subper){rv_mult$color} else {Period_num}}else{as.factor(Year)},
                                order = Period_num)
                        )+

                        # Define type of graph: bar
                        geom_bar(stat="identity",
                                position=position_dodge(),
                                colour="black",
                                na.rm = TRUE
                        )+

                        # Country group names below
                        facet_grid(~Ctry_group_num,
                                scales = "free_x",
                                space = "free_x",
                                switch = "x"
                        )+
                        theme(strip.placement = "outside",
                                strip.background = element_rect(fill = "gray"),
                                axis.title = element_blank()
                        )+

                        # Include title, subtitle, source and Y-axis title
                        labs(title = title_text_paragraph,
                                subtitle = subtitle_text,
                                caption = if(rv_mult$source){paste("Source: ", graph_source, ".", sep="")},
                                y = if(rv_mult$yaxis){yaxis_units} else {NULL}
                        )+

                        # Aesthetics
                        theme(panel.background = element_rect(fill=NA),
                                panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12, family = rv_mult$font),
                                legend.title = element_blank(),
                                axis.title.y = element_text()
                        )

                # Include thousands separating comma in Y-axis
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(rv_mult$digits_y), big.mark = ","),
                        breaks = pretty_breaks()
                )

                # Include data labels
                if(rv_mult$data_labels){p <- p + geom_text(
                        aes(x = Country_aux,
                                y = sum_value,
                                label = format(round(as.numeric(sum_value), rv_mult$digits),
                                        nsmall = rv_mult$digits, big.mark = ",")),
                        vjust= ifelse(table$sum_value <0 , 1.5, -0.5),
                        hjust= 0.5,
                        size = 3,
                        position = position_dodge(0.9),
                        family = rv_mult$font,
                        na.rm = TRUE)
                }

                if(rv_input$time_subper){

                        # Color palette / Hide legend if no time period break / Set legend position
                        if(rv_mult$time_subper){
                                if(rv_mult$stat == "None (raw annual data)" & rv_mult$time_range[2]-rv_mult$time_range[1]+1 > 9){
                                        p <- p + scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.9)
                                }else{
                                        p <- p + scale_fill_brewer(palette = rv_mult$color_pal)
                                }
                                
                                p <- p + theme(legend.position = rv_mult$legend_pos[[1]])


                        } else {p <- p + theme(legend.position = "none")}

                }else {p <- p + theme(legend.position = "none")}

                # Correct digits if numbers are repeated in Y-axis
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, rv_mult$digits_y)))
                digits <- rv_mult$digits_y

                while(yaxis_number_labels > yaxis_number_labels_obs){
                        digits <- digits + 1
                        p <- p + scale_y_continuous(name = yaxis_units,
                                labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                                breaks = pretty_breaks())

                        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                        x <- x[!is.na(x)]
                        yaxis_number_labels <- length(x)
                        yaxis_number_labels_obs <- length(unique(round(x, digits)))
                }

                # Append plot to the list of plots
                rv_plots$mult <- isolate(list.append(rv_plots$mult, p))
                
                # Append data
                # Transform Period_num back to numeric
                table$Value <- table$sum_value
                table$Ctry_group_num <- as.numeric(table$Ctry_group_num)
                table$Period_num <- as.numeric(table$Period_num)
                
                if(rv_mult$time_subper & rv_input$time_subper){
                        
                        table$Years <- NA
                        
                        # Period 1
                        if(rv_mult$time_range[1] > rv_input$time_range_start){
                                x <- paste0(rv_mult$time_range[1])
                                table$Years[table$Period_num == 1] <- x
                        }else{
                                x <- paste0(as.character(rv_input$time_range_start))
                                table$Years[table$Period_num == 1] <- x
                        }
                        if(rv_mult$time_range[2] < rv_input$time_limit_1){
                                table$Years[table$Period_num == 1] <- paste0(x, "-", rv_mult$time_range[2])
                        }else{
                                table$Years[table$Period_num == 1] <- paste0(x, "-", as.character(rv_input$time_limit_1))
                        }
                        
                        # Period 2
                        if(rv_input$time_subper_num == 2){
                                if(rv_mult$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste0(rv_mult$time_range[1])
                                        table$Years[table$Period_num == 2] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_1+1))
                                        table$Years[table$Period_num == 2] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", rv_mult$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }else{
                                if(rv_mult$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste0(rv_mult$time_range[1])
                                        table$Years[table$Period_num == 2] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_1+1))
                                        table$Years[table$Period_num == 2] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_limit_2){
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", rv_mult$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", as.character(rv_input$time_limit_2))
                                }
                        }
                        
                        # Period 3
                        if(rv_input$time_subper_num == 3){
                                if(rv_mult$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste0(rv_mult$time_range[1])
                                        table$Years[table$Period_num == 3] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_2+1))
                                        table$Years[table$Period_num == 3] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", rv_mult$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }else{
                                if(rv_mult$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste0(rv_mult$time_range[1])
                                        table$Years[table$Period_num == 3] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_2+1))
                                        table$Years[table$Period_num == 3] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_limit_3){
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", rv_mult$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", as.character(rv_input$time_limit_3))
                                }
                        }
                        
                        # Period 4
                        if(rv_input$time_subper_num == 4){
                                if(rv_mult$time_range[1] > rv_input$time_limit_3 + 1){
                                        x <- paste0(rv_mult$time_range[1])
                                        table$Years[table$Period_num == 4] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_3+1))
                                        table$Years[table$Period_num == 4] <- x
                                }
                                if(rv_mult$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 4] <- paste0(x, "-", rv_mult$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 4] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }
                }
                
                table$Title <- if(rv_mult$title){title_text}else{""} 
                table$Subtitle <- if(rv_mult$title){subtitle_text}else{""} 
                table$Y_axis <- if(rv_mult$yaxis){yaxis_units}else{""}
                table$Source <- if(rv_mult$source){graph_source}else{""}
                table$Period <- table$Period_aux

                # Select columns
                table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Years", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
                
                rv_plots$mult_data <- isolate(list.append(rv_plots$mult_data, table))

                # Show plot
                renderPlot(p)

        }

        # Call Prep data and Create plots
        output$gr_mult_out_plots <- renderUI({

                print("****************************")
                print("Plots - Mult: start renderUI")

                rv_plots$mult <- list()
                rv_plots$mult_data <- list()

                pd <- req(prepped_data_mult())

                rv_plots$mult_counter <- 0
                rv_plots$mult_counter_total <- length(isolate(pd))

                final <- isolate(tagList(map(pd, ~ createUI_mult(.))))

                print("Plots - Mult: finish renderUI")
                print("****************************")
                cat("\n")

                return(final)
        })

        ### Plots download handlers ----

        # Download plots as png zipped - large
        output$gr_mult_download_large <- downloadHandler(
                filename = 'gr_mult_plots_large.zip',
                content = function( file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$mult)){
                                name <- paste("multplot_large", i, ".png", sep="")
                                ggsave(name,
                                        plot = rv_plots$mult[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("multplot_large", i, ".png", sep=""))

                                name_data <- paste("multplot_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$mult_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("multplot_large_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip( file, vector_plots)
                }
        )

        # Download plots as png zipped - small
        output$gr_mult_download_small <- downloadHandler(
                filename = 'gr_mult_plots_small.zip',
                content = function( file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$mult)){

                                name <- paste("multplot_small", i, ".png", sep="")
                                ggsave(name,
                                        plot = rv_plots$mult[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("multplot_small", i, ".png", sep=""))

                                name_data <- paste("multplot_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$mult_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("multplot_small_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

##************************************************************************************************** ----
        
        ## Stacked single country plots ----

        ### Reactive values ----
        rv_stas <- reactiveValues(

                # Plot parameters
                ctries_ctry = def_list_stas$ctries_ctry,
                time_range = def_list_stas$time_range,
                plot_num = def_list_stas$plot_num,
                

                # Individual plot parameters
                vars = def_list_stas$vars,
                stacked_100 = def_list_stas$stacked_100,
                title_text = def_list_stas$title_text,
                yaxis_text = def_list_stas$yaxis_text,

                # Display parameters
                title = def_list_stas$title,
                yaxis = def_list_stas$yaxis,
                source = def_list_stas$source,
                data_labels = def_list_stas$data_labels,
                time_subper = def_list_stas$time_subper,
                time_subper_avg = def_list_stas$time_subper_avg,
                transform_zeros = def_list_stas$transform_zeros,
                transform_log = def_list_stas$transform_log,
                digits = def_list_stas$digits,
                digits_y = def_list_stas$digits_y,
                color = def_list_stas$color,
                font = def_list_stas$font,
                legend_pos = def_list_stas$legend_pos

        )

        ### Update inputs ----

        # Inputs that are independent of the number of plots
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {

                print("Plots - Stas [1]: Updating inputs")

                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
                aux_ctry <- aux_ctry[order(
                        aux_ctry$Ctry_group_num,
                        aux_ctry$Country), ]
                ctry_choices <- as.list(aux_ctry$Country)
                names(ctry_choices) <- aux_ctry$Ctry_slash_Group
                ctry_select <- rv_input$ctries_ctry

                updatePickerInput(
                        session = session,
                        inputId = 'gr_stas_id_ctries_ctry',
                        choices = ctry_choices,
                        selected = ctry_select,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Time input
                updateSliderInput(
                        session = session,
                        inputId = "gr_stas_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end)
                )
        })

        # Color palette
        output$gr_stas_id_color_ui <- renderUI({
                pickerInput(
                        inputId = "gr_stas_id_color",
                        label = "Select color palette",
                        choices = palette_vec,
                        selected = def_list_stas$color,
                        multiple = FALSE
                ) %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_palette")
                        )
        })

        # Generate reactive values for each plot (depends on number of plots)
        observeEvent(c(input$in_id_update,
                input$in_id_reset_confirm,
                input$gr_stas_id_plot_num),{
                  
                print("Plots - Stas [2]: Generate reactive values (depends on number of plots)")

                if(nrow(rv_df$dat_all)==0){return()}

                # If reactive value is null
                for(i in 1:input$gr_stas_id_plot_num){
                        
                        # We hard code the first plot to have the trade example plotted

                        # Variables names
                        if(is.null(eval(parse(text = paste0("rv_stas$vars", i))))){
                                eval(parse(text = paste0("rv_stas$vars", i, " <- rv_stas$vars")))
                        }
                        
                        # Stacked 100
                        if(is.null(eval(parse(text = paste0("rv_stas$stacked_100", i))))){
                                eval(parse(text = paste0("rv_stas$stacked_100", i, " <- rv_stas$stacked_100")))
                        }

                        # Title and Y-axis legend
                        if(is.null(eval(parse(text = paste0("rv_stas$title_text", i))))){
                                if(i==1){title <- rv_stas$title_text}else{title <- paste0("Title ", i)}
                                eval(parse(text = paste0("rv_stas$title_text", i, " <- title")))
                        }

                        if(is.null(eval(parse(text = paste0("rv_stas$yaxis_text", i))))){
                                if(i==1){yaxis <- rv_stas$yaxis_text}else{yaxis <- paste0("Y-axis title ", i)}
                                eval(parse(text = paste0("rv_stas$yaxis_text", i, " <- yaxis")))
                        }
                        
                }
        })


        # Render UI for inputs of each plot (number of input widgets created depends on choice of number of plots)
        output$gr_stas_id_input_panels <- renderUI({

                print("Plots - Stas [3]: Generate input widgets for individual plots")

                if(nrow(rv_df$dat_all)==0){return()}

                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                lapply(1:input$gr_stas_id_plot_num, function(i) {

                        # Inputs that depend on the number of plots

                        wellPanel(
                                style = "background-color: #ebf5fb",

                                h4(paste("Stacked plot "), i),

                                fluidPage(

                                        selectizeInput(
                                                inputId = paste0('gr_stas_id_vars', i),
                                                label = "Select variables",
                                                choices = aux_var,
                                                multiple = TRUE,
                                                select = eval(parse(text = paste0("rv_stas$vars", i))),
                                                options = list(placeholder = 'Select')
                                        ),
                                        
                                        materialSwitch(
                                                inputId = paste0('gr_stas_id_stacked_100', i),
                                                label = "100 percent stacked bars",
                                                value = eval(parse(text = paste0("rv_stas$stacked_100", i))),
                                                status = "primary",
                                                right = TRUE
                                        ),

                                        textInput(
                                                inputId = paste0("gr_stas_id_title_text",i),
                                                label = "Plot title text",
                                                value = isolate(eval(parse(text = paste0("rv_stas$title_text", i))))
                                        ),

                                        textInput(
                                                inputId = paste0("gr_stas_id_yaxis_text", i),
                                                label = "Y-axis text",
                                                value = isolate(eval(parse(text = paste0("rv_stas$yaxis_text", i))))
                                        )
                                )
                        )
                })
        })

        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{

                print("Plots - Stas [4]: Updating reactive values [changes in raw data]")
                
                # Country (if previously selected country is in new dataset, keep selection, else select first country from vector)
                aux_ctry <- unique(rv_df$dat_all$Country)
                rv_stas$ctries_ctry <- aux_ctry[1]

                # Variables (select all variables from new dataset)
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                rv_stas$vars <- aux_var

                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_stas$time_range <- aux_year

        }, ignoreInit = TRUE)

        ###  Update reactive values to changes in stas plot parameters ----

        observeEvent(c(input$gr_stas_id_update),{

                print("Plots - Stas [5]: Updating reactive values [update stas plot parameters]")

                # Plot parameters
                rv_stas$ctries_ctry <- input$gr_stas_id_ctries_ctry
                rv_stas$time_range <- input$gr_stas_id_time_range

                # Individual plot parameters
                rv_stas$plot_num <- input$gr_stas_id_plot_num

                for (i in 1:input$gr_stas_id_plot_num){

                        eval(parse(text = paste0("rv_stas$vars", i, " <- input$gr_stas_id_vars", i)))
                        eval(parse(text = paste0("rv_stas$stacked_100", i, " <- input$gr_stas_id_stacked_100", i)))
                        eval(parse(text = paste0("rv_stas$title_text", i, " <- input$gr_stas_id_title_text", i)))
                        eval(parse(text = paste0("rv_stas$yaxis_text", i, " <- input$gr_stas_id_yaxis_text", i)))

                }

                # Display parameters
                rv_stas$title <- input$gr_stas_id_title
                rv_stas$yaxis <- input$gr_stas_id_yaxis
                rv_stas$source <- input$gr_stas_id_source
                rv_stas$data_labels <- input$gr_stas_id_data_labels
                rv_stas$time_subper <- input$gr_stas_id_time_subper
                rv_stas$time_subper_avg <- input$gr_stas_id_time_subper_avg
                rv_stas$transform_zeros <- input$gr_stas_id_transform_zeros
                rv_stas$transform_log <- input$gr_stas_id_transform_log
                rv_stas$digits <- input$gr_stas_id_digits
                rv_stas$digits_y <- input$gr_stas_id_digits_y
                rv_stas$color <- input$gr_stas_id_color
                rv_stas$font <- input$gr_stas_id_font
                rv_stas$legend_pos <- input$gr_stas_id_legend_pos

        }, ignoreInit = TRUE)

        ### Plots ----

        # Prep data
        prepped_data_stas <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_stas_id_update),{

                print("Plots - Stas [6]: preparing data")

                if(nrow(rv_df$dat_all)==0){return()}
                if(length(rv_stas$ctries_ctry)==0){return()}

                # Filter by country
                df_aux <- rv_df$dat_all[rv_df$dat_all$Country %in% rv_stas$ctries_ctry, ]

                # Create variable_name | variable_code column
                df_aux$Var_slash_code <- paste(df_aux$Var_name, df_aux$Var_code, sep = " | ")

                #Create empty list for tibbles
                tibble_list <- list()

                for (i in 1:rv_stas$plot_num){

                        list_name <- paste0("table", i)

                        if(is.null(eval(parse(text = paste0("rv_stas$vars", i))))){

                                tibble_list[[list_name]] <- data.frame()

                        }else{

                                # Select variables
                                df_aux_2 <- df_aux[df_aux$Var_slash_code %in% eval(parse(text = paste0("rv_stas$vars", i))), ]

                                # Filter data by year
                                df_aux_2 <- df_aux_2 %>% filter(Year >= rv_stas$time_range[1] & Year <= rv_stas$time_range[2])

                                # Title
                                df_aux_2 <- df_aux_2 %>% tibble::add_column(
                                        Title = as.character(paste0(eval(parse(text = paste0("rv_stas$title_text", i))), " - ", rv_stas$ctries_ctry)),
                                        Y_axis = as.character(eval(parse(text = paste0("rv_stas$yaxis_text", i)))),
                                        Stacked_100 = as.logical(eval(parse(text = paste0("rv_stas$stacked_100", i))))
                                                )
                                
                                # Stacked 100
                                if(eval(parse(text = paste0("rv_stas$stacked_100",i)))){
                                        
                                        df_aux_2 <- df_aux_2 %>% 
                                                group_by(Year) %>% 
                                                mutate(Value_aux = 100 * Value / sum(Value) ) %>% 
                                                ungroup
                                        
                                        df_aux_2$Value <- df_aux_2$Value_aux 
                                        
                                }

                                # Append to list
                                df_aux_2 <- df_aux_2 %>% as_tibble()
                                tibble_list[[list_name]] <- df_aux_2

                        }
                        
                        rv_stas$tibble_list <- tibble_list
                }

                # Create subperiod variables to include rectangles
                if(rv_input$time_subper & rv_stas$time_subper){
                        subper_list <- subper_rectangles_fun(
                                subper_num = rv_input$time_subper_num,
                                time_range = rv_stas$time_range,
                                time_lim_1 = rv_input$time_limit_1,
                                time_lim_2 = rv_input$time_limit_2,
                                time_lim_3 = rv_input$time_limit_3,
                                time_name_1 = rv_input$time_name_1,
                                time_name_2 = rv_input$time_name_2,
                                time_name_3 = rv_input$time_name_3,
                                time_name_4 = rv_input$time_name_4
                        )

                        rv_stas$rectangle_text <- subper_list[[1]]
                        rv_stas$vertical_lines <- subper_list[[2]]
                }
                
                return(rv_stas$tibble_list)

        })
        
        # Create plots
        createUI_stas <- function(table) {

                rv_plots$stas_counter <- rv_plots$stas_counter + 1
                print(paste0("Render stas plot ", rv_plots$stas_counter, "/", rv_plots$stas_counter_total))

                rectangle_text <- rv_stas$rectangle_text
                vertical_lines <- rv_stas$vertical_lines

                if(all(is.na(table$Value))){return()}
                
                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""
                
                # Drop variables for which there all Values are NA
                table <- table %>%
                        group_by(Var_slash_code) %>%
                        filter(!all(is.na(Value))) %>%
                        ungroup


                # Input for title, subtitle Y-axis and source
                title_text <- if(rv_stas$title){unique(table$Title)}
                yaxis_units <- if(rv_stas$yaxis){unique(table$Y_axis)} else {NULL}

                if(rv_stas$source){
                        graph_source <- unique(table$Database)
                        graph_source[graph_source=="WDI"] <- "World Development Indicators"
                        graph_source <- paste(graph_source, collapse = ",")
                        graph_source <- paste0(graph_source, ".")
                }

                # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                intervals <- ifelse(rv_stas$time_range[2] - rv_stas$time_range[1] < 30, 1, 2)

                # Transform data: divide by trillions/billions/millions/thousands
                for (i in 4:1){
                        if(rv_stas$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                                if(rv_stas$title) {
                                        subtitle_text <- paste(
                                                c(subtitle_text, units_zeros[5 - i]),
                                                collapse = "")
                                        separator <- if(subtitle_text == ""){""} else {", "}
                                        if(rv_stas$yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, units_zeros[5 - i]),
                                                        collapse = separator)
                                        }
                                }
                                table$Value <- table$Value/(10^(3*i))
                        }
                }

                # Log transform
                if(rv_stas$transform_log & min(table$Value, na.rm = TRUE)>0){
                        subtitle_text <- paste(
                                c(subtitle_text, " (Log transformation)"),
                                collapse = "")
                        if(rv_stas$yaxis){
                                yaxis_units <- paste(
                                        c(yaxis_units, " (Log transformation)"),
                                        collapse = "")
                        }
                        table$Value <- log(table$Value)
                }


                # Totals
                totals <- table %>%
                        group_by(Year, Period) %>%
                        summarize(total = sum(Value), .groups = "drop")

                totals2 <- table %>%
                        group_by(Year) %>%
                        summarize(total = sum(Value), .groups = "drop")

                # Eliminates elements between parenthesis (and the parenthesis)
                legend_labels <- gsub("\\s*\\([^\\)]+\\)", "", as.character(unique(table$Var_name)))
                
                # If longer than 20 characters, divide legend labels in multiple lines
                legend_labels <- multiline_text_fun(text_vector = legend_labels, number_characters = 20)
                
                # Title
                title_text_paragraph <- str_wrap(title_text, width = 130)
                
                # Stacked 100
                if(unique(table$Stacked_100)){
                        subtitle_text <- ""
                        yaxis_units <- "Percent"
                }

                # Actually create plot
                p <- ggplot(table, aes(fill = Var_name, x = Year, y = Value))+
                        geom_bar(position="stack",
                                stat = "identity",
                                colour = "black", 
                                na.rm = TRUE) +
                        scale_fill_brewer(palette=rv_stas$color, labels = legend_labels) +

                        
                        # Include title, subtitle, source and Y-axis title
                        labs(title = title_text_paragraph,
                                subtitle = subtitle_text,
                                caption = if(rv_stas$source){paste("Source: ", graph_source, sep = "")},
                                y = if(rv_stas$yaxis){yaxis_units} else {NULL}
                        ) +
                        
                        # Aesthetics
                        theme_minimal() +
                        theme(panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(linetype = "dashed"),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12,  family = isolate(rv_stas$font)),
                                legend.title = element_blank()
                        )+
                        # Define intervals of Year axis
                        scale_x_continuous(name = "",
                                breaks = seq(rv_stas$time_range[1],
                                        rv_stas$time_range[2],
                                        by = intervals)
                        )+
                        coord_cartesian(xlim = c(rv_stas$time_range[1], rv_stas$time_range[2])) +
                        # Legend position
                        theme(legend.position = rv_stas$legend_pos[[1]])

                # Include data labels
                if(rv_stas$data_labels){p <- p +
                        geom_text(data = table,
                                aes(
                                        x = Year,
                                        y = Value,
                                        label = format(round(as.numeric(Value), rv_stas$digits),
                                                nsmall = rv_stas$digits, big.mark = ",")),
                                position = position_stack(vjust=0.5),
                                size = 3,
                                family = rv_stas$font, 
                                na.rm = TRUE)
                
                        if(!unique(table$Stacked_100)){
                                
                                p <- p + geom_text(data = totals,
                                                aes(x = Year,
                                                        y = total,
                                                        label = format(round(as.numeric(total), rv_stas$digits),
                                                                nsmall = rv_stas$digits, big.mark = ",")
                                                ),
                                                vjust = -1,
                                                hjust = 0.5,
                                                size = 3,
                                                family = rv_stas$font,
                                                inherit.aes = FALSE, 
                                                na.rm = TRUE)+ 
                                        geom_point(data = totals, 
                                                aes(x = Year, y = total), 
                                                color = "black", 
                                                size = 2, 
                                                inherit.aes = FALSE, 
                                                na.rm = TRUE)
                        }
                }

                # Increase range of Y axis to make room for the box indicating subperiod
                y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range <- y_max - y_min

                # Include thousands separating comma in Y-axis
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(rv_stas$digits_y), big.mark = ","),
                        breaks = pretty_breaks(),
                        limits = c(y_min, y_max + (y_range*0.1),
                                inherit.aes = FALSE)
                )
                
                # Get new axis limits that will be used as inputs to define the size of the subperiod rectangles
                y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range_new <- y_max - y_min

                #Define other parameters that will be used as inputs to define the size of the subperiod rectangles
                size_factor <- 0.08
                ALPHA <- 0.15
                
                # Subperiod rectangles, their labels and the dotted lines separating
                if(rv_input$time_subper & rv_stas$time_subper){
                        p <- p +
                                geom_rect(data = rectangle_text,
                                        aes(NULL, NULL, xmin = my_min_fun(Year_start), xmax = my_max_fun(Year_end)),
                                        ymin = y_max_new - y_range_new * size_factor ,
                                        ymax = y_max_new,
                                        colour = NA,
                                        fill="grey",
                                        alpha = 0.5,
                                        inherit.aes = FALSE
                                ) +
                                geom_label(data = rectangle_text,
                                        aes(x = Year_start + (Year_end - Year_start) / 2,
                                                y = y_max_new * ALPHA + (y_max_new - y_range_new * size_factor) * (1 - ALPHA),
                                                label = Period,
                                                family = rv_stas$font
                                        ),
                                        size = 3.3,
                                        fill = "grey",
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom",
                                        inherit.aes = FALSE)

                        for (i in vertical_lines) {
                                p <- p + geom_segment(x = i,
                                        y = y_max_new,
                                        xend = i,
                                        yend = y_max_new - y_range_new * size_factor,
                                        colour = "white",
                                        size = 1,
                                        alpha = 1,
                                        inherit.aes = FALSE) +
                                        geom_segment(x = i,
                                                y = y_max_new - y_range_new * size_factor,
                                                xend = i,
                                                yend = -Inf,
                                                colour = "grey",
                                                linetype = "dotted",
                                                inherit.aes = FALSE)
                        }
                }

                # Period average lines
                if(rv_stas$time_subper_avg){
                        if(rv_input$time_subper & rv_stas$time_subper){
                                vec_average <- totals %>%
                                        group_by(Period) %>%
                                        summarise_at(vars(total),
                                                list(Value_avg = mean),
                                                na.rm = TRUE)
                                vec_average <- as.data.frame(vec_average)

                                rectangle_text <- merge(x = rectangle_text,
                                        y = as.data.frame(vec_average),
                                        by = "Period")
                                for(i in 1:nrow(rectangle_text)){
                                        yvalue <- vec_average %>% filter(Period == rectangle_text$Period[i]) %>% select(Value_avg)
                                        period_average <- yvalue[[1]]
                                        p <- p + geom_segment(x = rectangle_text$Year_start[i],
                                                y = period_average,
                                                xend = rectangle_text$Year_end[i],
                                                yend = period_average,
                                                inherit.aes = FALSE)
                                }
                                p <- p + geom_label(
                                        data = rectangle_text,
                                        aes(x = Year_start+(Year_end-Year_start) / 2,
                                                y = (y_max_new - y_range_new * size_factor) - (y_max_new - (y_max_new * ALPHA + (y_max_new - y_range_new * size_factor) * (1 - ALPHA))),
                                                label = paste("Avg. (", round((Year_start + 0.5), 0), "-", round((Year_end - 0.5), 0) , "): ", format(round(as.numeric(Value_avg), rv_stas$digits), nsmall = rv_stas$digits, big.mark = ","), sep = ""),
                                                family = rv_stas$font
                                        ),
                                        size = 3.3,
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom",
                                        inherit.aes = FALSE)

                        }  else {
                                # Full period average
                                vec_average <- totals2 %>%
                                        summarise_at(vars(total),
                                                list(Value_avg = mean),
                                                na.rm = TRUE)
                                yvalue <- vec_average %>% select(Value_avg)
                                yvalue <- yvalue[[1]]

                                p <- p +
                                        geom_segment(
                                        aes(
                                                x = rv_stas$time_range[1],
                                                y = yvalue,
                                                xend = rv_stas$time_range[2],
                                                yend = yvalue
                                        ),
                                        inherit.aes = FALSE)+
                                        geom_label(
                                                aes(x = rv_stas$time_range[1] + (rv_stas$time_range[2] - rv_stas$time_range[1]) / 2,
                                                        y = (y_max_new - y_range_new * size_factor) - (y_max_new - (y_max_new * ALPHA + (y_max_new - y_range_new * size_factor)*(1 - ALPHA))),
                                                        label = paste("Avg. (", rv_stas$time_range[1], "-", rv_stas$time_range[2], "): ", format(round(as.numeric(yvalue), rv_stas$digits), nsmall = rv_stas$digits, big.mark = ","), sep = ""),
                                                        family = rv_stas$font
                                                ),
                                                size = 3.3,
                                                alpha = 0,
                                                label.size = NA,
                                                hjust = "center",
                                                vjust = "bottom",
                                                inherit.aes = FALSE)
                        }
                }

                # Correct digits if numbers are repeated in Y-axis
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, rv_stas$digits_y)))
                digits <- rv_stas$digits_y

                while(yaxis_number_labels > yaxis_number_labels_obs){
                        digits <- digits + 1
                        p <- p + scale_y_continuous(name = yaxis_units,
                                labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                                breaks = pretty_breaks())

                        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                        x <- x[!is.na(x)]
                        yaxis_number_labels <- length(x)
                        yaxis_number_labels_obs <- length(unique(round(x, digits)))
                }
                
                # Append plot to the list of plots
                rv_plots$stas <- list.append(rv_plots$stas, p)
                
                # Append data
                table$Title <- if(rv_stas$title){title_text}else{""} 
                table$Subtitle <- if(rv_stas$title){subtitle_text}else{""} 
                table$Y_axis <- if(rv_stas$yaxis){yaxis_units}else{""}
                table$Source <- if(rv_stas$source){graph_source}else{""}

                # Select columns
                table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
                
                
                rv_plots$stas_data <- list.append(rv_plots$stas_data, table)

                # Show plot
                renderPlot(p)

        }

        # Call Prep data and Create plots
        output$gr_stas_out_plots <- renderUI({

                print("****************************")
                print("Plots - Stas: start renderUI")

                rv_plots$stas <- list()
                rv_plots$stas_data <- list()

                pd <- req(prepped_data_stas())

                rv_plots$stas_counter <- 0
                rv_plots$stas_counter_total <- length(isolate(pd))

                final <- isolate(tagList(map(pd, ~ createUI_stas(.))))

                print("Plots - Stas: finish renderUI")
                print("****************************")
                cat("\n")

                return(final)

        })

        ### Plots download handlers ----

        # Download plots as png zipped - large
        output$gr_stas_download_large <- downloadHandler(
                filename = 'stacked_single_plots_large.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()

                        for (i in 1:length(rv_plots$stas)){
                                name <- paste("stacked_single_large", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$stas[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("stacked_single_large", i, ".png", sep = ""))

                                name_data <- paste("stacked_single_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$stas_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("stacked_single_large_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

        # Download plots as png zipped - small
        output$gr_stas_download_small <- downloadHandler(
                filename = 'stacked_single_plots_small.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$stas)){
                                name <- paste("stacked_single_small", i, ".png", sep = "")

                                # Increase intervals in X axis in small plots
                                intervals <- ifelse(max(rv_stas$time_range) - min(rv_stas$time_range) < 30, 2, 4)
                                rv_plots$stas[[i]] <- rv_plots$stas[[i]] +
                                        scale_x_continuous(name = "",
                                                breaks = seq(min(rv_stas$time_range),
                                                        max(rv_stas$time_range),
                                                        by = intervals))+
                                        theme(legend.position = "bottom")

                                ggsave(name,
                                        plot = rv_plots$stas[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("stacked_single_small", i, ".png", sep = ""))

                                name_data <- paste("stacked_single_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$stas_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("stacked_single_small_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

##************************************************************************************************** ----
        
        ## Stacked multiple country plots ----
        
        ### Reactive values ----
        rv_stam <- reactiveValues(
                
                # Plot parameters
                ctries = def_list_stam$ctries,
                time_range = def_list_stam$time_range,
                stat = def_list_stam$stat,
                plot_num = def_list_stam$plot_num,
                
                # Individual plot parameters
                vars = def_list_stam$vars,
                stacked_100 = def_list_stam$stacked_100,
                title_text = def_list_stam$title_text,
                yaxis_text = def_list_stam$yaxis_text,
                
                # Display parameters
                title = def_list_stam$title,
                yaxis = def_list_stam$yaxis,
                source = def_list_stam$source,
                data_labels = def_list_stam$data_labels,
                time_subper = def_list_stam$time_subper,
                transform_zeros = def_list_stam$transform_zeros,
                transform_log = def_list_stam$transform_log,
                ctry_short = def_list_stam$ctry_short,
                digits = def_list_stam$digits,
                digits_y = def_list_stam$digits_y,
                color = def_list_stam$color,
                font = def_list_stam$font,
                legend_pos = def_list_stam$legend_pos
                
        )        
        
        
        ### Update inputs ----
        
        # Inputs that are independent of the number of plots
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {
                
                print("Plots - Stam [1]: Updating inputs")
                
                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
                aux_ctry <- aux_ctry[order(
                        aux_ctry$Ctry_group_num,
                        aux_ctry$Country), ]
                ctry_choices <- as.list(aux_ctry$Country)
                names(ctry_choices) <- aux_ctry$Ctry_slash_Group
                ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)
                
                updatePickerInput(
                        session = session,
                        inputId = 'gr_stam_id_ctries',
                        choices = ctry_choices,
                        selected = ctry_select,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )
                
                # Time input
                updateSliderInput(
                        session = session,
                        inputId = "gr_stam_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end)
                )
        })
        
        # Color palette
        output$gr_stam_id_color_ui <- renderUI({
                pickerInput(
                        inputId = "gr_stam_id_color",
                        label = "Select color palette",
                        choices = palette_vec,
                        selected = def_list_stam$color,
                        multiple = FALSE
                ) %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_palette")
                        )
        })
        
        # Generate reactive values for each plot (depends on number of plots)
        observeEvent(c(input$in_id_update,
                input$in_id_reset_confirm,
                input$gr_stam_id_plot_num),{
                        
                        print("Plots - Stam [2]: Generate reactive values (depends on number of plots)")
                        
                        if(nrow(rv_df$dat_all)==0){return()}
                        
                        # If reactive value is null
                        for(i in 1:input$gr_stam_id_plot_num){
                                
                                # We hard code the first plot to have the trade example plotted
                                
                                # Variables names
                                if(is.null(eval(parse(text = paste0("rv_stam$vars", i))))){
                                        eval(parse(text = paste0("rv_stam$vars", i, " <- rv_stam$vars")))
                                }
                                
                                # Stacked 100
                                if(is.null(eval(parse(text = paste0("rv_stam$stacked_100", i))))){
                                        eval(parse(text = paste0("rv_stam$stacked_100", i, " <- rv_stam$stacked_100")))
                                }
                                
                                # Title and Y-axis legend
                                if(is.null(eval(parse(text = paste0("rv_stam$title_text", i))))){
                                        if(i==1){title <- rv_stam$title_text}else{title <- paste0("Title ", i)}
                                        eval(parse(text = paste0("rv_stam$title_text", i, " <- title")))
                                }
                                
                                if(is.null(eval(parse(text = paste0("rv_stam$yaxis_text", i))))){
                                        if(i==1){yaxis <- rv_stam$yaxis_text}else{yaxis <- paste0("Y-axis title ", i)}
                                        eval(parse(text = paste0("rv_stam$yaxis_text", i, " <- yaxis")))
                                }
                                
                        }
                })
        
        
        # Render UI for inputs of each plot (number of input widgets created depends on choice of number of plots)
        output$gr_stam_id_input_panels <- renderUI({
                
                print("Plots - Stam [3]: Generate input widgets for individual plots")
                
                if(nrow(rv_df$dat_all)==0){return()}
                
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                
                lapply(1:input$gr_stam_id_plot_num, function(i) {
                        
                        # Inputs that depend on the number of plots
                        
                        wellPanel(
                                style = "background-color: #ebf5fb",
                                
                                h4(paste("Stacked plot "), i),
                                
                                fluidPage(
                                        
                                        selectizeInput(
                                                inputId = paste0('gr_stam_id_vars', i),
                                                label = "Select variables",
                                                choices = aux_var,
                                                multiple = TRUE,
                                                select = eval(parse(text = paste0("rv_stam$vars", i))),
                                                options = list(placeholder = 'Select')
                                        ),
                                        
                                        materialSwitch(
                                                inputId = paste0('gr_stam_id_stacked_100', i),
                                                label = "100 percent stacked bars",
                                                value = eval(parse(text = paste0("rv_stam$stacked_100", i))),
                                                status = "primary",
                                                right = TRUE
                                        ),
                                        
                                        textInput(
                                                inputId = paste0("gr_stam_id_title_text",i),
                                                label = "Plot title text",
                                                value = isolate(eval(parse(text = paste0("rv_stam$title_text", i))))
                                        ),
                                        
                                        textInput(
                                                inputId = paste0("gr_stam_id_yaxis_text", i),
                                                label = "Y-axis text",
                                                value = isolate(eval(parse(text = paste0("rv_stam$yaxis_text", i))))
                                        )
                                )
                        )
                })
        })
        
        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
                
                print("Plots - Stam [4]: Updating reactive values [changes in raw data]")
                
                # Country 
                aux_ctries <- unique(rv_df$dat_all$Country)
                rv_stam$ctries <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]
                
                # Variables (select all variables from new dataset)
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                rv_stam$vars <- aux_var
                
                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_stam$time_range <- aux_year
                
        }, ignoreInit = TRUE)
        
        ###  Update reactive values to changes in stam plot parameters ----
        
        observeEvent(c(input$gr_stam_id_update),{
                
                print("Plots - Stam [5]: Updating reactive values [update stam plot parameters]")
                
                # Plot parameters
                rv_stam$ctries <- input$gr_stam_id_ctries
                rv_stam$time_range <- input$gr_stam_id_time_range
                rv_stam$stat <- input$gr_stam_id_stat
                
                # Individual plot parameters
                rv_stam$plot_num <- input$gr_stam_id_plot_num
                
                for (i in 1:input$gr_stam_id_plot_num){
                        
                        eval(parse(text = paste0("rv_stam$vars", i, " <- input$gr_stam_id_vars", i)))
                        eval(parse(text = paste0("rv_stam$stacked_100", i, " <- input$gr_stam_id_stacked_100", i)))
                        eval(parse(text = paste0("rv_stam$title_text", i, " <- input$gr_stam_id_title_text", i)))
                        eval(parse(text = paste0("rv_stam$yaxis_text", i, " <- input$gr_stam_id_yaxis_text", i)))
                        
                }
                
                # Display parameters
                rv_stam$title <- input$gr_stam_id_title
                rv_stam$yaxis <- input$gr_stam_id_yaxis
                rv_stam$source <- input$gr_stam_id_source
                rv_stam$data_labels <- input$gr_stam_id_data_labels
                rv_stam$time_subper <- input$gr_stam_id_time_subper
                rv_stam$transform_zeros <- input$gr_stam_id_transform_zeros
                rv_stam$transform_log <- input$gr_stam_id_transform_log
                rv_stam$ctry_short <- input$gr_stam_id_ctry_short
                rv_stam$digits <- input$gr_stam_id_digits
                rv_stam$digits_y <- input$gr_stam_id_digits_y
                rv_stam$color <- input$gr_stam_id_color
                rv_stam$font <- input$gr_stam_id_font
                rv_stam$legend_pos <- input$gr_stam_id_legend_pos
                
        }, ignoreInit = TRUE)
        
        ### Plots ----
        
        # Prep data
        prepped_data_stam <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_stam_id_update),{
                
                print("Plots - Stam [6]: preparing data")
                
                if(nrow(rv_df$dat_all)==0){return()}
                if(length(rv_stam$ctries)==0){return()}
                
                # Filter by country
                df_aux <- rv_df$dat_all[rv_df$dat_all$Country %in% rv_stam$ctries, ]
                
                # Create variable_name | variable_code column
                df_aux$Var_slash_code <- paste(df_aux$Var_name, df_aux$Var_code, sep = " | ")
                
                #Create empty list for tibbles
                tibble_list <- list()
                
                for (i in 1:rv_stam$plot_num){
                        
                        list_name <- paste0("table", i)
                        
                        if(is.null(eval(parse(text = paste0("rv_stam$vars", i))))){
                                
                                tibble_list[[list_name]] <- data.frame()
                                
                        }else{
                                
                                # Select variables
                                df_aux_2 <- df_aux[df_aux$Var_slash_code %in% eval(parse(text = paste0("rv_stam$vars", i))), ]
                                
                                # Filter data by year
                                df_aux_2 <- df_aux_2 %>% filter(Year >= rv_stam$time_range[1] & Year <= rv_stam$time_range[2])
                                
                                # Title
                                df_aux_2 <- df_aux_2 %>% tibble::add_column(
                                        Title = as.character(paste0(eval(parse(text = paste0("rv_stam$title_text", i))))),
                                        Y_axis = as.character(eval(parse(text = paste0("rv_stam$yaxis_text", i)))),
                                        Stacked_100 = as.logical(eval(parse(text = paste0("rv_stam$stacked_100", i))))
                                        
                                )
                                
                                # Stacked 100
                                if(eval(parse(text = paste0("rv_stam$stacked_100",i)))){
                                        
                                        df_aux_2 <- df_aux_2 %>% 
                                                group_by(Year, Country) %>% 
                                                mutate(Value_aux = 100 * Value / sum(Value) ) %>% 
                                                ungroup
                                        
                                        df_aux_2$Value <- df_aux_2$Value_aux 
                                        
                                }
                                
                                # Append to list
                                df_aux_2 <- df_aux_2 %>% as_tibble()
                                tibble_list[[list_name]] <- df_aux_2
                                
                        }
                        
                        rv_stam$tibble_list <- tibble_list
                }
                
                return(rv_stam$tibble_list)
                
        })
        
        # Create plots
        createUI_stam <- function(table) {
                
                rv_plots$stam_counter <- rv_plots$stam_counter +1
                print(paste0("Render stam plot ", rv_plots$stam_counter, "/", rv_plots$stam_counter_total))
                
                if(all(is.na(table$Value))){return()}
                
                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""
                
                # Drop variables for which there all Values are NA

                table <- table %>%
                        group_by(Var_slash_code) %>%
                        filter(!all(is.na(Value))) %>%
                        ungroup
                
                # Input for title, subtitle Y-axis and source
                title_text <- if(rv_stam$title){unique(table$Title)}
                yaxis_units <- if(rv_stam$yaxis){unique(table$Y_axis)} else {NULL}
                
                if(rv_stam$source){
                        graph_source <- unique(table$Database)
                        graph_source[graph_source=="WDI"] <- "World Development Indicators"
                        graph_source <- paste(graph_source, collapse = ",")
                        graph_source <- paste0(graph_source, ".")
                }
                
                # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                intervals <- ifelse(rv_stam$time_range[2] - rv_stam$time_range[1] < 30, 1, 2)
                
                # Transform data: divide by trillions/billions/millions/thousands
                for (i in 4:1){
                        if(rv_stam$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                                if(rv_stam$title) {
                                        subtitle_text <- paste(
                                                c(subtitle_text, units_zeros[5 - i]),
                                                collapse = "")
                                        separator <- if(subtitle_text == ""){""} else {", "}
                                        if(rv_stam$yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, units_zeros[5 - i]),
                                                        collapse = separator)
                                        }
                                }
                                table$Value <- table$Value/(10^(3*i))
                        }
                }
                
                # Log transform
                if(rv_stam$transform_log & min(table$Value, na.rm = TRUE)>0){
                        subtitle_text <- paste(
                                c(subtitle_text, " (Log transformation)"),
                                collapse = "")
                        if(rv_stam$yaxis){
                                yaxis_units <- paste(
                                        c(yaxis_units, " (Log transformation)"),
                                        collapse = "")
                        }
                        table$Value <- log(table$Value)
                }
                
                # Eliminates elements between parenthesis (and the parenthesis)
                legend_labels <- gsub("\\s*\\([^\\)]+\\)", "", as.character(unique(table$Var_name)))
                
                # If longer than 20 characters, divide legend labels in multiple lines
                legend_labels <- multiline_text_fun(text_vector = legend_labels, number_characters = 20)
                
                # Add years to Period variable
                if(rv_stam$time_subper & rv_input$time_subper){
                        
                        table$Period[table$Period_num == 1] <- rv_input$time_name_1
                        table$Period[table$Period_num == 2] <- rv_input$time_name_2
                        table$Period[table$Period_num == 3] <- rv_input$time_name_3
                        table$Period[table$Period_num == 4] <- rv_input$time_name_4
                        
                } else {
                        table$Period <- NA
                        table$Period_num <- NA
                }
                
                # Calculate variable to plot based on selected stat
                
                if(rv_stam$stat == "Average"){
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value = mean(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, Var_name, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period average"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period average"),
                                collapse = "")}
                }
                
                if(rv_stam$stat == "Median"){
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value = median(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, Var_name, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period median"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period median"),
                                collapse = "")}
                }
                
                if(rv_stam$stat == "Standard deviation"){
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value = sd(Value, na.rm=T)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, Var_name, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period standard deviation"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period standard deviation"),
                                collapse = "")}
                }
                
                if(rv_stam$stat == "Maximum"){
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value = my_max_fun(Value)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, Var_name, .keep_all = TRUE)
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period maximum"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period maximum"),
                                collapse = "")}
                }
                
                if(rv_stam$stat == "Minimum"){
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value =  my_min_fun(Value)) %>%
                                ungroup()
                        table <- table %>% distinct(Country, Period, Var_name, .keep_all = TRUE)
                        if(rv_stam$title){
                                if(subtitle_text == ""){subtitle_text <- paste(
                                        c(subtitle_text, "Period minimum"),
                                        collapse = "")}
                                else{subtitle_text <- paste(
                                        c(subtitle_text, ", period minimum"),
                                        collapse = "")}}
                }
                
                if(rv_stam$stat == "Most recent value"){
                        table <- table[!is.na(table$Value), ]
                        table <- table %>%
                                group_by(Country, Period, Var_name) %>%
                                mutate(sum_value = Value[which.max(Year)]) %>%
                                ungroup()
                        if(rv_stam$title){
                                if(subtitle_text == ""){subtitle_text <- paste(
                                        c(subtitle_text, "Period most recent value"),
                                        collapse = "")}
                                else{subtitle_text <- paste(
                                        c(subtitle_text, ", period most recent value"),
                                        collapse = "")}}
                }
                
                if(rv_stam$stat == "None (raw annual data)"){
                        table$sum_value <- table$Value
                }
                
                # Include years in subtitle if no sub-periods
                if(rv_stam$title & (!rv_stam$time_subper | !rv_input$time_subper)){
                        subtitle_text <- paste(
                                c(subtitle_text, " (", rv_stam$time_range[1], "-", rv_stam$time_range[2], ")"),
                                collapse = "")
                }
                
                # Create factor variables
                table$Period_num <- factor(table$Period_num,
                        levels = unique(table$Period_num),
                        labels = unique(table$Period)
                )
                
                table$Ctry_group_num <- factor(table$Ctry_group_num,
                        levels = unique(table$Ctry_group_num),
                        labels = unique(table$Ctry_group))
                
                table$Var_name <- factor(table$Var_name,
                        levels = unique(table$Var_name),
                        labels = unique(table$Var_name))
                
                table$Country  <- factor(table$Country ,
                        levels = unique(table$Country),
                        labels = unique(table$Country))
                
                # Short country names
                if(rv_stam$ctry_short){table$Country_aux <- table$Ctry_iso}else{table$Country_aux <- table$Country}
                
                # Totals
                totals <- table %>%
                        group_by(Country_aux, Ctry_group_num, Period_num) %>%
                        summarize(total = sum(sum_value), .groups = "drop")
                
                totals <- totals[order(
                        totals$Ctry_group_num,
                        totals$Country_aux,
                        totals$Period_num), ]
                
                totals2 <- table %>%
                        group_by(Country_aux, Ctry_group_num) %>%
                        summarize(total = sum(sum_value), .groups = "drop")
                
                totals2 <- totals2[order(
                        totals$Ctry_group_num,
                        totals$Country_aux), ]
                
                if(rv_stam$stat == "None (raw annual data)"){

                        totals <- table %>%
                                group_by(Country_aux, Ctry_group_num, Year) %>%
                                summarize(total = sum(sum_value), .groups = "drop")
                        
                        totals <- totals[order(
                                totals$Ctry_group_num,
                                totals$Country_aux,
                                totals$Year), ]
                        
                        totals2 <- table %>%
                                group_by(Country_aux, Year) %>%
                                summarize(total = sum(sum_value), .groups = "drop")
                        
                        totals2 <- totals2[order(
                                totals$Year,
                                totals$Country_aux), ]
                }
                
                # Title paragraph 
                title_text_paragraph <- str_wrap(title_text, width = 130)
                
                # Stacked 100
                if(unique(table$Stacked_100)){
                        subtitle_text <- ""
                        yaxis_units <- "Percent"
                }
                
                # Actually create plot
                p <- ggplot(data = table,
                        aes(fill = Var_name,
                                x = if(rv_stam$stat == "None (raw annual data)"){as.factor(Year)}else{Period_num},
                                y = sum_value,
                                order = if(rv_stam$stat == "None (raw annual data)"){as.factor(Year)}else{Period_num}
                                ),
                        
                        )+

                        # Define type of graph: bar
                        geom_bar(stat = "identity",
                                position = "stack",
                                colour = "black",
                                na.rm = TRUE
                        )+
                        
                        # Define color palette
                        scale_fill_brewer(palette=rv_stam$color, labels = legend_labels) +
                        
                        # Country names below
                        facet_nested( ~ Ctry_group_num + Country_aux,
                                scales = "free_x",
                                space = "free_x",
                                switch = "x",
                                nest_line = TRUE
                        )+

                        theme(strip.placement = "outside",
                                strip.background = element_rect(fill = "gray"),
                                axis.title = element_blank()
                        )+

                        # Include title, subtitle, source and Y-axis title
                        labs(title = if(rv_stam$title){title_text_paragraph},
                                subtitle = if(rv_stam$title){subtitle_text},
                                caption = if(rv_stam$source){paste("Source: ", graph_source, sep="")},
                                y = if(rv_stam$yaxis){yaxis_units} else {NULL}
                        ) +
                        
                        # Legend position
                        theme(legend.position = rv_stam$legend_pos[[1]])
                
                        # Include time subperiods
                        if(rv_input$time_subper){
                                if(rv_stam$time_subper){
                                        x_labels <- element_text(colour = "black", angle = 90, hjust = 1, vjust = 0.5)
                                } else {
                                        x_labels <- element_blank()
                                }
                        } else{
                                x_labels <- element_blank()
                        }

                # Aesthetics
                p <- p + 
                        theme(panel.background = element_rect(fill=NA),
                                panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 2, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = x_labels,
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12, family = rv_stam$font),
                                legend.title = element_blank(),
                                axis.title.y = element_text()
                        )
                
                # Include thousands separating comma in Y-axis
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(rv_stam$digits_y), big.mark = ","),
                        breaks = pretty_breaks()
                )
                
                # Include data labels
                
                        # Filter for bars which are too small 
                        y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                        y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                        y_range <- y_max - y_min
                        labels_filter <- y_range * 0.04
                
                if(rv_stam$data_labels){p <- p +
                        geom_text(data = table,
                                aes(
                                        x = if(rv_stam$stat == "None (raw annual data)"){as.factor(Year)}else{Period_num},
                                        y = sum_value,
                                        label = ifelse(sum_value > labels_filter,
                                                format(round(as.numeric(sum_value), rv_stam$digits),
                                                        nsmall = rv_stam$digits, big.mark = ","), "")),
                                position = position_stack(vjust=0.5),
                                size = 3,
                                family = rv_stam$font,
                                na.rm = TRUE)
                        
                        if(!unique(table$Stacked_100)){
                                
                                p <- p +                         geom_text(data = totals,
                                        aes(x = if(rv_stam$stat == "None (raw annual data)"){as.factor(Year)}else{Period_num},
                                                y = total,
                                                label = format(round(as.numeric(total), rv_stam$digits),
                                                        nsmall = rv_stam$digits, big.mark = ",")
                                        ),
                                        vjust = -1,
                                        hjust = 0.5,
                                        size = 3,
                                        family = rv_stam$font,
                                        inherit.aes = FALSE,
                                        na.rm = TRUE) +
                                        geom_point(data = totals,
                                                aes(x = if(rv_stam$stat == "None (raw annual data)"){as.factor(Year)}else{Period_num},
                                                        y = total),
                                                color = "black",
                                                size = 2,
                                                inherit.aes = FALSE,
                                                na.rm = TRUE)
                        }
                }
                
                # Correct digits if numbers are repeated in Y-axis
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, rv_stam$digits_y)))
                digits <- rv_stam$digits_y
                
                while(yaxis_number_labels > yaxis_number_labels_obs){
                        digits <- digits + 1
                        p <- p + scale_y_continuous(name = yaxis_units,
                                labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                                breaks = pretty_breaks())
                        
                        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                        x <- x[!is.na(x)]
                        yaxis_number_labels <- length(x)
                        yaxis_number_labels_obs <- length(unique(round(x, digits)))
                }
                
                # The following is to add the white line between the facet levels (Country and Country_group)
                
                # Convert the plot to a grob
                gt <- ggplotGrob(p)
                
                # White line separating levels
                line_grob <- grobTree(segmentsGrob(x0=0, x1=1, y0=0.5, y1=0.5, default.units="npc", gp = gpar(col = "white", lty = 1, lwd=4)))
                
                # Get the positions of the bottom strips in the layout: t = top, l = left, ...
                strip <-c(subset(gt$layout, grepl("strip-b", gt$layout$name), select = t:r))
                strip_2 <- which(grepl('strip-', gt$layout$name))
                
                # Change background color
                fills <- c(rep("grey", length(unique(table$Ctry_group_num))), rep("grey90", length(unique(table$Country_aux))))
                k <-1
                for (i in strip_2) {
                        j <- which(grepl('rect', gt$grobs[[i]]$grobs[[1]]$childrenOrder))
                        gt$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
                        k <- k+1
                }
                
                # Add line to grob
                gt <- gtable_add_grob(gt, line_grob, t = max(strip$t), l = min(strip$l), r = max(strip$r))
                
                # For storing in list
                p <- gt

                # Append plot to the list of plots
                rv_plots$stam <- list.append(rv_plots$stam, p)
                
                # Append data
                # Transform Period_num back to numeric
                table$Value <- table$sum_value
                table$Ctry_group_num <- as.numeric(table$Ctry_group_num)
                table$Period <- table$Period_num
                table$Period_num <- as.numeric(table$Period_num)
                
                table$Years <- NA

                if(rv_stam$time_subper & rv_input$time_subper){
                        
                        # Period 1
                        if(rv_stam$time_range[1] > rv_input$time_range_start){
                                x <- paste0(rv_stam$time_range[1])
                                table$Years[table$Period_num == 1] <- x
                        }else{
                                x <- paste0(as.character(rv_input$time_range_start))
                                table$Years[table$Period_num == 1] <- x
                        }
                        if(rv_stam$time_range[2] < rv_input$time_limit_1){
                                table$Years[table$Period_num == 1] <- paste0(x, "-", rv_stam$time_range[2])
                        }else{
                                table$Years[table$Period_num == 1] <- paste0(x, "-", as.character(rv_input$time_limit_1))
                        }
                        
                        # Period 2
                        if(rv_input$time_subper_num == 2){
                                if(rv_stam$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste0(rv_stam$time_range[1])
                                        table$Years[table$Period_num == 2] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_1+1))
                                        table$Years[table$Period_num == 2] <- x
                                }
                                if(rv_stam$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", rv_stam$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }else{
                                if(rv_stam$time_range[1] > rv_input$time_limit_1 + 1){
                                        x <- paste0(rv_stam$time_range[1])
                                        table$Years[table$Period_num == 2] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_1+1))
                                        table$Years[table$Period_num == 2] <- x
                                }
                                if(rv_stam$time_range[2] < rv_input$time_limit_2){
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", rv_stam$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 2] <- paste0(x, "-", as.character(rv_input$time_limit_2))
                                }
                        }
                        
                        # Period 3
                        if(rv_input$time_subper_num == 3){
                                if(rv_stam$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste0(rv_stam$time_range[1])
                                        table$Years[table$Period_num == 3] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_2+1))
                                        table$Years[table$Period_num == 3] <- x
                                }
                                if(rv_stam$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", rv_stam$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }else{
                                if(rv_stam$time_range[1] > rv_input$time_limit_2 + 1){
                                        x <- paste0(rv_stam$time_range[1])
                                        table$Years[table$Period_num == 3] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_2+1))
                                        table$Years[table$Period_num == 3] <- x
                                }
                                if(rv_stam$time_range[2] < rv_input$time_limit_3){
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", rv_stam$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 3] <- paste0(x, "-", as.character(rv_input$time_limit_3))
                                }
                        }
                        
                        # Period 4
                        if(rv_input$time_subper_num == 4){
                                if(rv_stam$time_range[1] > rv_input$time_limit_3 + 1){
                                        x <- paste0(rv_stam$time_range[1])
                                        table$Years[table$Period_num == 4] <- x
                                }else{
                                        x <- paste0(as.character(rv_input$time_limit_3+1))
                                        table$Years[table$Period_num == 4] <- x
                                }
                                if(rv_stam$time_range[2] < rv_input$time_range_end){
                                        table$Years[table$Period_num == 4] <- paste0(x, "-", rv_stam$time_range[2])
                                }else{
                                        table$Years[table$Period_num == 4] <- paste0(x, "-", as.character(rv_input$time_range_end))
                                }
                        }
                }
                table$Title <- if(rv_stam$title){title_text}else{""} 
                table$Subtitle <- if(rv_stam$title){subtitle_text}else{""} 
                table$Y_axis <- if(rv_stam$yaxis){yaxis_units}else{""}
                table$Source <- if(rv_stam$source){graph_source}else{""}
                
                # Select columns
                table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Years", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source" ))
                rv_plots$stam_data <- list.append(rv_plots$stam_data, table)
                
                # Show plot
                renderPlot(grid.draw(gt))
                
        }
        
        # Call Prep data and Create plots
        output$gr_stam_out_plots <- renderUI({
                
                print("****************************")
                print("Plots - Stam: start renderUI")
                
                rv_plots$stam <- list()
                rv_plots$stam_data <- list()
                
                pd <- req(prepped_data_stam())
                
                rv_plots$stam_counter <- 0
                rv_plots$stam_counter_total <- length(isolate(pd))
                
                final <- isolate(tagList(map(pd, ~ createUI_stam(.))))
                
                print("Plots - Stam: finish renderUI")
                print("****************************")
                cat("\n")
                
                return(final)
                
        })
        
        ### Plots download handlers ----
        
        # Download plots as png zipped - large
        output$gr_stam_download_large <- downloadHandler(
                filename = 'stacked_multiple_plots_large.zip',
                content = function(file){
                        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        
                        # Save the plots
                        vector_plots <- vector()
                        
                        for (i in 1:length(rv_plots$stam)){
                                name <- paste("stacked_multiple_large", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$stam[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("stacked_multiple_large", i, ".png", sep = ""))
                                
                                name_data <- paste("stacked_multiple_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$stam_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("stacked_multiple_large_data", i, ".csv", sep = ""))
                                
                        }
                        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        
        # Download plots as png zipped - small
        output$gr_stam_download_small <- downloadHandler(
                filename = 'stacked_multiple_plots_small.zip',
                content = function(file){
                        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        
                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$stam)){
                                name <- paste("stacked_multiple_small", i, ".png", sep = "")
                                
                                # Increase intervals in X axis in small plots
                                intervals <- ifelse(max(rv_stam$time_range) - min(rv_stam$time_range) < 30, 2, 4)
                                rv_plots$stam[[i]] <- rv_plots$stam[[i]] +
                                        scale_x_continuous(name = "",
                                                breaks = seq(min(rv_stam$time_range),
                                                        max(rv_stam$time_range),
                                                        by = intervals))+
                                        theme(legend.position = "bottom")
                                
                                ggsave(name,
                                        plot = rv_plots$stam[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("stacked_multiple_small", i, ".png", sep = ""))
                                
                                name_data <- paste("stacked_multiple_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$stam_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("stacked_multiple_small_data", i, ".csv", sep = ""))
                                
                        }
                        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        
##************************************************************************************************** ----
        
        ## Line plots: single variable ----
         
        ### Reactive values ----
        rv_line <- reactiveValues(

                # Plot parameters
                ctries =def_list_line$ctries,
                vars = def_list_line$vars,
                time_range = def_list_line$time_range,

                # Display parameters
                title = def_list_line$title,
                yaxis = def_list_line$yaxis,
                source = def_list_line$source,
                data_labels = def_list_line$data_labels,
                time_subper = def_list_line$time_subper,
                transform_zeros = def_list_line$transform_zeros,
                transform_log = def_list_line$transform_log,
                ctry_short = def_list_line$ctry_short,
                digits = def_list_line$digits,
                digits_y = def_list_line$digits_y,
                font = def_list_line$font,
                highlight = def_list_line$highlight,
                highlight_ctry = def_list_line$highlight_ctry,
                highlight_color = def_list_line$highlight_color

        )
         
        ### Update inputs ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {

                print("Plots - Line [1]: Updating inputs")

                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
                aux_ctry <- aux_ctry[order(
                        aux_ctry$Ctry_group_num,
                        aux_ctry$Country), ]
                ctry_choices <- as.list(aux_ctry$Country)
                names(ctry_choices) <- aux_ctry$Ctry_slash_Group
                ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)

                updatePickerInput(
                        session = session,
                        inputId = 'gr_line_id_ctries',
                        choices = ctry_choices,
                        selected = ctry_select,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Variable input
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                updatePickerInput(
                        session = session,
                        inputId = 'gr_line_id_vars',
                        label = "Select variables",
                        choices = aux_var,
                        selected = aux_var,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )

                # Time input
                updateSliderInput(
                        session = session,
                        inputId = "gr_line_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end)
                )

                # Highlighted country input
                updateSelectInput(
                        inputId = 'gr_line_id_highlight_ctry',
                        choices = ctry_choices,
                        selected = ctry_select[1])

        })

        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{

                print("Plots - Line [2]: Updating reactive values [changes in raw data]")

                # Country 
                aux_ctries <- unique(rv_df$dat_all$Country)
                rv_line$ctries <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]
                
                # Hightlight country
                rv_line$highlight_ctry <- aux_ctries[1]
                
                # Variables (select all variables from new dataset)
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
                rv_line$vars <- aux_var

                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_line$time_range <- aux_year

        }, ignoreInit = TRUE)

        ###  Update reactive values to changes in line plot parameters ----

        observeEvent(c(input$gr_line_id_update),{
                
                print("Plots - Line [2]: Updating reactive values [update bar plot parameters]")
                
                # Plot parameters
                rv_line$ctries <- input$gr_line_id_ctries
                rv_line$vars <- input$gr_line_id_vars
                rv_line$time_range <- input$gr_line_id_time_range
                
                # Display parameters
                rv_line$title <- input$gr_line_id_title
                rv_line$yaxis <- input$gr_line_id_yaxis
                rv_line$source <- input$gr_line_id_source
                rv_line$data_labels <- input$gr_line_id_data_labels
                rv_line$time_subper <- input$gr_line_id_time_subper
                rv_line$transform_zeros <- input$gr_line_id_transform_zeros
                rv_line$transform_log <- input$gr_line_id_transform_log
                rv_line$ctry_short <- input$gr_line_id_ctry_short
                rv_line$digits <- input$gr_line_id_digits
                rv_line$digits_y <- input$gr_line_id_digits_y
                rv_line$font <- input$gr_line_id_font
                rv_line$highlight <- input$gr_line_id_highlight
                rv_line$highlight_ctry <- input$gr_line_id_highlight_ctry
                rv_line$highlight_color <- input$gr_line_id_highlight_color

        }, ignoreInit = TRUE)

        ### Plots ----
        
        # Prep data
        prepped_data_line <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_line_id_update),{
                
                print("Plots - Line [3]: preparing data")
                
                if(nrow(rv_df$dat_all)==0){return()}
                if(length(rv_line$ctries)==0){return()}

                graph_input <- prep_data_for_graphs(
                        df = rv_df$dat_all,
                        vars = rv_line$vars,
                        ctries = rv_line$ctries,
                        t_start = rv_line$time_range[1],
                        t_end = rv_line$time_range[2]
                )

                # Create subperiod variables to include rectangles
                if(rv_input$time_subper & rv_line$time_subper){
                        subper_list <- subper_rectangles_fun(
                                subper_num = rv_input$time_subper_num,
                                time_range = rv_line$time_range,
                                time_lim_1 = rv_input$time_limit_1,
                                time_lim_2 = rv_input$time_limit_2,
                                time_lim_3 = rv_input$time_limit_3,
                                time_name_1 = rv_input$time_name_1,
                                time_name_2 = rv_input$time_name_2,
                                time_name_3 = rv_input$time_name_3,
                                time_name_4 = rv_input$time_name_4
                        )

                        rv_line$rectangle_text <- subper_list[[1]]
                        rv_line$vertical_lines <- subper_list[[2]]
                }

                return(graph_input)

        })
       
        # Create plots
        createUI_line <- function(table) {

                rv_plots$line_counter <- rv_plots$line_counter +1
                print(paste0("Render line plot ", rv_plots$line_counter, "/", rv_plots$line_counter_total))
                
                rectangle_text <- rv_line$rectangle_text
                vertical_lines <- rv_line$vertical_lines
                
                if(all(is.na(table$Value))){return()}
        
                # Parameters for plot
                
                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""
        
                title_text <- if(rv_line$title){paste0(unique(table$Var_name))}
        
                yaxis_units <- if(rv_line$yaxis){
                        if(is.na(unique(table$Units))){NULL} else {unique(table$Units)}
                } else {NULL}
        
                if(rv_line$source){
                        graph_source <- unique(table$Database)
                        if(graph_source == "WDI"){
                                graph_source <- "World Development Indicators"}
                }
        
                # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                intervals <- ifelse(max(rv_line$time_range) - min(rv_line$time_range) < 30, 1, 2)
        
                # Transform data: divide by trillions/billions/millions/thousands
                for (i in 4:1){
                        if(rv_line$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                                if(rv_line$title) {
                                        subtitle_text <- paste(
                                                c(subtitle_text, units_zeros[5 - i]),
                                                collapse = "")
                                        separator <- if(subtitle_text == ""){""} else {", "}
                                        if(rv_line$yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, units_zeros[5 - i]),
                                                        collapse = separator)
                                        }
                                }
                                table$Value <- table$Value/(10^(3*i))
                        }
                }
        
                # Log transformation
                if(rv_line$transform_log & min(table$Value, na.rm = TRUE)>0){
                        subtitle_text <- paste(
                                c(subtitle_text, " (Log transformation)"),
                                collapse = "")
                        if(rv_line$yaxis){
                                yaxis_units <- paste(
                                        c(yaxis_units, " (Log transformation)"),
                                        collapse = "")
                        }
                        table$Value <- log(table$Value)
                }
        
                # Warning sign if no data for this variable
                if(all(is.na(table$Value))) {subtitle_text <- "No data available"}
        
                # Short country names
                if(rv_line$ctry_short){
                        table$Country_aux <- table$Ctry_iso
                }else{
                        table$Country_aux <- table$Country
                        }

                # Actually Create plot
                
                table <- table %>% group_by(Country_aux) %>% mutate(Value_aux = na.locf(Value, na.rm = F))
                
                aux_table <- table[!is.na(table$Value),]

                aux_table <- aux_table %>%
                        group_by(Country_aux) %>%
                        slice(which.max(Year)) %>%
                        select(c("Country_aux", "Year"))

                aux_table$label2 <- aux_table$Country_aux

                table <- merge(
                        x = table,
                        y = aux_table,
                        by = c("Country_aux", "Year"),
                        all.x = TRUE
                )
                
                table <- table %>%
                        arrange(Ctry_group_num, Country_aux, Year) %>%
                        mutate(Country_aux = factor(Country_aux),
                                Country_aux = fct_reorder2(Country_aux, Ctry_group_num, Country)) %>%
                        mutate(label = if_else(Year == max(Year), as.character(Country_aux), NA_character_))


                        # Plot if no single country highlight option
                        if(!rv_line$highlight){
                                
                        p <- ggplot(data = table, aes(x = Year, y = Value, group = Country_aux, color = Country_aux)) +
                                geom_point(size = 2) +
                                geom_line(data = table[!is.na(table$Value),], size = 1)
                                
                        # Labels
                        p <- p + geom_text_repel(data = table,
                                aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = gsub("^.*$", "  ", label)), 
                                        family = rv_line$font,
                                        size = 3,
                                        segment.alpha = 0,
                                        box.padding = 0.1,
                                        point.padding = 0.6,
                                        nudge_x = 0.7,
                                        force = 0.5,
                                        hjust = 0,
                                        direction = "y",
                                        na.rm = TRUE,
                                        xlim = c(max(table$Year) + ceiling((max(table$Year)-min(table$Year))*0.04)
                                                , max(table$Year) + ceiling((max(table$Year)-min(table$Year))*0.30))
                                ) +
                                geom_text_repel(data = . %>% filter(!is.na(label2)),
                                        aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = paste0(" ", label2)),
                                        family = rv_line$font,
                                        size = 3,
                                        segment.curvature = 0,
                                        segment.color = 'grey',
                                        segment.size = 0.25,
                                        box.padding = 0.1,
                                        point.padding = 0.6,
                                        nudge_x = 0.7,
                                        force = 0.5,
                                        hjust = 0,
                                        direction = "y",
                                        na.rm = TRUE,
                                        xlim = c(max(table$Year) + ceiling((max(table$Year)-min(table$Year))*0.04)
                                                , max(table$Year) + ceiling((max(table$Year)-min(table$Year))*0.30))
                                )+ 
                                scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.9)
                        
                } else{
                        # Plot if highlight option selected
                        # Filter data
                        highlighted_country_df <- filter(table,
                                Country %in% rv_line$highlight_ctry)
                        rest_df <- filter(table,
                                !Country %in% rv_line$highlight_ctry)
                        
                        p <- ggplot(data = rest_df, aes(x = Year, y = Value, group = Country_aux, color = Country_aux)) +
                                geom_point(size = 2) +
                                geom_line(data = rest_df[!is.na(rest_df$Value),], size = 1)
                        
                        # Labels
                        p <- p + geom_text_repel(data = rest_df,
                                aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = gsub("^.*$", "  ", label)), 
                                family = rv_line$font,
                                size = 3,
                                segment.alpha = 0,
                                box.padding = 0.1,
                                point.padding = 0.6,
                                nudge_x = 0.7,
                                force = 0.5,
                                hjust = 0,
                                direction = "y",
                                na.rm = TRUE,
                                xlim = c(max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.04)
                                        , max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.30))
                        ) +
                                geom_text_repel(data = . %>% filter(!is.na(label2)),
                                        aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = paste0(" ", label2)),
                                        family = rv_line$font,
                                        size = 3,
                                        segment.curvature = 0,
                                        segment.color = 'grey',
                                        segment.size = 0.25,
                                        box.padding = 0.1,
                                        point.padding = 0.6,
                                        nudge_x = 0.7,
                                        force = 0.5,
                                        hjust = 0,
                                        direction = "y",
                                        na.rm = TRUE,
                                        xlim = c(max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.04)
                                                , max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.30))
                                )+ 
                                scale_colour_grey()
                        
                        if(!all(is.na(highlighted_country_df$Value))){
                                p <- p +                                 geom_line(data = highlighted_country_df, aes(x = Year, y = Value), color = rv_line$highlight_color, size = 1.5) +
                                        geom_point(data = highlighted_country_df, aes(x = Year, y = Value), fill = rv_line$highlight_color, size = 4, stroke = 0.75, colour = "black", shape = 21) +
                                        geom_text_repel(data = highlighted_country_df,
                                                aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = gsub("^.*$", "  ", label)),
                                                family = rv_line$font,
                                                size = 3,
                                                segment.curvature = 0,
                                                segment.color = 'grey',
                                                color = rv_line$highlight_color,
                                                segment.size = 0.25,
                                                box.padding = 0.1,
                                                point.padding = 0.6,
                                                nudge_x = 0.7,
                                                force = 0.5,
                                                hjust = 0,
                                                direction = "y",
                                                na.rm = TRUE,
                                                xlim = c(max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.04)
                                                        , max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.30))
                                        )+
                                        geom_text_repel(data = highlighted_country_df %>% filter(!is.na(label2)),
                                                aes(x = Year, y = Value_aux, group = Country_aux, color = Country_aux, label = paste0(" ", label2)),
                                                family = rv_line$font,
                                                size = 5,
                                                segment.curvature = 0,
                                                segment.color = 'grey',
                                                color = rv_line$highlight_color,
                                                fontface = "bold",
                                                segment.size = 0.25,
                                                box.padding = 0.1,
                                                point.padding = 0.6,
                                                nudge_x = 0.7,
                                                force = 0.5,
                                                hjust = 0,
                                                direction = "y",
                                                na.rm = TRUE,
                                                xlim = c(max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.04)
                                                        , max(rest_df$Year) + ceiling((max(rest_df$Year)-min(rest_df$Year))*0.30))
                                        )
                        }
                }
                
                # Short country names
                if(rv_line$ctry_short){
                        p <- p + scale_x_continuous(name="",
                                breaks = seq(min(rv_line$time_range),
                                        max(rv_line$time_range),
                                        by = intervals),
                                limits=c(min(table$Year)-0.5, max(table$Year)+ ceiling((max(table$Year)-min(table$Year))*0.08)),
                                expand = c(0, 0)
                        )
                }else{
                        p <- p + scale_x_continuous(name="",
                                breaks = seq(min(rv_line$time_range),
                                        max(rv_line$time_range),
                                        by = intervals),
                                limits=c(min(table$Year)-0.5, max(table$Year)+ ceiling((max(table$Year)-min(table$Year))*0.15)),
                                expand = c(0, 0)
                        )
                }
                
                title_text_paragraph <- str_wrap(title_text, width = 130)
                
                p <- p +
                        # Include title, subtitle, source and Y-axis title
                        labs(title = title_text_paragraph,
                                subtitle = subtitle_text,
                                caption = if(rv_line$source){paste("Source: ", graph_source, ".", sep = "")},
                                y = if(rv_line$yaxis){yaxis_units}
                        )+
                        # Aesthetics
                        theme_minimal() +
                        theme(
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(linetype = "dashed"),
                                panel.grid.minor.y = element_blank(),
                                panel.grid.minor.x = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 3, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12,  family = rv_line$font),
                                legend.position = "none"
                                )
        
                # Include data labels
                if(rv_line$data_labels){p <- p +
                        geom_text(data = table,
                                aes(x = Year, y = Value, label = format(round(as.numeric(Value), rv_line$digits), nsmall = rv_line$digits, big.mark = ",")),
                                vjust = ifelse(table$Value <0 , 1.5, -0.5),
                                hjust = 0.5,
                                size = 3,
                                family = rv_line$font, 
                                na.rm = TRUE,
                                inherit.aes = FALSE
                                )
                }
        
                # Increase margins inside graph by extending the range 15% (7.5% above and below)
                y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range <- y_max - y_min
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(rv_line$digits_y), big.mark = ","),
                        breaks = pretty_breaks(),
                        limits = c(y_min, y_max + (y_range * 0.1)))
        
                # To use in vertical lines
                y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                y_range_new <- y_max - y_min
        
                size_factor <- 0.08
                ALPHA <- 0.15

                # Subperiod rectangles, their labels and the dotted lines separating
                if(rv_input$time_subper & rv_line$time_subper ){
                        p <- p +
                                geom_rect(data = rectangle_text,
                                        aes(NULL, NULL, xmin = my_min_fun(Year_start), xmax = my_max_fun(Year_end)),
                                        ymin = y_max_new - y_range_new * size_factor ,
                                        ymax = y_max_new,
                                        colour = NA,
                                        fill = "grey",
                                        alpha = 0.5,
                                        inherit.aes = FALSE)+
                                geom_label(data = rectangle_text,
                                        aes(x = Year_start + (Year_end - Year_start)/2,
                                                y = y_max_new * ALPHA + (y_max_new - y_range_new * size_factor)*(1 - ALPHA),
                                                label = Period,
                                                family = rv_line$font
                                        ),
                                        size = 3.3,
                                        fill = "grey",
                                        alpha = 0,
                                        label.size = NA,
                                        hjust = "center",
                                        vjust = "bottom",
                                        inherit.aes = FALSE)
        
                        for (i in vertical_lines) {
                                p <- p + geom_segment(x = i,
                                        y = y_max_new,
                                        xend = i,
                                        yend = y_max_new - y_range_new * size_factor,
                                        colour = "white",
                                        size = 1,
                                        alpha = 1) +
                                        geom_segment(x = i,
                                                y = y_max_new - y_range_new * size_factor,
                                                xend = i,
                                                yend = -Inf,
                                                colour = "grey",
                                                linetype = "dotted")
                        }
                }
        
                # Correct digits if numbers are repeated in Y-axis
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, rv_line$digits_y)))
                digits <- rv_line$digits_y
        
                while(yaxis_number_labels > yaxis_number_labels_obs){
                        digits <- digits + 1
                        p <- p + scale_y_continuous(name = yaxis_units,
                                labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                                breaks = pretty_breaks())
        
                        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                        x <- x[!is.na(x)]
                        yaxis_number_labels <- length(x)
                        yaxis_number_labels_obs <- length(unique(round(x, digits)))
                }
        
                # Append plot to the list of plots
                rv_plots$line <- isolate(list.append(rv_plots$line, p))
                
                # Append data
                table$Title <- if(rv_line$title){title_text}else{""} 
                table$Subtitle <- if(rv_line$title){subtitle_text}else{""} 
                table$Y_axis <- if(rv_line$yaxis){yaxis_units}else{""}
                table$Source <- if(rv_line$source){graph_source}else{""}
                
                # Select columns
                table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
                
                rv_plots$line_data <- isolate(list.append(rv_plots$line_data, table))
        
                # Show plot
                renderPlot(p)
        }
        
        # Call Prep data and Create plots
        output$gr_line_out_plots <- renderUI({

                print("****************************")
                print("Plots - Line: start renderUI")

                rv_plots$line <- list()
                rv_plots$line_data <- list()

                pd <- req(prepped_data_line())

                rv_plots$line_counter <- 0
                rv_plots$line_counter_total <- length(isolate(pd))

                final <- isolate(tagList(map(pd, ~ createUI_line(.))))

                print("Plots - Line: finish renderUI")
                print("****************************")
                cat("\n")

                return(final)
        })

        ### Plots download handlers ----
        
        # Download plots as png zipped - large
        output$gr_line_download_large <- downloadHandler(
                filename = 'gr_line_plots_large.zip',
                content = function(file){
        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
        
                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$line)){
        
                                name <- paste("lineplot_large", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$line[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("lineplot_large", i, ".png", sep = ""))
        
                                name_data <- paste("lineplot_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$line_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("lineplot_large_data", i, ".csv", sep = ""))
        
                        }
        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        
        # Download plots as png zipped - small
        output$gr_line_download_small <- downloadHandler(
                filename = 'gr_line_plots_small.zip',
                content = function(file){
        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
        
                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$line)){
        
                                # Increase intervals in X axis in small plots
                                intervals <- ifelse(max(rv_line$time_range) - min(rv_line$time_range) < 30, 2, 4)
                                if(rv_line$ctry_short){
                                        rv_plots$line[[i]] <- rv_plots$line[[i]] +
                                                scale_x_continuous(name = "",
                                                        breaks = seq(min(rv_line$time_range),
                                                                max(rv_line$time_range),
                                                                by = intervals),
                                                        limits=c(min(rv_line$time_range)-0.5, max(rv_line$time_range)+ ceiling((max(rv_line$time_range)-min(rv_line$time_range))*0.15)),
                                                        expand = c(0, 0)
                                                )
                                }else{
                                        rv_plots$line[[i]] <- rv_plots$line[[i]] +
                                                scale_x_continuous(name = "",
                                                        breaks = seq(min(rv_line$time_range),
                                                                max(rv_line$time_range),
                                                                by = intervals),
                                                        limits=c(min(rv_line$time_range)-0.5, max(rv_line$time_range)+ ceiling((max(rv_line$time_range)-min(rv_line$time_range))*0.35)),
                                                        expand = c(0, 0)
                                                )
                                }

        
                                name <- paste("lineplot_small", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$line[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("lineplot_small", i, ".png", sep = ""))
        
                                name_data <- paste("lineplot_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$line_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("lineplot_small_data", i, ".csv", sep = ""))
        
                        }
        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        
##************************************************************************************************** ----

## Line plots: multiple variables ----

### Reactive values ----
rv_linm <- reactiveValues(
        
        # Plot parameters
        ctries = def_list_linm$ctries,
        time_range = def_list_linm$time_range,
        plot_num = def_list_linm$plot_num,
        
        # Individual plot parameters
        vars = def_list_linm$vars,
        title_text = def_list_linm$title_text,
        yaxis_text = def_list_linm$yaxis_text,
        
        # Display parameters
        title = def_list_linm$title,
        yaxis = def_list_linm$yaxis,
        source = def_list_linm$source,
        data_labels = def_list_linm$data_labels,
        time_subper = def_list_linm$time_subper,
        transform_zeros = def_list_linm$transform_zeros,
        transform_log = def_list_linm$transform_log,
        ctry_short = def_list_linm$ctry_short,
        digits = def_list_linm$digits,
        digits_y = def_list_linm$digits_y,
        font = def_list_linm$font,
        legend_pos = def_list_linm$legend_pos
        
)        


### Update inputs ----

# Inputs that are independent of the number of plots
observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {
        
        print("Plots - Linm [1]: Updating inputs")
        
        # Country input
        aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
        aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
        aux_ctry <- aux_ctry[order(
                aux_ctry$Ctry_group_num,
                aux_ctry$Country), ]
        ctry_choices <- as.list(aux_ctry$Country)
        names(ctry_choices) <- aux_ctry$Ctry_slash_Group
        ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)
        
        updatePickerInput(
                session = session,
                inputId = 'gr_linm_id_ctries',
                choices = ctry_choices,
                selected = rv_linm$ctries,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Time input
        updateSliderInput(
                session = session,
                inputId = "gr_linm_id_time_range",
                min = rv_input$time_range_start,
                max = rv_input$time_range_end,
                step = 1,
                value = c(rv_input$time_range_start, rv_input$time_range_end)
        )
})

# Generate reactive values for each plot (depends on number of plots)
observeEvent(c(input$in_id_update,
        input$in_id_reset_confirm,
        input$gr_linm_id_plot_num),{
                
                print("Plots - Linm [2]: Generate reactive values (depends on number of plots)")
                
                if(nrow(rv_df$dat_all)==0){return()}
                
                # If reactive value is null
                for(i in 1:input$gr_linm_id_plot_num){
                        
                        # We hard code the first plot to have the trade example plotted
                        
                        # Variables names
                        if(is.null(eval(parse(text = paste0("rv_linm$vars", i))))){
                                eval(parse(text = paste0("rv_linm$vars", i, " <- rv_linm$vars")))
                        }
                        
                        # Title and Y-axis legend
                        if(is.null(eval(parse(text = paste0("rv_linm$title_text", i))))){
                                if(i==1){title <- rv_linm$title_text}else{title <- paste0("Title ", i)}
                                eval(parse(text = paste0("rv_linm$title_text", i, " <- title")))
                        }
                        
                        if(is.null(eval(parse(text = paste0("rv_linm$yaxis_text", i))))){
                                if(i==1){yaxis <- rv_linm$yaxis_text}else{yaxis <- paste0("Y-axis title ", i)}
                                eval(parse(text = paste0("rv_linm$yaxis_text", i, " <- yaxis")))
                        }
                        
                }
        })


# Render UI for inputs of each plot (number of input widgets created depends on choice of number of plots)
output$gr_linm_id_input_panels <- renderUI({
        
        print("Plots - Linm [3]: Generate input widgets for individual plots")
        
        if(nrow(rv_df$dat_all)==0){return()}
        
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        
        lapply(1:input$gr_linm_id_plot_num, function(i) {
                
                # Inputs that depend on the number of plots
                
                wellPanel(
                        style = "background-color: #ebf5fb",
                        
                        h4(paste("Stacked plot "), i),
                        
                        fluidPage(
                                
                                selectizeInput(
                                        inputId = paste0('gr_linm_id_vars', i),
                                        label = "Select variables",
                                        choices = aux_var,
                                        multiple = TRUE,
                                        select = eval(parse(text = paste0("rv_linm$vars", i))),
                                        options = list(placeholder = 'Select')
                                ),
                                
                                textInput(
                                        inputId = paste0("gr_linm_id_title_text",i),
                                        label = "Plot title text",
                                        value = isolate(eval(parse(text = paste0("rv_linm$title_text", i))))
                                ),
                                
                                textInput(
                                        inputId = paste0("gr_linm_id_yaxis_text", i),
                                        label = "Y-axis text",
                                        value = isolate(eval(parse(text = paste0("rv_linm$yaxis_text", i))))
                                )
                        )
                )
        })
})

###  Update reactive values to changes in raw data ----
observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
        
        print("Plots - Linm [4]: Updating reactive values [changes in raw data]")
        
        # Country 
        aux_ctries <- unique(rv_df$dat_all$Country)
        # rv_linm$ctries <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]
        rv_linm$ctries <- aux_ctries[aux_ctries %in% rv_linm$ctries]
        
        # Variables (select all variables from new dataset)
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        rv_linm$vars <- aux_var
        
        # Time range (select all years of new dataset)
        aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
        rv_linm$time_range <- aux_year
        
}, ignoreInit = TRUE)

###  Update reactive values to changes in linm plot parameters ----

observeEvent(c(input$gr_linm_id_update),{
        
        print("Plots - Linm [5]: Updating reactive values [update linm plot parameters]")
        
        # Plot parameters
        rv_linm$ctries <- input$gr_linm_id_ctries
        rv_linm$time_range <- input$gr_linm_id_time_range
        
        # Individual plot parameters
        rv_linm$plot_num <- input$gr_linm_id_plot_num
        
        for (i in 1:input$gr_linm_id_plot_num){
                
                eval(parse(text = paste0("rv_linm$vars", i, " <- input$gr_linm_id_vars", i)))
                eval(parse(text = paste0("rv_linm$title_text", i, " <- input$gr_linm_id_title_text", i)))
                eval(parse(text = paste0("rv_linm$yaxis_text", i, " <- input$gr_linm_id_yaxis_text", i)))
                
        }
        
        # Display parameters
        rv_linm$title <- input$gr_linm_id_title
        rv_linm$yaxis <- input$gr_linm_id_yaxis
        rv_linm$source <- input$gr_linm_id_source
        rv_linm$data_labels <- input$gr_linm_id_data_labels
        rv_linm$time_subper <- input$gr_linm_id_time_subper
        rv_linm$transform_zeros <- input$gr_linm_id_transform_zeros
        rv_linm$transform_log <- input$gr_linm_id_transform_log
        rv_linm$ctry_short <- input$gr_linm_id_ctry_short
        rv_linm$digits <- input$gr_linm_id_digits
        rv_linm$digits_y <- input$gr_linm_id_digits_y
        rv_linm$font <- input$gr_linm_id_font
        rv_linm$legend_pos <- input$gr_linm_id_legend_pos
        
}, ignoreInit = TRUE)

### Plots ----

# Prep data
prepped_data_linm <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_linm_id_update),{
        
        print("Plots - Linm [6]: preparing data")
        
        if(nrow(rv_df$dat_all)==0){return()}
        if(length(rv_linm$ctries)==0){return()}
        
        # Filter by country
        df_aux <- rv_df$dat_all[rv_df$dat_all$Country %in% rv_linm$ctries, ]
        
        # Create variable_name | variable_code column
        df_aux$Var_slash_code <- paste(df_aux$Var_name, df_aux$Var_code, sep = " | ")
        
        #Create empty list for tibbles
        tibble_list <- list()
        
        for (i in 1:rv_linm$plot_num){
                
                list_name <- paste0("table", i)
                
                if(is.null(eval(parse(text = paste0("rv_linm$vars", i))))){
                        
                        tibble_list[[list_name]] <- data.frame()
                        
                }else{
                        
                        # Select variables
                        df_aux_2 <- df_aux[df_aux$Var_slash_code %in% eval(parse(text = paste0("rv_linm$vars", i))), ]
                        
                        # Filter data by year
                        df_aux_2 <- df_aux_2 %>% filter(Year >= rv_linm$time_range[1] & Year <= rv_linm$time_range[2])
                        
                        # Title
                        df_aux_2 <- df_aux_2 %>% tibble::add_column(
                                Title = as.character(paste0(eval(parse(text = paste0("rv_linm$title_text", i))))),
                                Y_axis = as.character(eval(parse(text = paste0("rv_linm$yaxis_text", i)))),
                        )
                        
                        # Append to list
                        df_aux_2 <- df_aux_2 %>% as_tibble()
                        tibble_list[[list_name]] <- df_aux_2
                        
                }
                
                rv_linm$tibble_list <- tibble_list
        }
        
        # Create subperiod variables to include rectangles
        if(rv_input$time_subper & rv_linm$time_subper){
                subper_list <- subper_rectangles_fun(
                        subper_num = rv_input$time_subper_num,
                        time_range = rv_linm$time_range,
                        time_lim_1 = rv_input$time_limit_1,
                        time_lim_2 = rv_input$time_limit_2,
                        time_lim_3 = rv_input$time_limit_3,
                        time_name_1 = rv_input$time_name_1,
                        time_name_2 = rv_input$time_name_2,
                        time_name_3 = rv_input$time_name_3,
                        time_name_4 = rv_input$time_name_4
                )
                
                rv_linm$rectangle_text <- subper_list[[1]]
                rv_linm$vertical_lines <- subper_list[[2]]
        }
        
        return(rv_linm$tibble_list)
        
})

# Create plots
createUI_linm <- function(table) {
        
        rv_plots$linm_counter <- rv_plots$linm_counter +1
        print(paste0("Render linm plot ", rv_plots$linm_counter, "/", rv_plots$linm_counter_total))
        
        rectangle_text <- rv_linm$rectangle_text
        vertical_lines <- rv_linm$vertical_lines
        
        if(all(is.na(table$Value))){return()}
        
        title_text <- ""
        subtitle_text <- ""
        yaxis_units <- ""
        graph_source <- ""
        
        # Input for title, subtitle Y-axis and source
        title_text <- if(rv_linm$title){unique(table$Title)}
        yaxis_units <- if(rv_linm$yaxis){unique(table$Y_axis)} else {NULL}
        
        if(rv_linm$source){
                graph_source <- unique(table$Database)
                graph_source[graph_source=="WDI"] <- "World Development Indicators"
                graph_source <- paste(graph_source, collapse = ",")
                graph_source <- paste0(graph_source, ".")
        }
        
        # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
        intervals <- ifelse(rv_linm$time_range[2] - rv_linm$time_range[1] < 30, 1, 2)
        
        # Transform data: divide by trillions/billions/millions/thousands
        for (i in 4:1){
                if(rv_linm$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                        if(rv_linm$title) {
                                subtitle_text <- paste(
                                        c(subtitle_text, units_zeros[5 - i]),
                                        collapse = "")
                                separator <- if(subtitle_text == ""){""} else {", "}
                                if(rv_linm$yaxis){
                                        yaxis_units <- paste(
                                                c(yaxis_units, units_zeros[5 - i]),
                                                collapse = separator)
                                }
                        }
                        table$Value <- table$Value/(10^(3*i))
                }
        }
        
        # Log transform
        if(rv_linm$transform_log & min(table$Value, na.rm = TRUE)>0){
                subtitle_text <- paste(
                        c(subtitle_text, " (Log transformation)"),
                        collapse = "")
                if(rv_linm$yaxis){
                        yaxis_units <- paste(
                                c(yaxis_units, " (Log transformation)"),
                                collapse = "")
                }
                table$Value <- log(table$Value)
        }
        
        # Eliminates elements between parenthesis (and the parenthesis)
        legend_labels <- gsub("\\s*\\([^\\)]+\\)", "", as.character(unique(table$Var_name)))
        
        # If longer than 20 characters, divide legend labels in multiple lines
        legend_labels <- multiline_text_fun(text_vector = legend_labels, number_characters = 20)
        
        # Title
        title_text_paragraph <- str_wrap(title_text, width = 130)
        
        # Actually create plot
        
        p <- ggplot(data = table, aes(x = Year, y = Value, group = interaction(Country, Var_name), color = Country, shape=Var_name )) +
                geom_line(data = table[!is.na(table$Value),], size = 1.5) + 
                geom_point(size = 4)
        
        #         # Aesthetics
        p <- p +
                labs(title = title_text_paragraph,
                        subtitle = subtitle_text,
                        caption = if(rv_linm$source){paste("Source: ", graph_source, sep = "")},
                        y = if(rv_linm$yaxis){yaxis_units} else {NULL}) + 
                theme_minimal() +
                theme(panel.border = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_line(linetype = "dashed"),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(size = 12, face = "bold"),
                        plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                        plot.caption = element_text(hjust = 0, size = 10),
                        axis.ticks.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x = element_text(colour = "black"),
                        axis.text.y = element_text(colour = "black"),
                        text = element_text(size = 12,  family = isolate(rv_linm$font)),
                        legend.title = element_blank()
                )+
                
                # Define intervals of Year axis
                scale_x_continuous(name = "",
                        breaks = seq(rv_linm$time_range[1],
                                rv_linm$time_range[2],
                                by = intervals)
                )+
                scale_shape(labels = legend_labels)+
                coord_cartesian(xlim = c(rv_linm$time_range[1], rv_linm$time_range[2]))+
                scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.9)+
                
                # Legend position
                theme(legend.position = rv_linm$legend_pos[[1]])
        

        
        # Include data labels
        if(rv_linm$data_labels){
                
                p <- p +
                geom_text(data = table,
                        aes(x = Year, y = Value, label = format(round(as.numeric(Value), rv_linm$digits), nsmall = rv_linm$digits, big.mark = ",")),
                        vjust = -0.5,
                        size = 3,
                        family = rv_linm$font,
                        na.rm = TRUE,
                        inherit.aes = FALSE)

        }
        
        # Increase range of Y axis to make room for the box indicating subperiod
        y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
        y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
        y_range <- y_max - y_min
        
        # Include thousands separating comma in Y-axis
        p <- p + scale_y_continuous(name = yaxis_units,
                labels = comma_format(accuracy = 1/10^(rv_linm$digits_y), big.mark = ","),
                breaks = pretty_breaks(),
                limits = c(y_min, y_max + (y_range*0.1),
                        inherit.aes = FALSE)
        )
        
        # Get new axis limits that will be used as inputs to define the size of the subperiod rectangles
        y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
        y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
        y_range_new <- y_max - y_min
        
        #Define other parameters that will be used as inputs to define the size of the subperiod rectangles
        size_factor <- 0.08
        ALPHA <- 0.15
        
        # Subperiod rectangles, their labels and the dotted lines separating
        if(rv_input$time_subper & rv_linm$time_subper){
                p <- p +
                        geom_rect(data = rectangle_text,
                                aes(NULL, NULL, xmin = my_min_fun(Year_start), xmax = my_max_fun(Year_end)),
                                ymin = y_max_new - y_range_new * size_factor ,
                                ymax = y_max_new,
                                colour = NA,
                                fill="grey",
                                alpha = 0.5,
                                inherit.aes = FALSE
                        ) +
                        geom_label(data = rectangle_text,
                                aes(x = Year_start + (Year_end - Year_start) / 2,
                                        y = y_max_new * ALPHA + (y_max_new - y_range_new * size_factor) * (1 - ALPHA),
                                        label = Period,
                                        family = rv_linm$font
                                ),
                                size = 3.3,
                                fill = "grey",
                                alpha = 0,
                                label.size = NA,
                                hjust = "center",
                                vjust = "bottom",
                                inherit.aes = FALSE)

                for (i in vertical_lines) {
                        p <- p + geom_segment(x = i,
                                y = y_max_new,
                                xend = i,
                                yend = y_max_new - y_range_new * size_factor,
                                colour = "white",
                                size = 1,
                                alpha = 1,
                                inherit.aes = FALSE) +
                                geom_segment(x = i,
                                        y = y_max_new - y_range_new * size_factor,
                                        xend = i,
                                        yend = -Inf,
                                        colour = "grey",
                                        linetype = "dotted",
                                        inherit.aes = FALSE)
                }
        }

        # Correct digits if numbers are repeated in Y-axis
        x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
        x <- x[!is.na(x)]
        yaxis_number_labels <- length(x)
        yaxis_number_labels_obs <- length(unique(round(x, rv_linm$digits_y)))
        digits <- rv_linm$digits_y
        
        while(yaxis_number_labels > yaxis_number_labels_obs){
                digits <- digits + 1
                p <- p + scale_y_continuous(name = yaxis_units,
                        labels = comma_format(accuracy = 1/10^(digits), big.mark = ","),
                        breaks = pretty_breaks())
                
                x <- ggplot_build(p)$layout$panel_params[[1]]$y.sec$breaks
                x <- x[!is.na(x)]
                yaxis_number_labels <- length(x)
                yaxis_number_labels_obs <- length(unique(round(x, digits)))
        }
        
        # Append plot to the list of plots
        rv_plots$linm <- list.append(rv_plots$linm, p)
        
        # Append data
        table$Title <- if(rv_linm$title){title_text}else{""} 
        table$Subtitle <- if(rv_linm$title){subtitle_text}else{""} 
        table$Y_axis <- if(rv_linm$yaxis){yaxis_units}else{""}
        table$Source <- if(rv_linm$source){graph_source}else{""}
        
        # Select columns
        table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
        
        
        rv_plots$linm_data <- list.append(rv_plots$stas_linm, table)
        
        # Show plot
        renderPlot(p)
        
        
        

}

# Call Prep data and Create plots
output$gr_linm_out_plots <- renderUI({
        
        print("****************************")
        print("Plots - Linm: start renderUI")
        
        rv_plots$linm <- list()
        rv_plots$linm_data <- list()
        
        pd <- req(prepped_data_linm())
        
        rv_plots$linm_counter <- 0
        rv_plots$linm_counter_total <- length(isolate(pd))
        
        final <- isolate(tagList(map(pd, ~ createUI_linm(.))))
        
        print("Plots - Linm: finish renderUI")
        print("****************************")
        cat("\n")
        
        return(final)
        
})

### Plots download handlers ----

# Download plots as png zipped - large
output$gr_linm_download_large <- downloadHandler(
        filename = 'line_multiple_plots_large.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                
                for (i in 1:length(rv_plots$linm)){
                        name <- paste("line_multiple_large", i, ".png", sep = "")
                        ggsave(name,
                                plot = rv_plots$linm[[i]],
                                device = "png",
                                width = 11.5,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("line_multiple_large", i, ".png", sep = ""))
                        
                        name_data <- paste("line_multiple_large_data", i, ".csv", sep = "")
                        write.csv(rv_plots$linm_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("line_multiple_large_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip(file, vector_plots)
        }
)

# Download plots as png zipped - small
output$gr_linm_download_small <- downloadHandler(
        filename = 'line_multiple_plots_small.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                for (i in 1:length(rv_plots$linm)){
                        name <- paste("line_multiple_small", i, ".png", sep = "")
                        
                        # Increase intervals in X axis in small plots
                        intervals <- ifelse(max(rv_linm$time_range) - min(rv_linm$time_range) < 30, 2, 4)
                        rv_plots$linm[[i]] <- rv_plots$linm[[i]] +
                                scale_x_continuous(name = "",
                                        breaks = seq(min(rv_linm$time_range),
                                                max(rv_linm$time_range),
                                                by = intervals))+
                                theme(legend.position = "bottom")
                        
                        ggsave(name,
                                plot = rv_plots$linm[[i]],
                                device = "png",
                                width = 5.75,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("line_multiple_small", i, ".png", sep = ""))
                        
                        name_data <- paste("line_multiple_small_data", i, ".csv", sep = "")
                        write.csv(rv_plots$linm_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("line_multiple_small_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip(file, vector_plots)
        }
)
        
##************************************************************************************************** ----

## Ranking across time plots ----

### Reactive values ----
rv_rnkt <- reactiveValues(
        
        # Plot parameters
        ctries =def_list_rnkt$ctries,
        vars = def_list_rnkt$vars,
        time_range = def_list_rnkt$time_range,
        incomplete = def_list_rnkt$incomplete,
        descending = def_list_rnkt$descending,
        
        # Display parameters
        ctries_disp =def_list_rnkt$ctries_disp,
        title = def_list_rnkt$title,
        yaxis = def_list_rnkt$yaxis,
        source = def_list_rnkt$source,
        time_subper = def_list_rnkt$time_subper,
        ctry_short = def_list_rnkt$ctry_short,
        font = def_list_rnkt$font,
        highlight = def_list_rnkt$highlight,
        highlight_ctry = def_list_rnkt$highlight_ctry,
        highlight_color = def_list_rnkt$highlight_color
        
)

### Update inputs ----
observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {
        
        print("Plots - Rnkt [1]: Updating inputs")
        
        # Country input
        aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
        aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
        aux_ctry <- aux_ctry[order(
                aux_ctry$Ctry_group_num,
                aux_ctry$Country), ]
        ctry_choices <- as.list(aux_ctry$Country)
        names(ctry_choices) <- aux_ctry$Ctry_slash_Group
        ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)
        
        updatePickerInput(
                session = session,
                inputId = 'gr_rnkt_id_ctries',
                choices = ctry_choices,
                selected = ctry_select,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Country display
        updatePickerInput(
                session = session,
                inputId = 'gr_rnkt_id_ctries_disp',
                choices = ctry_choices,
                selected = ctry_select,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Variable input
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        
        updatePickerInput(
                session = session,
                inputId = 'gr_rnkt_id_vars',
                label = "Select variables",
                choices = aux_var,
                selected = rv_rnkt$vars,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Time input
        updateSliderInput(
                session = session,
                inputId = "gr_rnkt_id_time_range",
                min = rv_input$time_range_start,
                max = rv_input$time_range_end,
                step = 1,
                value = c(rv_input$time_range_start, rv_input$time_range_end)
        )
        
        # Highlighted country input
        updateSelectInput(
                inputId = 'gr_rnkt_id_highlight_ctry',
                choices = ctry_choices,
                selected = ctry_select[1])
        
})

###  Update reactive values to changes in raw data ----
observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
        
        print("Plots - Rnkt [2]: Updating reactive values [changes in raw data]")
        
        # Country 
        aux_ctries <- unique(rv_df$dat_all$Country)
        rv_rnkt$ctries <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]
        
        # Countries display
        rv_rnkt$ctries_disp <- aux_ctries[aux_ctries %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg)]
        
        # Hightlight country
        rv_rnkt$highlight_ctry <- aux_ctries[1]
        
        # Variables (select all variables from new dataset)
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        rv_rnkt$vars <- aux_var
        
        # Time range (select all years of new dataset)
        aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
        rv_rnkt$time_range <- aux_year
        
}, ignoreInit = TRUE)

###  Update reactive values to changes in line plot parameters ----

observeEvent(c(input$gr_rnkt_id_update),{
        
        print("Plots - Rnkt [2]: Updating reactive values [update bar plot parameters]")
        
        # Plot parameters
        rv_rnkt$ctries <- input$gr_rnkt_id_ctries
        rv_rnkt$vars <- input$gr_rnkt_id_vars
        rv_rnkt$time_range <- input$gr_rnkt_id_time_range
        rv_rnkt$incomplete <- input$gr_rnkt_id_incomplete
        rv_rnkt$descending <- input$gr_rnkt_id_descending
        
        # Display parameters
        rv_rnkt$ctries_disp <- input$gr_rnkt_id_ctries_disp
        rv_rnkt$title <- input$gr_rnkt_id_title
        rv_rnkt$yaxis <- input$gr_rnkt_id_yaxis
        rv_rnkt$source <- input$gr_rnkt_id_source
        rv_rnkt$time_subper <- input$gr_rnkt_id_time_subper
        rv_rnkt$ctry_short <- input$gr_rnkt_id_ctry_short
        rv_rnkt$font <- input$gr_rnkt_id_font
        rv_rnkt$highlight <- input$gr_rnkt_id_highlight
        rv_rnkt$highlight_ctry <- input$gr_rnkt_id_highlight_ctry
        rv_rnkt$highlight_color <- input$gr_rnkt_id_highlight_color
        
}, ignoreInit = TRUE)

### Plots ----

# Prep data
prepped_data_rnkt <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_rnkt_id_update),{
        
        print("Plots - Rnkt [3]: preparing data")
        
        if(nrow(rv_df$dat_all)==0){return()}
        if(length(rv_rnkt$ctries)==0){return()}
        
        graph_input <- prep_data_for_graphs(
                df = rv_df$dat_all,
                vars = rv_rnkt$vars,
                ctries = rv_rnkt$ctries,
                t_start = rv_rnkt$time_range[1],
                t_end = rv_rnkt$time_range[2]
        )
        
        # Create subperiod variables to include rectangles
        if(rv_input$time_subper & rv_rnkt$time_subper){
                subper_list <- subper_rectangles_fun(
                        subper_num = rv_input$time_subper_num,
                        time_range = rv_rnkt$time_range,
                        time_lim_1 = rv_input$time_limit_1,
                        time_lim_2 = rv_input$time_limit_2,
                        time_lim_3 = rv_input$time_limit_3,
                        time_name_1 = rv_input$time_name_1,
                        time_name_2 = rv_input$time_name_2,
                        time_name_3 = rv_input$time_name_3,
                        time_name_4 = rv_input$time_name_4
                )
                
                rv_rnkt$rectangle_text <- subper_list[[1]]
                rv_rnkt$vertical_lines <- subper_list[[2]]
        }
        
        return(graph_input)
        
})

# Create plots
createUI_rnkt <- function(table) {
        
        rv_plots$rnkt_counter <- rv_plots$rnkt_counter +1
        print(paste0("Render rank dynamic plot ", rv_plots$rnkt_counter, "/", rv_plots$rnkt_counter_total))
        
        rectangle_text <- rv_rnkt$rectangle_text
        vertical_lines <- rv_rnkt$vertical_lines
        
        if(all(is.na(table$Value))){return()}
        
        # Parameters for plot
        
        title_text <- ""
        subtitle_text <- ""
        yaxis_units <- ""
        graph_source <- ""
        
        title_text <- if(rv_rnkt$title){paste0(unique(table$Var_name))}
        title_text_paragraph <- str_wrap(title_text, width = 130)
        
        yaxis_units <- if(rv_rnkt$yaxis){
                "Rank"
        } else {NULL}
        
        if(rv_rnkt$source){
                graph_source <- unique(table$Database)
                if(graph_source == "WDI"){
                        graph_source <- "World Development Indicators"}
        }
        
        # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
        intervals <- ifelse(max(rv_rnkt$time_range) - min(rv_rnkt$time_range) < 30, 1, 2)
        
        # Warning sign if no data for this variable
        if(all(is.na(table$Value))) {subtitle_text <- "No data available"}
        
        # Short country names
        if(rv_rnkt$ctry_short){
                table$Country_aux <- table$Ctry_iso
                distance <- 0.5
                limits <- NULL
        }else{
                table$Country_aux <- table$Country
                distance <- 0.5
                limits <- c(min(rv_rnkt$time_range)-3, max(rv_rnkt$time_range)+3)
        }
        
        # Filter countries with incomplete data
        if(rv_rnkt$incomplete){
                table <- table %>%
                        group_by(Country_aux) %>%
                        filter(!any(is.na(Value)))
        }
        
        # Descending order
        
        table$Oringal_value <- table$Value
        
        if(rv_rnkt$descending){
                table$Value <- -table$Value
        }
        
        # Ranking
        table <- table %>% group_by(Year) %>% 
                mutate(Value = rank(Value, na.last="keep", ties.method= "first"))
        
        table_allvalues <- table
        
        # Filter display countries
        table <- filter(table,
                Country %in% rv_rnkt$ctries_disp)

        # Actually Create plot
        
        if(!rv_rnkt$highlight){
                
                p <- ggplot(table, aes(x = Year, y = Value, color = Country_aux)) +
                        geom_line(size = 2) +
                        geom_point(size = 4) +
                        geom_text(data = filter(table, Year == min(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = min(rv_rnkt$time_range) - distance),
                                fontface = "bold",
                                hjust = 1,
                                family = rv_rnkt$font) +
                        geom_text(data = filter(table, Year == max(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = max(rv_rnkt$time_range) + distance),
                                fontface = "bold",
                                hjust = 0,
                                family = rv_rnkt$font) + 
                        scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.9)

        }else{
                
                # Plot if highlight option selected
                # Filter data
                highlighted_country_df <- filter(table,
                        Country %in% rv_rnkt$highlight_ctry)
                
                rest_df <- filter(table,
                        !Country %in% rv_rnkt$highlight_ctry)
                
                # Plot
                p <- ggplot(data = rest_df, aes(x = Year, y = Value, color = Country_aux)) +
                        
                        # Non selected countries
                        geom_line(data = rest_df, aes(x = Year, y = Value), size = 2) +
                        geom_point(data = rest_df, aes(x = Year, y = Value), size = 4) +
                        scale_colour_grey()+
                        geom_text(data = filter(rest_df, Year == min(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = min(rv_rnkt$time_range) - distance),
                                fontface = "bold",
                                hjust = 1,
                                family = rv_rnkt$font) +
                        geom_text(data = filter(rest_df, Year == max(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = max(rv_rnkt$time_range) + distance),
                                fontface = "bold",
                                hjust = 0,
                                family = rv_rnkt$font)

                if(!all(is.na(highlighted_country_df$Value))){
                        p <- p +
                        # Selected countries
                        geom_line(data = highlighted_country_df, aes(x = Year, y = Value), color = rv_rnkt$highlight_color, size = 2) +
                        geom_point(data = highlighted_country_df, aes(x = Year, y = Value), color = rv_rnkt$highlight_color, size = 4) +
                        
                        geom_text(data = filter(highlighted_country_df, Year == min(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = min(rv_rnkt$time_range) - distance),
                                fontface = "bold",
                                hjust = 1, color = rv_rnkt$highlight_color,
                                family = rv_rnkt$font
                                ) +
                        geom_text(data = filter(highlighted_country_df, Year == max(rv_rnkt$time_range)),
                                aes(label = Country_aux, x = max(rv_rnkt$time_range) + distance, colour = rv_rnkt$highlight_color),
                                fontface = "bold",
                                hjust = 0, color = rv_rnkt$highlight_color,
                                family = rv_rnkt$font
                                )
                }
        }
        
        p <- p +
                scale_x_continuous(name = "", breaks = min(rv_rnkt$time_range):max(rv_rnkt$time_range), limits = limits)+
                # Theme ans aesthetics
                theme_minimal() +
                theme(panel.grid.major.y = element_blank(),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        plot.title = element_text(size = 12, face = "bold"),
                        plot.margin = margin(0.25, 3, 1, 0.25, "cm"),
                        plot.caption = element_text(hjust = 0, size = 10),
                        axis.ticks.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x = element_text(colour = "black"),
                        axis.text.y = element_text(colour = "black"),
                        text = element_text(size = 12,  family = rv_rnkt$font),
                        legend.position = "none")+
                # Include title, subtitle, source and Y-axis title
                labs(title = title_text_paragraph,
                        subtitle = subtitle_text,
                        caption = if(rv_rnkt$source){paste("Source: ", graph_source, ".", sep = "")},
                        y = if(rv_rnkt$yaxis){yaxis_units})
                #Reverse axis (1 on top)
                # scale_y_reverse()
        
        # Increase margins inside graph by extending the range 15% (7.5% above and below)
        y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
        y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

        y_range <- y_max - y_min
        
        if(length(unique(table_allvalues$Country))<=30){ybreaks <- seq(0, my_max_fun(table_allvalues$Value), by = 1)
        ybreaks <- ybreaks[2:length(ybreaks)]
        }
        if(length(unique(table_allvalues$Country))>30 & length(unique(table_allvalues$Country))<=60){ybreaks <- seq(0, my_max_fun(table_allvalues$Value), by = 2)
        ybreaks <- ybreaks[2:length(ybreaks)]}
        if(length(unique(table_allvalues$Country))>60 & length(unique(table_allvalues$Country))<=120){ybreaks <- seq(0, my_max_fun(table_allvalues$Value), by = 4)
        ybreaks <- ybreaks[2:length(ybreaks)]}
        if(length(unique(table_allvalues$Country))>120){ybreaks <- seq(0, my_max_fun(table_allvalues$Value), by = 10)
        ybreaks <- ybreaks[2:length(ybreaks)]}
        
        
        if(rv_input$time_subper & rv_rnkt$time_subper ){
                p <- p + scale_y_reverse(limits = c(my_max_fun(table_allvalues$Value), - ((my_max_fun(table_allvalues$Value) - my_min_fun(table_allvalues$Value)) * 0.025)), breaks = ybreaks)} 
        else{
                p <- p + scale_y_reverse(limits = c(my_max_fun(table_allvalues$Value), 1), breaks = ybreaks)         
                }
        
        # To use in vertical lines
        y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
        y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
        y_range_new <- y_max_new - y_min_new

        size_factor <- 0.06
        ALPHA <- 0.15

        # Subperiod rectangles, their labels and the dotted lines separating
        if(rv_input$time_subper & rv_rnkt$time_subper ){
                p <- p +
                        geom_rect(data = rectangle_text,
                                aes(NULL, NULL, xmin = my_min_fun(Year_start), xmax = my_max_fun(Year_end)),
                                ymin = y_max_new, 
                                ymax = y_max_new - y_range_new * size_factor,
                                colour = NA,
                                fill = "grey",
                                alpha = 1,
                                inherit.aes = FALSE)+
                        geom_label(data = rectangle_text,
                                aes(x = Year_start + (Year_end - Year_start)/2,
                                        y = -(y_max_new * ALPHA + (y_max_new - y_range_new * size_factor)*(1 - ALPHA)),
                                        label = Period,
                                        family = rv_rnkt$font
                                ),
                                size = 3.3,
                                fill = "grey",
                                alpha = 0,
                                label.size = NA,
                                hjust = "center",
                                vjust = "bottom",
                                inherit.aes = FALSE)

                for (i in vertical_lines) {
                        p <- p + geom_segment(x = i,
                                y = y_max_new,
                                xend = i,
                                yend = y_max_new - y_range_new * size_factor,
                                colour = "white",
                                size = 1,
                                alpha = 1) +
                                geom_segment(x = i,
                                        y = y_max_new - y_range_new * size_factor,
                                        xend = i,
                                        yend = -Inf,
                                        colour = "grey",
                                        linetype = "dotted")
                }
        }

        # Append plot to the list of plots
        rv_plots$rnkt <- isolate(list.append(rv_plots$rnkt, p))
        
        # Append data
        table$Title <- if(rv_rnkt$title){title_text}else{""} 
        table$Subtitle <- if(rv_rnkt$title){subtitle_text}else{""} 
        table$Y_axis <- if(rv_rnkt$yaxis){yaxis_units}else{""}
        table$Source <- if(rv_rnkt$source){graph_source}else{""}
        table$Descending <- rv_rnkt$descending

        # Select columns
        table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Period", "Period_num", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
        
        rv_plots$rnkt_data <- isolate(list.append(rv_plots$rnkt_data, table))
        
        # Show plot
        renderPlot(p)
}

# Call Prep data and Create plots
output$gr_rnkt_out_plots <- renderUI({
        
        print("****************************")
        print("Plots - Rnkt: start renderUI")
        
        rv_plots$rnkt <- list()
        rv_plots$rnkt_data <- list()
        
        pd <- req(prepped_data_rnkt())
        
        rv_plots$rnkt_counter <- 0
        rv_plots$rnkt_counter_total <- length(isolate(pd))
        
        final <- isolate(tagList(map(pd, ~ createUI_rnkt(.))))
        
        print("Plots - Rnkt: finish renderUI")
        print("****************************")
        cat("\n")
        
        return(final)
})

### Plots download handlers ----

# Download plots as png zipped - large
output$gr_rnkt_download_large <- downloadHandler(
        filename = 'gr_rnkt_plots_large.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                for (i in 1:length(rv_plots$rnkt)){
                        
                        name <- paste("rnktplot_large", i, ".png", sep = "")
                        ggsave(name,
                                plot = rv_plots$rnkt[[i]],
                                device = "png",
                                width = 11.5,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("rnktplot_large", i, ".png", sep = ""))
                        
                        name_data <- paste("rnktplot_large_data", i, ".csv", sep = "")
                        write.csv(rv_plots$rnkt_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("rnktplot_large_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip(file, vector_plots)
        }
)

# Download plots as png zipped - small
output$gr_rnkt_download_small <- downloadHandler(
        filename = 'gr_rnkt_plots_small.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                for (i in 1:length(rv_plots$rnkt)){
                        
                        # Increase intervals in X axis in small plots
                        intervals <- ifelse(max(rv_rnkt$time_range) - min(rv_rnkt$time_range) < 30, 2, 4)
                        if(rv_rnkt$ctry_short){
                                rv_plots$rnkt[[i]] <- rv_plots$rnkt[[i]] +
                                        scale_x_continuous(name = "",
                                                breaks = seq(min(rv_rnkt$time_range),
                                                        max(rv_rnkt$time_range),
                                                        by = intervals),
                                                limits=c(min(rv_rnkt$time_range)-0.5, max(rv_rnkt$time_range)+ ceiling((max(rv_rnkt$time_range)-min(rv_rnkt$time_range))*0.15)),
                                                expand = c(0, 0)
                                        )
                        }else{
                                rv_plots$rnkt[[i]] <- rv_plots$rnkt[[i]] +
                                        scale_x_continuous(name = "",
                                                breaks = seq(min(rv_rnkt$time_range),
                                                        max(rv_rnkt$time_range),
                                                        by = intervals),
                                                limits=c(min(rv_rnkt$time_range)-5, max(rv_rnkt$time_range)+ ceiling((max(rv_rnkt$time_range)-min(rv_rnkt$time_range))*0.5)),
                                                expand = c(0, 0)
                                        )
                        }
                        
                        
                        name <- paste("rnktplot_small", i, ".png", sep = "")
                        ggsave(name,
                                plot = rv_plots$rnkt[[i]],
                                device = "png",
                                width = 5.75,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("rnktplot_small", i, ".png", sep = ""))
                        
                        name_data <- paste("rnktplot_small_data", i, ".csv", sep = "")
                        write.csv(rv_plots$rnkt_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("rnktplot_small_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip(file, vector_plots)
        }
)

##************************************************************************************************** ----
        
## Static ranking ----

### Reactive values ----
rv_rnks <- reactiveValues(
        
        # Plot parameters
        ctries = def_list_rnks$ctries,
        vars = def_list_rnks$vars,
        stat = def_list_rnks$stat,
        time_range = def_list_rnks$time_range,
        
        # Display parameters
        title = def_list_rnks$title,
        yaxis = def_list_rnks$yaxis,
        source = def_list_rnks$source,
        data_labels = def_list_rnks$data_labels,
        time_subper = def_list_rnks$time_subper,
        transform_zeros = def_list_rnks$transform_zeros,
        transform_log = def_list_rnks$transform_log,
        ctry_short = def_list_rnks$ctry_short,
        digits = def_list_rnks$digits,
        digits_y = def_list_rnks$digits_y,
        font = def_list_rnks$font,
        color_pal = def_list_rnks$color_pal,
        color = def_list_rnks$color,
        horizontal = def_list_rnks$horizontal,
        highlight = def_list_rnks$highlight,
        highlight_ctry = def_list_rnks$highlight_ctry
        
)

### Update inputs ----
observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {
        
        print("Plots - Rnks [1]: Updating inputs")
        
        # Country input
        aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
        aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
        aux_ctry <- aux_ctry[order(
                aux_ctry$Ctry_group_num,
                aux_ctry$Country), ]
        ctry_choices <- as.list(aux_ctry$Country)
        names(ctry_choices) <- aux_ctry$Ctry_slash_Group
        ctry_select <- c(rv_input$ctries_ctry, rv_input$ctries_str, rv_input$ctries_asp, rv_input$ctries_reg)
        
        updatePickerInput(
                session = session,
                inputId = 'gr_rnks_id_ctries',
                choices = ctry_choices,
                selected = ctry_choices,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Variable input
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        
        updatePickerInput(
                session = session,
                inputId = 'gr_rnks_id_vars',
                label = "Select variables",
                choices = aux_var,
                selected = aux_var,
                options = list(size = 15,
                        `actions-box` = TRUE)
        )
        
        # Time input
        updateSliderInput(
                session = session,
                inputId = "gr_rnks_id_time_range",
                min = rv_input$time_range_start,
                max = rv_input$time_range_end,
                step = 1,
                value = c(rv_input$time_range_start, rv_input$time_range_end)
        )
        
        # Highlighted country input
        updateSelectInput(
                inputId = 'gr_rnks_id_highlight_ctry',
                choices = ctry_choices,
                selected = ctry_choices[1])
        
})

###  Update reactive values to changes in raw data ----
observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{
        
        print("Plots - Rnks [2]: Updating reactive values [changes in raw data]")
        
        # Country 
        aux_ctries <- unique(rv_df$dat_all$Country)
        rv_rnks$ctries <- aux_ctries
        
        # Hightlight country
        rv_rnks$highlight_ctry <- aux_ctries[1]
        
        # Variables (select all variables from new dataset)
        aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))
        rv_rnks$vars <- aux_var
        
        # Time range (select all years of new dataset)
        aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
        rv_rnks$time_range <- aux_year
        
}, ignoreInit = TRUE)

###  Update reactive values to changes in rnks plot parameters ----

observeEvent(c(input$gr_rnks_id_update),{
        
        print("Plots - Rnks [2]: Updating reactive values [update rnks plot parameters]")
        
        # Plot parameters
        rv_rnks$ctries <- input$gr_rnks_id_ctries
        rv_rnks$vars <- input$gr_rnks_id_vars
        rv_rnks$stat <- input$gr_rnks_id_stat
        rv_rnks$time_range <- input$gr_rnks_id_time_range
        
        # Display parameters
        rv_rnks$title <- input$gr_rnks_id_title
        rv_rnks$yaxis <- input$gr_rnks_id_yaxis
        rv_rnks$source <- input$gr_rnks_id_source
        rv_rnks$data_labels <- input$gr_rnks_id_data_labels
        rv_rnks$transform_zeros <- input$gr_rnks_id_transform_zeros
        rv_rnks$transform_log <- input$gr_rnks_id_transform_log
        rv_rnks$ctry_short <- input$gr_rnks_id_ctry_short
        rv_rnks$digits <- input$gr_rnks_id_digits
        rv_rnks$digits_y <- input$gr_rnks_id_digits_y
        rv_rnks$font <- input$gr_rnks_id_font
        rv_rnks$color <- input$gr_rnks_id_color
        rv_rnks$horizontal <- input$gr_rnks_id_horizontal
        rv_rnks$highlight <- input$gr_rnks_id_highlight
        rv_rnks$highlight_ctry <- input$gr_rnks_id_highlight_ctry


}, ignoreInit = TRUE)

### Plots ----

# Prep data
prepped_data_rnks <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_rnks_id_update),{
        
        print("Plots - Rnks [3]: preparing data")
        
        if(nrow(rv_df$dat_all)==0){return()}
        
        graph_input <- prep_data_for_graphs(
                df = rv_df$dat_all,
                vars = rv_rnks$vars,
                ctries = rv_rnks$ctries,
                t_start = rv_rnks$time_range[1],
                t_end = rv_rnks$time_range[2]
        )
        
        return(graph_input)
})

# Create plots
createUI_rnks <- function(table) {
        
        rv_plots$rnks_counter <- rv_plots$rnks_counter +1
        print(paste0("Render rnks plot ", rv_plots$rnks_counter, "/", rv_plots$rnks_counter_total))
        
        if(all(is.na(table$Value))){return()}
        
        title_text <- ""
        subtitle_text <- ""
        yaxis_units <- ""
        graph_source <- ""
        
        # Input for title, subtitle Y-axis and source
        title_text <- if(rv_rnks$title){unique(table$Var_name)}
        
        yaxis_units <- if(rv_rnks$yaxis){
                if(is.na(unique(table$Units))){NULL} else {unique(table$Units)}
        }else {NULL}
        
        if(rv_rnks$source){
                graph_source <- unique(table$Database)
                if(graph_source == "WDI"){
                        graph_source <- "World Development Indicators"}
        }
        
        # Transform data: divide by trillions/billions/millions/thousands
        for (i in 4:1){
                if(rv_rnks$transform_zeros & max(abs(table$Value), na.rm = TRUE)>(10^(3*i))){
                        if(rv_rnks$title) {
                                subtitle_text <- paste(
                                        c(subtitle_text, units_zeros[5 - i]),
                                        collapse = "")
                                separator <- if(subtitle_text == ""){""} else {", "}
                                if(rv_rnks$yaxis){
                                        yaxis_units <- paste(
                                                c(yaxis_units, units_zeros[5 - i]),
                                                collapse = separator)
                                }
                        }
                        table$Value <- table$Value/(10^(3*i))
                }
        }
        
        # Log transform
        if(rv_rnks$transform_log & min(table$Value, na.rm = TRUE)>0){
                subtitle_text <- paste(
                        c(subtitle_text, " (Log transformation)"),
                        collapse = "")
                if(rv_rnks$yaxis){
                        yaxis_units <- paste(
                                c(yaxis_units, " (Log transformation)"),
                                collapse = "")
                }
                table$Value <- log(table$Value)
        }
        
        # Calculate variable to plot based on selected stat
        
        if(rv_rnks$stat == "Average"){
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value = mean(Value, na.rm=T)) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(subtitle_text == ""){subtitle_text <- paste(
                        c(subtitle_text, "Period average"),
                        collapse = "")}
                else{subtitle_text <- paste(
                        c(subtitle_text, ", period average"),
                        collapse = "")}
        }
        
        if(rv_rnks$stat == "Median"){
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value = median(Value, na.rm=T)) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(subtitle_text == ""){subtitle_text <- paste(
                        c(subtitle_text, "Period median"),
                        collapse = "")}
                else{subtitle_text <- paste(
                        c(subtitle_text, ", period median"),
                        collapse = "")}
        }
        
        if(rv_rnks$stat == "Standard deviation"){
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value = sd(Value, na.rm=T)) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(subtitle_text == ""){subtitle_text <- paste(
                        c(subtitle_text, "Period standard deviation"),
                        collapse = "")}
                else{subtitle_text <- paste(
                        c(subtitle_text, ", period standard deviation"),
                        collapse = "")}
        }
        
        if(rv_rnks$stat == "Maximum"){
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value = my_max_fun(Value)) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(subtitle_text == ""){subtitle_text <- paste(
                        c(subtitle_text, "Period maximum"),
                        collapse = "")}
                else{subtitle_text <- paste(
                        c(subtitle_text, ", period maximum"),
                        collapse = "")}
        }
        
        if(rv_rnks$stat == "Minimum"){
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value =  my_min_fun(Value)) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(rv_rnks$title){
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period minimum"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period minimum"),
                                collapse = "")}}
        }
        
        if(rv_rnks$stat == "Most recent value"){
                table <- table[!is.na(table$Value), ]
                table <- table %>%
                        group_by(Country) %>%
                        mutate(sum_value = Value[which.max(Year)]) %>%
                        ungroup()
                table <- table %>% distinct(Country, .keep_all = TRUE)
                if(rv_rnks$title){
                        if(subtitle_text == ""){subtitle_text <- paste(
                                c(subtitle_text, "Period most recent value"),
                                collapse = "")}
                        else{subtitle_text <- paste(
                                c(subtitle_text, ", period most recent value"),
                                collapse = "")}}
        }
        
        # Short country names
        if(rv_rnks$ctry_short){table$Country_aux <- table$Ctry_iso}else{table$Country_aux <- table$Country}
        
        # Title
        title_text_paragraph <- str_wrap(title_text, width = 130)
        
        # Sort
        if(rv_rnks$horizontal){table <- table[order(table$sum_value), ]}else{table <- table[order(-table$sum_value), ] }
        
        table$Country_aux <- factor(table$Country_aux, levels = table$Country_aux)
        
        # Ball sizes
        ballsize <- 6
        if(length(table$Country_aux)>50){ballsize <- 5}
        if(length(table$Country_aux)>100){ballsize <- 4}
        if(length(table$Country_aux)>150){ballsize <- 3}
        
        # Include years in subtitle if no sub-periods
        if(rv_rnks$title){
                subtitle_text <- paste(
                        c(subtitle_text, " (", rv_rnks$time_range[1], "-", rv_rnks$time_range[2], ")"),
                        collapse = "")
        }
        
        # THIS WILL CREATE PROBLEMS EVENTUALLY! SO FAR IT WORKS THOUGH, WITH A WARNING...
        # Warning: Vectorized input to `element_text()` is not officially supported.
        # Vectorized input to `element_text()` is not officially supported.
        if(rv_rnks$highlight){
                aux_table <- subset(table, !is.na(sum_value))
                color_ball <- ifelse(aux_table$Country == rv_rnks$highlight_ctry, "tomato2", "grey")
                face_text_label <- ifelse(aux_table$Country == rv_rnks$highlight_ctry, "bold", "plain")
                size_text_label <- ifelse(aux_table$Country == rv_rnks$highlight_ctry, 14, 10)
        }else{
                aux_table <- subset(table, !is.na(sum_value))
                color_ball <- "tomato2"
                face_text_label <- "plain"
                size_text_label <- 10
        }

        # Actually create plot
        p <- ggplot(data = subset(table, !is.na(sum_value)),
                aes(x = Country_aux,
                        y = sum_value)
                )+
                geom_segment(aes(x=Country_aux, 
                        xend=Country_aux, 
                        y=min(sum_value), 
                        yend=max(sum_value)), 
                        linetype="dashed", 
                        size=0.1) +
                geom_point(col=color_ball, size=ballsize) +
                labs(title = title_text_paragraph,
                        subtitle = subtitle_text,
                        caption = if(rv_rnks$source){paste("Source: ", graph_source, ".", sep="")}
                )+
                theme(panel.background = element_rect(fill=NA),
                        panel.border = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(size = 12, face = "bold"),
                        plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                        plot.caption = element_text(hjust = 0, size = 10),
                        axis.ticks.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x = if(!rv_rnks$horizontal & rv_rnks$yaxis){element_text(angle = 90, vjust = 0.5, hjust=1, colour = "black", face = face_text_label, size = size_text_label)} else {element_text(vjust = 0.5, hjust=0.5, colour = "black")}, 
                        axis.text.y = if(!rv_rnks$horizontal & rv_rnks$yaxis){element_text(colour = "black")}else{element_text(colour = "black", face = face_text_label, size = size_text_label)},
                        text = element_text(size = 12, family = rv_rnks$font),
                        legend.title = element_blank()
                )
        
        if(rv_rnks$horizontal){
                
                p <- p + coord_flip()
                
        }
        
        if(rv_rnks$yaxis) {
                
                p <- p + labs(
                        y = yaxis_units,
                        x = NULL
                )  
        }
        

                
        # Append plot to the list of plots
        rv_plots$rnks <- isolate(list.append(rv_plots$rnks, p))
        
        # Append data
        # # Transform Period_num back to numeric
        table$Value <- table$sum_value
        table$Title <- if(rv_rnks$title){title_text}else{""} 
        table$Subtitle <- if(rv_rnks$title){subtitle_text}else{""} 
        table$Y_axis <- if(rv_rnks$yaxis){yaxis_units}else{""}
        table$Source <- if(rv_rnks$source){graph_source}else{""}

        # Select columns

        table <- table %>% select(c("Var_name", "Var_code", "Units", "Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", "Value", "Database", "Title", "Subtitle", "Y_axis", "Source"))
        
        rv_plots$rnks_data <- isolate(list.append(rv_plots$rnks_data, table))
        
        # Show plot
        renderPlot(p)
        
}

# Call Prep data and Create plots
output$gr_rnks_out_plots <- renderUI({
        
        print("****************************")
        print("Plots - Rnks: start renderUI")
        
        rv_plots$rnks <- list()
        rv_plots$rnks_data <- list()
        
        pd <- req(prepped_data_rnks())
        
        rv_plots$rnks_counter <- 0
        rv_plots$rnks_counter_total <- length(isolate(pd))
        
        final <- isolate(tagList(map(pd, ~ createUI_rnks(.))))
        
        print("Plots - Rnks: finish renderUI")
        print("****************************")
        cat("\n")
        
        return(final)
})

### Plots download handlers ----

# Download plots as png zipped - large
output$gr_rnks_download_large <- downloadHandler(
        filename = 'gr_rnks_plots_large.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                
                for (i in 1:length(rv_plots$rnks)){
                        
                        name <- paste("rnksplot_large", i, ".png", sep="")
                        ggsave(name,
                                plot = rv_plots$rnks[[i]],
                                device = "png",
                                width = 11.5,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("rnksplot_large", i, ".png", sep=""))
                        
                        name_data <- paste("rnksplot_large_data", i, ".csv", sep = "")
                        write.csv(rv_plots$rnks_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("rnksplot_large_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip( file, vector_plots)
        }
)

# Download plots as png zipped - small
output$gr_rnks_download_small <- downloadHandler(
        filename = 'gr_rnks_plots_small.zip',
        content = function(file){
                
                # Set temporary working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                
                # Save the plots
                vector_plots <- vector()
                for (i in 1:length(rv_plots$rnks)){
                        
                        name <- paste("rnksplot_small", i, ".png", sep="")
                        ggsave(name,
                                plot = rv_plots$rnks[[i]],
                                device = "png",
                                width = 5.75,
                                height = 5.75,
                                units = "in")
                        vector_plots <- c(vector_plots, paste("rnksplot_small", i, ".png", sep=""))
                        
                        name_data <- paste("rnksplot_small_data", i, ".csv", sep = "")
                        write.csv(rv_plots$rnks_data[[i]], name_data, row.names = FALSE)
                        vector_plots <- c(vector_plots, paste("rnksplot_small_data", i, ".csv", sep = ""))
                        
                }
                
                # Zip them up
                zip(file, vector_plots)
        }
)

##************************************************************************************************** ----        
        
        ## Scatter plots ----

        # Reactive values for scatter
        rv_scat <- reactiveValues(

                # Plot parameters
                ctries = def_list_scat$ctries,
                ctries_small = def_list_scat$ctries_small,
                # ctries_excluded = def_list_scat$ctries_excluded,

                # Individual plot parameters
                plot_num = def_list_scat$plot_num,
                stat_x = def_list_scat$stat_x,
                stat_y = def_list_scat$stat_y,

                # Display parameters
                title = def_list_scat$title,
                title_text = def_list_scat$title_text,
                xaxis = def_list_scat$xaxis,
                yaxis = def_list_scat$yaxis,
                source = def_list_scat$source,
                note = def_list_scat$note,
                transform_zeros = def_list_scat$transform_zeros,
                transform_log = def_list_scat$transform_log,
                ctry_short = def_list_scat$ctry_short,
                digits_x = def_list_scat$digits_x,
                digits_y = def_list_scat$digits_y,
                font = def_list_scat$font,
                highlight = def_list_scat$highlight,
                highlight_ctry = def_list_scat$highlight_ctry,
                highlight_color = def_list_scat$highlight_color,
                highlight_group = def_list_scat$highlight_group,
                
                scat_regline = def_list_scat$scat_regline,
                scat_regline_ci = def_list_scat$scat_regline_ci,
                scat_45line = def_list_scat$scat_45line,
                scat_quad_avg = def_list_scat$scat_quad_avg,
                scat_quad_med = def_list_scat$scat_quad_med

        )

        ### Update inputs ----

        # Inputs that are independent of the number of plots

        observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {

                print("Plots - Scat [1]: Updating inputs")

                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep = " | ")
                aux_ctry <- aux_ctry[order(
                        aux_ctry$Ctry_group_num,
                        aux_ctry$Country), ]
                ctry_choices <- as.list(aux_ctry$Country)
                names(ctry_choices) <- aux_ctry$Ctry_slash_Group
                
                updatePickerInput(
                        session = session,
                        inputId = 'gr_scat_id_ctries',
                        choices = ctry_choices,
                        selected = ctry_choices,
                        options = list(size = 15,
                                `actions-box` = TRUE)
                )
                
                # Highlight country
                updateSelectInput(
                        inputId = 'gr_scat_id_highlight_ctry',
                        choices = ctry_choices,
                        selected = ctry_choices[1]
                )

        })
        
        # Generate reactive values for each plot (depends on choice of number of plots)
        observeEvent(
                c(input$in_id_update,
                input$in_id_reset_confirm,
                input$gr_scat_id_plot_num),
                {

                print("Plots - Scat [2]: Generate reactive values (depends on number of plots)")

                if(nrow(rv_df$dat_all)==0){return()}

                # Filter by country
                initial_data_reshaped <- rv_df$dat_all

                # Filter small countries (default)
                initial_data_reshaped <- initial_data_reshaped[initial_data_reshaped$Country %in% small_country_filter_df$Country, ]

                # Move variables to columns
                initial_data_reshaped <- reshape2::dcast(initial_data_reshaped, Country + Ctry_iso + Ctry_group + Ctry_group_num + Year ~ Var_name, value.var="Value")

                # Variables
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                # If reactive value is null, then we apply range of average and st dev (which are the default statistics)
                for(n in 1:input$gr_scat_id_plot_num){
                        
                        # Title and Y-axis legend
                        if(is.null(eval(parse(text = paste0("rv_scat$title_text", n))))){
                                if(n==1){title <- rv_scat$title_text}else{title <- paste0("Title ", n)}
                                eval(parse(text = paste0("rv_scat$title_text", n, " <- title")))
                        }

                        # Variables names (X variable by default is per capita GDP, in logs)
                        if(is.null(eval(parse(text = paste0("rv_scat$x_var", n))))){
                                eval(parse(text = paste0("rv_scat$x_var", n, " <- aux_var[3]")))
                        }
                        if(is.null(eval(parse(text = paste0("rv_scat$y_var", n))))){
                                eval(parse(text = paste0("rv_scat$y_var", n, " <- aux_var[", n,"]")))
                        }

                        # Time range
                        if(is.null(eval(parse(text = paste0("rv_scat$x_time", n))))){
                                eval(parse(text = paste0("rv_scat$x_time", n, "[1] <- rv_input$time_range_start")))
                                eval(parse(text = paste0("rv_scat$x_time", n, "[2] <- rv_input$time_range_end")))
                        }else{
                                if(eval(parse(text = paste0("rv_scat$x_time", n, "[1]"))) < rv_input$time_range_start){
                                        eval(parse(text = paste0("rv_scat$x_time", n, "[1] <- rv_input$time_range_start")))
                                }
                                if(eval(parse(text = paste0("rv_scat$x_time", n, "[2]"))) > rv_input$time_range_end){
                                        eval(parse(text = paste0("rv_scat$x_time", n, "[2] <- rv_input$time_range_end")))
                                }
                        }


                        if(is.null(eval(parse(text = paste0("rv_scat$y_time", n))))){
                                eval(parse(text = paste0("rv_scat$y_time", n, "[1] <- rv_input$time_range_start")))
                                eval(parse(text = paste0("rv_scat$y_time", n, "[2] <- rv_input$time_range_end")))
                        }else{
                                if(eval(parse(text = paste0("rv_scat$y_time", n, "[1]"))) < rv_input$time_range_start){
                                        eval(parse(text = paste0("rv_scat$y_time", n, "[1] <- rv_input$time_range_start")))
                                }
                                if(eval(parse(text = paste0("rv_scat$y_time", n, "[2]"))) > rv_input$time_range_end){
                                        eval(parse(text = paste0("rv_scat$y_time", n, "[2] <- rv_input$time_range_end")))
                                }
                        }

                        # Log transform (X variable by default is per capita GDP, in logs)
                        if(is.null(eval(parse(text = paste0("rv_scat$x_log", n))))){
                                eval(parse(text = paste0("rv_scat$x_log", n, " <- TRUE")))
                        }
                        if(is.null(eval(parse(text = paste0("rv_scat$y_log", n))))){
                                eval(parse(text = paste0("rv_scat$y_log", n, " <- rv_scat$transform_log")))
                        }

                        # Select stat (default: X average, Y st dev)
                        if(is.null(eval(parse(text = paste0("rv_scat$stat_x", n))))){
                                eval(parse(text = paste0("rv_scat$stat_x", n, " <- rv_scat$stat_x")))
                        }
                        if(is.null(eval(parse(text = paste0("rv_scat$stat_y", n))))){
                                eval(parse(text = paste0("rv_scat$stat_y", n, " <- rv_scat$stat_y")))
                        }
                }
        })
        
        # Render UI for inputs of each scatter (number of input widgets created depends on choice of number of plots)

        output$gr_scat_id_input_panels <- renderUI({

                print("Plots - Scat [3]: Generate input widgets that depend on number of plots")
                
                if(nrow(rv_df$dat_all)==0){return()}

                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep = " | "))

                lapply(1:input$gr_scat_id_plot_num, function(i) {

                        # Inputs that depend on the number of plots but NOT on other scatter inputs

                        wellPanel(
                                style = "background-color: #ebf5fb",

                                h4(paste("Scatter plot "), i),

                                fluidPage(
                                        
                                        textInput(
                                                inputId = paste0("gr_scat_id_title_text",i),
                                                label = "Plot title text",
                                                value = isolate(eval(parse(text = paste0("rv_scat$title_text", i))))
                                        ),

                                        fluidRow(
                                                column(6,
                                                        h5("Horizontal axis")
                                                ),
                                                column(6,
                                                        h5("Vertical axis")
                                                )

                                        ),

                                        tags$b("Select variables"),

                                        fluidRow(
                                                column(6,
                                                        selectizeInput(
                                                                inputId = paste0('gr_scat_id_vars_x_', i),
                                                                label = "",
                                                                choices = aux_var,
                                                                select = eval(parse(text = paste0('rv_scat$x_var', i))),
                                                                options = list(placeholder = 'Select')
                                                        )

                                                ),
                                                column(6,
                                                        selectizeInput(
                                                                inputId = paste0('gr_scat_id_vars_y_', i),
                                                                label = "",
                                                                choices = aux_var,
                                                                select = eval(parse(text = paste0('rv_scat$y_var', i))),
                                                                options = list(placeholder = 'Select')
                                                        )
                                                )
                                        ),

                                        tags$b("Select time range"),

                                        fluidRow(
                                                column(6,
                                                        sliderInput(
                                                                inputId =  paste0('gr_scat_id_time_range_x_', i),
                                                                label = "",
                                                                sep = "",
                                                                min = rv_input$time_range_start,
                                                                max = rv_input$time_range_end,
                                                                step = 1,
                                                                value = c(eval(parse(text = paste0('rv_scat$x_time', i, "[1]"))),
                                                                        eval(parse(text = paste0('rv_scat$x_time', i, "[2]"))))
                                                        )
                                                ),
                                                column(6,
                                                        sliderInput(
                                                                inputId =  paste0('gr_scat_id_time_range_y_', i),
                                                                label = "",
                                                                sep = "",
                                                                min = rv_input$time_range_start,
                                                                max = rv_input$time_range_end,
                                                                step = 1,
                                                                value = c(eval(parse(text = paste0('rv_scat$y_time', i, "[1]"))),
                                                                        eval(parse(text = paste0('rv_scat$y_time', i, "[2]"))))
                                                        )
                                                )
                                        ),

                                        tags$b("Log transformation (if positive)"),

                                        fluidRow(
                                                column(6,
                                                        materialSwitch(
                                                                inputId = paste0("gr_scat_id_transform_log_x_", i),
                                                                label = "",
                                                                value = eval(parse(text = paste0('rv_scat$x_log', i))),
                                                                status = "primary",
                                                                right = TRUE
                                                        )
                                                ),
                                                column(6,
                                                        materialSwitch(
                                                                inputId = paste0("gr_scat_id_transform_log_y_", i),
                                                                label = "",
                                                                value = eval(parse(text = paste0('rv_scat$y_log', i))),
                                                                status = "primary",
                                                                right = TRUE
                                                        )
                                                )
                                        ),

                                        tags$b("Method of aggregation by period"),

                                        fluidRow(
                                                column(6,
                                                        pickerInput(
                                                                inputId = paste0('gr_scat_id_stat_x_', i),
                                                                label = "",
                                                                choices = stats_vec[1:6],
                                                                select = eval(parse(text = paste0('rv_scat$stat_x', i))),
                                                                multiple = FALSE
                                                        )
                                                ),
                                                column(6,
                                                        pickerInput(
                                                                inputId = paste0('gr_scat_id_stat_y_', i),
                                                                label = "",
                                                                choices = stats_vec[1:6],
                                                                select = eval(parse(text = paste0('rv_scat$stat_y', i))),
                                                                multiple = FALSE
                                                        )
                                                )
                                        )
                                )
                        )
                })
        })
        
        ###  Update reactive values to changes in raw data ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm),{

                print("Plots - Scat [4]: Updating reactive values [changes in raw data]")

                # Country (if previously selected country is in new dataset, keep selection, else select first country from vector)
                rv_scat$ctries <- unique(rv_df$dat_all$Country)
                
                # Hightlight country
                rv_scat$highlight_ctry <- unique(rv_df$dat_all$Country)[1]

                # Time range (select all years of new dataset)
                aux_year <- c(rv_input$time_range_start, rv_input$time_range_end)
                rv_scat$time_range <- aux_year

        }, ignoreInit = TRUE)
         
        ###  Update reactive values to changes in scat plot parameters ----

        observeEvent(c(input$gr_scat_id_update),{
                
                print("Plots - Scat [5]: Updating reactive values [update scat plot parameters]")
                
                # Plot parameters
                rv_scat$ctries <- input$gr_scat_id_ctries
                rv_scat$ctries_small <- input$gr_scat_id_ctries_small
                
                # Individual plot parameters
                rv_scat$plot_num <- input$gr_scat_id_plot_num
                
                for (i in 1:input$gr_scat_id_plot_num){
                        
                        eval(parse(text = paste0("rv_scat$title_text", i, " <- input$gr_scat_id_title_text", i)))
                        
                        eval(parse(text = paste0("rv_scat$x_var", i, " <- input$gr_scat_id_vars_x_", i)))
                        eval(parse(text = paste0("rv_scat$y_var", i, " <- input$gr_scat_id_vars_y_", i)))
                        
                        eval(parse(text = paste0("rv_scat$x_time", i, " <- input$gr_scat_id_time_range_x_", i)))
                        eval(parse(text = paste0("rv_scat$y_time", i, " <- input$gr_scat_id_time_range_y_", i)))
                        
                        eval(parse(text = paste0("rv_scat$x_log", i, " <- input$gr_scat_id_transform_log_x_", i)))
                        eval(parse(text = paste0("rv_scat$y_log", i, " <- input$gr_scat_id_transform_log_y_", i)))
                        
                        eval(parse(text = paste0("rv_scat$stat_x", i, " <- input$gr_scat_id_stat_x_", i)))
                        eval(parse(text = paste0("rv_scat$stat_y", i, " <- input$gr_scat_id_stat_y_", i)))
                        
                }
                
                # Display parameters
                rv_scat$title <- input$gr_scat_id_title
                rv_scat$xaxis <- input$gr_scat_id_xaxis
                rv_scat$yaxis <- input$gr_scat_id_yaxis
                rv_scat$source <- input$gr_scat_id_source
                rv_scat$note <- input$gr_scat_id_note
                rv_scat$transform_zeros <- input$gr_scat_id_transform_zeros
                rv_scat$ctry_short <- input$gr_scat_id_ctry_short
                rv_scat$digits_x <- input$gr_scat_id_digits_x
                rv_scat$digits_y <- input$gr_scat_id_digits_y
                rv_scat$font <- input$gr_scat_id_font
                rv_scat$highlight <- input$gr_scat_id_highlight
                rv_scat$highlight_ctry <- input$gr_scat_id_highlight_ctry
                rv_scat$highlight_color <- input$gr_scat_id_highlight_color
                rv_scat$highlight_group <- input$gr_scat_id_highlight_group
                
                rv_scat$scat_regline <- input$gr_scat_id_regline
                rv_scat$scat_regline_ci <- input$gr_scat_id_regline_ci
                rv_scat$scat_45line <- input$gr_scat_id_45line
                rv_scat$scat_quad_avg <- input$gr_scat_id_quad_avg
                rv_scat$scat_quad_med <- input$gr_scat_id_quad_med
                
        }, ignoreInit = TRUE)


        ### Plots ----

        prepped_data_scat <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm, input$gr_scat_id_update),{

                print("Plots - Scat [6]: preparing data")

                if(length(input$gr_scat_id_ctries)==0){return()}
                
                # Filter by country
                initial_data_reshaped <- rv_df$dat_all[rv_df$dat_all$Country %in% rv_scat$ctries, ]
                
                # Filter small countries
                if(input$gr_scat_id_ctries_small){
                        initial_data_reshaped <- initial_data_reshaped[initial_data_reshaped$Country %in% c(small_country_filter_df$Country, rv_input$ctries_reg), ]
                }
                
                # Move variables to columns
                initial_data_reshaped <- reshape2::dcast(initial_data_reshaped, Country + Ctry_iso + Ctry_group + Ctry_group_num + Year ~ Var_name, value.var="Value")
                
                #Create empty list for tibbles
                tibble_list <- list()
                
                for (i in 1:input$gr_scat_id_plot_num){
                        
                        rv_scat$xaxis_title_zeros <- ""
                        rv_scat$yaxis_title_zeros <- ""
                        
                        Title_text <- eval(parse(text = paste0("rv_scat$title_text", i)))
                        
                        x_var <- eval(parse(text = paste0("rv_scat$x_var", i)))
                        y_var <- eval(parse(text = paste0("rv_scat$y_var", i)))
                        
                        x_time <- eval(parse(text = paste0("rv_scat$x_time", i)))
                        y_time <- eval(parse(text = paste0("rv_scat$y_time", i)))
                        
                        x_log <- eval(parse(text = paste0("rv_scat$x_log", i)))
                        y_log <- eval(parse(text = paste0("rv_scat$y_log", i)))
                        
                        stat_x <- eval(parse(text = paste0("rv_scat$stat_x", i)))
                        stat_y <- eval(parse(text = paste0("rv_scat$stat_y", i)))
                        
                        # If a variable missing, then empty tibble
                        if(is.null(x_var) | is.null(y_var)){
                                list_name <- paste0("table", i)
                                tibble_list[[list_name]] <- data.frame()
                        }else{
                                if(x_var == "" | y_var == ""){
                                        list_name <- paste0("table", i)
                                        tibble_list[[list_name]] <- data.frame()
                                }else{
                                        print(paste0("Both X and Y variables were selected for plot ", i,"... starting to build tibble"))
                                        
                                        x_var_code <- strsplit(x_var, " \\| ")[[1]][2]
                                        y_var_code <- strsplit(y_var, " \\| ")[[1]][2]
                                        x_var_name <- strsplit(x_var, " \\| ")[[1]][1]
                                        y_var_name <- strsplit(y_var, " \\| ")[[1]][1]
                                        x_var_units <- unique(filter(rv_df$dat_all, Var_code %in% x_var_code) %>% select(Units))
                                        y_var_units <- unique(filter(rv_df$dat_all, Var_code %in% y_var_code) %>% select(Units))
                                        x_var_source <- unique(filter(rv_df$dat_all, Var_code %in% x_var_code) %>% select(Database))
                                        y_var_source <- unique(filter(rv_df$dat_all, Var_code %in% y_var_code) %>% select(Database))
                                        
                                        # Rename variables in aux
                                        
                                        toselect1 <- c("Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", x_var_name[1], y_var_name[1])
                                        toselect2 <- c("Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year", x_var_name[1])
                                        
                                        if(x_var_name != y_var_name){
                                                aux <- initial_data_reshaped %>% select(all_of(toselect1))
                                                colnames(aux)[which(names(aux) == x_var_name)] <- "X_VAR"
                                                colnames(aux)[which(names(aux) == y_var_name)] <- "Y_VAR"
                                        } else {
                                                aux <- initial_data_reshaped %>% select(all_of(toselect2))
                                                colnames(aux)[which(names(aux) == x_var_name)] <- "X_VAR"
                                                aux$Y_VAR <- aux$X_VAR
                                        }
                                        
                                        aux <- aux %>% as_tibble()
                                        
                                        # Create logic variables indicating whether year should be included
                                        aux$Year_include_x <- aux$Year >= x_time[1] & aux$Year <= x_time[2]
                                        aux$Year_include_y <- aux$Year >= y_time[1] & aux$Year <= y_time[2]
                                        
                                        # Transform data: divide by trillions/billions/millions/thousands
                                        for (j in 4:1){
                                                
                                                if(input$gr_scat_id_transform_zeros & max(abs(aux$X_VAR), na.rm = TRUE)>(10^(3*j))){
                                                        if(input$gr_scat_id_xaxis) {
                                                                rv_scat$xaxis_title_zeros <- paste0(" ", units_zeros[5 - j])
                                                                x_var_name <- paste0(x_var_name, ", ", units_zeros[5 - j])
                                                        }
                                                        aux$X_VAR <- aux$X_VAR/(10^(3*j))
                                                }
                                                
                                                if(input$gr_scat_id_transform_zeros & max(abs(aux$Y_VAR), na.rm = TRUE)>(10^(3*j))){
                                                        
                                                        if(input$gr_scat_id_yaxis) {
                                                                rv_scat$yaxis_title_zeros <- paste0(" ", units_zeros[5 - j])
                                                                y_var_name <- paste0(y_var_name, ", ", units_zeros[5 - j])
                                                        }
                                                        aux$Y_VAR <- aux$Y_VAR/(10^(3*j))
                                                }
                                        }
                                        
                                        # Log transformation
                                        
                                        if(x_log & my_min_fun(aux$X_VAR)>0){
                                                aux$X_VAR <- log(aux$X_VAR)
                                                x_var_name <- paste0(x_var_name, " (Log)")
                                        } else {xaxis_title_log <- ""}
                                        
                                        if(y_log & my_min_fun(aux$Y_VAR)>0){
                                                aux$Y_VAR <- log(aux$Y_VAR)
                                                y_var_name <- paste0(y_var_name, " (Log)")
                                        } else {yaxis_title_log <- ""}
                                        
                                        # Calculate statistic by country: X variable
                                        
                                        if(stat_x == "Average"){
                                                var1 <- aggregate(x = list(X_var = aux$X_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_x = aux$Year_include_x),
                                                        FUN = mean, na.rm = TRUE, na.action=NULL)
                                        }
                                        
                                        if(stat_x == "Median"){
                                                var1 <- aggregate(x = list(X_var = aux$X_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_x = aux$Year_include_x),
                                                        FUN = median, na.rm = TRUE, na.action=NULL)
                                        }
                                        
                                        if(stat_x == "Maximum"){
                                                var1 <- aggregate(x = list(X_var = aux$X_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_x = aux$Year_include_x),
                                                        FUN = my_max_fun)
                                        }
                                        
                                        if(stat_x == "Minimum"){
                                                var1 <- aggregate(x = list(X_var = aux$X_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_x = aux$Year_include_x),
                                                        FUN = my_min_fun)
                                        }
                                        
                                        if(stat_x == "Standard deviation"){
                                                var1 <- aggregate(x = list(X_var = aux$X_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_x = aux$Year_include_x),
                                                        FUN = sd, na.rm = TRUE)
                                        }
                                        
                                        if(stat_x == "Most recent value"){
                                                var1_aux <- aux[aux$Year_include_x == TRUE, ]
                                                var1_aux <- var1_aux[var1_aux$Year == max(var1_aux$Year) & (!is.na(var1_aux$X_VAR)), c("Country", "X_VAR")]
                                                aux2 <- unique(aux[, c("Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year_include_x" )])
                                                var1 <- merge(
                                                        x = aux2,
                                                        y = var1_aux,
                                                        by = c("Country"),
                                                        all.x = TRUE)
                                                var1 <- rename(var1, X_var = X_VAR)
                                        }
                                        
                                        var1 <- var1[var1$Year_include_x == TRUE, ]
                                        
                                        # Calculate statistic by country: Y variable
                                        
                                        if(stat_y == "Average"){
                                                var2 <- aggregate(x = list(Y_var = aux$Y_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_y = aux$Year_include_y),
                                                        FUN = mean, na.rm = TRUE, na.action=NULL)}
                                        
                                        if(stat_y == "Median"){
                                                var2 <- aggregate(x = list(Y_var = aux$Y_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_y = aux$Year_include_y),
                                                        FUN = median, na.rm = TRUE, na.action=NULL)}
                                        
                                        if(stat_y == "Maximum"){
                                                var2 <- aggregate(x = list(Y_var = aux$Y_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_y = aux$Year_include_y),
                                                        FUN = my_max_fun)}
                                        
                                        if(stat_y == "Minimum"){
                                                var2 <- aggregate(x = list(Y_var = aux$Y_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_y = aux$Year_include_y),
                                                        FUN = my_min_fun)}
                                        
                                        if(stat_y == "Standard deviation"){
                                                var2 <- aggregate(x = list(Y_var = aux$Y_VAR), by = list(Country = aux$Country, Ctry_iso = aux$Ctry_iso, Ctry_group = aux$Ctry_group, Ctry_group_num = aux$Ctry_group_num, Year_include_y = aux$Year_include_y),
                                                        FUN = sd, na.rm = TRUE)
                                        }
                                        
                                        if(stat_y == "Most recent value"){
                                                var2_aux <- aux[aux$Year_include_y == TRUE, ]
                                                var2_aux <- var2_aux[var2_aux$Year == max(var2_aux$Year) & (!is.na(var2_aux$X_VAR)), c("Country", "Y_VAR")]
                                                aux2 <- unique(aux[, c("Country", "Ctry_iso", "Ctry_group", "Ctry_group_num", "Year_include_y" )])
                                                var2 <- merge(
                                                        x = aux2,
                                                        y = var2_aux,
                                                        by = c("Country"),
                                                        all.x = TRUE)
                                                var2 <- rename(var2, Y_var = Y_VAR)
                                        }
                                        
                                        var2 <- var2[var2$Year_include_y == TRUE, ]
                                        
                                        
                                        # Merge variables
                                        aux <- merge(
                                                x = var1,
                                                y = var2,
                                                by = c("Country", "Ctry_iso", "Ctry_group", "Ctry_group_num"),
                                                all.x = TRUE)
                                        
                                        aux <- aux %>% select(Country, Ctry_iso, Ctry_group, Ctry_group_num, X_var, Y_var)
                                        
                                        aux <- aux[order(
                                                aux$Ctry_group_num,
                                                aux$Country), ]
                                        
                                        rownames(aux) <- NULL
                                        
                                        # Columns with plot parameters
                                        
                                        aux <- aux %>% tibble::add_column(
                                                
                                                Title_text = as.character(Title_text),
                                                
                                                X_var_name = as.character(x_var_name),
                                                X_var_units = as.character(x_var_units),
                                                X_var_source = as.character(x_var_source),
                                                X_var_period = as.character(c(paste0(x_time[1], "-", x_time[2]))),
                                                X_var_stat = as.character(stat_x),
                                                
                                                Y_var_name = as.character(y_var_name),
                                                Y_var_units = as.character(y_var_units),
                                                Y_var_source = as.character(y_var_source),
                                                Y_var_period = as.character(c(paste0(y_time[1], "-", y_time[2]))),
                                                Y_var_stat = as.character(stat_y),
                                                
                                                scat_regline = as.logical(rv_scat$scat_regline),
                                                scat_regline_ci = as.logical(rv_scat$scat_regline_ci),
                                                scat_45line = as.logical(rv_scat$scat_45line),
                                                scat_quad_avg = as.logical(rv_scat$scat_quad_avg),
                                                scat_quad_med = as.logical(rv_scat$scat_quad_med),
                                                scat_regline_text = as.character("\n Note: the blue line represents the linear regression line."),
                                                scat_45line_text = as.character("\n Note: the black line represents the 45 degree line (y=x)."),
                                                scat_quad_avg_text = as.character("\n Note: plot is divided into quadrants by average values."),
                                                scat_quad_med_text = as.character("\n Note: plot is divided into quadrants by median values."),
                                        )
                                        
                                        excluded_countries <- setdiff(rv_df$dat_all$Country, aux$Country)
                                        
                                        if(length(excluded_countries)==0){
                                                excluded_countries_text <- ""
                                        }else{
                                                excluded_countries_text <- "Excluded countries: "
                                                for (j in 1:length(excluded_countries)){
                                                        if(j==1){
                                                                excluded_countries_text <- paste0(excluded_countries_text, excluded_countries[j])
                                                        }else{
                                                                excluded_countries_text <- paste0(excluded_countries_text, "; ", excluded_countries[j])
                                                        }
                                                        
                                                }
                                                excluded_countries_text <- paste0(excluded_countries_text, ".")
                                        }

                                        aux$excluded_countries <- multiline_text_fun(text_vector = excluded_countries_text, number_characters = 180)
                                        aux$excluded_countries_short <-  multiline_text_fun(text_vector = excluded_countries_text, number_characters = 60)
                                        
                                        # Append to list
                                        list_name <- paste0("table", i)
                                        
                                        aux <- aux %>% as_tibble()
                                        tibble_list[[list_name]] <- aux
                                        rv_scat$tibble_list <- tibble_list
                                        
                                }
                        }
                }
                
                return(rv_scat$tibble_list)

        })

        createUI_scat <- function(table){

                rv_plots$scat_counter <- rv_plots$scat_counter +1
                print(paste0("Render scat plot ", rv_plots$scat_counter, "/", rv_plots$scat_counter_total))

                if(all(is.na(table$X_var)) | all(is.na(table$Y_var))){return()}

                # Parameters for plot

                title_text <- ""
                subtitle_text <- ""
                yaxis_units <- ""
                graph_source <- ""
                
                if(rv_scat$title){
                        title_text <- unique(table$Title_text)
                }

                if(rv_scat$xaxis){
                        xaxis_title <- paste0(unique(table$X_var_name),
                                ", ",
                                unique(table$X_var_period),
                                " ",
                                unique(tolower(table$X_var_stat))
                        )
                        xaxis_title <- str_wrap(xaxis_title, width=100)

                } else {NULL}

                if(rv_scat$yaxis){
                        yaxis_title <- paste0(unique(table$Y_var_name),
                                ", ",
                                unique(table$Y_var_period),
                                " ",
                                unique(tolower(table$Y_var_stat))
                        )
                        yaxis_title <- str_wrap(yaxis_title, width=50)

                } else {NULL}

                if(rv_scat$source){
                        graph_source_x <- unique(table$X_var_source)
                        graph_source_y <- unique(table$Y_var_source)

                        if(graph_source_x == "WDI"){
                                graph_source_x <- "World Development Indicators. "}

                        if(graph_source_y == "WDI"){
                                graph_source_y <- "World Development Indicators. "}

                        if(graph_source_x == graph_source_y){
                                graph_source <- graph_source_x
                        } else {
                                graph_source <- paste0(graph_source_x, " and ", graph_source_y, ". ")
                        }

                }

                if(rv_scat$note){
                        if(rv_scat$scat_regline | !rv_scat$scat_regline_ci){graph_source <- paste0(graph_source, "\n", "Note: the blue line is the linear regression line.")}
                        if(rv_scat$scat_regline_ci){graph_source <- paste0(graph_source, "\n", "Note: the blue line is the linear regression line. The grey area is the 95% confidence interval.")}
                        if(rv_scat$scat_45line){graph_source <- paste0(graph_source, "\n", "Note: the black line is the 45 degree line (y=x).")}
                        if(rv_scat$scat_quad_avg){graph_source <- paste0(graph_source, "\n", "Note: plot is divided into quadrants by average values.")}
                        if(rv_scat$scat_quad_med){graph_source <- paste0(graph_source, "\n", "Note: plot is divided into quadrants by median values.")}
                        if(unique(table$excluded_countries) != ""){graph_source <- paste0(graph_source, "\n", unique(table$excluded_countries))}
                }


                # Highlight
                if(rv_scat$highlight){

                        table$Highlight <- (table$Country %in% rv_scat$highlight_ctry)

                        table <- table %>% mutate(Fontface =if_else(Highlight, "bold", "plain"))

                        table <- table[order(
                                table$Highlight,
                                table$Country), ]

                        p <- ggplot(table,
                                aes(x = X_var, y = Y_var, color = Highlight)) +
                                scale_color_manual(values = c("#999999", rv_scat$highlight_color))+
                                theme(legend.position = "none") +
                                geom_text(aes(label = if(rv_scat$ctry_short){Ctry_iso}else{Country}, size = Highlight),
                                        fontface = table$Fontface,
                                        family = rv_scat$font, na.rm = TRUE) +
                                scale_size_manual(values=c(3, 6))

                }
                else {
                        if(rv_scat$highlight_group){
                                table$Highlight <- (table$Country %in% c(rv_input$ctries_ctry, rv_input$ctries_asp, rv_input$ctries_str, rv_input$ctries_reg))

                                table <- table %>% mutate(Fontface =if_else(Highlight, "bold", "plain"))

                                table <- table[order(
                                        table$Highlight,
                                        table$Country), ]

                                p <- ggplot(table,
                                        aes(x = X_var, y = Y_var, color = Highlight)) +
                                        scale_color_manual(values = c("#999999", "black"))+
                                        theme(legend.position = "none") +
                                        geom_text(aes(label = if(rv_scat$ctry_short){Ctry_iso}else{Country}, size = Highlight),
                                                fontface = table$Fontface,
                                                family = rv_scat$font, na.rm = TRUE) +
                                        scale_size_manual(values=c(3, 4))
                        }else{
                                p <- ggplot(table, aes(x = X_var, y = Y_var))+
                                        scale_color_manual(values = c("#999999"))+
                                        theme(legend.position = "none") +
                                        geom_text(aes(label = if(rv_scat$ctry_short){Ctry_iso}else{Country}),
                                                fontface = "plain",
                                                family = rv_scat$font, na.rm = TRUE)+
                                        scale_size_manual(values=c(2))
                        }
                }
                
                title_text_paragraph <- str_wrap(title_text, width = 130)

                # Plot
                p <- p +

                        # Title and caption
                        labs(title = if(rv_scat$title){title_text_paragraph},
                                caption = if(rv_scat$source){paste("Source: ", graph_source, sep = "")})

                        # Increase margins inside graph by extending the range
                        y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                        y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                        y_range <- y_max - y_min

                        p <- p + scale_y_continuous(name = if(rv_scat$yaxis){yaxis_title}else{""},
                                labels = comma_format(accuracy = 1/10^(rv_scat$digits_y), big.mark = ","),
                                breaks = pretty_breaks(),
                                limits = c(y_min, y_max + (y_range * 0.1)))

                        x_min <- ggplot_build(p)$layout$panel_params[[1]]$x.range[1]
                        x_max <- ggplot_build(p)$layout$panel_params[[1]]$x.range[2]
                        x_range <- x_max - x_min

                        p <- p + scale_x_continuous(name = if(rv_scat$xaxis){xaxis_title}else{""},
                                labels = comma_format(accuracy = 1/10^(rv_scat$digits_x), big.mark = ","),
                                breaks = pretty_breaks(),
                                limits = c(x_min, x_max + (x_range * 0.1)))

                        # Aesthetics
                        p <- p + theme_minimal() +
                                theme(
                                panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_line(linetype = "dashed"),
                                panel.grid.minor = element_blank(),
                                plot.title = element_text(size = 12, face = "bold"),
                                plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                                plot.caption = element_text(hjust = 0, size = 10),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                text = element_text(size = 12,  family = rv_scat$font),
                                legend.key = element_rect(fill = "white"),
                                legend.title = element_blank(),
                                legend.position = "none"
                                        )

                        if(rv_scat$scat_regline_ci){
                                p <- p + geom_smooth(method = lm, color = "#0721B3", size = 0.75)
                        }

                        if(!rv_scat$scat_regline_ci & rv_scat$scat_regline){
                                p <- p + geom_smooth(method = lm, se = FALSE, color = "#0721B3", size = 0.75)
                        }

                        if(rv_scat$scat_45line){
                                p <- p + geom_abline(slope = 1, intercept = 0)
                        }

                        if(rv_scat$scat_quad_med){
                                p <- p + geom_hline(yintercept = median(table$Y_var, na.rm = TRUE))
                                p <- p + geom_vline(xintercept = median(table$X_var, na.rm = TRUE))
                        }

                        if(rv_scat$scat_quad_avg){
                                p <- p + geom_hline(yintercept = mean(table$Y_var, na.rm = TRUE))
                                p <- p + geom_vline(xintercept = mean(table$X_var, na.rm = TRUE))
                        }

                # Append plot to the list of plots
                rv_plots$scat <- isolate(list.append(rv_plots$scat, p))

                table <- table %>% filter(!is.na(Ctry_iso))
                table <- table[order(
                        table$Ctry_group_num,
                        table$Country), ]
                rv_plots$scat_data <- isolate(list.append(rv_plots$scat_data, table))

                # Show plot
                renderPlot(p)

        }

        output$gr_scat_out_plots <- renderUI({

                print("****************************")
                print("Plots - Scat [7]: start renderUI")

                rv_plots$scat <- list()
                rv_plots$scat_data <- list()

                pd <- req(prepped_data_scat())

                rv_plots$scat_counter <- 0
                rv_plots$scat_counter_total <- length(isolate(pd))

                final <- isolate(tagList(map(pd, ~ createUI_scat(.))))

                print("Plots - Scat: finish renderUI")
                print("****************************")
                cat("\n")

                return(final)
        })

        ### Plots download handlers----

        # Download plots as png zipped - large
        output$gr_scat_download_large <- downloadHandler(
                filename = 'gr_scat_plots_large.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$scat)){
                                name <- paste("scatplot_large", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$scat[[i]],
                                        device = "png",
                                        width = 11.5,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("scatplot_large", i, ".png", sep = ""))

                                name_data <- paste("scatplot_large_data", i, ".csv", sep = "")
                                write.csv(rv_plots$scat_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("scatplot_large_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )

        # Download plots as png zipped - small
        output$gr_scat_download_small <- downloadHandler(
                filename = 'gr_scat_plots_small.zip',
                content = function(file){

                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))

                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$scat)){

                                # Adjust title and note length
                                table <- rv_plots$scat_data[[i]]

                                if(rv_scat$title){
                                        title_text <- unique(table$Title_text)
                                }

                                if(rv_scat$source){
                                        graph_source_x <- unique(table$X_var_source)
                                        graph_source_y <- unique(table$Y_var_source)

                                        if(graph_source_x == "WDI"){
                                                graph_source_x <- "World Development Indicators. "}

                                        if(graph_source_y == "WDI"){
                                                graph_source_y <- "World Development Indicators. "}

                                        if(graph_source_x == graph_source_y){
                                                graph_source <- graph_source_x
                                        } else {
                                                graph_source <- paste0(graph_source_x, " and ", graph_source_y, ". ")
                                        }

                                }

                                if(rv_scat$note){
                                        if(rv_scat$scat_regline | !rv_scat$scat_regline_ci){graph_source <- paste0(graph_source, "\n", "Note: the blue line is the linear regression line.")}
                                        if(rv_scat$scat_regline_ci){graph_source <- paste0(graph_source, "Note: the blue line is the linear regression line.", "\n", "The grey area is the confidence interval.")}
                                        if(rv_scat$scat_45line){graph_source <- paste0(graph_source, "\n", "Note: the black line is the 45 degree line (y=x).")}
                                        if(rv_scat$scat_quad_avg){graph_source <- paste0(graph_source, "\n", "Note: plot is divided into quadrants by average values.")}
                                        if(rv_scat$scat_quad_med){graph_source <- paste0(graph_source, "\n", "Note: plot is divided into quadrants by median values.")}
                                        if(unique(table$excluded_countries) != ""){graph_source <- paste0(graph_source, "\n", unique(table$excluded_countries_short))}
                                }
                                
                                title_text_paragraph <- str_wrap(title_text, width = 60)

                                rv_plots$scat[[i]] <- rv_plots$scat[[i]] +
                                        labs(title = if(rv_scat$title){title_text_paragraph},
                                                caption = if(rv_scat$source){paste("Source: ", graph_source, sep = "")})
                                
                                if(rv_scat$xaxis){
                                        xaxis_title <- paste0(unique(table$X_var_name),
                                                ", ",
                                                unique(table$X_var_period),
                                                " ",
                                                unique(tolower(table$X_var_stat))
                                        )
                                        xaxis_title <- str_wrap(xaxis_title, width=50)
                                        
                                } else {NULL}
                                
                                rv_plots$scat[[i]] <- rv_plots$scat[[i]] + scale_x_continuous(name = if(rv_scat$xaxis){xaxis_title}else{""})


                                name <- paste("scatplot_small", i, ".png", sep = "")
                                ggsave(name,
                                        plot = rv_plots$scat[[i]],
                                        device = "png",
                                        width = 5.75,
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("scatplot_small", i, ".png", sep = ""))

                                name_data <- paste("scatplot_small_data", i, ".csv", sep = "")
                                write.csv(rv_plots$scat_data[[i]], name_data, row.names = FALSE)
                                vector_plots <- c(vector_plots, paste("scatplot_small_data", i, ".csv", sep = ""))

                        }

                        # Zip them up
                        zip(file, vector_plots)
                }
        )



        
}

shinyApp(ui = ui, server = server)