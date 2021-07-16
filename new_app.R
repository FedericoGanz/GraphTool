# global.r ----

## Load libraries ----

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)

library(dipsaus)
library(WDI)
library(dplyr)
library(RColorBrewer)
library(tools)
library(readxl)
library(haven)
library(tidyr)
library(plyr)
library(bsplus)
library(DT)
# library(remotes)
# library(Rcpp)
library(summarytools)
# library(panelr)
library(reshape2)
library(ExPanDaR)
library(zip)

## Fix variables ----

# Load dataframes from WDI package to get countries
ctry_df <- as.data.frame(WDI_data$country)
ctry_vec <- sort(ctry_df[ctry_df[ ,"region"] != "Aggregates", "country"])
reg_vec <- sort(ctry_df[ctry_df[ ,"region"] == "Aggregates", "country"])

# Create dataframe with 
flags_df <- filter(ctry_df, !region %in% c("Aggregates")) %>% select(country, iso2c, iso3c)
flags_df$URL <- paste( "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", tolower(flags_df$iso2c), ".svg", sep="")
flags_df$URL[flags_df$iso2c == "JG"] <- "https://upload.wikimedia.org/wikipedia/commons/f/f5/Flag_of_the_Channel_Islands_1.png"

# Load dataframes from WDI package to get variables
var_df <- as.data.frame(WDI_data$series)
var_df$name_indicator <- paste(var_df$name, var_df$indicator, sep=" | ")
wdi_vars_aux <- c("GDP growth (annual %) | NY.GDP.MKTP.KD.ZG",
        "Inflation, consumer prices (annual %) | FP.CPI.TOTL.ZG",
        "Population, total | SP.POP.TOTL")
var_df <- var_df %>% arrange(factor(name_indicator, levels = wdi_vars_aux))
var_vec <- var_df$name_indicator

# Color palettes vector
palette_vec <- row.names(filter(brewer.pal.info, colorblind == TRUE))

# Font list
font_list <- list(Calibri = "Calibri", Arial = "sans", Courier = "mono", TimesNewRoman = "serif")

# Positions list
position_list <- list(Top = "top", Bottom = "bottom", Right = "right", Left = "left")

# Statistics vector
stats_vec <- c("Average", "Median", "Maximum", "Minimum", "Standard deviation", "Most recent value")

# Functions
my_max_fun <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my_min_fun <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

create_empty_panel <-  function(df, country_vec, year_vec){
        
        # variable_df: dataframe containing Var_name (column 1) and units (column 2)
        # country_vec: vector containing country_iso
        # year_vec: vector containing years
        var_headers <- names(df) %in% c("Var_name", "Var_code", "Units", "Database")

        proc_var <- df[ , var_headers]
        proc_var <- proc_var[!duplicated(proc_var), ]
        proc_var <- proc_var[order( proc_var[, "Var_name"], proc_var[,"Units"] ), ]
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
        print(proc_ctry)
        proc_ctry <- data.frame(proc_ctry[rep(seq_len(nrow(proc_ctry)), each = num_year), ])
        proc_ctry <- do.call("rbind", replicate(num_var, proc_ctry, simplify = FALSE))
        colnames(proc_ctry)[1] <- "Ctry_iso"
        
        proc_year <- data.frame(sort(unique(year_vec)))
        colnames(proc_year)[1] <- "Year"
        # proc_year <- transform(proc_year, Year = as.numeric(proc_year))          # In case there is a character entry in the column
        print(paste0("Unique years: ", num_year))
        print(proc_year)
        
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
        countries_all = FALSE, 
        variables, 
        start_year, 
        end_year){
        
        
        # Print messages indicating selected parameters
        print("Starting WDI download process")
        print("(1/3) Selected countries/regions (ISO-2):")
        if(countries_all){
                message_ctries <- c("All")
        } else {message_ctries <- ctry_df[ctry_df[,"country"] %in% c(countries_ctry, countries_str, countries_asp, countries_reg), "iso2c"]}
        print(message_ctries)
        print("(2/3) Selected variables (code):")
        print(variables)
        print("(3/3) Year range:")
        print(paste0(start_year, " - ", end_year))

        # Create iso2c countries vector 
        countries <- c(countries_ctry, countries_str, countries_asp, countries_reg)
        ctries_select_iso2 <- ctry_df[ctry_df[,"country"] %in% countries, "iso2c"]
        if((length(ctries_select_iso2) == 0 & countries_all == FALSE) | length(variables) == 0){
                print("A WDI download parameter (countries or variable) is missing: no WDI data download")
                return()
        }
        if(countries_all == TRUE){
                countries <- c(ctry_vec, reg_vec)
                ctries_select_iso2 <- c("all")}
        
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
        
        # Store which variables were downloaded and which weren't
        vars_downloaded <- variables[variables %in% names(raw_wdi)]
        print(paste0(c("Variables successfully downloaded: ", variables)))
        vars_not_downloaded <- variables[!variables %in% names(raw_wdi)]
        print(paste0(c("Variables not downloaded: ", vars_not_downloaded)))
        
        # Return if download is empty
        if(class(raw_wdi)=="NULL") {
                print("No data was downloaded for the selected countries/years/variables")
                return()}
        
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
                                ifelse(proc_wdi$country %in% countries_reg, "Region",
                                        ifelse((!proc_wdi$country %in% countries_reg) & (proc_wdi$country %in% reg_vec),"Rest (region)" , "Rest")
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
                extra = "drop"
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
        print("WDI data download was successful")
        return(proc_wdi)
}

#Function "nice_wdi_fun_meta": creates nice WDI metadata dataframe

nice_wdi_fun_meta <- function(dataframe){
        
        var_headers <- names(dataframe) %in% c("Var_name", "Var_code", "Units", "Database", "Source")
        
        proc <- dataframe[ , var_headers]
        proc <- proc[!duplicated(proc), ]

        # Merge data for WDI variables
        if(!empty(dataframe)){
                
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
        } else {
                
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

# Function: prepare data for dynamic graphs
prep_data_for_graphs <- function(
        df,
        vars,
        ctries,
        t_start,                              
        t_end                                 
        ){
        if(!is.null(vars)){
                datfra <- data.frame(vars)
                colnames(datfra)[1] <- "VARS"
                datfra <- separate(data = datfra, 
                        col = VARS,
                        into = c("A", "Var_code"), 
                        sep  =  " \\| ",
                        extra = "merge")
                plot_input <- df %>%
                        as_tibble() %>%
                        filter(Country == ctries & 
                                        Var_code %in% datfra$Var_code &
                                        Year >= t_start &
                                        Year <= t_end) %>%
                        group_split(Var_name)
                
                return(plot_input)
        }
}
                
        
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

# Default input parameters
def_list <- list(
        # Pakistan
        # ctries_ctry = "Pakistan",
        # ctries_str = c("Bangladesh", "Egypt, Arab Rep.", "Ethiopia", "India"),
        # ctries_asp = c("Indonesia", "Mexico", "Turkey", "Vietnam"),
        # Bhutan
        ctries_ctry = "Bhutan",
        ctries_str = c("Benin", "Bolivia", "Kyrgyz Republic", "Lao PDR"),
        ctries_asp = c("Lesotho", "Mongolia", "Eswatini", "Tajikistan"),
        ctries_reg = "South Asia",
        ctries_all = FALSE,
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
        wdi_vars = c("GDP growth (annual %) | NY.GDP.MKTP.KD.ZG",
                "Inflation, consumer prices (annual %) | FP.CPI.TOTL.ZG",
                "Population, total | SP.POP.TOTL"),
        imf_vars = NULL,
        update_button_show = FALSE
)

# Clear input parameters
reset_list <- list(
        ctries_ctry = character(0),
        ctries_str = character(0),
        ctries_asp = character(0),
        ctries_reg = character(0),
        ctries_all = FALSE,
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
        imf_vars = character(0),
        update_button_show = FALSE
)

# Initial  WDI dataframe
ctries_select <- c(def_list$ctries_ctry ,
        def_list$ctries_str,
        def_list$ctries_asp,
        def_list$ctries_reg)
ctries_select_iso2 <- ctry_df[ctry_df[,"country"] %in% ctries_select, "iso2c"]
datfra <- data.frame(def_list$wdi_vars)
datfra <- separate(data = datfra, 
        col = def_list.wdi_vars,
        into = c("A", "Var_code"), 
        sep  =  " \\| ",
        extra = "merge")
variables <- sort(datfra[,2])
initial_wdi_df <- nice_wdi_fun(countries_ctry = def_list$ctries_ctry,
        countries_str = def_list$ctries_str,
        countries_asp = def_list$ctries_asp,
        countries_reg = def_list$ctries_reg,
        countries_all = def_list$ctries_all,
        variables = variables,
        start_year = def_list$time_range_start, 
        end_year = def_list$time_range_end)

initial_wdi_df_meta <- nice_wdi_fun_meta(initial_wdi_df)

# Add period and period number variables
if(def_list$time_range_end-def_list$time_range_start >= 2){
        if(def_list$time_subper){
                if(def_list$time_subper_num == 2){
                        initial_wdi_df$Period[initial_wdi_df$Year <= def_list$time_limit_1] <- def_list$time_name_1
                        initial_wdi_df$Period_num[initial_wdi_df$Year <= def_list$time_limit_1] <- 1
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_1] <- def_list$time_name_2
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_1] <- 2
                        
                }
                if(def_list$time_subper_num == 3){
                        initial_wdi_df$Period[initial_wdi_df$Year <= def_list$time_limit_1] <- def_list$time_name_1
                        initial_wdi_df$Period_num[initial_wdi_df$Year <= def_list$time_limit_1] <- 1
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_1 & 
                                        initial_wdi_df$Year <= def_list$time_limit_2] <- def_list$time_name_2
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_1 & 
                                        initial_wdi_df$Year <= def_list$time_limit_2] <- 2
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_2] <- def_list$time_name_3
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_2] <- 3
                }
                if(def_list$time_subper_num == 4){
                        initial_wdi_df$Period[initial_wdi_df$Year <= def_list$time_limit_1] <- def_list$time_name_1
                        initial_wdi_df$Period_num[initial_wdi_df$Year <= def_list$time_limit_1] <- 1
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_1 & 
                                        initial_wdi_df$Year <= def_list$time_limit_2] <- def_list$time_name_2
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_1 & 
                                        initial_wdi_df$Year <= def_list$time_limit_2] <- 2
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_2 & 
                                        initial_wdi_df$Year <= def_list$time_limit_3] <- def_list$time_name_3
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_2 & 
                                        initial_wdi_df$Year <= def_list$time_limit_3] <- 3
                        initial_wdi_df$Period[initial_wdi_df$Year > def_list$time_limit_3 ] <- def_list$time_name_4
                        initial_wdi_df$Period_num[initial_wdi_df$Year > def_list$time_limit_3 ] <- 4
                }
        }
}



# Default fixed parameters
fix_list <- list(
        time_range_min = 1960,
        time_range_max = 2030,
        col_spinner = "#009FDA",
        col_graphs_bar = "#EC7063",
        col_graphs_mult = "#EC7063",
        col_graphs_line = "#FC4024",
        col_palette_mult = "Oranges",
        font = "Calibri",
        digits = 1,
        digits_y = 0
)

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

# ui.r ----

ui <- fluidPage(
        
        # Load useShinyjs 
        shinyjs::useShinyjs(),
        
        # Load modals
        load_help_text,
        load_palette,
        
        # Theme
        theme = shinytheme("cerulean"),
        
        # Header
        titlePanel("Download & Plot Data Tool"),
        
        # Navigation bar
        navbarPage(title = NULL,
                
                # tabPanel(title = "About",
                #         
                #         
                # 
                # ),
                
                tabPanel(title = "Data",
                        
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
                                                        selected = def_list$ctries_ctry,
                                                        options = list(size = 15, `live-search` = TRUE)
                                                ),
                                                
                                                # Input: Comparators - Structural
                                                selectizeInput(
                                                        inputId = "in_id_ctries_str",
                                                        label = "Structural comparators",
                                                        choices = ctry_vec[!ctry_vec %in% c(def_list$ctries_ctry, def_list$ctries_asp)],
                                                        multiple = TRUE,
                                                        selected = def_list$ctries_str),
                                                
                                                # Input: Comparators - Aspirational
                                                selectizeInput(
                                                        inputId = "in_id_ctries_asp",
                                                        label = "Aspirational comparators",
                                                        choices = ctry_vec[!ctry_vec %in% c(def_list$ctries_ctry, def_list$ctries_str)],
                                                        multiple = TRUE,
                                                        selected = def_list$ctries_asp),
                                                
                                                # Input: Comparators - Regions
                                                selectizeInput(
                                                        inputId = "in_id_ctries_reg",
                                                        label = "Regions",
                                                        choices = reg_vec,
                                                        multiple = TRUE,
                                                        selected = def_list$ctries_reg),
                                                
                                                # Download all countries
                                                materialSwitch(
                                                        inputId = "in_id_ctries_all",
                                                        label = "Include all countries and regions",
                                                        value = def_list$ctries_all,
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
                                                        min = fix_list$time_range_min,
                                                        max = fix_list$time_range_max,
                                                        step = 1,
                                                        value = c(def_list$time_range_start, def_list$time_range_end)),
                                                
                                                # Input: Subperiods
                                                conditionalPanel(
                                                        
                                                        # Subperiod switch only appears if there are 2 or more years in range
                                                        condition = "input.in_id_time_range[1]-input.in_id_time_range[0]>=1",
                                                        materialSwitch (
                                                                inputId = "in_id_time_subper",
                                                                label = "Divide range in subperiods",
                                                                value = def_list$time_subper,
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
                                                                        value = def_list$time_subper_num),
                                                                
                                                                textInput(
                                                                        inputId = "in_id_time_name_1",
                                                                        label="Period 1 name",
                                                                        value = def_list$time_name_1),
                                                                
                                                                textInput(
                                                                        inputId = "in_id_time_name_2",
                                                                        label="Period 2 name",
                                                                        value = def_list$time_name_2),
                                                                
                                                                sliderInput(
                                                                        inputId = "in_id_time_limit_1",
                                                                        label = "Select last year of Period 1",
                                                                        sep = "",
                                                                        min = def_list$time_range_start,
                                                                        max = def_list$time_range_end,
                                                                        step = 1,
                                                                        value = def_list$time_limit_1),
                                                                
                                                                conditionalPanel(
                                                                        
                                                                        # Subperiod 3's name and the end of period 2 appear only if number of subperiods selected is 3 or 4
                                                                        condition = "input.in_id_time_subper_num == 3 | input.in_id_time_subper_num == 4",
                                                                        
                                                                        textInput(inputId = "in_id_time_name_3",
                                                                                label="Period 3 name",
                                                                                value = def_list$time_name_3),
                                                                        
                                                                        
                                                                        sliderInput(
                                                                                inputId = "in_id_time_limit_2",
                                                                                label = "Select last year of Period 2",
                                                                                sep = "",
                                                                                min = def_list$time_limit_1 + 1,
                                                                                max = def_list$time_range_end - 1,
                                                                                step = 1,
                                                                                value = def_list$time_limit_2)
                                                                        
                                                                ),
                                                                
                                                                conditionalPanel(
                                                                        
                                                                        # Subperiod 4's name and the end of period 3 appear only if number of subperiods selected is 4
                                                                        condition = "input.in_id_time_subper_num == 4",
                                                                        
                                                                        textInput(inputId = "in_id_time_name_4",
                                                                                label="Period 4 name",
                                                                                value = def_list$time_name_4),
                                                                        
                                                                        sliderInput(
                                                                                inputId = "in_id_time_limit_3",
                                                                                label = "Select last year of Period 3",
                                                                                sep = "",
                                                                                min = def_list$time_limit_2 + 1,
                                                                                max = def_list$time_range_end - 1,
                                                                                step = 1,
                                                                                value = def_list$time_limit_3)
                                                                        
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
                                                        options = list(maxOptions = 500)),
                                                
                                                # Input: WDI variables
                                                selectizeInput(
                                                        inputId = "in_id_imf_vars",
                                                        label = "IMF variables (Name | Code)",
                                                        choices = NULL,
                                                        multiple = TRUE)
                                        ),
                                        
                                        # Upload data from external file
                                        wellPanel( 
                                                
                                                style = "background-color: #ebf5fb",
                                                
                                                h4("Step 4: Upload data from external file"),
        
                                                uiOutput('in_id_ext_ui'),
                                                
                                                # Messages
                                                uiOutput("in_id_ext_mes_ui1"),
                                                uiOutput("in_id_ext_mes_ui2")
                                        
                                        )
                                ),
                                
                                mainPanel(
                                        
                                        tabsetPanel(type = "tabs",
                                                
                                                # Output: Table
                                                tabPanel("Table",
                                                        
                                                        # Table 1: data
                                                        tags$br(),
                                                        dataTableOutput("out_data_table") %>% withSpinner(
                                                                type = 3, 
                                                                color = fix_list$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        # Data download button
                                                        tags$br(),
                                                        downloadButton("out_download_data", 
                                                                "Download data")
                                                ),
                                                
                                                # Output: Metadata
                                                tabPanel("Metadata",
                                                        
                                                        # Table 2: metadata
                                                        tags$br(),
                                                        dataTableOutput("out_metadata_table") %>% withSpinner(
                                                                type = 3, 
                                                                color = fix_list$col_spinner, 
                                                                color.background = "white"),
                                                        
                                                        # Data download button
                                                        tags$br(),
                                                        downloadButton("out_download_metadata", 
                                                                "Download metadata")
                                                ),
                                                
                                                # Output: Summary
                                                tabPanel("Summary",
                                                        tags$br(),
                                                        
                                                        h4("Summary"),
                                                        htmlOutput("out_summary"),
                                                        
                                                        h4("Missing values"),
                                                        plotOutput("out_summary_missing")
                                                
                                                ),
                                                
                                                # Output: Country table
                                                tabPanel("Countries/regions WDI",
                                                        
                                                        tags$br(),
                                                        h4("Countries"),
                                                        dataTableOutput("out_ctry_df"),
                                                        
                                                        tags$br(),
                                                        h4("Aggregates"),
                                                        dataTableOutput("out_ctry_df_agg"),
                                                        
                                                        # Download button
                                                        tags$br(),
                                                        downloadButton("out_download_ctry_df", 
                                                                "Download")
                                                        
                                                )
                                        )
                                )
                        )
                ),
                
                navbarMenu(title = "Bar plots",

                        tabPanel(title = "Single Country",
                                
                                sidebarLayout(
                                        
                                        sidebarPanel(
                                                
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Plot parameters"),
                                                        
                                                        # Select countries
                                                        pickerInput(
                                                                inputId = "gr_bar_id_ctries_ctry",
                                                                label = "Select country",
                                                                choices = flags_df$country,
                                                                choicesOpt = list(content =  
                                                                                mapply(flags_df$country, flags_df$URL, FUN = function(x, y) {
                                                                                        HTML(paste(
                                                                                                tags$img(src = y, width = 20, height = 15),
                                                                                                x
                                                                                        ))
                                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                                ),
                                                                selected = def_list$ctries_ctry,
                                                                options = list(size = 15,
                                                                        `live-search` = TRUE)
                                                        ),
                                                        
                                                        # Select variables
                                                        pickerInput(
                                                                inputId = "gr_bar_id_vars",
                                                                label = "Select variables", 
                                                                choices = def_list$wdi_vars,
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        # Select range
                                                        sliderInput(
                                                                inputId = "gr_bar_id_time_range",
                                                                label = "Select range",
                                                                sep = "",
                                                                min = def_list$time_range_start,
                                                                max = def_list$time_range_end,
                                                                step = 1,
                                                                value = c(def_list$time_range_start, def_list$time_range_end)
                                                        )
                                                ),
                                                
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        # Display options ----
                                                        h4("Display options"),
                                                        
                                                        # Include title
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_title",
                                                                label = "Include title",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_yaxis",
                                                                label = "Include Y-axis units",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include source
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_source",
                                                                label = "Include source",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include data labels
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_data_labels",
                                                                label = "Include data labels",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_subper == true",
                                                                materialSwitch (
                                                                        inputId = "gr_bar_id_time_subper",
                                                                        label = "Include subperiods",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                        ),
                                                        
                                                        # Include period/subperiod averages
                                                        conditionalPanel(
                                                                condition = "input.gr_bar_id_time_subper == true",
                                                                materialSwitch(
                                                                        inputId = "gr_bar_id_time_subper_avg",
                                                                        label = "Include period/subperiod averages",
                                                                        value = FALSE,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                )
                                                        ),
                                                        
                                                        # Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Transform data to logs (applies only to positive series)
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_transform_log",
                                                                label = "Transform data to logs (applies only to positive series)",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Number of digits
                                                        numericInput(
                                                                inputId = "gr_bar_id_digits",
                                                                label = "Number of digits in labels",
                                                                min = 0,
                                                                value = fix_list$digits
                                                        ),
                                                        
                                                        # Number of digits
                                                        numericInput(
                                                                inputId = "gr_bar_id_digits_y",
                                                                label = "Number of digits in Y-axis",
                                                                min = 0,
                                                                value = fix_list$digits_y
                                                        ),
                                                        
                                                        # Select color and transparency of bars
                                                        colourpicker::colourInput(
                                                                inputId = "gr_bar_id_color",
                                                                label = "Select color and transparency of bars",
                                                                value = fix_list$col_graphs_bar,
                                                                allowTransparent = TRUE,
                                                                closeOnClick = TRUE
                                                        ),
                                                        
                                                        # Select font
                                                        pickerInput(
                                                                inputId = "gr_bar_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = fix_list$font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                
                                                        )
                                                )
                                        ),
                                        
                                        mainPanel(
                                                
                                                # Show error message if no data
                                                tags$br(),

                                                # Plots' dynamic UI
                                                uiOutput("gr_bar_out_plots") %>% withSpinner(type = 3, 
                                                        color = fix_list$col_spinner, 
                                                        color.background = "white"),
                                                helpText("Select download option (zipped .png files)"),
                                                
                                                # Download graphs options
                                                downloadButton("gr_bar_download_large", "Long Plots"),
                                                downloadButton("gr_bar_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br()
                                                
                                        )
                                )
                        ),

                        tabPanel(title = "Multiple Countries",
                                
                                sidebarLayout(
                                        
                                        sidebarPanel(
                                                
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Plot parameters"),
                                                        
                                                        # Select countries
                                                        pickerInput(
                                                                inputId = "gr_mult_id_ctries",
                                                                label = "Select countries/regions", 
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
                                                                label = "Select measure", 
                                                                choices = stats_vec,
                                                                multiple = FALSE
                                                        ),
                                                        
                                                        # Select range
                                                        sliderInput(
                                                                inputId = "gr_mult_id_time_range",
                                                                label = "Select range",
                                                                sep = "",
                                                                min = def_list$time_range_start,
                                                                max = def_list$time_range_end,
                                                                step = 1,
                                                                value = c(def_list$time_range_start, def_list$time_range_end)
                                                        )
                                                                
                                                ),
                                                
                                                wellPanel( 
                                                        
                                                        style = "background-color: #ebf5fb",
                                                        
                                                        h4("Display options"),
                                                        
                                                        # Include title
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_title",
                                                                label = "Include title",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_yaxis",
                                                                label = "Include Y-axis units",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include source
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_source",
                                                                label = "Include source",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include data labels
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_data_labels",
                                                                label = "Include data labels",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Include subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_subper == true",
                                                                materialSwitch (inputId = "gr_mult_id_time_subper",
                                                                        label = "Include subperiods",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                )
                                                        ),
                                                        
                                                        # Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Transform data to logs (applies only to positive series)
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_transform_log",
                                                                label = "Transform data to logs (applies only to positive series)",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Change country names to short
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_ctry_short",
                                                                label = "Short country names (ISO 3)",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # Number of digits
                                                        numericInput(
                                                                inputId = "gr_mult_id_digits",
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = fix_list$digits
                                                        ),
                                                        
                                                        # Number of digits Y-axis
                                                        numericInput(
                                                                inputId = "gr_mult_id_digits_y",
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = fix_list$digits_y
                                                        ),
                                                        
                                                        # Select font
                                                        pickerInput(
                                                                inputId = "gr_mult_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = fix_list$font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                        ),
                                                        
                                                        # Select color palette
                                                        conditionalPanel(
                                                                condition = "input.gr_mult_id_time_subper == true",
                                                                uiOutput('gr_mult_id_color_ui')
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_mult_id_time_subper == false",
                                                                colourpicker::colourInput(
                                                                        inputId = "gr_mult_id_color",
                                                                        label = "Select color and transparency of bars",
                                                                        value = fix_list$col_graphs_mult,
                                                                        allowTransparent = TRUE,
                                                                        closeOnClick = TRUE)
                                                        ),
                                                        
                                                        # Select legend position
                                                        pickerInput(
                                                                inputId = "gr_mult_id_legend_pos",
                                                                label = "Select legends position", 
                                                                choices = position_list,
                                                                selected = "top",
                                                                multiple = FALSE
                                                        )
                                                )
                                                
                                        ),
                                        mainPanel(
                                                
                                                # Show error message if no data
                                                tags$br(),
                                                
                                                # Plots' dynamic UI
                                                uiOutput("gr_mult_out_plots") %>% withSpinner(type = 3, 
                                                        color = fix_list$col_spinner, 
                                                        color.background = "white"),
                                                helpText("Select download option (zipped .png files)"),
                                                
                                                # Download graphs options
                                                downloadButton("gr_mult_download_large", "Long Plots"),
                                                downloadButton("gr_mult_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br()
                                        )
                                )
                        )
                ),
                
                tabPanel(title = "Line plots",
                        
                        sidebarLayout(
                                
                                sidebarPanel(
                                        
                                        wellPanel( 
                                                
                                                style = "background-color: #ebf5fb",
                                                
                                                h4("Plot parameters"),
                                                
                                        ),
                                        
                                        wellPanel( 
                                                
                                                style = "background-color: #ebf5fb",
                                                
                                                h4("Display options"),
                                                
                                        )
                                        
                                ),
                                
                                mainPanel(
                                        
                                )
                        )
                ),
                
                tabPanel(title = "Scatter plots",
                        
                        sidebarLayout(
                                
                                sidebarPanel(
                                        
                                        wellPanel( 
                                                
                                                style = "background-color: #ebf5fb",
                                                
                                                h4("Plot parameters"),
                                                
                                        ),
                                        
                                        wellPanel( 
                                                
                                                style = "background-color: #ebf5fb",
                                                
                                                h4("Display options"),
                                                
                                        )
                                        
                                ),
                                
                                mainPanel(
                                        

                                )
                        )
                )
        )
)

# server.r ----
server <- function(input, output, session) {
        
        ## Data tab ----        

        # Update inputs based on other inputs
        
        # Update structural comparators available choices
        observeEvent(c(input$in_id_ctries_ctry, input$in_id_ctries_asp) ,{

                updateSelectizeInput(
                        inputId = 'in_id_ctries_str',
                        choices = ctry_vec[!ctry_vec %in% c(input$in_id_ctries_ctry , input$in_id_ctries_asp)],
                        selected = input$in_id_ctries_str)
                
        })
        
        # Update aspirational comparators available choices
        observeEvent(c(input$in_id_ctries_ctry, input$in_id_ctries_str) ,{
                
                updateSelectizeInput(
                        inputId = 'in_id_ctries_asp',
                        choices = ctry_vec[!ctry_vec %in% c(input$in_id_ctries_ctry , input$in_id_ctries_str)],
                        selected = input$in_id_ctries_asp)
                
        })
        
        # Update subperiod inputs as range changes
        observeEvent(input$in_id_time_range, {
                
                updateNumericInput(
                        session = session,
                        inputId = 'in_id_time_subper_num',
                        min = 2,
                        max = min(4, input$in_id_time_range[2] - input$in_id_time_range[1] + 1),
                        value = min(input$in_id_time_range[2] - input$in_id_time_range[1] + 1, input$in_id_time_subper_num))
                
        })
        
        observeEvent(c(input$in_id_time_range,
                input$in_id_time_subper,
                input$in_id_time_subper_num,
                input$in_id_time_limit_1,
                input$in_id_time_limit_2,
                input$in_id_time_limit_3
                ), {
                
                if(input$in_id_time_subper_num >= 3){
                        max_lim_1 <- input$in_id_time_limit_2 - 1 
                } else {max_lim_1 <- input$in_id_time_range[2] - 1}
                        
                updateSliderInput(
                        session = session,
                        inputId = "in_id_time_limit_1",
                        min = input$in_id_time_range[1],
                        max = max_lim_1,
                        step = 1,
                        value = input$in_id_time_limit_1)
                
                if(input$in_id_time_subper_num == 4){
                        max_lim_2 <- input$in_id_time_limit_3 - 1
                } else {max_lim_2 <- input$in_id_time_range[2] - 1}
                                
                updateSliderInput(
                        session = session,
                        inputId = "in_id_time_limit_2",
                        min = input$in_id_time_limit_1 + 1,
                        max = max_lim_2,
                        step = 1,
                        value = input$in_id_time_limit_2)
                
                updateSliderInput(
                        session = session,
                        inputId = "in_id_time_limit_3",
                        min = input$in_id_time_limit_2 + 1,
                        max = input$in_id_time_range[2] - 1,
                        step = 1,
                        value = input$in_id_time_limit_3)
        
        })
        
        # Update WDI variables
        updateSelectizeInput(
                session = session,
                inputId = 'in_id_wdi_vars',
                selected = def_list$wdi_vars,
                choices = var_vec,
                server = TRUE
                )

        # Upload external data
        output$in_id_ext_ui <- renderUI({
                fileInput('in_id_ext', label = "Select file")  %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_text")
                        )
        })
        
        observeEvent(c(input$in_id_ctries_ctry, 
                input$in_id_ctries_str,
                input$in_id_ctries_asp,
                input$in_id_ctries_reg,
                input$in_id_ctries_all),{
                        ctry_aux <- c(input$in_id_ctries_ctry, 
                                input$in_id_ctries_str,
                                input$in_id_ctries_asp,
                                input$in_id_ctries_reg)

                if(is.null(ctry_aux) & input$in_id_ctries_all == FALSE){

                        onclick("in_id_ext",
                                showModal(session = session,
                                        modalDialog(
                                        title = "Warning",
                                        "Select Countries and Time period before loading external data.",
                                        easyClose = TRUE
                                ))
                        )
                } else {
                        if(length(ctry_aux)==0 & input$in_id_ctries_all == FALSE){

                                print(length(ctry_aux)==0 & input$in_id_ctries_all == FALSE)
                                onclick("in_id_ext",
                                        showModal(session = session,
                                                modalDialog(
                                                title = "Warning",
                                                "Select Countries and Time period before loading external data.",
                                                easyClose = TRUE
                                        ))
                                )
                        } else {
                                onclick("in_id_ext",
                                        removeModal(session = session))
                        }
                }
        })



        
        # Update external data message
        
        observeEvent( c(input$in_id_ctries_ctry, 
                input$in_id_ctries_str,
                input$in_id_ctries_asp,
                input$in_id_ctries_reg,
                input$in_id_ctries_all,
                input$in_id_time_range
                ), {
        
                selected_ctries <- c(input$in_id_ctries_ctry, 
                        input$in_id_ctries_str,
                        input$in_id_ctries_asp,
                        input$in_id_ctries_reg)
                
                output$in_id_ext_mes_ui1 <- renderUI({
                        if(is.null(selected_ctries) & input$in_id_ctries_all == FALSE){
                                return(tags$p("Select Countries and Time period before loading external data.", 
                                        style = "color:red"))
                        }
                        
                })
        })

        # Inputs reactive values
        
        # Create reactive values initially containing list with default inputs
        rv_input <- reactiveValues(
                
                # Country inputs
                ctries_ctry = def_list$ctries_ctry,
                ctries_str = def_list$ctries_str,
                ctries_asp = def_list$ctries_asp,
                ctries_reg = def_list$ctries_reg,
                ctries_all = def_list$ctries_all,
                
                # The following 2 inputs are a function of the above. They will be calculated later.
                ctries_select = character(0),
                ctries_select_iso2 = character(0), 
                
                # Time inputs
                time_range_start = def_list$time_range_start,
                time_range_end = def_list$time_range_end,
                time_subper = def_list$time_subper,
                time_subper_num = def_list$time_subper_num,
                time_limit_1 = def_list$time_limit_1,
                time_limit_2 = def_list$time_limit_2,
                time_limit_3 = def_list$time_limit_3,
                time_name_1 = def_list$time_name_1,
                time_name_2 = def_list$time_name_2,
                time_name_3 = def_list$time_name_3,
                time_name_4 = def_list$time_name_4,
                
                # Variable inputs
                wdi_vars = def_list$wdi_vars,
                # The following input is a function of the above. They will be calculated later.
                wdi_vars_code = character(0),
                
                imf_vars = def_list$imf_vars,
                
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
        
        # Create reactive values for plot lists
        
        rv_plots <- reactiveValues(
                
                bar = list(),
                mult = list(),
                line = list(),
                scat = list
                
        )
        
        
        # Listen to any changes in inputs
        # This reactive function will be invalidated (and invalidate everything that depends on it) 
        # when at least one input changes (it does not matter if we change it once and then back to 
        # the original position), but it will not react if we press clear or update.
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
                        input$in_id_ctries_all,
                        
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
                        input$in_id_imf_vars,
                        
                        # External
                        input$in_id_ext,
                        
                        # Reset
                        input$in_id_reset_confirm
                ) , {
                        
                        # Vector of changes                
                        no_changes_vec <- c(
                                
                                # Country inputs
                                setequal(rv_input$ctries_ctry, input$in_id_ctries_ctry),
                                setequal(rv_input$ctries_str, input$in_id_ctries_str),
                                setequal(rv_input$ctries_asp, input$in_id_ctries_asp),
                                setequal(rv_input$ctries_reg, input$in_id_ctries_reg),
                                setequal(rv_input$ctries_all, input$in_id_ctries_all),
                                
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
                                setequal(rv_input$imf_vars, input$in_id_imf_vars),
                                
                                # External data
                                is.null(input$in_id_ext$name)
                                
                        )
                        
                        # If all elements in vector no_changes_vec are TRUE, then return FALSE, 
                        # else TRUE

                        return(!all(no_changes_vec))
                        
                }, ignoreInit = TRUE)
        
        # Show update button
        observeEvent(listen_inputs(), {
                
                rv_input$update_button_show <- FALSE
                if(listen_inputs()){rv_input$update_button_show <- TRUE}

                # If at least one input changed, show update button
                if(rv_input$update_button_show){
                        shinyjs::show("in_id_update")
                        } else {shinyjs::hide("in_id_update")}
                
                
        })
        
        # Update reactive values list when observing input$in_id_update 
        observeEvent(input$in_id_update, {
                
                # Country inputs
                rv_input$ctries_ctry <- input$in_id_ctries_ctry
                rv_input$ctries_str <- input$in_id_ctries_str
                rv_input$ctries_asp <- input$in_id_ctries_asp
                rv_input$ctries_reg <- input$in_id_ctries_reg
                rv_input$ctries_all <- input$in_id_ctries_all
                
                if(input$in_id_ctries_all){rv_input$ctries_selected <- c(ctry_vec, reg_vec)} else {
                        rv_input$ctries_select <- c(input$in_id_ctries_ctry ,
                                input$in_id_ctries_str,
                                input$in_id_ctries_asp,
                                input$in_id_ctries_reg)
                }
                
                rv_input$ctries_select_iso2 <- ctry_df[ctry_df[,"country"] %in% rv_input$ctries_select, "iso2c"]
                
                
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
                
                # Variable inputs
                rv_input$wdi_vars <- input$in_id_wdi_vars
                rv_input$wdi_vars_code <- filter(var_df, name_indicator %in% input$in_id_wdi_vars) %>% pull(indicator)
                
                rv_input$imf_vars <- input$in_id_imf_vars

        })
        
        # Hide update button after updating
        observeEvent(input$in_id_update, {
                
                shinyjs::hide("in_id_update")
                
                # External data
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
                
                        # Reset reactive value list
                        
                                # Country inputs
                                rv_input$ctries_ctry <- reset_list$ctries_ctry
                                rv_input$ctries_str <- reset_list$ctries_str
                                rv_input$ctries_asp <- reset_list$ctries_asp
                                rv_input$ctries_reg <- reset_list$ctries_reg
                                rv_input$ctries_all <- reset_list$ctries_all
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
                                rv_input$imf_vars <- reset_list$imf_vars
                                
                                # Show update button
                                rv_input$update_button_show <- reset_list$update_button_show
                                
                                # Dataframes
                                rv_df$dat_wdi <- empty_data_df
                                rv_df$dat_imf <- empty_data_df
                                rv_df$dat_ext <- empty_data_df
                                rv_df$dat_ext_last <- empty_data_df
                                rv_df$dat_all <- empty_data_df
                                rv_df$met_wdi <- empty_metadata_df
                                rv_df$met_imf <- empty_metadata_df
                                rv_df$met_ext <- empty_metadata_df
                                rv_df$met_ext_last <- empty_metadata_df
                                rv_df$met_all <- empty_metadata_df
                        
                        # Reset inputs
                        
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
                                
                                # All
                                updateMaterialSwitch(
                                        session = session,
                                        inputId = "in_id_ctries_all",
                                        value = reset_list$ctries_all)
                                
                                # Time range
                                updateSliderInput(
                                        session = session,
                                        inputId = "in_id_time_range",
                                        min = fix_list$time_range_min,
                                        max = fix_list$time_range_max,
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
                                        server = TRUE,
                                        selected = reset_list$wdi_vars)
                                
                                # IMF variables
                                updateSelectizeInput(
                                        session = session,
                                        inputId = "in_id_imf_vars",
                                        choices = NULL,
                                        server = TRUE,
                                        selected = reset_list$imf_vars)
                                
                                # External data
                                output$in_id_ext_ui <- renderUI({
                                        fileInput('in_id_ext', label = "Select file") %>%
                                                shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                                bs_attach_modal(id_modal = "help_text")
                                                )
                                })
                                
                                # Hide update button
                                
                                # shinyjs::hide("in_id_update")
                                
                }
        })
        
        # Hide update button
        observeEvent(input$in_id_reset_confirm, {
                shinyjs::hide("in_id_update")
        })
        
        
        # Reactive data_wdi
        dat_wdi <- eventReactive(input$in_id_update, {
                
                data <- nice_wdi_fun(countries_ctry = rv_input$ctries_ctry,
                        countries_str = rv_input$ctries_str,
                        countries_asp = rv_input$ctries_asp,
                        countries_reg = rv_input$ctries_reg,
                        countries_all = rv_input$ctries_all,
                        variables = rv_input$wdi_vars_code,
                        start_year = rv_input$time_range_start, 
                        end_year = rv_input$time_range_end)
                
                return(data)
                
        })
        
        # Load external data
        observeEvent(c(input$in_id_ext, 
                input$in_id_ctries_ctry, 
                input$in_id_ctries_str, 
                input$in_id_ctries_asp, 
                input$in_id_ctries_reg,
                input$in_id_ctries_all,
                input$in_id_time_range
                ), {
                
                # If there is no file loaded, then return
                if(is.null(input$in_id_ext)){return()}       
                
                # Print messages with filter parameters                
                print("Starting external data import process")
                print("(1/2) Selected countries/regions (ISO-2):")
                if(input$in_id_ctries_all == TRUE){
                        message_ctries <- c("All")
                } else {message_ctries <- ctry_df[ctry_df[,"country"] %in% c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, input$in_id_ctries_reg), "iso2c"]}
                print(message_ctries)
                print("(2/2) Year range:")
                print(paste0(input$in_id_time_range[1], " - ", input$in_id_time_range[2]))
                
                # Read file
                file_extension <- file_ext(input$in_id_ext$name)
                print("Extension of the loaded file:")
                print(file_extension)


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
                        output$in_id_ext_mes_ui2 <- renderUI({
                                return(tags$p("Wrong file extension. The file must be one of the following formats: .txt, .xls, .xlsx, .csv, .dta", 
                                        style = "color:red"))
                        })
                        return()
                }
                
                output$in_id_ext_mes_ui2 <- renderUI({
                        return()
                })

                # Keep only the following variables
                headers <- names(df_external) %in% c("Var_name", "Var_code", "Description", "Units", "Ctry_iso", "Year", "Value", "Source", "Database", "Source_data", "Source_org")
                merge_aux <- names(df_external)[names(df_external) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Database")]
                df_external <- df_external[ , headers]
                
                # Check all necessary variables were included
                if((!"Var_name" %in% names(df_external)) |
                                (!"Units" %in% names(df_external)) |
                                (!"Ctry_iso" %in% names(df_external)) |
                                (!"Year" %in% names(df_external)) |
                                (!"Value" %in% names(df_external))
                ){
                        print("One of the following variables is missing from the uploaded dataset: Var_name, Units, Ctry_iso, Year, Value.")
                        output$in_id_ext_mes_ui2 <- renderUI({
                                vec <- c("Var_name", "Units", "Ctry_iso", "Year", "Value")
                                missing_variables <- vec[!vec %in% names(df_external)]
                                text <- paste(missing_variables, collapse =", ")
                                text <- paste("Error in file upload. The following required variables are missing from the last uploaded dataset: ", 
                                        text, ". Data has not been included.",
                                        collapse ="")
                                return(tags$p(text, style = "color:red"))
                        })
                        return()}
                
                # Empty message if all variables included
                output$in_id_ext_mes_ui2 <- renderUI({
                        return()
                })
                
                # To ensure we have the same number of rows per variable
                # create a dataset with num_ctries * num_years rows * num_variable
                # that includes only "Country", "Year" and "Variable"
                
                if(input$in_id_ctries_all == TRUE){
                        ctries_select_iso3 <- unique(ctry_df$iso3c)
                } else {
                        ctries_select <- c(input$in_id_ctries_ctry, input$in_id_ctries_str, input$in_id_ctries_asp, input$in_id_ctries_reg)
                        if(is.null(ctries_select)){
                                print("No countries selected.")
                                return()}
                        ctries_select_iso3 <- ctry_df[ctry_df[,"country"] %in% ctries_select, "iso3c"]
                }

                years <- input$in_id_time_range[1]:input$in_id_time_range[2]
                
                # If no defined parameters for time range or countries, then return
                if(length(ctries_select_iso3)==0 | length(years)==0){
                        print("Missing Year or Country parameter")
                        return()
                }

                proc <- create_empty_panel(df_external, ctries_select_iso3, years)

                # Include country names
                proc <- merge(x = proc,
                        y = ctry_df[ , c("country", "iso3c")],
                        by.x="Ctry_iso", 
                        by.y="iso3c",
                        all.x = TRUE)

                names(proc)[names(proc) == "country"] <- "Country"

                # Merge empty dataframe with uploaded data
                df_external <- merge(
                        x = proc,
                        y = df_external,
                        by = merge_aux,
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
                df_external$Ctry_group[df_external$Country %in% input$in_id_ctries_reg] <- "Region"
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
                                (df_external$Country %in% reg_vec)] <- "Rest (region)"
                
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
                
                # Save dataframe in global
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
                
                aux_met_ext_last <- df_external[ , c("Var_name",
                        "Var_code",
                        "Description",
                        "Units",
                        "Source_data",
                        "Source_org",
                        "Database",
                        "Source")]
                
                rv_df$met_ext_last <- aux_met_ext_last[!duplicated(aux_met_ext_last), ]

        })
        
        
        # Merge datasets
        observeEvent(input$in_id_update, {
                
                # Append last external dataset to all loaded external datasets
                rv_df$dat_ext <- rbind.fill(rv_df$dat_ext, rv_df$dat_ext_last)
                rv_df$dat_ext <- rv_df$dat_ext[!duplicated(rv_df$dat_ext), ]
                rv_df$dat_ext_last <- empty_data_df
                rv_df$met_ext <- rbind.fill(rv_df$met_ext, rv_df$met_ext_last)
                rv_df$met_ext <- rv_df$met_ext[!duplicated(rv_df$met_ext), ]
                rv_df$dat_ext_last <- empty_metadata_df
                
                # Update WDI dataset
                rv_df$dat_wdi <- dat_wdi()
                
                # Merge WDI data with all external data
                rv_df$dat_all <- rbind.fill(rv_df$dat_wdi, rv_df$dat_ext)
                
                # Add period and period number variables
                if(rv_input$time_range_end-rv_input$time_range_start >= 2){
                        if(rv_input$time_subper){
                                if(rv_input$time_subper_num == 2){
                                        rv_df$dat_all$Period[rv_df$dat_all$Year <= rv_input$time_limit_1] <- rv_input$time_name_1
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year <= rv_input$time_limit_1] <- 1
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_1] <- rv_input$time_name_2
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_1] <- 2
                                        
                                }
                                if(rv_input$time_subper_num == 3){
                                        rv_df$dat_all$Period[rv_df$dat_all$Year <= rv_input$time_limit_1] <- rv_input$time_name_1
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year <= rv_input$time_limit_1] <- 1
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_1 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_2] <- rv_input$time_name_2
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_1 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_2] <- 2
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_2] <- rv_input$time_name_3
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_2] <- 3
                                }
                                if(rv_input$time_subper_num == 4){
                                        rv_df$dat_all$Period[rv_df$dat_all$Year <= rv_input$time_limit_1] <- rv_input$time_name_1
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year <= rv_input$time_limit_1] <- 1
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_1 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_2] <- rv_input$time_name_2
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_1 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_2] <- 2
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_2 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_3] <- rv_input$time_name_3
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_2 & 
                                                        rv_df$dat_all$Year <= rv_input$time_limit_3] <- 3
                                        rv_df$dat_all$Period[rv_df$dat_all$Year > rv_input$time_limit_3 ] <- rv_input$time_name_4
                                        rv_df$dat_all$Period_num[rv_df$dat_all$Year > rv_input$time_limit_3 ] <- 4
                                }
                        }
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
                
        })
        
        # Render table        
        output$out_data_table <- renderDataTable({

                # Table
                if(input$in_id_update == 0 & length(input$in_id_reset_confirm) == 0){
                        x <- initial_wdi_df} else{
                                if(input$in_id_update >= 1){
                                        x <- rv_df$dat_all}else{
                                        if(input$in_id_update == 0 & input$in_id_reset_confirm == FALSE){
                                                x <- initial_wdi_df
                                        }else{x <- rv_df$dat_all}
                                }
                        }

                # Eliminate variables Ctry_group_num, Period_num and Source
                x <- subset(x, select=-c(Ctry_group_num, Period_num, Source))
                
                # Datatable
                datatable(x,
                        rownames = FALSE,
                        options = list(pageLength = 25,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                ) %>%
                        formatCurrency(columns = "Value",
                                currency = "", 
                                interval = 3, 
                                mark = ",",
                                digits = 1)
        })
        
        # Download data handler
        output$out_download_data <- downloadHandler(
                filename = function() {
                        paste('data', '.csv', sep='')
                },
                content = function(file) {
                        write.csv(rv_df$dat_all, file, row.names = FALSE)
                }
        )
        
        # Event reactive metadata
        metadata <- eventReactive(c(input$in_id_update, input$in_id_reset_confirm),{
                
                if(empty(rv_df$dat_all)){
                        print("Empty dataset. Return empty metadata")
                        rv_df$met_all <- empty_metadata_df
                        return(rv_df$met_all)}

                # Merge data for WDI variables
                
                rv_df$met_wdi <- nice_wdi_fun_meta(rv_df$dat_wdi)
                rv_df$met_all <- rbind(rv_df$met_wdi, rv_df$met_ext)

                return(rv_df$met_all)
                
        })
        
        # Render metadata table        
        output$out_metadata_table <- renderDataTable({
                
                datatable(metadata(), rownames = FALSE, options = list(scrollX = T))
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
        
        # Render summary table
        output$out_summary <- renderUI({
                
                # Reshape and select data
                aux <- reshape2::dcast(rv_df$dat_all, Ctry_iso + Year ~ Var_name, value.var="Value")
                # Print summary
                print(dfSummary(aux), headings = FALSE, method = 'render', bootstrap.css = FALSE,
                        graph.magnif = 0.75, valid.col = FALSE, style = "grid")
                
        })
        
        # Render summary missing values plot
        output$out_summary_missing <- renderPlot({
                
                # Reshape and select data
                aux <- reshape2::dcast(rv_df$dat_all, Ctry_iso + Year ~ Var_name, value.var="Value")
                
                # Create plot
                p <- prepare_missing_values_graph(aux, ts_id = "Year")

                p
        })
        
        # Render countries table        
        output$out_ctry_df <- renderDataTable({
                
                ctry_df <- ctry_df %>% arrange(factor(country, levels = c(ctry_vec, reg_vec)))
                ctry_df <- subset(ctry_df, select=-c(capital, longitude, latitude, lending))
                
                names(ctry_df)[names(ctry_df) == "iso3c"] <- "Iso3c"
                names(ctry_df)[names(ctry_df) == "iso2c"] <- "Iso2c"
                names(ctry_df)[names(ctry_df) == "country"] <- "Country/Region"
                names(ctry_df)[names(ctry_df) == "region"] <- "Region"
                names(ctry_df)[names(ctry_df) == "income"] <- "Income"
                
                rv_df$country_list <- ctry_df 
                
                aux <- ctry_df[ctry_df$Region != "Aggregates", ]
                
                datatable(aux,
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )
        })
        
        # Render countries table        
        output$out_ctry_df_agg <- renderDataTable({
                
                aux <- rv_df$country_list[rv_df$country_list$Region == "Aggregates", ]
                
                datatable(aux,
                        rownames = FALSE,
                        options = list(pageLength = 15,
                                columnDefs = list( list(className = 'dt-left', targets = "_all")),
                                scrollX = T)
                )
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
                
        ## Bar plots - Single country tab ----
        
        ### Update inputs ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {
                
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

                flags_df$Ctry_slash_Group <- paste(flags_df$country, flags_df$Ctry_group, sep=" | ")
                
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
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep=" | "))
                
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
                        session= session,
                        inputId = "gr_bar_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end))
                
                # Subperiod average
                updateMaterialSwitch(
                        session= session,
                        inputId="gr_bar_id_time_subper_avg",
                        value = FALSE)

        })
        
        ### Plots ----
        
        # Prep data
        prepped_data_bar <- reactive({
                
                graph_input <- prep_data_for_graphs(
                        df = rv_df$dat_all,
                        vars = input$gr_bar_id_vars,
                        ctries = input$gr_bar_id_ctries_ctry,
                        t_start = input$gr_bar_id_time_range[1],
                        t_end = input$gr_bar_id_time_range[2]
                )
                
                return(graph_input)
                

        })
        
        
        # Create plots
        createUI_bar <- function(table) {
                
                if(!all(is.na(table$Value))){
                
                        # Parameters for plot
                        
                        # Input for number of digits to be used in y-axis and data labels
                        digits_num <- input$gr_bar_id_digits
                        
                        # Input for trillions/billions/millions/thousands transformation
                        accuracy_yaxis <- 1/10^(input$gr_bar_id_digits_y)
                        units_zeros <- c("Trillions", "Billions", "Millions", "Thousands")
                        
                        # Input for title, subtitle Y-axis and source
                        title_text <- ""
                        
                        subtitle_text <- ""
                        
                        title_text <- if(input$gr_bar_id_title){
                                paste0(unique(table$Var_name), " - ", unique(table$Country))}
                        
                        yaxis_units <- if(input$gr_bar_id_yaxis){
                                        if(is.na(unique(table$Units))){NULL} else {unique(table$Units)}
                                } else {NULL}
        
                        if(input$gr_bar_id_source){
                                graph_source <- unique(table$Database)
                                if(graph_source=="WDI"){
                                        graph_source<- "World Development Indicators"}
                                }
                        
                        # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years 
                        intervals <- ifelse(input$gr_bar_id_time_range[2] - input$gr_bar_id_time_range[1] < 30, 1, 2)
                        
                        # Transform data: divide by trillions/billions/millions/thousands
                        for (i in 4:1){
                                if(input$gr_bar_id_transform_zeros & max(abs(table$Value), na.rm =TRUE)>(10^(3*i))){
                                        if(input$gr_bar_id_title) {
                                                subtitle_text <- paste(
                                                        c(subtitle_text, units_zeros[5-i]), 
                                                        collapse = "")
                                                separator <- if(subtitle_text==""){""} else {", "}
                                                if(input$gr_bar_id_yaxis){
                                                        yaxis_units <- paste(
                                                                c(yaxis_units, units_zeros[5-i]), 
                                                                collapse = separator)
                                                }
                                        }
                                        table$Value <- table$Value/(10^(3*i))
                                }
                        }
                        
                        # Log transform
                        if(input$gr_bar_id_transform_log & min(table$Value, na.rm =TRUE)>0){
                                subtitle_text <- paste(
                                        c(subtitle_text, " (Log transformation)"), 
                                        collapse = "")
                                if(input$gr_bar_id_yaxis){                                                
                                        yaxis_units <- paste(
                                                c(yaxis_units, " (Log transformation)"), 
                                                collapse = "")
                                }
                                table$Value <- log(table$Value)
                        }

                        
                        # Create subperiod variables to include rectangles
                        if(rv_input$time_subper & input$gr_bar_id_time_subper){
                                
                                # If 2 subperiods, then there are 3 cases
                                if(rv_input$time_subper_num == 2){
                                        
                                        # Case 1 
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] > rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2)
                                                )
                                        }
                                        # Case 2 
                                        if(input$gr_bar_id_time_range[2] <= rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1)
                                                )
                                        }
                                        # Case 3
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2)
                                                )
                                        }
                                }
                                # If 3 subperiods, then there are 6 cases
                                if(rv_input$time_subper_num == 3){
                                        
                                        # Case 1
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] > rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2, rv_input$time_name_3)
                                                )
                                        }
                                        # Case 2
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_2 & input$gr_bar_id_time_range[2] > rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2)
                                                )
                                        }
                                        # Case 3
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_1 & input$gr_bar_id_time_range[1] < rv_input$time_limit_2 & input$gr_bar_id_time_range[2] > rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_2+0.5), 
                                                        Year_end=c(rv_input$time_limit_2+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2, rv_input$time_name_3)
                                                )
                                        }
                                        # Case 4
                                        if(input$gr_bar_id_time_range[2] <= rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1)
                                                )
                                        }
                                        # Case 5
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_1 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2)
                                                )
                                        }
                                        # Case 6
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_3)
                                                )
                                        }
                                }
                                # If 4 subperiods, then there are 10 cases
                                if(rv_input$time_subper_num == 4){
                                        # Case 1
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] > rv_input$time_limit_3){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5, rv_input$time_limit_3+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5, rv_input$time_limit_3+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2, rv_input$time_name_3, rv_input$time_name_4)
                                                )
                                        }
                                        # Case 2
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_3 & input$gr_bar_id_time_range[2] > rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, rv_input$time_limit_2+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2, rv_input$time_name_3)
                                                )
                                        }
                                        # Case 3
                                        if(input$gr_bar_id_time_range[1] >= input$in_id_time_limit_1 & input$gr_bar_id_time_range[1] < rv_input$time_limit_2 & input$gr_bar_id_time_range[2] > rv_input$time_limit_3){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_2+0.5, rv_input$time_limit_3+0.5), 
                                                        Year_end=c(rv_input$time_limit_2+0.5, rv_input$time_limit_3+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2, rv_input$time_name_3, rv_input$time_name_4)
                                                )
                                        }
                                        # Case 4
                                        if(input$gr_bar_id_time_range[1] < rv_input$time_limit_1 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_2 & input$gr_bar_id_time_range[2] > rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_1+0.5), 
                                                        Year_end=c(rv_input$time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1, rv_input$time_name_2)
                                                )
                                        }
                                        # Case 5
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_1 & input$gr_bar_id_time_range[1] < rv_input$time_limit_2 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_3 & input$gr_bar_id_time_range[2] > rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_2+0.5), 
                                                        Year_end=c(rv_input$time_limit_2+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2, rv_input$time_name_3)
                                                )
                                        }
                                        # Case 6
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_2 & input$gr_bar_id_time_range[1] < rv_input$time_limit_3 & input$gr_bar_id_time_range[2] > rv_input$time_limit_3){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5, rv_input$time_limit_3+0.5), 
                                                        Year_end=c(rv_input$time_limit_3+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_3, rv_input$time_name_4)
                                                )
                                        }
                                        # Case 7
                                        if(input$gr_bar_id_time_range[2] <= rv_input$time_limit_1){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_1)
                                                )
                                        }
                                        # Case 8
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_1 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_2){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_2)
                                                )
                                        }
                                        # Case 9
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_2 & input$gr_bar_id_time_range[2] <= rv_input$time_limit_3){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_3)
                                                )
                                        }
                                        # Case 10
                                        if(input$gr_bar_id_time_range[1] >= rv_input$time_limit_3){
                                                rectangle_text <- data.frame(
                                                        Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                        Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                        Period=c(rv_input$time_name_4)
                                                )
                                        }
                                }
                                
                                vertical_lines <- rectangle_text$Year_end
                                vertical_lines <- vertical_lines[1:(length(vertical_lines)-1)]
                        }
                        
                        # Actually create plot
                        p <- ggplot(table, aes(x = Year, y = Value)) +
                                # Define type of graph: bar 
                                geom_bar(stat="identity", 
                                        fill=input$gr_bar_id_color,
                                        colour="black"
                                )+
                                # Include title, subtitle, source and Y-axis title
                                labs(title = title_text,
                                        subtitle = subtitle_text,
                                        caption = if(input$gr_bar_id_source){paste("Source: ", graph_source, ".", sep="")},
                                        y = if(input$gr_bar_id_yaxis){yaxis_units} else {NULL}
                                )+
                                # Aesthetics
                                theme(panel.background = element_blank(),
                                        panel.border = element_blank(),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        plot.title = element_text(size = 12, face = "bold"),
                                        plot.margin = margin(0.25, 0.25, 1, 0.25, "cm"),
                                        plot.caption = element_text(hjust = 0, size = 10),
                                        axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.text.x = element_text(colour = "black"),
                                        axis.text.y = element_text(colour = "black"),
                                        text = element_text(size=12,  family=input$gr_bar_id_font)
                                )+
                                # Define intervals of Year axis
                                scale_x_continuous(name="",
                                        breaks = seq(input$gr_bar_id_time_range[1],
                                                input$gr_bar_id_time_range[2],
                                                by = intervals)
                                )+
                                coord_cartesian(xlim = c(input$gr_bar_id_time_range[1], input$gr_bar_id_time_range[2]))
                        
                        
                        # Include data labels        
                        if(input$gr_bar_id_data_labels){p <- p + geom_text(
                                aes(
                                        x = Year,
                                        y = Value,
                                        label = format(round(as.numeric(Value), digits_num), 
                                                nsmall=digits_num, big.mark=",")),
                                vjust= ifelse(table$Value <0 , 1.5, -0.5),
                                hjust= 0.5,
                                size = 3,
                                family=input$gr_bar_id_font)
                        }
                        
                        # Increase range of Y axis to make room for the box indicating subperiod
                        y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                        y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                        y_range <- y_max - y_min
                        
                        # Include thousands separating comma in Y-axis
                        p <- p + scale_y_continuous(name=  yaxis_units,
                                labels = comma_format(accuracy = accuracy_yaxis, big.mark = ","),
                                breaks=pretty_breaks(),
                                limits = c(y_min, y_max+(y_range*0.1))
                        )
                        
                        # Get new axis limits that will be used as inputs to define the size of the subperiod rectangles
                        y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                        y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                        y_range_new <- y_max - y_min
                        
                        #Define other parameters that will be used as inputs to define the size of the subperiod rectangles
                        size_factor <- 0.08
                        ALPHA <- 0.15
                        
                        # Subperiod rectangles, their labels and the dotted lines separating 
                        if(rv_input$time_subper & input$gr_bar_id_time_subper){
                                p <- p + 
                                        geom_rect(data = rectangle_text,
                                                aes(NULL, NULL, xmin=-Inf, xmax=Inf),
                                                ymin = y_max_new - y_range_new*size_factor ,
                                                ymax = y_max_new,
                                                colour = NA,
                                                fill="grey",
                                                alpha = 0.5
                                        )+
                                        geom_label(data=rectangle_text,
                                                aes(x = Year_start+(Year_end-Year_start)/2, 
                                                        y = y_max_new*ALPHA + (y_max_new - y_range_new*size_factor)*(1-ALPHA), 
                                                        label = Period,
                                                        family = input$gr_bar_id_font
                                                ),
                                                size=3.3,
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
                                                        y = y_max_new - y_range_new*size_factor,
                                                        xend = i, 
                                                        yend = -Inf,
                                                        colour = "grey",
                                                        linetype = "dotted")
                                }
                        }
                        
                        # Period average lines
                        if(input$gr_bar_id_time_subper_avg){
                                if(rv_input$time_subper & input$gr_bar_id_time_subper){
                                        vec_average <- table %>%
                                                group_by(Period) %>%
                                                summarise_at(vars(Value),
                                                        list(Value_avg = mean),
                                                        na.rm=T)
                                        vec_average <- as.data.frame(vec_average)
                                        
                                        rectangle_text <- merge(x = rectangle_text, 
                                                y = as.data.frame(vec_average), 
                                                by = "Period")
                                        
                                        for(i in 1:nrow(rectangle_text)){
                                                yvalue <- vec_average %>% filter(Period == rectangle_text$Period[i]) %>% select(Value_avg)
                                                period_average <- yvalue[[1]]
                                                p <- p + geom_segment(x=rectangle_text$Year_start[i],
                                                        y = period_average,
                                                        xend=rectangle_text$Year_end[i],
                                                        yend=period_average)
                                        }
                                        p <- p + geom_label(
                                                data=rectangle_text,
                                                aes(x = Year_start+(Year_end-Year_start)/2,
                                                        y = (y_max_new - y_range_new*size_factor) - (y_max_new - (y_max_new*ALPHA + (y_max_new - y_range_new*size_factor)*(1-ALPHA))),
                                                        label = paste("Average: ", format(round(as.numeric(Value_avg), digits_num), nsmall=digits_num, big.mark=","), sep = ""),
                                                        family = input$gr_bar_id_font
                                                ),
                                                size=3.3,
                                                alpha = 0,
                                                label.size = NA,
                                                hjust = "center",
                                                vjust = "bottom")
                                        
                                }  else {
                                        vec_average <- table %>%
                                                summarise_at(vars(Value),
                                                        list(Value_avg = mean),
                                                        na.rm=T)
                                        yvalue <- vec_average %>% select(Value_avg)
                                        yvalue <- yvalue[[1]]
                                        p <- p + geom_segment(x=rectangle_text$Year_start[i],
                                                y = yvalue,
                                                xend=rectangle_text$Year_end[i],
                                                yend=yvalue)
                                }
                        }
                        
                        # Append plot to the list of plots
                        rv_plots$bar <- isolate(list.append(rv_plots$bar, p))
                        
                        # Show plot
                        renderPlot(p)
                
                }
        }
        
        # Call Prep data and Create plots 
        output$gr_bar_out_plots <- renderUI({
                
                rv_plots$bar <- list()
                
                pd <- req(prepped_data_bar())
                
                tagList(map(pd, ~ createUI_bar(.)))

        })
        
        # Plot download handlers ----
        
        # Download plots as png zipped - large
        output$gr_bar_download_large <- downloadHandler(
                filename = 'gr_bar_out_plots_large.zip',
                content = function( file){
                        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        
                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$bar)){
                                name <- paste("barplot_large", i, ".png", sep="")
                                ggsave(name, 
                                        plot = rv_plots$bar[[i]], 
                                        device = "png",
                                        width = 11.5, 
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("barplot_large", i, ".png", sep=""))
                                
                        }
                        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        
        
        # Download plots as png zipped - small
        output$gr_bar_download_small <- downloadHandler(
                filename = 'gr_bar_out_plots_small.zip',
                content = function( file){
                        
                        # Set temporary working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        
                        # Save the plots
                        vector_plots <- vector()
                        for (i in 1:length(rv_plots$bar)){
                                name <- paste("barplot_small", i, ".png", sep="")
                                
                                # Increase intervals in X axis in small plots
                                intervals <- ifelse(max(input$gr_bar_id_time_range)-min(input$gr_bar_id_time_range)<30, 2, 4)
                                rv_plots$bar[[i]] <- rv_plots$bar[[i]] + 
                                        scale_x_continuous(name="",
                                                breaks = seq(min(input$gr_bar_id_time_range),
                                                        max(input$gr_bar_id_time_range),
                                                        by = intervals))
                                
                                ggsave(name, 
                                        plot = rv_plots$bar[[i]], 
                                        device = "png",
                                        width = 5.75, 
                                        height = 5.75,
                                        units = "in")
                                vector_plots <- c(vector_plots, paste("barplot_small", i, ".png", sep=""))
                                
                        }
                        
                        # Zip them up
                        zip(file, vector_plots)
                }
        )
        

        ## Bar plots - Multiple country tab ----                
        
        
        ### Update inputs ----
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm), {
                
                # Country input
                aux_ctry <- unique(rv_df$dat_all %>% select(Country, Ctry_group, Ctry_group_num))
                aux_ctry$Ctry_slash_Group <- paste(aux_ctry$Country, aux_ctry$Ctry_group, sep=" | ")
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
                aux_var <- unique(paste(rv_df$dat_all$Var_name, rv_df$dat_all$Var_code, sep=" | "))
                
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
                        session= session,
                        inputId = "gr_mult_id_time_range",
                        min = rv_input$time_range_start,
                        max = rv_input$time_range_end,
                        step = 1,
                        value = c(rv_input$time_range_start, rv_input$time_range_end)
                )

        })
        
        ### Plots ----        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        ## Line plots ----                
        
        ### Update inputs ----
        
        observeEvent(c(input$in_id_update, input$in_id_reset_confirm) , {
               
                
                
                 
        })
        
        ### Plots ----   
        
        
}

shinyApp(ui = ui, server = server)