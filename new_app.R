# global.r ----

## Load libraries ----

library(shiny)
library(shinyjs)
library(dipsaus)
library(WDI)
library(dplyr)
library(RColorBrewer)
library(tools)
library(readxl)
library(haven)


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
        headers <- names(df) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Value", "Database")
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

# Downloads WDI data and creates "nice" dataframe
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
        digits = 1
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
                                                materialSwitch (
                                                        inputId = "in_id_ctries_all",
                                                        label = "Include all countries and regions",
                                                        value = def_list$ctries_all,
                                                        status = "primary",
                                                        right = TRUE),
                                                
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
                                                uiOutput("in_id_ext_mes_ui2"),
                                        
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
                                                        tags$br(),
                                                        
                                                ),
                                                
                                                # Output: Summary
                                                tabPanel("Summary",
                                                        tags$br(),
                                                
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
                                                                        `live-search` = TRUE)),
                                                        
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
                                                        ),
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
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = fix_list$digits
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
                                                uiOutput("gr_bar_out_query_result"),
                                                
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
        
        observe({
                selected_ctries <- c(input$in_id_ctries_ctry, 
                        input$in_id_ctries_str,
                        input$in_id_ctries_asp,
                        input$in_id_ctries_reg)
                if(is.null(selected_ctries) & input$in_id_ctries_all == FALSE){
                        onclick("in_id_ext",
                                showModal(modalDialog(
                                        title = "Warning",
                                        "Select Countries and Time period before loading external data.",
                                        easyClose = TRUE
                                ))
                        )
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
                dat_imf = empty_data_df,
                dat_ext = empty_data_df,
                dat_ext_last = empty_data_df,
                dat_all = empty_data_df,
                met_wdi = empty_metadata_df,
                met_imf = empty_metadata_df,
                met_ext = empty_metadata_df,
                met_all = empty_metadata_df
                
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
                                
                                shinyjs::hide("in_id_update")
                                
                }
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
                headers <- names(df_external) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Value", "Database")
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
                        print("Missing year or country parameters")
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
                
                # If no variable code, then concatenate variable name and dataset variable and use as var_code
                if(!"Var_code" %in% names(df_external)) {
                        df_external$Var_code <- paste(df_external$Var_name, df_external$Database, sep="_")
                } else {
                        df_external$Var_code[is.na(df_external$Var_code)] <- paste(df_external$Var_name, df_external$Database, sep="_")
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
                rv_df$dat_ext_last <- df_external
                
                # Return
                return(rv_df$dat_ext_last)

        })
        
        
        # Merge datasets
        observeEvent(input$in_id_update, {
                
                # Append last external dataset to all loaded external datasets
                rv_df$dat_ext <- rbind.fill(rv_df$dat_ext, rv_df$dat_ext_last)
                
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
                
                print(is.null(input$in_id_update))
                print(input$in_id_update)
                print(is.null(input$in_id_reset_confirm))
                print(input$in_id_update == 0)
                print(is.null(input$in_id_reset_confirm))
                print(input$in_id_reset_confirm == FALSE)
                print(length(input$in_id_reset_confirm))
                
                print("AAAAAAAAAAAAAAAAAAAAAAAAA")
                print(is.null(input$in_id_update) & length(input$in_id_reset_confirm) == 0)
                print(input$in_id_update == 0 & input$in_id_reset_confirm == FALSE)
                
                
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
                

        
}

shinyApp(ui = ui, server = server)