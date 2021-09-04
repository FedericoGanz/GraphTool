
# Shiny web application.
# It downloads data from WDI and generates plots in a standardized way.

# Load libraries ----

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(WDI)
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(panelr)
library(DT)
library(shinythemes)
library(ggthemes)
library(purrr)
library(scales)
library(rlist)
library(grid)
library(gridExtra)
library(devtools)
library(zip)
library(data.table)
library(shinycssloaders)
library(bsplus)
library(colourpicker)
library(RColorBrewer)

# Load calibri font
library(extrafont)
# font_import(pattern = "calibri")
# loadfonts(device = "win")

# Useful functions
my_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my_min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)


# Load necessary dataframes, vectors and lists ----

        # Load dataframes from WDI data to get variables and countries
        ctry_df <- as.data.frame(WDI_data[2][[1]])
        var_df <- as.data.frame(WDI_data[1][[1]])

        # Vectors with country and region names
        ctry_vec <- sort(ctry_df[ctry_df[ ,"region"] != "Aggregates", "country"])
        reg_vec <- sort(ctry_df[ctry_df[ ,"region"] == "Aggregates", "country"])
        
        # Vector with variable "name|indicator"
        var_df$name_indicator <- paste(var_df$name, var_df$indicator, sep=" | ")
        var_df <- var_df[order(var_df$name_indicator), ] 
        var_vec <- var_df$name_indicator
        
        # Vectors with max and min years that can be included in the data
        gb_in_time_slider_min <<- 1960
        gb_in_time_slider_max <<- 2030
        
        # Flags
        flags <- filter(ctry_df, !region %in% c("Aggregates")) %>% select(country, iso2c)
        flags$URL <- paste( "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", tolower(flags$iso2c), ".svg", sep="")
        flags$URL[flags$iso2c == "JG"] <- "https://upload.wikimedia.org/wikipedia/commons/f/f5/Flag_of_the_Channel_Islands_1.png"
        
        # Font list
        font_list <- list(Calibri = "Calibri", Arial = "sans", Courier = "mono", TimesNewRoman = "serif")
        
        # Color palettes
        x <- brewer.pal.info
        x <- filter(x, colorblind == TRUE)
        colorblind_palette <- row.names(x)
        
        # Positions list
        position_list <- list(Top = "top", Bottom = "bottom", Right = "right", Left = "left")

        # Empty dataframes
        empty_data <- data.frame(
                Var_name = character(0),
                Var_code = character(0),
                Units = character(0),
                Country = character(0),
                Ctry_iso = character(0),
                Ctry_group = character(0),
                Year = numeric(length = 0L),
                Period = character(0),
                Period_num = numeric(length = 0L),
                Value = numeric(length = 0L),
                Database = character(0),
                Ctry_group_num = numeric(length = 0L),
                Source = character(0))
        
        empty_metadata <- data.frame(
                Var_name = character(0),
                Var_code = character(0),
                Description = character(0),
                Units = character(0),
                Source_data = character(0),
                Source_org = character(0),
                Database = character(0),
                Source = character(0))
        
        empty_datatable <- subset(empty_data, select=-c(Var_code, Source))
        
        # Vector with list of statistics
        statistics <- c("Average", "Median", "Maximum", "Minimum", "Standard deviation", "Most recent value")

# Reset function: set global parameters to default values ----

reset <- function(){

        print("Setting global parameters to default values")

        # Inputs

                # Countries
                gb_in_countries_ctry <<- character(0)
                gb_in_countries_asp <<- character(0)
                gb_in_countries_str <<- character(0)
                gb_in_countries_reg <<- character(0)
                gb_in_countries_all <<- FALSE

                # Time
                gb_in_time_range_start <<- 2000         # Make sure it makes sense with time slider max and min
                gb_in_time_range_end <<- 2019           # Make sure it makes sense with time slider max and min
                gb_in_time_subper <<- FALSE             # Logical vector with one element defining whether subperiods will be included
                gb_in_time_subper_num <<- 2             # Select between 2, 3 or 4
                gb_in_time_limit_1 <<- 2008             # Make sure it makes sense with time_range parameters(start and end) and the other subperiod limits
                gb_in_time_limit_2 <<- 2014             # Make sure it makes sense with time_range parameters(start and end) and the other subperiod limits
                gb_in_time_limit_3 <<- 2018
                gb_in_time_name_1 <<- character(0)
                gb_in_time_name_2 <<- character(0)
                gb_in_time_name_3 <<- character(0)
                gb_in_time_name_4 <<- character(0)

                # Variables
                gb_in_wdi_vars <<- character(0)

                # Datasets
                gb_in_datasets <<- c("WDI")
                gb_in_remove_dup <<- TRUE
                gb_in_remove_na <<- FALSE

                # Show/hide tables
                gb_in_show_table <<- TRUE

        # Dataframes

                # Data
                gb_df_wdi <<- empty_data
                gb_df_external_last <<- empty_data %>% select()
                gb_df_external_all <<- empty_data
                gb_df_all_data_raw <<- empty_data
                gb_df_all_data <<- empty_datatable
                gb_df_metadata <<- empty_metadata

        # Counters

                # Plots
                gb_counter_bar <<- 0
                gb_counter_mult <<- 0
                gb_counter_line <<- 0

        # Auxiliary vectors
        gb_in_countries_selected <<- character(0)
        gb_in_countries_selected_iso2c <<- character(0)
        gb_in_wdi_vars_code <<- character(0)
        gb_in_time_range <<- c(gb_in_time_range_start, gb_in_time_range_end)
        
}

# Reset function: set global parameters to selected values ----        
        
selected_default <- function(){
        
        print("Setting global parameters to selected values")

        # Inputs
        
                # Countries    
                
                # Bhutan
                # gb_in_countries_ctry <<- c("Bhutan")                                            # Select only one country as analyzed country. For country names check ctry_vec.
                # gb_in_countries_asp <<- c("Botswana", "Namibia", "Panama", "Paraguay")          # Select structural countries. No limit. Check that there is no overlapping with analyzed country and aspirational countries. For country names check ctry_vec.
                # gb_in_countries_str <<- c("Benin", "Bolivia", "Eswatini", "Kyrgyz Republic")    # Select aspirational countries. No limit. Check that there is no overlapping with analyzed country and structural countries. For country names check ctry_vec.
                # Pakistan
                gb_in_countries_ctry <<- c("Pakistan")                                                            # Select only one country as analyzed country. For country names check ctry_vec.
                gb_in_countries_str <<- c("Bangladesh", "Egypt, Arab Rep.", "Ethiopia", "India")  # Select structural countries. No limit. Check that there is no overlapping with analyzed country and aspirational countries. For country names check ctry_vec.
                gb_in_countries_asp <<- c("Indonesia", "Mexico", "Turkey", "Vietnam")             # Select aspirational countries. No limit. Check that there is no overlapping with analyzed country and structural countries. For country names check ctry_vec.
                gb_in_countries_reg <<- c("South Asia")                                           # select regions to be included. No limit. For country names check reg_vec.
                gb_in_countries_all <<- FALSE                                                     #  Logical vector with one element defining whether all countries will be included

                # Time
                gb_in_time_range_start <<- 2000                                                 # Make sure it makes sense with time slider max and min
                gb_in_time_range_end <<- 2019                                                   # Make sure it makes sense with time slider max and min
                gb_in_time_subper <<- TRUE                                                      # Logical vector with one element defining whether subperiods will be included
                gb_in_time_subper_num <<- 3                                                     # Select between 2, 3 or 4
                gb_in_time_limit_1 <<- 2008                                                     # Make sure it makes sense with time_range parameters(start and end) and the other subperiod limits
                gb_in_time_limit_2 <<- 2014                                                     # Make sure it makes sense with time_range parameters(start and end) and the other subperiod limits
                gb_in_time_limit_3 <<- 2018                                                     # Make sure it makes sense with time_range parameters(start and end) and the other subperiod limits
                gb_in_time_name_1 <<- "Historical"
                gb_in_time_name_2 <<- "Recent past"
                gb_in_time_name_3 <<- "Present"
                gb_in_time_name_4 <<- "Period 4"

                # Variables                                                                     # Find out variable name and code of the variables you want to select (check var_df)
                gb_in_wdi_vars <<- c("GDP growth (annual %) | NY.GDP.MKTP.KD.ZG",
                        "Inflation, consumer prices (annual %) | FP.CPI.TOTL.ZG",
                        "GDP per capita, PPP (current international $) | NY.GDP.PCAP.PP.CD")
                
                # Aesthetic parameters
                gb_ae_colour_spinner <<- c("#009FDA")                                           # Select color. Check options at: https://www.color-hex.com/
                gb_ae_colour_graphs <<- c("#EC7063")                                            # Select color. Check options at: https://www.color-hex.com/
                gb_ae_colour_graphs_line <<- c("#FC4024")                                            # Select color. Check options at: https://www.color-hex.com/
                gb_ae_colour_palette_mult <<- c("Oranges")                                      # Select color. Check options at: https://www.color-hex.com/
                gb_ae_selected_font <<- c("Calibri")                                            # Select among 4 available fonts: (Calibri = "Calibri", Arial = "sans", Courier = "mono", TimesNewRoman = "serif"). Example: for TimesNewRoman type "serif" instead of "Calibri".

                # Calculate auxiliary vectors
                gb_in_countries_selected <<- c(gb_in_countries_ctry, gb_in_countries_asp, gb_in_countries_str, gb_in_countries_reg)
                gb_in_countries_selected_iso2c <<- ctry_df[ctry_df[ ,"country"] %in% gb_in_countries_selected, "iso2c"]
                gb_in_wdi_vars_code <<- filter(var_df, name_indicator %in% gb_in_wdi_vars) %>% pull(indicator)
                gb_in_time_range <<- c(gb_in_time_range_start, gb_in_time_range_end)                  
}

# Call parameters
reset()
selected_default()

# Initial country lists
init_available_str <- ctry_vec[!ctry_vec %in% c(gb_in_countries_ctry, gb_in_countries_asp)]
init_available_asp <- ctry_vec[!ctry_vec %in% c(gb_in_countries_ctry, gb_in_countries_str)]

# Help text for upload data
load_help_text <- bs_modal(
        id = "help_text",
        title = "Instructions to upload external data",
        body = includeHTML("help_upload_body.html"),
        size = "medium")

# Color palette info
load_palette <- bs_modal(
        id = "help_palette",
        title = "Color palettes",
        body = HTML('<img src="https://www.r-graph-gallery.com/38-rcolorbrewers-palettes_files/figure-html/thecode-1.png" width="500" hight="200" >'),
        size = "medium")


# Define UI ----
ui <- function(request){
        fluidPage(
                
                # Style, headers and other general parameters ----
                        
                        # Add modals
                        load_help_text,
                        load_palette,
                        
                        # Theme
                        theme = shinytheme("cerulean"),
                        
                        # Horizontal lines format
                        tags$head(tags$style(HTML("hr {border: 0.75px dotted #585858;}"))),
                        
                        # Header
                        titlePanel("Download & Plot Data Tool"),
                        
                        # Load useShinyjs 
                        shinyjs::useShinyjs(),
                
                # Navigation bar
                navbarPage(title = NULL,
                        
                        # UI.1. Data tab ----
                        tabPanel("Load Data",
                                
                                # Sidebar layout
                                sidebarLayout(
                                
                                        # UI.1.1. Sidebar: Inputs ----
                                        sidebarPanel(
                                        
                                                # UI.1.1.1. Step 1: Select countries ----
                                                h4("Step 1: Select countries"),
                                                
                                                        # Input: Country of analysis
                                                        pickerInput(
                                                                inputId = "in_id_countries_ctry",
                                                                label = "Select country of analysis",
                                                                choices = flags$country,
                                                                choicesOpt = list(content =  
                                                                                mapply(flags$country, flags$URL, FUN = function(country, flagUrl) {
                                                                                        HTML(paste(
                                                                                                tags$img(src = flagUrl, width = 20, height = 15),
                                                                                                country
                                                                                        ))
                                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                                        ),
                                                                selected = gb_in_countries_ctry,
                                                                options = list(size = 15,
                                                                        `live-search` = TRUE)),
                                                        
                                                        # Input: Comparators - Structural
                                                        selectizeInput(
                                                                inputId = "in_id_countries_str",
                                                                label = "Structural comparators",
                                                                choices = init_available_str,
                                                                multiple = TRUE,
                                                                selected = gb_in_countries_str),
                        
                                                        # Input: Comparators - Aspirational
                                                        selectizeInput(
                                                                inputId = "in_id_countries_asp",
                                                                label = "Aspirational comparators",
                                                                choices = init_available_asp,
                                                                multiple = TRUE,
                                                                selected = gb_in_countries_asp),
                                                        
                                                        # Input: Comparators - Regions
                                                        selectizeInput(
                                                                inputId = "in_id_countries_reg",
                                                                label = "Regions",
                                                                choices = reg_vec,
                                                                multiple = TRUE,
                                                                selected = gb_in_countries_reg),
                                                        
                                                        # Download all countries
                                                        materialSwitch (
                                                                inputId = "in_id_countries_all",
                                                                label = "Include all countries",
                                                                value = gb_in_countries_all,
                                                                status = "primary",
                                                                right = TRUE),
                                                        
                                                        # Update countries button
                                                        shinyjs::hidden(actionBttn(
                                                                inputId = "in_id_countries_update",
                                                                style = "fill",
                                                                label = "Apply changes: Countries",
                                                                color = "success",
                                                                size = "xs")),
                                                        hr(),
                                                
                                                # UI.1.1.2. Step 2: Select time period  ----
                                                h4("Step 2: Select time period"),
                                                
                                                        # Input: Slider for the year span
                                                        sliderInput(
                                                                inputId = "in_id_time_range",
                                                                label = "Select year range",
                                                                sep = "",
                                                                min = gb_in_time_slider_min,
                                                                max = gb_in_time_slider_max,
                                                                step = 1,
                                                                value = gb_in_time_range),
                                                        
                                                        # Input: Subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_range[1]-input.in_id_time_range[0]>=1",
                                                                materialSwitch (
                                                                        inputId = "in_id_time_subper",
                                                                        label = "Divide range in subperiods",
                                                                        value = gb_in_time_subper,
                                                                        status = "primary",
                                                                        right = TRUE),
        
                                                                conditionalPanel(
                                                                        condition = "input.in_id_time_subper == true && (input.in_id_time_range[1]-input.in_id_time_range[0])>=1",
                                                                
                                                                        # Input: Number of subperiods
                                                                        numericInput(
                                                                                inputId = "in_id_time_subper_num",
                                                                                label = "Number of subperiods (min=2; max=4)",
                                                                                min = 2,
                                                                                max = 4,
                                                                                value = gb_in_time_subper_num),
                                                                        
                                                                        textInput(
                                                                                inputId = "in_id_time_name_1",
                                                                                label="Period 1 name",
                                                                                value = gb_in_time_name_1),
                                                                        
                                                                        textInput(
                                                                                inputId = "in_id_time_name_2",
                                                                                label="Period 2 name",
                                                                                value = gb_in_time_name_2),
                                                                        
                                                                        sliderInput(
                                                                                inputId = "in_id_time_limit_1",
                                                                                label = "Select last year of Period 1",
                                                                                sep = "",
                                                                                min = gb_in_time_range_start,
                                                                                max = gb_in_time_range_end,
                                                                                step = 1,
                                                                                value = gb_in_time_limit_1),
                                                                        
                                                                        conditionalPanel(
                                                                                condition = "input.in_id_time_subper_num == 3 | input.in_id_time_subper_num == 4",
                                                                                uiOutput("in_id_time_name_3_ui"),
                                                                                uiOutput("in_id_time_limit_2_ui")
                                                                        ),
                                                                        
                                                                        conditionalPanel(
                                                                                condition = "input.in_id_time_subper_num == 4",
                                                                                uiOutput("in_id_time_name_4_ui"),
                                                                                uiOutput("in_id_time_limit_3_ui")
                                                                        )
                                                                )
                                                        ),
                
                                                        # Update time button
                                                        shinyjs::hidden(actionBttn(
                                                                inputId = "in_id_time_update",
                                                                style = "fill",
                                                                label = "Apply changes: Time",
                                                                color = "success",
                                                                size = "xs")),
                                                        hr(),
                                                
                                                # UI.1.1.3. Step 3: Select WDI variables (optional)  ----
                                                h4("Step 3: Select WDI variables (optional)"),
                                                
                                                        # Input: Variables
                                                        selectizeInput(
                                                                inputId = "in_id_wdi_vars",
                                                                label = "Select WDI variables (Name | Code)",
                                                                choices = NULL,
                                                                multiple = TRUE), 
        
                                                        # Update WDI variables button
                                                        shinyjs::hidden(actionBttn(
                                                                inputId = "in_id_vars_update",
                                                                style = "fill",
                                                                label = "Apply changes: WDI variables",
                                                                color = "success",
                                                                size = "xs")),
                                                        hr(),
                                                        
                                                # UI.1.1.4. Step 4: Upload data from external file (optional) ----
                                                h4("Step 4: Upload data from external file (optional)"),
                                                
                                                        # Messages
                                                        uiOutput("in_id_external_message_ui1"),
                                                        uiOutput('in_id_external_ui'),
                                                        uiOutput("in_id_external_message_ui2"),
                                                        uiOutput("in_id_external_message_ui3"),
                                                        
                                                        # Upload external file button
                                                        shinyjs::hidden(actionBttn(
                                                                inputId = "in_id_external_update",
                                                                style = "fill",
                                                                label = "Upload File",
                                                                color = "success",
                                                                size = "xs")),
                                                        hr(),
                                                
                                                # UI.1.1.5. Step 5: Select the data sources included in the final dataset ----
                                                h4("Step 5: Select the data sources included in the final dataset"),
                                                        
                                                        # Select datasets to include
                                                        checkboxGroupInput(
                                                                inputId = "in_id_datasets", 
                                                                label = "Select datasets to include",
                                                                choices = gb_in_datasets,
                                                                selected = gb_in_datasets),
                                                        
                                                        # Remove duplicated/NA rows
                                                        strong("Remove duplicated/NA rows"),
                                                        materialSwitch (
                                                                inputId = "in_id_remove_dup",
                                                                label = "Remove duplicated rows",
                                                                value = gb_in_remove_dup,
                                                                status = "primary",
                                                                right = TRUE),
                                                        materialSwitch (
                                                                inputId = "in_id_remove_na",
                                                                label = "Remove rows with Value NA",
                                                                value = gb_in_remove_na,
                                                                status = "primary",
                                                                right = TRUE),
                                                        
                                                        # Clear data
                                                        actionBttn(
                                                                inputId = "in_id_reset",
                                                                label = "Clear data",
                                                                style = "fill",
                                                                color = "danger",
                                                                size = "xs"
                                                                ),
                                                
                                                        # Update datasets
                                                        shinyjs::hidden(actionBttn(
                                                                inputId = "in_id_datasets_update",
                                                                style = "fill",
                                                                label = "Apply changes",
                                                                color = "success",
                                                                size = "xs")),
                                                        hr(),
                                                
                                                # UI.1.1.6. Step 6: Show/hide data and metadata tables  ----
                                                h4("Step 6: Show/hide data and metadata tables"),
                                                materialSwitch (
                                                        inputId = "in_id_show_table",
                                                        label = "Show tables",
                                                        value = gb_in_show_table,
                                                        status = "primary",
                                                        right = TRUE),
                                        ),
                                        
                                        # UI.1.2. Sidebar: Main panel ----
                                        mainPanel(
                                                
                                                tabsetPanel(type = "tabs",
                                                                
                                                        # UI.1.2.1. Table ----
                                                        tabPanel("Table",

                                                                # Show text if no data for query
                                                                tags$br(),
                                                                uiOutput("out_query_message_data"),
                                                                uiOutput("out_show_table_message_data"),
                                                                uiOutput("out_missing_vars_message_data"),
                                                                
                                                                # Table 1: data
                                                                dataTableOutput("out_data_table") %>% withSpinner(
                                                                        type = 3, 
                                                                        color = gb_ae_colour_spinner, 
                                                                        color.background = "white"),
                                                                
                                                                # Data download button
                                                                downloadButton("out_download_data", 
                                                                        "Download data")
                                                        ),
                                                        
                                                        # UI.1.2.2. Metadata ----
                                                        tabPanel("Metadata",
                                                                
                                                                # Show text if no data for query
                                                                tags$br(),
                                                                uiOutput("out_query_result_metadata"),
                                                                uiOutput("out_show_table_message_metadata"),
                                                                uiOutput("out_missing_vars_message_metadata"),
                                                                
                                                                # Table 2: metadata
                                                                dataTableOutput("out_metadata_table") %>% withSpinner(
                                                                        type = 3, 
                                                                        color = gb_ae_colour_spinner, 
                                                                        color.background = "white"),
                                                                
                                                                # Metadata download button
                                                                downloadButton("out_download_metadata", 
                                                                        "Download metadata")
                                                        ),
                                                        
                                                        # UI.1.2.3. Summary ----
                                                        tabPanel("Summary",
                                                                
                                                                # Show text if no data for query
                                                                tags$br(),
                                                                htmlOutput("out_summary")
                                                        )
                                                )
                                        )
                                )
                        ),
                                                        
                        # UI.2. Single Country Bar Plots tab ----
                        tabPanel("Single Country Bar Plots",
                                
                                # Sidebar layout
                                sidebarLayout(
                                        
                                        # UI.2.1. Sidebar: Inputs ----
                                        sidebarPanel(
                                                
                                                # UI.2.1.1. Plot parameters ----
                                                h4("Plot parameters"),
                                                
                                                        # UI.2.1.1.1. Select country of analysis
                                                        pickerInput(
                                                                inputId = "gr_bar_id_countries_ctry",
                                                                label = "Select country",
                                                                choices = flags$country,
                                                                choicesOpt = list(content =  
                                                                                mapply(flags$country, flags$URL, FUN = function(country, flagUrl) {
                                                                                        HTML(paste(
                                                                                                tags$img(src = flagUrl, width = 20, height = 15),
                                                                                                country
                                                                                        ))
                                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                                        ),
                                                                selected = gb_in_countries_ctry,
                                                                options = list(size = 15,
                                                                        `live-search` = TRUE)),
                                                        
                                                        # UI.2.1.1.2. Select variables
                                                        pickerInput(
                                                                inputId = "gr_bar_id_vars",
                                                                label = "Select variables", 
                                                                choices = c("updatedInServer"),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.1.3. Select range
                                                        sliderInput(
                                                                inputId = "gr_bar_id_time_range",
                                                                label = "Select range",
                                                                sep = "",
                                                                min = gb_in_time_range_start,
                                                                max = gb_in_time_range_end,
                                                                step = 1,
                                                                value = c(gb_in_time_range_start,gb_in_time_range_end)
                                                        ),
                                                        hr(),
                                                        
                                                # UI.2.1.2. Display options ----
                                                h4("Display options"),
                                                
                                                        # UI.2.1.2.1. Include title
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_title",
                                                                label = "Include title",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.2. Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_yaxis",
                                                                label = "Include Y-axis units",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.3. Include source
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_source",
                                                                label = "Include source",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.4. Include data labels
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_data_labels",
                                                                label = "Include data labels",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.5. Include subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_subper == true",
                                                                materialSwitch (
                                                                        inputId = "gr_bar_id_time_subper",
                                                                        label = "Include subperiods",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                        ),
                                                        
                                                        # UI.2.1.2.6. Include period/subperiod averages
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

                                                        
                                                        # UI.2.1.2.7. Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.8. Transform data to logs (applies only to positive series)
                                                        materialSwitch(
                                                                inputId = "gr_bar_id_transform_log",
                                                                label = "Transform data to logs (applies only to positive series)",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.9. Number of digits
                                                        numericInput(
                                                                inputId = "gr_bar_id_digits",
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = 1
                                                        ),
                                                        
                                                        # UI.2.1.2.10. Select color and transparency of bars
                                                        colourpicker::colourInput(
                                                                inputId = "gr_bar_id_color",
                                                                label = "Select color and transparency of bars",
                                                                value = gb_ae_colour_graphs,
                                                                allowTransparent = TRUE,
                                                                closeOnClick = TRUE
                                                        ),
                                                        
                                                        # UI.2.1.2.11. Select font
                                                        pickerInput(
                                                                inputId = "gr_bar_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = gb_ae_selected_font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                                
                                                        ),
                                                ),
                                                        
                                        # UI.2.2. Sidebar: Main panel ----
                                        mainPanel(
                                                
                                                # UI.2.2.1. Show error message if no data ----
                                                tags$br(),
                                                uiOutput("gr_bar_out_query_result"),
                                                
                                                # UI.2.2.2. Dynamic UI for the plots ----
                                                uiOutput("gr_bar_out_plots") %>% withSpinner(type = 3, 
                                                        color = gb_ae_colour_spinner, 
                                                        color.background = "white"),
                                                helpText("Select download option (zipped .png files)"),
                                                
                                                # UI.2.2.3. Download graphs options ----
                                                downloadButton("gr_bar_download_large", "Long Plots"),
                                                downloadButton("gr_bar_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br()
                                                
                                        )
                                )
                        ),
                                                
                        
                        # UI.3. Multiple Country Bar Plots ----
                        tabPanel("Multiple Country Bar Plots",
                                        
                                # Sidebar layout with input and output definitions
                                sidebarLayout(
                                                
                                        # UI.3.1. Sidebar: Inputs ----
                                        sidebarPanel(
                                                        
                                                # UI.3.1.1. Plot parameters ----
                                                h4("Plot parameters"),
                                                
                                                        # UI.3.1.1.1. Select country of analysis
                                                        pickerInput(
                                                                inputId = "gr_mult_id_countries",
                                                                label = "Select countries/regions", 
                                                                choices = c("updatedInServer"),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        uiOutput("gr_mult_message_select_ctries"),        
                                                
                                                        # UI.3.1.1.2.1. Select variables
                                                        pickerInput(
                                                                inputId = "gr_mult_id_vars",
                                                                label = "Select variables", 
                                                                choices = c("updatedInServer"),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                
                                                        # UI.3.1.1.2.2. Select statistic
                                                        pickerInput(
                                                                inputId = "gr_mult_id_stat",
                                                                label = "Select measure", 
                                                                choices = statistics,
                                                                multiple = FALSE
                                                        ),
                                                        
                                                        # UI.3.1.1.3. Select range
                                                        sliderInput(
                                                                inputId = "gr_mult_id_time_range",
                                                                label = "Select range",
                                                                sep = "",
                                                                min = gb_in_time_range_start,
                                                                max = gb_in_time_range_end,
                                                                step = 1,
                                                                value = c(gb_in_time_range_start, gb_in_time_range_end)
                                                        ),
                                                        hr(),
                                                        
                                                # UI.3.1.2. Display options ----
                                                h4("Display options"),
                                                
                                                        # UI.3.1.2.1. Include title
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_title",
                                                                label = "Include title",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.2. Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_yaxis",
                                                                label = "Include Y-axis units",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.3. Include source
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_source",
                                                                label = "Include source",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.4. Include data labels
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_data_labels",
                                                                label = "Include data labels",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.5. Include subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_subper == true",
                                                                materialSwitch (inputId = "gr_mult_id_time_subper",
                                                                        label = "Include subperiods",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE
                                                                ),
                                                        ),
                                                        
                                                        # UI.3.1.2.6. Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.7. Transform data to logs (applies only to positive series)
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_transform_log",
                                                                label = "Transform data to logs (applies only to positive series)",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.8. Change country names to short
                                                        materialSwitch(
                                                                inputId = "gr_mult_id_ctry_short",
                                                                label = "Short country names (ISO 3)",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.3.1.2.9. Number of digits
                                                        numericInput(
                                                                inputId = "gr_mult_id_digits",
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = 1
                                                        ),
                                                        
                                                        # UI.3.1.2.10. Select font
                                                        pickerInput(
                                                                inputId = "gr_mult_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = gb_ae_selected_font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                        ),
                                                
                                                        # UI.3.1.2.11. Select color palette
                                                        conditionalPanel(
                                                                condition = "input.gr_mult_id_time_subper == true",
                                                                uiOutput('gr_mult_id_color_ui')
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_mult_id_time_subper == false",
                                                                colourpicker::colourInput(
                                                                        inputId = "gr_mult_id_color",
                                                                        label = "Select color and transparency of bars",
                                                                        value = gb_ae_colour_graphs,
                                                                        allowTransparent = TRUE,
                                                                        closeOnClick = TRUE)
                                                        ),
                                                
                                                        # UI.3.1.2.12. Select legend position
                                                        pickerInput(
                                                                inputId = "gr_mult_id_legend_pos",
                                                                label = "Select legends position", 
                                                                choices = position_list,
                                                                selected = "top",
                                                                multiple = FALSE
                                                        ),

                                                ),
                                                
                                        # UI.3.2. Sidebar: Main panel ----
                                        mainPanel(
                                                        
                                                # UI.3.2.1. Show error message if no data ----
                                                tags$br(),
                                                uiOutput("gr_mult_out_query_result"),
                                                
                                                # UI.3.2.2. Dynamic UI for the plots ----
                                                uiOutput("gr_mult_out_plots") %>% withSpinner(type = 3, 
                                                        color = gb_ae_colour_spinner, 
                                                        color.background = "white"),
                                                helpText("Select download option (zipped .png files)"),
                                                
                                                # UI.3.2.3. Download graphs options ----
                                                downloadButton("gr_mult_download_large", "Long Plots"),
                                                downloadButton("gr_mult_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br()
                                        )
                                )
                        ),
                        
                        
                        
                        
                        # UI.4. Line plot tab ----
                        tabPanel("Line Plots",
                        
                                # Sidebar layout with input and output definitions
                                sidebarLayout(
                                        
                                        # UI.4.1. Sidebar: Inputs ----
                                        sidebarPanel(
                                               
                                                # UI.4.1.1. Plot parameters ----
                                                h4("Plot parameters"),
                                                
                                                        # UI.4.1.1.1. Select countries/regions
                                                        pickerInput(
                                                                inputId = "gr_line_id_countries",
                                                                label = "Select countries/regions", 
                                                                choices = c("updatedInServer"),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        uiOutput("gr_line_message_select_ctries"),
                                                        
                                                        # UI.4.1.1.2. Select variables
                                                        pickerInput(
                                                                inputId = "gr_line_id_vars",
                                                                label = "Select variables", 
                                                                choices = c("a", "b"),
                                                                options = list(size = 15,
                                                                        `actions-box` = TRUE),
                                                                multiple = TRUE
                                                        ),
                                                        
                                                        uiOutput("gr_line_message_select_vars"),
                                                        
                                                        # UI.4.1.1.3. Select range
                                                        sliderInput(
                                                                inputId = "gr_line_id_time_range",
                                                                label = "Select range",
                                                                sep = "",
                                                                min = gb_in_time_range_start,
                                                                max = gb_in_time_range_end,
                                                                step = 1,
                                                                value = c(gb_in_time_range_start,gb_in_time_range_end)
                                                        ),
                                                        
                                                        hr(),
                                                        
                                                # UI.4.1.2. Display options ----
                                                h4("Display options"),
                                                
                                                        # UI.4.1.2.1. Include title
                                                        materialSwitch(
                                                                inputId = "gr_line_id_title",
                                                                label = "Include title",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.2. Include Y-axis units
                                                        materialSwitch(
                                                                inputId = "gr_line_id_yaxis",
                                                                label = "Include Y-axis units",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.3. Include source
                                                        materialSwitch(
                                                                inputId = "gr_line_id_source",
                                                                label = "Include source",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.4. Include data labels
                                                        materialSwitch(
                                                                inputId = "gr_line_id_data_labels",
                                                                label = "Include data labels",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.5. Include subperiods
                                                        conditionalPanel(
                                                                condition = "input.in_id_time_subper == true",
                                                                materialSwitch (
                                                                        inputId = "gr_line_id_time_subper",
                                                                        label = "Include subperiods",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE)
                                                        ),
                                                        
                                                        # UI.4.1.2.6. Transform data to Trillion/Billion/Million/Thousands
                                                        materialSwitch(
                                                                inputId = "gr_line_id_transform_zeros",
                                                                label = "Transform data to Trillion/Billion/Million/Thousands",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.7. Transform data to logs (applies only to positive series)
                                                        materialSwitch(
                                                                inputId = "gr_line_id_transform_log",
                                                                label = "Transform data to logs (applies only to positive series)",
                                                                value = FALSE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                
                                                        # UI.4.1.2.8. Change country names to short
                                                        materialSwitch(
                                                                inputId = "gr_line_id_ctry_short",
                                                                label = "Short country names (ISO 3)",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE
                                                        ),
                                                        
                                                        # UI.4.1.2.9. Number of digits
                                                        numericInput(
                                                                inputId = "gr_line_id_digits",
                                                                label = "Number of digits",
                                                                min = 0,
                                                                value = 0
                                                        ),
                                                        
                                                        # UI.4.1.2.10. Select font
                                                        pickerInput(
                                                                inputId = "gr_line_id_font",
                                                                label = "Select font",
                                                                choices = font_list,
                                                                selected = gb_ae_selected_font,
                                                                choicesOpt = list(
                                                                        content = c("<div style='font-family: Calibri'>Calibri</div>", 
                                                                                "<div style='font-family: sans'>Arial</div>", 
                                                                                "<div style='font-family: mono'>Courier</div>", 
                                                                                "<div style='font-family: serif'>TimesNewRoman</div>"))
                                                        ),
                                                        
                                                        # UI.4.1.2.10. Select highlighted country/region and color
                                                        materialSwitch(
                                                                inputId = "gr_line_highlight",
                                                                label = "Highlight a country/region",
                                                                value = TRUE,
                                                                status = "primary",
                                                                right = TRUE),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_line_highlight == true",
                                                                
                                                                # Input: Country of analysis
                                                                selectInput(
                                                                        inputId = "gr_line_highlight_ctry",
                                                                        label = "Select highlighted country/region",
                                                                        choices = c("input.ctry",
                                                                                "input.structural",
                                                                                "input.aspirational",
                                                                                "input.regions"),
                                                                        selected = "input.ctry"),
                                                                
                                                                # Input: Line plot color
                                                                colourpicker::colourInput(
                                                                        inputId ="gr_line_highlight_color", 
                                                                        label = "Select colors of highlighted line", 
                                                                        value = gb_ae_colour_graphs_line,
                                                                        allowTransparent = TRUE,
                                                                        closeOnClick = TRUE),
                                                        ),
                                                        
                                                        conditionalPanel(
                                                                condition = "input.gr_line_highlight == false",
                                                                materialSwitch (
                                                                        inputId = "gr_line_highlight_legend",
                                                                        label = "Include legends",
                                                                        value = TRUE,
                                                                        status = "primary",
                                                                        right = TRUE),
                                                        )
                                                ),
                                                        
                                        # UI.4.2. Sidebar: Main panel ----
                                        mainPanel(
                                        
                                                # UI.4.2.1. Show text if no data for query ----
                                                tags$br(),
                                                uiOutput("gr_line_out_query_result"),
                                                
                                                # UI.4.2.2. Dynamic UI for the plots ----
                                                uiOutput("gr_out_line_plots") %>% withSpinner(type = 3,
                                                        color=gb_ae_colour_spinner,
                                                        color.background = "white"),
                                                helpText("Select download option (zipped .png files)"),
                                                
                                                # UI.4.2.3. Download graphs options ----
                                                downloadButton("gr_line_download_large", "Long Plots"),
                                                downloadButton("gr_line_download_small", "Small Plots"),
                                                tags$br(),
                                                tags$br()
                                        )
                                )
                        )
                ),
                                                
        )        
}

# Define server logic ----

server <- function(input, output, session){
        
        # SL.1. Data tab ----

                # SL.1.1. Inputs and data processing ----
                
                        ## SL.1.1.1. Countries choice: update countries available in menus (UI.1.1.1.) ====
                                
                                # Create reactive object to the primary countries inputs
                                fun_listen_countries <- reactive({
                                        no_changes <- as.logical(c(
                                                setequal(gb_in_countries_ctry, input$in_id_countries_ctry),
                                                setequal(gb_in_countries_str, input$in_id_countries_str),
                                                setequal(gb_in_countries_asp, input$in_id_countries_asp),
                                                setequal(gb_in_countries_reg, input$in_id_countries_reg),
                                                setequal(gb_in_countries_all, input$in_id_countries_all)
                                                ))
                                        return(all(no_changes))
                                })
                                
                                # Update structural countries input menu
                                observeEvent(fun_listen_countries(), {
                                        changes <- !fun_listen_countries()
                                        if(changes){
                                                print("Updating input menu: data/inputs/countries/structural")
                                                x <- ctry_vec[!ctry_vec %in% c(input$in_id_countries_ctry, input$in_id_countries_asp)]
                                                updateSelectizeInput(
                                                        inputId = 'in_id_countries_str',
                                                        choices = x,
                                                        selected = input$in_id_countries_str)
                                        }
                                }, ignoreInit = TRUE)
                                
                                # Update aspirational countries input menu
                                observeEvent(fun_listen_countries(), {
                                        changes <- !fun_listen_countries()
                                        if(changes){
                                                print("Updating input menu: data/inputs/countries/aspirational")
                                                x <- ctry_vec[!ctry_vec %in% c(input$in_id_countries_ctry , input$in_id_countries_str)]
                                                updateSelectizeInput(
                                                        inputId = 'in_id_countries_asp',
                                                        choices = x,
                                                        selected = input$in_id_countries_asp)
                                        }
                                }, ignoreInit = TRUE)
                                
                                # Updating global parameters for countries
                                observeEvent(fun_listen_countries(), {
                                        changes <- !fun_listen_countries()
                                        if(changes){
                                                print("Input parameters for Countries were modified...")
                                                print("Displaying 'Apply changes' button for countries...")
                                                shinyjs::show("in_id_countries_update")
                                        } else {
                                                shinyjs::hide("in_id_countries_update")
                                        }
                                }, ignoreInit = TRUE)
                                
                                observeEvent(input$in_id_countries_update, {
                                        gb_in_countries_ctry <<- input$in_id_countries_ctry
                                        gb_in_countries_str <<- input$in_id_countries_str
                                        gb_in_countries_asp <<- input$in_id_countries_asp
                                        gb_in_countries_all <<- input$in_id_countries_all
                                        print("Global parameters for Countries were modified")
                                        shinyjs::hide("in_id_countries_update")
                                        print("Hiding 'Apply changes' button for countries...")
                                }, ignoreInit = TRUE)
                                
                        ## SL.1.1.2. Update: Subperiods (UI.1.1.2.) ====
                                
                                # Update number of subperiods menu
                                # Updates only with changes in in_id_time_range
                                # The max number of subperiods allowed is min(4, num_years_included, in_id_time_subper_num)
                                
                                fun_listen_time_range <- reactive({
                                        no_changes <- as.logical(c(
                                                setequal(gb_in_time_range, input$in_id_time_range)
                                        ))
                                        return(all(no_changes))
                                        
                                })
                                
                                observeEvent(fun_listen_time_range(),{
                                        changes <- !fun_listen_time_range()
                                        if(changes){
                                                print("Updating input menu: data/inputs/range/in_id_time_subper_num")
                                                time_range <- input$in_id_time_range
                                                num_years_included <- time_range[2] - time_range[1] + 1
                                                updateNumericInput(
                                                        session = session,
                                                        inputId = "in_id_time_subper_num",
                                                        min = 2,
                                                        max = min(4, num_years_included),
                                                        value = min(num_years_included, input$in_id_time_subper_num))
                                        }
                                }, ignoreInit = TRUE)
                                
                                # Update limit between period 1 and 2
                                # Updates only with changes in in_id_time_range
                                # The max allowed value is the max of in_id_time_range, the min allowed value is the min of in_id_time_range
                                
                                observeEvent(fun_listen_time_range(),{
                                        changes <- !fun_listen_time_range()
                                        if(changes){
                                                print("Updating input menu: data/inputs/range/in_id_time_limit_1")
                                                time_range <- input$in_id_time_range
                                                in_id_time_limit_1_original <- input$in_id_time_limit_1
                                                in_id_time_limit_1_new <- if(in_id_time_limit_1_original >= time_range[1] & in_id_time_limit_1_original <= time_range[2]){
                                                        in_id_time_limit_1_original} else {
                                                                time_range[1] + 1}
                                                updateSliderInput(
                                                        session = session,
                                                        inputId = "in_id_time_limit_1",
                                                        min = time_range[1],
                                                        max = time_range[2],
                                                        step = 1,
                                                        value = in_id_time_limit_1_new)
                                        }
                                }, ignoreInit = TRUE)
                                
                                # Update limit between period 2 and 3
                                output$in_id_time_limit_2_ui <- renderUI({
                                        
                                        time_range <- gb_in_time_range

                                        if("in_id_time_subper_num" %in% names(input)){
                                                if (input$in_id_time_subper_num >= 3) {
        
                                                        if("in_id_time_limit_2" %in% names(input)){
                                                                in_id_time_limit_2_original <- input$in_id_time_limit_2
                                                        } else {
                                                                in_id_time_limit_2_original <- gb_in_time_limit_2
                                                        }
                                                        
                                                        if("in_id_time_limit_1" %in% names(input)){
                                                                in_id_time_limit_2_new <- if(in_id_time_limit_2_original >= input$in_id_time_limit_1 & in_id_time_limit_2_original <= time_range[2]){
                                                                        in_id_time_limit_2_original} else {
                                                                                print("Updating input menu: data/inputs/range/in_id_time_limit_2")
                                                                                input$in_id_time_limit_1 + 1}
                                                        } else {
                                                                print("Updating input menu: data/inputs/range/in_id_time_limit_2")
                                                                in_id_time_limit_2_new <- in_id_time_limit_2_original
                                                        }
                                                        
                                                        sliderInput(
                                                                inputId = "in_id_time_limit_2",
                                                                label = "Select last year of Period 2",
                                                                sep = "",
                                                                min = input$in_id_time_limit_1,
                                                                max = time_range[2],
                                                                step = 1,
                                                                value = in_id_time_limit_2_new)
                                                }
                                        }
                                })
                                
                                # Update period 3 text input
                                output$in_id_time_name_3_ui <- renderUI({
                                        if("in_id_time_subper_num" %in% names(input)){
                                                if(input$in_id_time_subper_num >= 3) {
                                                        textInput(inputId = "in_id_time_name_3",
                                                                label="Period 3 name",
                                                                value = gb_in_time_name_3)
                                                }
                                        }
                                })
                                
                                # Update limit between period 3 and 4
                                output$in_id_time_limit_3_ui <- renderUI({
                                        
                                        time_range <- gb_in_time_range
                                        
                                        if("in_id_time_subper_num" %in% names(input)){
                                                
                                                if(input$in_id_time_subper_num == 4) {
                                                        
                                                        if("in_id_time_limit_3" %in% names(input)){
                                                                in_id_time_limit_3_original <- input$in_id_time_limit_3
                                                        } else {
                                                                in_id_time_limit_3_original <- gb_in_time_limit_3
                                                        }
                                                        
                                                        if("in_id_time_limit_2" %in% names(input)) {
                                                                lower_limit <- input$in_id_time_limit_2
                                                                in_id_time_limit_3_new <- if(in_id_time_limit_3_original >= lower_limit & in_id_time_limit_3_original <= time_range[2]){
                                                                        in_id_time_limit_3_original} else {
                                                                                print("Updating input menu: data/inputs/range/in_id_time_limit_3")
                                                                                input$in_id_time_limit_2 + 1} 
                                                        } else {
                                                                lower_limit <- input$in_id_time_limit_1 + 1
                                                                in_id_time_limit_3_new <- if(in_id_time_limit_3_original >= lower_limit & in_id_time_limit_3_original <= time_range[2]){
                                                                        in_id_time_limit_3_original} else {
                                                                                print("Updating input menu: data/inputs/range/in_id_time_limit_3")
                                                                                input$in_id_time_limit_1 + 2
                                                                        }
                                                        }
                                                        
                                                        sliderInput(inputId = "in_id_time_limit_3",
                                                                label = "Select last year of Period 3",
                                                                sep = "",
                                                                min = lower_limit,
                                                                max = time_range[2],
                                                                step = 1,
                                                                value = in_id_time_limit_3_new)
                                                }
                                        }
                                })
                                
                                # Update period 4 text input
                                output$in_id_time_name_4_ui <- renderUI({
                                        if (input$in_id_time_subper_num == 4) {
                                                textInput(inputId = "in_id_time_name_4",
                                                        label="Period 4 name",
                                                        value = gb_in_time_name_4)
                                        }
                                })
                                
                                # Show/hide update time button
                                # Create reactive object to the primary time inputs
                                fun_listen_time <- reactive({
                                        # The logical vector must adapt to the number of subperiods to avoid errors
                                        if(input$in_id_time_subper == FALSE){                                        
                                                no_changes <- as.logical(c(
                                                        setequal(gb_in_time_range, input$in_id_time_range),
                                                        setequal(gb_in_time_subper, input$in_id_time_subper)
                                                        # setequal(gb_in_time_subper_num, input$in_id_time_subper_num)
                                                        # setequal(gb_in_time_name_1, input$in_id_time_name_1)
                                                        # setequal(gb_in_time_name_2, input$in_id_time_name_2)
                                                        # setequal(gb_in_time_name_3, input$in_id_time_name_3)
                                                        # setequal(gb_in_time_name_4, input$in_id_time_name_4)
                                                        # setequal(gb_in_time_limit_1, input$in_id_time_limit_1)
                                                        # setequal(gb_in_time_limit_2, input$in_id_time_limit_2)
                                                        # setequal(gb_in_time_limit_3, input$in_id_time_limit_3)
                                                ))
                                        }                                        
                                        if(input$in_id_time_subper == TRUE & input$in_id_time_subper_num == 2){                                        
                                                no_changes <- as.logical(c(
                                                setequal(gb_in_time_range, input$in_id_time_range), 
                                                setequal(gb_in_time_subper, input$in_id_time_subper),
                                                setequal(gb_in_time_subper_num, input$in_id_time_subper_num),
                                                setequal(gb_in_time_name_1, input$in_id_time_name_1),
                                                setequal(gb_in_time_name_2, input$in_id_time_name_2),
                                                # setequal(gb_in_time_name_3, input$in_id_time_name_3)
                                                # setequal(gb_in_time_name_4, input$in_id_time_name_4)
                                                setequal(gb_in_time_limit_1, input$in_id_time_limit_1)
                                                # setequal(gb_in_time_limit_2, input$in_id_time_limit_2)
                                                # setequal(gb_in_time_limit_3, input$in_id_time_limit_3)
                                                ))
                                        }
                                        if(input$in_id_time_subper == TRUE & input$in_id_time_subper_num == 3){
                                                no_changes <- as.logical(c(
                                                setequal(gb_in_time_range, input$in_id_time_range), 
                                                setequal(gb_in_time_subper, input$in_id_time_subper),
                                                setequal(gb_in_time_subper_num, input$in_id_time_subper_num),
                                                setequal(gb_in_time_name_1, input$in_id_time_name_1),
                                                setequal(gb_in_time_name_2, input$in_id_time_name_2),
                                                setequal(gb_in_time_name_3, input$in_id_time_name_3),
                                                # setequal(gb_in_time_name_4, input$in_id_time_name_4)
                                                setequal(gb_in_time_limit_1, input$in_id_time_limit_1),
                                                setequal(gb_in_time_limit_2, input$in_id_time_limit_2)
                                                # setequal(gb_in_time_limit_3, input$in_id_time_limit_3)
                                                ))
                                        }
                                        if(input$in_id_time_subper == TRUE & input$in_id_time_subper_num == 4){                                        
                                                no_changes <- as.logical(c(
                                                setequal(gb_in_time_range, input$in_id_time_range), 
                                                setequal(gb_in_time_subper, input$in_id_time_subper),
                                                setequal(gb_in_time_subper_num, input$in_id_time_subper_num),
                                                setequal(gb_in_time_name_1, input$in_id_time_name_1),
                                                setequal(gb_in_time_name_2, input$in_id_time_name_2),
                                                setequal(gb_in_time_name_3, input$in_id_time_name_3),
                                                setequal(gb_in_time_name_4, input$in_id_time_name_4),
                                                setequal(gb_in_time_limit_1, input$in_id_time_limit_1),
                                                setequal(gb_in_time_limit_2, input$in_id_time_limit_2),
                                                setequal(gb_in_time_limit_3, input$in_id_time_limit_3)
                                                ))}
                                        return(all(no_changes))
                                })
                                
                                observeEvent(fun_listen_time(), {
                                        changes <- !fun_listen_time()
                                        if(changes){
                                                shinyjs::show("in_id_time_update")
                                                } else {
                                                        shinyjs::hide("in_id_time_update")
                                                }
                                }, ignoreInit = TRUE)
                                
                                observeEvent(input$in_id_time_update, {
                                        gb_in_time_range <<- input$in_id_time_range
                                        gb_in_time_subper <<- input$in_id_time_subper
                                        gb_in_time_subper_num <<- as.numeric(input$in_id_time_subper_num)
                                        gb_in_time_name_1 <<- input$in_id_time_name_1
                                        gb_in_time_name_2 <<- input$in_id_time_name_2
                                        gb_in_time_name_3 <<- input$in_id_time_name_3
                                        gb_in_time_name_4 <<- input$in_id_time_name_4
                                        gb_in_time_limit_1 <<- as.numeric(input$in_id_time_limit_1)
                                        gb_in_time_limit_2 <<- as.numeric(input$in_id_time_limit_2)
                                        gb_in_time_limit_3 <<- as.numeric(input$in_id_time_limit_3)
                                        print("Global parameters for Time were modified")
                                        shinyjs::hide("in_id_time_update")
                                        print("Hiding 'Apply changes' button for time")
                                }, ignoreInit = TRUE)
                                 
                        ## SL.1.1.3. Variable choice: selectizeInput external server (UI.1.1.3.) ====
                                
                                # Update variables menu
                                updateSelectizeInput(
                                        session = session,
                                        inputId = 'in_id_wdi_vars',
                                        choices = var_vec,
                                        selected = gb_in_wdi_vars,
                                        server = TRUE)
                                
                                # Show/hide update time button
                                fun_listen_vars <- reactive({
                                        if(is.null(input$in_id_wdi_vars)){x <- character(0)} else {x <- input$in_id_wdi_vars}                               
                                        no_changes <- as.logical(c(
                                                setequal(gb_in_wdi_vars, x)
                                        ))

                                        return(all(no_changes))
                                })
                                
                                observeEvent(fun_listen_vars(), {
                                        changes <- !fun_listen_vars()
                                        if(changes){
                                                shinyjs::show("in_id_vars_update")
                                        } else {
                                                shinyjs::hide("in_id_vars_update")
                                        }
                                }, ignoreInit = TRUE)
                                
                                observeEvent(input$in_id_vars_update, {
                                        shinyjs::hide("in_id_vars_update")
                                }, ignoreInit = TRUE)
                                       
                        ## SL.1.1.4. File input (UI.1.1.4.) ====
                                
                                output$in_id_external_ui <- renderUI({
                                        fileInput('in_id_external', label = "Select file") %>%
                                                shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                                bs_attach_modal(id_modal = "help_text")
                                                )
                                })
                                
                                # Show/hide update external upload button
                                observeEvent(input$in_id_external, {
                                        shinyjs::show("in_id_external_update")
                                },ignoreInit = TRUE)
                                
                                observeEvent(input$in_id_external_update, {
                                        shinyjs::hide("in_id_external_update")
                                }, ignoreInit = TRUE)

                        ## SL.1.1.5. Load data from WDI ====
                        
                        # Update WDI search parameters: countries        
                        observeEvent(input$in_id_countries_update, {
                                
                                # Create single vector with all countries
                                gb_in_countries_selected <<- c(
                                        input$in_id_countries_ctry ,
                                        input$in_id_countries_str,
                                        input$in_id_countries_asp,
                                        input$in_id_countries_reg)
                                
                                # Create vector with selected countries iso2c
                                gb_in_countries_selected_iso2c <<- ctry_df[ctry_df[,"country"] %in% gb_in_countries_selected, "iso2c"]
                                
                                # Condition: if all countries selected, set all
                                if(input$in_id_countries_all){
                                        gb_in_countries_selected_iso2c <<- c("all")
                                        gb_in_countries_selected <<- c(ctry_vec, reg_vec)
                                }
                        }, ignoreInit = TRUE)
                                
                        # Update WDI search parameters: time
                        observeEvent(input$in_id_time_update, {
                                gb_in_time_range_start <<- as.numeric(min(input$in_id_time_range, isolate(input$in_id_time_range)))
                                gb_in_time_range_end <<- as.numeric(max(input$in_id_time_range, isolate(input$in_id_time_range)))
                        }, ignoreInit = TRUE)
                        
                        # Update WDI search parameters: variables
                        observeEvent(input$in_id_vars_update, {
                                gb_in_wdi_vars_code <<- filter(var_df, name_indicator %in% input$in_id_wdi_vars) %>% pull(indicator)
                        }, ignoreInit = TRUE)
                        
                        # Reactive to any change in the parameters
                        fun_listen_wdi_params <- reactive({
                                print("At least one WDI parameter has been introduced/modified... (SL.1.2.)")
                                list(
                                        input$in_id_countries_update,
                                        input$in_id_time_update,
                                        input$in_id_vars_update
                                )
                        })
                        
                        # Download WDI data
                        observeEvent(fun_listen_wdi_params(),{
                                print("Parameters for WDI data download currently loaded... (SL.1.2.)")
                                print("(1/3) Selected countries/regions:")
                                print(gb_in_countries_selected)
                                print("(2/3) Selected variables:")
                                print(gb_in_wdi_vars_code)
                                print("(3/3) Year range:")
                                print(gb_in_time_range)

                                # Condition: if selected countries or variables empty - stop
                                if(length(gb_in_countries_selected)==0 | length(gb_in_time_range)==0 | length(gb_in_wdi_vars_code)==0){
                                        print("A WDI download parameter (countries or variable) is missing: no WDI data download (gb_df_wdi empty)")
                                        return(gb_df_wdi <<- data.frame())
                                }
                                
                                # Data download
                                print("All required WDI parameters are loaded: beginning WDI data download... (SL.1.2.)")
                                raw_wdi <- WDI(
                                        country = gb_in_countries_selected_iso2c,
                                        indicator = gb_in_wdi_vars_code,
                                        start = gb_in_time_range_start,
                                        end = gb_in_time_range_end,
                                        extra = FALSE,
                                        cache = NULL,
                                        latest = NULL,
                                        language = "en")
                
                                # Variables downloaded/not downloaded
                                vars_downloaded <- gb_in_wdi_vars_code[gb_in_wdi_vars_code %in% names(raw_wdi)]                
                                vars_not_downloaded <- gb_in_wdi_vars_code[!gb_in_wdi_vars_code %in% names(raw_wdi)]
                
                                # Data processing
                                        # Return if there is no data for the selected var/years
                                        if(class(raw_wdi)=="NULL") {
                                                print("No data was downloaded for the selected var/years")
                                                return()}
                
                                        # To ensure we have the same number of rows per variable
                                        # Create a dataset with num_countries * num_years rows
                                        # that includes only variable "country" and year
                                        num_years <- gb_in_time_range_end - gb_in_time_range_start + 1
                                        gb_df_wdi <- rep(gb_in_countries_selected, each = num_years)
                                        gb_df_wdi <- data.frame(gb_df_wdi)
                                        names(gb_df_wdi)[names(gb_df_wdi) == "gb_df_wdi"] <- "country"
                                        gb_df_wdi$year <- gb_in_time_range_start:gb_in_time_range_end
                
                                        # Complete "empty" dataframe with downloaded data
                                        for (i in vars_downloaded){
                                                aux <- raw_wdi[ , c("country", "year", i)]
                                                aux <- aux %>% drop_na(all_of(i))
                                                gb_df_wdi <- merge(
                                                        x=gb_df_wdi,
                                                        y=aux,
                                                        by = c("country", "year"),
                                                        all.x = TRUE
                                                )
                                        }
                
                                        # Set data to long format
                                        gb_df_wdi <- gb_df_wdi %>% 
                                                gather(key = "Var_code", 
                                                        value = "Value",
                                                        all_of(vars_downloaded))
                
                                        # Include iso3c code
                                        gb_df_wdi <- merge(
                                                x = gb_df_wdi, 
                                                y = ctry_df[ , c("country", "iso3c")], 
                                                by = c("country")
                                        )
                                        
                                        # Include variable name
                                        gb_df_wdi <- merge(x = gb_df_wdi, 
                                                y = var_df[ ,c("name", "indicator")], 
                                                by.x = c("Var_code"),
                                                by.y = c("indicator"))
                                        
                                        # Include country group column 
                                        #(Analized/Structural/Aspirational)
                                        gb_df_wdi$Ctry_group <- ifelse(
                                                gb_df_wdi$country %in% gb_in_countries_ctry , "Analized", 
                                                        ifelse(gb_df_wdi$country %in% gb_in_countries_str, "Structural", 
                                                                ifelse(gb_df_wdi$country %in% gb_in_countries_asp, "Aspirational", 
                                                                        ifelse(gb_df_wdi$country %in% gb_in_countries_reg, "Region", 
                                                                                ifelse(gb_df_wdi$country %in% reg_vec,"Rest (region)" , "Rest")
                                                                        )
                                                                )
                                                        )
                                        )
                                        
                                        # Create variable used for sorting by country group, then it will be dropped
                                        gb_df_wdi$Ctry_group_num <- ifelse(
                                                gb_df_wdi$country %in% gb_in_countries_ctry , 1, 
                                                        ifelse(gb_df_wdi$country %in% gb_in_countries_str, 2, 
                                                                ifelse(gb_df_wdi$country %in% gb_in_countries_asp, 3,
                                                                        ifelse(gb_df_wdi$country %in% gb_in_countries_reg, 4,
                                                                                ifelse(gb_df_wdi$country %in% reg_vec, 6, 5)
                                                                        )
                                                                )
                                                        )
                                        )
                                        
                                        # Generate "Units" variable (whatever is 
                                        # between brackets in Name will be taken as units)
                                        gb_df_wdi <- gb_df_wdi %>% separate(
                                                name,  
                                                c(NA, "Units"), 
                                                sep = "([\\(\\)])", 
                                                remove = FALSE,
                                                extra = "drop"
                                                )
                                        gb_df_wdi$Units <- lapply(gb_df_wdi$Units , function(x) {
                                                if(!is.na(x)){gsub("%", "percent", x)}
                                        })
                                        capFirst <- function(s) {
                                                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
                                        }
                                        gb_df_wdi$Units <- capFirst(gb_df_wdi$Units)
                                        gb_df_wdi$Units[gb_df_wdi$Units == "NULL"] <- NA
                                        
                                        # Rename columns
                                        names(gb_df_wdi)[names(gb_df_wdi) == "name"] <- "Var_name"
                                        names(gb_df_wdi)[names(gb_df_wdi) == "iso3c"] <- "Ctry_iso"
                                        names(gb_df_wdi)[names(gb_df_wdi) == "country"] <- "Country"
                                        names(gb_df_wdi)[names(gb_df_wdi) == "year"] <- "Year"
                
                                        # Add variable indicating that data was downloaded from DWI
                                        gb_df_wdi$Database <- "WDI"
                                        gb_df_wdi$Source <- "WDI"
                                        
                                        # Add empty variable Period 
                                        gb_df_wdi$Period <- as.character(NA)
                                        
                                        # Order columns and sort 
                                        gb_df_wdi <- gb_df_wdi %>% select(
                                                "Var_name",
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
                                        
                                        gb_df_wdi <- gb_df_wdi[order(
                                                gb_df_wdi$Var_name,
                                                gb_df_wdi$Var_code,
                                                gb_df_wdi$Ctry_group_num,
                                                gb_df_wdi$Ctry_iso,
                                                gb_df_wdi$Year), ]
                                        
                                        # Store data in global
                                        gb_df_wdi <<- gb_df_wdi
                                        print("Download from WDI was successful: gb_df_wdi dataframe stored in Global Environment.")
                                        print("**************************************")
                        })
                        
                        ## SL.1.1.6. Load data from external csv file (UI.1.1.4.) ====
                        observeEvent(input$in_id_external,{
                                print("Checking parameters for external data upload... (SL.1.3.)")
                                if(length(gb_in_countries_selected) == 0 | length(gb_in_time_range) == 0){
                                        print("A parameter (countries or time period) is missing: no WDI data download (gb_df_wdi empty)")
                                        return()
                                }
                                
                                # Input
                                inFile <- input$in_id_external
                                if(is.null(inFile))
                                        return()
                                
                                # Read .csv
                                df_external <- read.csv(inFile$datapath, 
                                        header = TRUE)
                                
                                # Keep only the following variables
                                headers <- names(df_external) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Value", "Database")
                                df_external <- df_external[ , headers]
                                
                                # Store excluded variables
                                '%!in%' <- function(x,y)!('%in%'(x,y))
                                headers_excluded <- names(df_external) %!in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Value", "Database")
                                
                                # Check all necessary variables were included
                                if((!"Var_name" %in% names(df_external)) |
                                        (!"Units" %in% names(df_external)) |
                                        (!"Ctry_iso" %in% names(df_external)) |
                                        (!"Year" %in% names(df_external)) |
                                        (!"Value" %in% names(df_external))
                                ){return()}
                                
                                # If no source Dataset information, then add file name as dataset variable
                                if(!"Database" %in% names(df_external)) {
                                        df_external$Database <- inFile$name
                                } else {df_external$Database[is.na(df_external$Database)] <- inFile$name}
                                
                                # If no variable code, then concatenate variable name and dataset variable and use as var_code
                                if(!"Var_code" %in% names(df_external)) {
                                        df_external$Database <- paste(df_external$Var_name, df_external$Database, sep="_")
                                } else {
                                        df_external$Var_code[is.na(df_external$Var_code)] <- paste(df_external$Var_name, df_external$Database, sep="_")
                                }
                                
                                # Include country names
                                df_external <- merge(x = df_external,
                                        y = ctry_df[ , c("country", "iso3c")],
                                        by.x="Ctry_iso", 
                                        by.y="iso3c")
                                
                                # Rename column 
                                names(df_external)[names(df_external) == "country"] <- "Country"
                                
                                # Generate country group string variable and country group number variable to sort
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_ctry ] <- 1
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_ctry ] <- "Analized"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_str] <- 2
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_str] <- "Structural"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_asp] <- 3
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_asp] <- "Aspirational"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_reg] <- 4
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_reg] <- "Region"
                                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) & 
                                                !(df_external$Country %in% reg_vec)] <- 5
                                df_external$Ctry_group[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                !(df_external$Country %in% reg_vec)] <- "Rest"
                                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                (df_external$Country %in% reg_vec)] <- 6
                                df_external$Ctry_group[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                (df_external$Country %in% reg_vec)] <- "Rest (region)"
                                
                                df_external <- df_external[order(
                                        df_external$Var_name,
                                        df_external$Var_code,
                                        df_external$Ctry_group_num,
                                        df_external$Ctry_iso,
                                        df_external$Year), ]
                                
                                # To ensure we have the same number of rows per variable
                                # Create a dataset with num_countries * num_years rows
                                # that includes only variable "country" and year
                                        
                                        # Years
                                        years <- gb_in_time_range_start:gb_in_time_range_end
                                        num_years <- length(years)
                                        
                                        # Countries
                                        ctry_external <- data.frame(rep(gb_in_countries_selected, each = num_years))
                                        colnames(ctry_external) <- c("Country")
                                        ctry_external <- merge(x = ctry_external,
                                                y = ctry_df[ , c("country", "iso3c")],
                                                by.x="Country", 
                                                by.y="country")
                                        names(ctry_external)[names(ctry_external) == "iso3c"] <- "Ctry_iso"
                                        num_countries <- nrow(ctry_external)
                                        
                                        #Variables
                                        vars_external <- distinct(select(df_external, Var_code, Var_name, Units, Database))
                                        num_vars <- nrow(vars_external)
                                        
                                        # Repeat countries dataframe (Country and Ctry_iso) by number of variables
                                        aux_external_ctry <- rbind(ctry_external, ctry_external[rep(1:nrow(ctry_external), each=num_vars-1), ])
                                        
                                        # Repeat years vector by number of countries
                                        Year <- rep(years, length(gb_in_countries_selected))
                                        
                                        # Merge countries and year (aux_external_ctry)
                                        aux_external_ctry <- data.frame(aux_external_ctry, Year)
                                        
                                        # Repeat variables by countries*years (aux_external_vars)
                                        aux_external_vars <- rbind(vars_external, vars_external[rep(1:nrow(vars_external), num_countries-1), ])
                                        aux_external_vars <- aux_external_vars[order(
                                                aux_external_vars$Var_code,
                                                aux_external_vars$Var_name,
                                                aux_external_vars$Units,
                                                aux_external_vars$Database
                                                ), ]
                                        
                                        # Merge aux_external_ctry and aux_external_vars
                                        aux_external <- data.frame(aux_external_ctry, aux_external_vars, row.names = NULL)
                                        
                                        # Complete "empty" dataframe with downloaded data
                                        df_external <- merge(
                                                x = aux_external,
                                                y = df_external,
                                                by = c("Country", "Ctry_iso", "Year", "Var_name", "Var_code", "Units", "Database"),
                                                all.x = TRUE
                                        )
                                
                                # Generate country group string variable and country group number variable to sort
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_ctry ] <- 1
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_ctry ] <- "Analized"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_str] <- 2
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_str] <- "Structural"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_asp] <- 3
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_asp] <- "Aspirational"
                                df_external$Ctry_group_num[df_external$Country %in% input$in_id_countries_reg] <- 4
                                df_external$Ctry_group[df_external$Country %in% input$in_id_countries_reg] <- "Region"
                                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                        !(df_external$Country %in% input$in_id_countries_str) & 
                                        !(df_external$Country %in% input$in_id_countries_asp) &
                                        !(df_external$Country %in% input$in_id_countries_reg) &
                                        !(df_external$Country %in% reg_vec)] <- 5
                                df_external$Ctry_group[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                !(df_external$Country %in% reg_vec)] <- "Rest"
                                df_external$Ctry_group_num[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                (df_external$Country %in% reg_vec)] <- 6
                                df_external$Ctry_group[!(df_external$Country %in% input$in_id_countries_ctry) & 
                                                !(df_external$Country %in% input$in_id_countries_str) & 
                                                !(df_external$Country %in% input$in_id_countries_asp) &
                                                !(df_external$Country %in% input$in_id_countries_reg) &
                                                (df_external$Country %in% reg_vec)] <- "Rest (region)"

                                
                                # If no source Dataset information, then add file name as dataset variable
                                df_external$Database[is.na(df_external$Database)] <- inFile$name
                                
                                # Source
                                df_external$Source <- inFile$name
                                
                                # Make sure Year and Value variables are numeric
                                df_external <- transform(df_external, Year = as.numeric(Year), Value = as.numeric(Value))
                                
                                # Add empty variable Period 
                                df_external$Period <- as.character(NA)
                                
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
                                gb_df_external_last <<- df_external
                                print("External upload was successful: gb_df_external_last dataframe was stored in Global Environment.")
                                
                        })
                        
                        # Append last uploaded external file to list of files 
                        observeEvent(input$in_id_external_update, {
                                print("Merging gb_df_external_last to gb_df_external_all...")
                                gb_df_external_all <<- rbind.fill(gb_df_external_all, gb_df_external_last)
                                print("The gb_df_external_all dataframe was stored in Global Environment.")
                                print("**************************************")
                        })
                        
                        ## SL.1.1.7. Warning messages indicating problems with uploaded external files (UI.1.1.4.) ====
                        
                        # Message indicating that a file with the same name was previously uploaded
                        
                        to_listen_in_id_external_m1 <- reactive({
                                empty_countries <- as.logical(c(
                                        length(input$in_id_countries_ctry) == 0,
                                        length(input$in_id_countries_asp) == 0,
                                        length(input$in_id_countries_str) == 0,
                                        length(input$in_id_countries_reg) == 0
                                ))
                                return(all(empty_countries) & input$in_id_countries_all == FALSE)
                        })
                        
                        output$in_id_external_message_ui1 <- renderUI({
                                x <- to_listen_in_id_external_m1()
                                if(x){
                                        print("Printing warning message: 'Load Countries and Time period selection before loading external data.'... (UI.1.1.4.)")
                                        return(p("Load Countries and Time period selection before loading external data.", 
                                                        style = "color:red"))
                                }
                        })
                        
                        to_listen_in_id_external <- reactive({
                                list(input$in_id_external)
                        })
                        
                        output$in_id_external_message_ui2 <- renderUI({
                                to_listen_in_id_external()
                                x <- input$in_id_external$name
                                if(!empty(gb_df_external_last)){
                                        if(x %in% gb_in_datasets){
                                                print("Printing warning message: 'File uploaded. Warning: a file with the same name was previously uploaded'... (UI.1.1.4.)")
                                                return(p("File uploaded. Warning: a file with the same name was previously uploaded.", 
                                                        style = "color:red"))
                                        }
                                }
                        })
                        
                        # Message indicating missing variable in external file
                        output$in_id_external_message_ui3 <- renderUI({
                                
                                to_listen_in_id_external()
                                
                                # Input
                                inFile <- input$in_id_external
                                if(is.null(inFile))
                                        return(NULL)
                                
                                # Read .csv
                                df <- read.csv(inFile$datapath, 
                                        header = TRUE)
                                
                                # Check all necessary variables were included
                                if(("Var_name" %in% names(df)) &
                                                ("Units" %in% names(df)) &
                                                ("Ctry_iso" %in% names(df)) &
                                                ("Year" %in% names(df)) &
                                                ("Value" %in% names(df))
                                ){return()} else{
                                        '%!in%' <- function(x,y)!('%in%'(x,y))
                                        vec <- c("Var_name", "Units", "Ctry_iso", "Year", "Value")
                                        missing_variables <- vec[vec %!in% names(df)]
                                        text <- paste(missing_variables, collapse =", ")
                                        text <- paste("Error in file upload. The following required variables are missing from the last uploaded dataset: ", 
                                                text, ". The data has not been included.",
                                                collapse ="")
                                        print("Printing warning message: 'Error in file upload. The following required variables are missing...' (UI.1.1.4.)")
                                        return(p(text, style = "color:red"))
                                }
                        })
                        
                        ## SL.1.1.8. Update the loaded datasets input menu (UI.1.1.5.) ====
                        observeEvent(input$in_id_external_update,{
                                inFile <- input$in_id_external
                                if(is.null(inFile))
                                        return(NULL)
                                
                                # Read .csv
                                df <- read.csv(inFile$datapath, 
                                        header = TRUE)
                                
                                # Keep only the following variables
                                headers <- names(df) %in% c("Var_name", "Var_code", "Units", "Ctry_iso", "Year", "Value", "Database")
                                df <- df[ , headers]
                                
                                # Check all necessary variables were included
                                if((!"Var_name" %in% names(df)) |
                                                (!"Units" %in% names(df)) |
                                                (!"Ctry_iso" %in% names(df)) |
                                                (!"Year" %in% names(df)) |
                                                (!"Value" %in% names(df))
                                ) {return()}
                                
                                # Append
                                
                                print("Appending uploaded external file to datasets_loaded vector...")
                                
                                gb_in_datasets <<- append(gb_in_datasets, input$in_id_external$name)
                                
                                print("Updating input menu: data/inputs/datasets")
                                
                                updateCheckboxGroupInput(
                                        inputId = 'in_id_datasets',
                                        choices = gb_in_datasets,
                                        selected = input$in_id_datasets)
                        })

                        ## SL.1.1.9. Merge all data uploaded (WDI and external) into a single dataframe and filter applying country and range criteria ====
                                to_listen_data <- reactive({
                                        
                                        print("At least one input parameter that defines gb_df_all_data has been introduced/modified...")

                                        list(
                                                input$in_id_countries_update[1],
                                                input$in_id_time_update[1],
                                                input$in_id_vars_update[1],
                                                input$in_id_datasets,
                                                input$in_id_remove_dup,
                                                input$in_id_remove_na
                                        )
                                })
                        
                        # Reactive
                        observeEvent(to_listen_data(),{
                                
                                print("Merging gb_df_wdi and gb_df_external_all (they can be empty) into a single dataframe (gb_df_all_data)...")
                                
                                # Upload 
                                gb_df_all_data <<- rbind.fill(gb_df_wdi, gb_df_external_all)
                                
                                if(empty(gb_df_all_data)){return(print("Dataframe gb_df_all_data is empty"))}
                                
                                # Apply country and year filter
                                gb_df_all_data <<- gb_df_all_data %>% filter( 
                                        Year >= gb_in_time_range_start & Year <= gb_in_time_range_end &
                                        Country %in% gb_in_countries_selected &
                                        Source %in% input$in_id_datasets)
                                
                                # Order data
                                gb_df_all_data <<- gb_df_all_data[order(gb_df_all_data$Var_name,
                                        gb_df_all_data$Var_code,
                                        gb_df_all_data$Ctry_group_num,
                                        gb_df_all_data$Ctry_iso,
                                        gb_df_all_data$Year),]
                                
                                # Include year periods
                                if(gb_in_time_range[2]-gb_in_time_range[1]>=2){
                                        if(gb_in_time_subper){
                                                if(gb_in_time_subper_num == 2){
                                                        gb_df_all_data$Period[gb_df_all_data$Year <= gb_in_time_limit_1] <<- gb_in_time_name_1
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year <= gb_in_time_limit_1] <<- 1
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_1] <<- gb_in_time_name_2
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_1] <<- 2
                                                        
                                                }
                                                if(gb_in_time_subper_num == 3){
                                                        gb_df_all_data$Period[gb_df_all_data$Year <= gb_in_time_limit_1] <<- gb_in_time_name_1
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year <= gb_in_time_limit_1] <<- 1
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_1 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_2] <<- gb_in_time_name_2
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_1 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_2] <<- 2
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_2] <<- gb_in_time_name_3
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_2] <<- 3
                                                }
                                                if(gb_in_time_subper_num == 4){
                                                        gb_df_all_data$Period[gb_df_all_data$Year <= gb_in_time_limit_1] <<- gb_in_time_name_1
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year <= gb_in_time_limit_1] <<- 1
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_1 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_2] <<- gb_in_time_name_2
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_1 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_2] <<- 2
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_2 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_3] <<- gb_in_time_name_3
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_2 & 
                                                                        gb_df_all_data$Year <= gb_in_time_limit_3] <<- 3
                                                        gb_df_all_data$Period[gb_df_all_data$Year > gb_in_time_limit_3 ] <<- gb_in_time_name_4
                                                        gb_df_all_data$Period_num[gb_df_all_data$Year > gb_in_time_limit_3 ] <<- 4
                                                }
                                        }
                                }
                                
                                # Store all data before removing rows
                                gb_df_all_data_raw <<- gb_df_all_data
                                
                                # Eliminate variables Ctry_group_num and Source
                                gb_df_all_data <<- subset(gb_df_all_data, select=-c(Ctry_group_num, Period_num, Source))
                                
                                # Eliminate duplicated rows
                                if(input$in_id_remove_dup){
                                        gb_df_all_data <<- gb_df_all_data %>% distinct()
                                }
                                
                                # Eliminate rows with NA
                                if(input$in_id_remove_na){
                                        gb_df_all_data <<- gb_df_all_data[!is.na(gb_df_all_data$Value),]
                                }
                
                                # Store in global
                                gb_df_all_data <<- gb_df_all_data
                                print("Merging gb_df_wdi and gb_df_external_all was successful: gb_df_all_data dataframe stored in Global Environment")
                                print("**************************************")
                        })

                        ## SL.1.1.10. Clear all data uploaded and reset menus ====
                        
                        # Warning asking if you want to delete data
                        observeEvent(input$in_id_reset, {
                                print("Displaying clear data alert")
                                confirmSweetAlert(
                                        session = session,
                                        inputId = "in_id_reset_confirmation",
                                        type = "warning",
                                        title = "Delete all loaded data?",
                                        closeOnClickOutside = TRUE
                                )
                        })
                        
                        # Clear input data from external file uploader
                        observeEvent(input$in_id_reset_confirmation, {
                                if(input$in_id_reset_confirmation){
                                        print("Clearing: data/inputs/external")
                                        output$in_id_external_ui <- renderUI({
                                                fileInput('in_id_external', label = "Select file") %>%
                                                        shinyInput_label_embed(
                                                                shiny_iconlink() %>%
                                                                        bs_attach_modal(id_modal = "help_text")
                                                        )
                                        })
                                }
                        })
                        
                        observeEvent(input$in_id_reset_confirmation, {
                                if(input$in_id_reset_confirmation){
                                        
                                        reset()

                                        # Reset input menus
                                        print("Reseting input menus...")
                                        
                                        # Countries menus
                                        updatePickerInput(
                                                inputId = 'in_id_countries_ctry',
                                                label = "Select country of analysis",
                                                session = session,
                                                choices = flags$country,
                                                choicesOpt = list(content =  
                                                                mapply(flags$country, flags$URL, FUN = function(country, flagUrl) {
                                                                        HTML(paste(
                                                                                tags$img(src = flagUrl, width = 20, height = 15),
                                                                                country
                                                                        ))
                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                ),
                                                selected = character(0),
                                                options = list(size = 15,
                                                        `live-search` = TRUE)
                                        )
                                        
                                        x <- ctry_vec[!ctry_vec %in% c(gb_in_countries_ctry, gb_in_countries_asp)]
                                        updateSelectizeInput(
                                                inputId = 'in_id_countries_str',
                                                choices = x,
                                                selected = gb_in_countries_str)
                                        
                                        x <- ctry_vec[!ctry_vec %in% c(gb_in_countries_ctry, gb_in_countries_str)]
                                        updateSelectizeInput(
                                                inputId = 'in_id_countries_asp',
                                                choices = x,
                                                selected = gb_in_countries_asp)
                                        
                                        updateSelectizeInput(
                                                inputId = "in_id_countries_reg",
                                                choices = reg_vec,
                                                selected = character(0))
                                        
                                        # Time variables
                                        updateMaterialSwitch(
                                                inputId = "in_id_time_subper",
                                                value = gb_in_time_subper,
                                                session = session)
                                        
                                        updateNumericInput(
                                                inputId = "in_id_time_subper_num",
                                                label = "Number of subperiods (min=2; max=4)",
                                                min = 2,
                                                max = 4,
                                                value = gb_in_time_subper_num)

                                        # Variable menu
                                        updateSelectizeInput(
                                                session = session,
                                                inputId = 'in_id_wdi_vars',
                                                choices = var_vec,
                                                server = TRUE)
                                        
                                        # Datasets menu
                                        updateCheckboxGroupInput(
                                                inputId = 'in_id_datasets',
                                                choices = gb_in_datasets,
                                                selected = gb_in_datasets)
                                }
                        })
                        
                        observeEvent(input$in_id_reset_confirmation, {
                                hiding <- if(input$in_id_reset_confirmation){TRUE}
                                if(hiding){
                                        print("'Clear data' modifies input menus. That would trigger green 'Update' buttons. Hiding all input update buttons if changes in input menus are triggered by clear data")
                                        # Hide update buttons
                                        shinyjs::hide("in_id_countries_update")
                                        shinyjs::hide("in_id_time_update")
                                        shinyjs::hide("in_id_vars_update")
                                        shinyjs::hide("in_id_external_update")
                                        # Reset 'hiding' variable to FALSE
                                        hiding <- if(input$in_id_reset_confirmation){FALSE}
                                }
                        })
                        
                        
        # SL.1.2. Main panel (data presentation) ---- 

                ## SL.1.2.1. Present data table, download data handler and error messages ====
                        
                        # Load data function

                        get_data <- reactive({
                                to_listen_data()
                                input$in_id_reset_confirmation
                                return(gb_df_all_data)
                        })
                        
                        output$out_data_table <- renderDataTable({
                                x <- get_data()
                                if (empty(x)){
                                        print("Rendering table: empty gb_df_all_data, no table rendered")
                                        return()} else {if(input$in_id_show_table) {
                                                print("Rendering table: showing gb_df_all_data")
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
                                                }
                                }
                        })
                        
                        # Download data handler
                        output$out_download_data <- downloadHandler(
                                print("Creating downloadHandler for gb_df_all_data..."),
                                filename = function() {
                                        paste('data', '.csv', sep='')
                                },
                                content = function(file) {
                                        write.csv(gb_df_all_data, file, row.names = FALSE)
                                }
                        )
                        
                        # Error message if there is no data
                        output$out_query_message_data <- renderUI({
                                to_listen_data()
                                input$in_id_reset_confirmation

                                # No data downloaded
                                if(empty(gb_df_all_data)){
                                        print("Printing error message (empty gb_df_all_data): No data has been loaded.")
                                        return(p("No data has been loaded.",
                                                style = "color:red")
                                        )
                                }
                                
                                # Data is downloaded, but no values
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)) {
                                        print("Printing warning message: Data has been loaded, but the variable 'Values' is empty.")
                                        return(p("Data has been loaded, but the variable 'Values' is empty.", 
                                                style = "color:red")
                                        )
                                }
                        })
                        
                        # Message indicating variables not included in table because of no data
                        output$out_missing_vars_message_data <- renderUI({
                                to_listen_data()
                                if(!empty(gb_df_all_data)){
                                        vars_not_included <- gb_in_wdi_vars_code[!gb_in_wdi_vars_code 
                                                %in% gb_df_wdi$Var_code]
                                        
                                        # When some data was downloaded
                                        # No variable missing
                                        if(length(vars_not_included)==0){
                                                return()
                                                
                                                # Some variables are missing
                                        } else {
                                                print("Printing warning message: 'No data downloaded from WDI for the following variables:...'")
                                                text <- paste(vars_not_included, collapse ="; ")
                                                text <- paste("No data downloaded from WDI for the following variables: ", text, collapse ="")
                                        return(
                                                p(text, style = "color:red"))
                                        }
                                }
                        })
                        
                        # Message indicating that show table option is not selected (data table)
                        output$out_show_table_message_data <- renderUI({
                                if(!input$in_id_show_table){
                                        print("Printing warning message: 'Select Show Tables option to see Data Table'")
                                        text <- "Select Show Tables option to see Data Table"}
                        })
        
                # SL.1.2.2. Present metadata table, download metadata handler and error messages ====
                
                        # Load metatada
                        observeEvent(to_listen_data(),{
                                
                                print("Creating metadata dataframe (gb_df_metadata)...")
                
                                # No data downloaded
                                if(is.null(gb_df_all_data)){
                                        print("Dataframe gb_df_all_data is null: empty metadata.")
                                        return()
                                }
                                
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)){
                                        print("Dataframe gb_df_all_data has no Values: empty metadata.")
                                        return()
                                }
                                
                                # WDI Metadata 
                                if(!empty(gb_df_wdi)){
                                        
                                        print("Fetching metadata for WDI variables...")
                                        
                                        # Data is downloaded and there is at least one value
                                        metadata_table_wdi <- var_df %>% 
                                                filter(indicator %in% gb_df_wdi$Var_code)
                                        
                                        # Generate "Units" variable (whatever is 
                                        # between brackets in Name will be taken as units)
                                        metadata_table_wdi <- metadata_table_wdi %>% separate(name,  
                                                c(NA,"Units"), 
                                                sep = "([\\(\\)])", 
                                                remove=FALSE,
                                                extra="drop")
                                        metadata_table_wdi$Units <- lapply(metadata_table_wdi$Units , function(x) {
                                                if(!is.na(x)) {gsub("%", "percent", x)}
                                        })
                                        capFirst <- function(s) {
                                                paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
                                        }
                                        metadata_table_wdi$Units <- capFirst(metadata_table_wdi$Units)
                                        metadata_table_wdi$Units[metadata_table_wdi$Units=="NULL"] <- ""
                                        
                                        # Rename columns
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
                                        
                                        # Order columns and sort 
                                        metadata_table_wdi <- metadata_table_wdi %>% select("Var_name",
                                                "Var_code",
                                                "Description",
                                                "Units",
                                                "Source_data",
                                                "Source_org"
                                        )
                                        
                                        # Add variable indicating that data was downloaded from DWI
                                        metadata_table_wdi$Database <- "WDI"
                                        metadata_table_wdi$Source <- "WDI"                        
                                        
                                        # Metadata table WDI
                                        metadata_table_wdi <- metadata_table_wdi[order(metadata_table_wdi$Var_name),]
                                        gb_df_metadata <<- metadata_table_wdi
                                        print("Metadata for WDI variables was successfully uploaded to Global Environment.")
                                }
                                
                                # Uploaded files metadata
                                if(!empty(gb_df_external_all)){
                                        print("Fetching metadata for variables uploaded using external files...")
                                        headers <- names(gb_df_external_all) %in% c("Var_name", "Var_code", "Description", "Units", "Source_data", "Source_org", "Database", "Source")
                                        metadata_table_uploaded <- gb_df_external_all[ , headers]
                                        metadata_table_uploaded <- metadata_table_uploaded %>% distinct()
                                        gb_df_metadata <<- rbind.fill(gb_df_metadata, metadata_table_uploaded)
                                        print("Metadata for external variables was successfully uploaded to Global Environment.")
                                }
                                
                                # Eliminate duplicated rows
                                if(input$in_id_remove_dup){
                                        gb_df_metadata <<- gb_df_metadata %>% distinct()
                                }
                                
                                # Filter metadata and store in Global Environment
                                gb_df_metadata <<- gb_df_metadata %>% filter(
                                                Source %in% input$in_id_datasets)
                                
                                gb_df_metadata <<- subset(gb_df_metadata, 
                                        select=-c(Source))
                                
                                print("Metadata dataframe was successfully created.")
                                print("**************************************")
                        })
                        
                        # Present metadata
                        output$out_metadata_table <- renderDataTable({
                                to_listen_data()
                                if(input$in_id_show_table & !empty(gb_df_metadata)) {
                                        print("Rendering metadata table...")
                                        datatable(gb_df_metadata, rownames = FALSE, options = list(scrollX = T))
                                }
                        })
                        
                        # Download metadata handler
                        output$out_download_metadata <- downloadHandler(
                                print("Creating downloadHandler for metadata..."),
                                filename = function() {
                                        paste('metadata', '.csv', sep='')
                                },
                                content = function(file) {
                                        write.csv(gb_df_metadata, file, row.names = FALSE)
                                }
                        )        
                        
                        # Error message if there is no data
                        output$out_query_result_metadata <- renderUI({
                                to_listen_data()
                                
                                # No data downloaded from WDI
                                if(empty(gb_df_all_data)){
                                        print("Printing error message in Metadata tab (empty gb_df_all_data): No data has been loaded.")
                                        return(p("No data has been loaded.", 
                                                style = "color:red")
                                        )
                                }
                                
                                # Data is downloaded, but no values
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)) {
                                        print("Printing error message in Metadata tab: Data has been loaded, but the variable 'Values' is empty.")
                                        return(p("Data has been loaded, but the variable Values are all missing.", 
                                                style = "color:red")
                                        )
                                }
                        })
                        
                        # Message indicating variables not included in table because of no data (Metadata)
                        output$out_missing_vars_message_metadata <- renderUI({
                                to_listen_data()
                                vars_not_included <- gb_in_wdi_vars_code[!gb_in_wdi_vars_code 
                                        %in% gb_df_wdi$Var_code]
                                
                                # When some data was downloaded
                                # No variable missing
                                if(length(vars_not_included)==0){
                                        return()
                                        
                                        # Some variables are missing
                                } else {
                                        print("Printing error message in metadata tab: 'No data available in WDI for the selected countries/years for the'")
                                        text <- paste(vars_not_included, collapse =", ")
                                        text <- paste("No data available in WDI for the selected countries/years for the following: ", 
                                                text, 
                                                collapse ="")
                                return(
                                        p(text, style = "color:red"))
                                }
                        })
                        
                        # Message indicating that show table option is not selected (metadata table)
                        output$out_show_table_message_metadata <- renderUI({
                                if(!input$in_id_show_table){
                                        print("Printing warning message: 'Select Show Tables option to see Data Table'")
                                        text <- "Select Show Tables option to see Metadata Table"}
                        })
                
                # SL.1.2.3. Present summary ----
                output$out_summary <- renderUI({
                        print("Creating summary of uploaded data...")
                        to_listen_data()
                        if(empty(gb_df_all_data)){
                                print("Printing warning message: 'Select Show Tables option to see Data Table'")
                                return(HTML("<p style='color:red;'>No data has been loaded.</p>"))
                        } else {
                                text_num_rows <- nrow(gb_df_all_data)
                                text1 <- paste("Number of rows included in final data: ", 
                                        text_num_rows, 
                                        collapse ="")
                                
                                text_num_del_rows <- nrow(gb_df_all_data_raw) - nrow(gb_df_all_data)
                                if(nrow(gb_df_all_data_raw) - nrow(gb_df_all_data) == 0){text_num_del_rows<-"0"}
                                text2 <- paste("Number of rows deleted: ", 
                                        text_num_del_rows, 
                                        collapse ="")
                                
                                text_num_datasets <- length(input$in_id_datasets)
                                if(length(gb_in_datasets)==0){text_num_datasets<-"0"}
                                text3 <- paste("Number of datasets included in final data: ", 
                                        text_num_datasets, 
                                        collapse ="")
                                
                                text_num_variables <- length(unique(gb_df_all_data$Var_code))
                                if(length(unique(gb_df_all_data$Var_code))==0){text_num_variables<- "0"}
                                text4 <- paste("Number of variables included in final data: ", 
                                        text_num_variables, 
                                        collapse ="")
                                
                                text_num_countries <- if(input$in_id_countries_all) {
                                        length(ctry_vec)+length(input$in_id_countries_reg)} else{
                                        length(gb_in_countries_selected_iso2c)
                                        }
                                text5 <- paste("Number of countries/regions included in final data: ", 
                                        text_num_countries, 
                                        collapse ="")
                                
                                HTML(paste(text1, text2, text3, text4, text5, sep="<br/>"))
                        }
                })        
        

        # SL.2. Single Country Bar Plots tab ----

                # SL.2.1. Inputs and data processing ----
                        
                        ## SL.2.1.1. Error message if there is no data ====
                        output$gr_bar_out_query_result <- renderUI({
                                to_listen_data()
                                
                                # No data downloaded from WDI
                                if(empty(gb_df_all_data)){
                                        print("Printing warning message in Bar: 'No data has been loaded.'")
                                        return(p("No data has been loaded.", 
                                                style = "color:red")
                                        )
                                }
                                
                                # Data is downloaded, but no values
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)) {
                                        return(p("Data has been loaded, but the variable Values are all missing.", 
                                                style = "color:red")
                                        )
                                }
                        })
                
                        ## SL.2.1.2. Variable choice: update variable checkbox ====
                        observeEvent(to_listen_data(),{
                                x <- unique(paste(gb_df_all_data$Var_name, gb_df_all_data$Var_code, sep=" | "))
                                if(is.null(x)) {x <- character(0)}
                                updatePickerInput(
                                        inputId = 'gr_bar_id_vars',
                                        label = "Select variables",
                                        session = session,
                                        choices = x,
                                        selected = x,
                                        options = list(size = 15,
                                                `actions-box` = TRUE)
                                )
                        })
                        
                        
                        ## SL.2.1.3. Message when there are no variables uploaded ====
                        output$message_select_vars <- renderUI(
                                if(is.null(gb_df_all_data$Var_name)){helpText("Upload data using Load Data tab")}
                        )
                        
                        
                        ## SL.2.1.4. Country choice: update selectInput ====
                        observeEvent(to_listen_data(),{
                                
                                to_listen_data()

                                x <- unique(gb_df_all_data$Country)
                                x <- x[!x %in% reg_vec]
                                
                                y <- filter(gb_df_all_data, Ctry_group %in% c("Analized")) %>% pull(Country)
                                y <- unique(y)
                                
                                
                                # Include group in flags
                                
                                flags_bar <- filter(flags, country %in% x)
                                flags_bar <- flags_bar[match(x, flags_bar$country),]
                                
                                flags_bar$Ctry_group <- ifelse(
                                        flags_bar$country %in% gb_in_countries_ctry , "Analized", 
                                        ifelse(flags_bar$country %in% gb_in_countries_str, "Structural", 
                                                ifelse(flags_bar$country %in% gb_in_countries_asp, "Aspirational", 
                                                        ifelse(flags_bar$country %in% gb_in_countries_reg, "Region",
                                                                ifelse(flags_bar$country %in% reg_vec, "Rest (region)", "Rest" )
                                                        )
                                                )
                                        )
                                )
                                
                                flags_bar$Ctry_Ctry_group <- paste(flags_bar$country, flags_bar$Ctry_group, sep=" | ")
                                
                                updatePickerInput(
                                                inputId = "gr_bar_id_countries_ctry",
                                                label = "Select country of analysis",
                                                session = session,
                                                choices = flags_bar$country,
                                                choicesOpt = list(content =  
                                                                mapply(flags_bar$Ctry_Ctry_group, flags_bar$URL, FUN = function(country, flagUrl) {
                                                                        HTML(paste(
                                                                                tags$img(src = flagUrl, width = 20, height = 15),
                                                                                country
                                                                        ))
                                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                        ),
                                                selected = y,
                                                options = list(size = 15,
                                                        `live-search` = TRUE)
                                )
                        })
                        
                        ## SL.2.1.5. Year choice: update selectInput ====
                        observeEvent(input$in_id_time_update,{
                                updateSliderInput(
                                        session= session,
                                        inputId = "gr_bar_id_time_range",
                                        min = gb_in_time_range_start,
                                        max = gb_in_time_range_end,
                                        step = 1,
                                        value = gb_in_time_range)}
                                )
                        
                        ## SL.2.1.6. Subperiod averages: update slider ====
                        observeEvent(input$gr_bar_id_time_subper,{
                                updateMaterialSwitch(
                                        session= session,
                                        inputId="gr_bar_id_time_subper_avg",
                                        value=FALSE)
                                }
                        )
                
                # SL.2.2. Create plots ----

                        ## SL.2.2.1. Prep data ====
                        prepped_data_bar <- reactive({
                                to_listen_data()
                                if(!is.null(input$gr_bar_id_vars)){
                                        datfra <- data.frame(input$gr_bar_id_vars)
                                        datfra <- separate(data = datfra, 
                                                col = input.gr_bar_id_vars,
                                                into = c("A", "Var_code"), 
                                                sep  =  " \\| ",
                                                extra = "merge")
                                        gr_bar_id_vars_var <- sort(datfra[,2])
                                        gb_df_all_data %>%
                                                as_tibble() %>%
                                                filter(Country == input$gr_bar_id_countries_ctry & 
                                                                Var_code %in% gr_bar_id_vars_var &
                                                                Year >= min(input$gr_bar_id_time_range) &
                                                                Year <= max(input$gr_bar_id_time_range)) %>%
                                                select(Var_name, Year, Value, Country, Units, Database, Period) %>%
                                                group_split(Var_name)
                                }
                        })
                        
                        
                        ## SL.2.2.2. Create plots ====
                        createUI_bar <- function(table) {
                                
                                # Parameters for plot
                                        # Input for number of digits to be used in y-axis and data labels
                                        digits_num <- input$gr_bar_id_digits
                                        # Input for trillions/billions/millions/thousands transformation
                                        accuracy_yaxis <- 1/10^(digits_num)
                                        units_zeros <- c("Trillions", "Billions", "Millions", "Thousands")
                                        # Input for title, subtitle Y-axis and source
                                        title_text <- ""
                                        title_text <- if(input$gr_bar_id_title){
                                                paste0(unique(table$Var_name), " - ", unique(table$Country))}
                                        subtitle_text <- ""
                                        yaxis_units <- if(input$gr_bar_id_yaxis){
                                                unique(table$Units)} else {NULL}
                                        graph_source <- c("")
                                        if(input$gr_bar_id_source){
                                                graph_source <- unique(table$Database)}
                                        if(graph_source=="WDI"){graph_source<- "World Development Indicators"}
                                        # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years 
                                        intervals <- ifelse(input$gr_bar_id_time_range[2] - input$gr_bar_id_time_range[1] < 30, 1, 2)
                                
                                # Plot counter and plot names
                                gb_counter_bar <<- gb_counter_bar + 1
                                plot_name <- paste0("plot_bar_", as.character(gb_counter_bar))
                                
                                
                                # Transform data: divide by trillions/billions/millions/thousands
                                for (i in 4:1){
                                        if(!all(is.na(table$Value))){
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
                                }
                                
                                # Log transform
                                if(!all(is.na(table$Value))){
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
                                }
                                
                                
                                # Warning sign if no data for this variable
                                if(all(is.na(table$Value))) {subtitle_text <- "No data available"}
                                
                                # Create subperiod variables to include rectangles
                                if(gb_in_time_subper & input$gr_bar_id_time_subper){
                                        
                                        # If 2 subperiods, then there are 3 cases
                                        if(gb_in_time_subper_num == 2){

                                                # Case 1 
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] > gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 2 
                                                if(input$gr_bar_id_time_range[2] <= gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                        }
                                        # If 3 subperiods, then there are 6 cases
                                        if(gb_in_time_subper_num == 3){

                                                # Case 1
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] > gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 2
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_2 & input$gr_bar_id_time_range[2] > gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_1 & input$gr_bar_id_time_range[1] < gb_in_time_limit_2 & input$gr_bar_id_time_range[2] > gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 4
                                                if(input$gr_bar_id_time_range[2] <= gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 5
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_1 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 6
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3)
                                                        )
                                                }
                                        }
                                        # If 4 subperiods, then there are 10 cases
                                        if(gb_in_time_subper_num == 4){
                                                # Case 1
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] > gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 2
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_3 & input$gr_bar_id_time_range[2] > gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5,input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_bar_id_time_range[1] >= input$in_id_time_limit_1 & input$gr_bar_id_time_range[1] < gb_in_time_limit_2 & input$gr_bar_id_time_range[2] > gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 4
                                                if(input$gr_bar_id_time_range[1] < gb_in_time_limit_1 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_2 & input$gr_bar_id_time_range[2] > gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 5
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_1 & input$gr_bar_id_time_range[1] < gb_in_time_limit_2 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_3 & input$gr_bar_id_time_range[2] > gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 6
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_2 & input$gr_bar_id_time_range[1] < gb_in_time_limit_3 & input$gr_bar_id_time_range[2] > gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_3+0.5, input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 7
                                                if(input$gr_bar_id_time_range[2] <= gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 8
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_1 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 9
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_2 & input$gr_bar_id_time_range[2] <= gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 10
                                                if(input$gr_bar_id_time_range[1] >= gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_bar_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_bar_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_4)
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
                                                breaks = seq(min(input$gr_bar_id_time_range),
                                                        max(input$gr_bar_id_time_range),
                                                        by = intervals)
                                                )+
                                        coord_cartesian(xlim = c(min(input$gr_bar_id_time_range), max(input$gr_bar_id_time_range)))
                                
                                        
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
                                        ALPHA<- 0.15
                                        
                                        # Subperiod rectangles, their labels and the dotted lines separating 
                                        if(gb_in_time_subper & input$gr_bar_id_time_subper & !all(is.na(table$Value))){
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
                                                        p <- p + geom_segment(x=i, 
                                                                y=y_max_new,
                                                                xend=i, 
                                                                yend=y_max_new - y_range_new*size_factor,
                                                                colour = "white",
                                                                size=1,
                                                                alpha=1) +
                                                                geom_segment(x=i, 
                                                                        y=y_max_new - y_range_new*size_factor,
                                                                        xend=i, 
                                                                        yend=-Inf,
                                                                        colour = "grey",
                                                                        linetype = "dotted")
                                                }
                                        }
                
                                        # Period average lines
                                        if(input$gr_bar_id_time_subper_avg & !all(is.na(table$Value))){
                                                if(gb_in_time_subper & input$gr_bar_id_time_subper){
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
                                myplots_bar <<- list.append(myplots_bar, p) 
                                names(myplots_bar)[gb_counter_bar] <<- plot_name
                                
                                # Show plot
                                renderPlot(p)
                        }
                
                        ## SL.2.2.3. Call Prep data and Create plots ====
                        output$gr_bar_out_plots <- renderUI({
                                myplots_bar <<- list()
                                gb_counter_bar <<- 0
                                pd <- req(prepped_data_bar())
                                tagList(map(
                                        pd,
                                        ~ createUI_bar(.)
                                        )
                                )
                        })
                
                # SL.2.3. Plot download handlers ----
                
                ## SL.2.3.1.Download plots as png zipped - large ====
                output$gr_bar_download_large <- downloadHandler(
                        filename = 'gr_bar_out_plots_large.zip',
                        content = function( file){
                                
                                # Set temporary working directory
                                owd <- setwd( tempdir())
                                on.exit( setwd( owd))
                                
                                # Save the plots
                                vector_plots <- vector()
                                for (i in 1:length(myplots_bar)){
                                        name <- paste("barplot_large", i, ".png", sep="")
                                        ggsave(name, 
                                                plot = myplots_bar[[i]], 
                                                device = "png",
                                                width = 11.5, 
                                                height = 5.75,
                                                units = "in")
                                        vector_plots <- c(vector_plots, paste("barplot_large", i, ".png", sep=""))
        
                                }
        
                                # Zip them up
                                zip( file, vector_plots)
                        }
                )
                
                
                ## SL.2.3.2. Download plots as png zipped - small ====
                output$gr_bar_download_small <- downloadHandler(
                        filename = 'gr_bar_out_plots_small.zip',
                        content = function( file){
                                
                                # Set temporary working directory
                                owd <- setwd( tempdir())
                                on.exit( setwd( owd))
                                
                                # Save the plots
                                vector_plots <- vector()
                                for (i in 1:length(myplots_bar)){
                                        name <- paste("barplot_small", i, ".png", sep="")
                                        
                                        # Increase intervals in X axis in small plots
                                        intervals <- ifelse(max(input$gr_bar_id_time_range)-min(input$gr_bar_id_time_range)<30, 2, 4)
                                        myplots_bar[[i]] <- myplots_bar[[i]] + 
                                                scale_x_continuous(name="",
                                                        breaks = seq(min(input$gr_bar_id_time_range),
                                                                max(input$gr_bar_id_time_range),
                                                                by = intervals))
                                        
                                        ggsave(name, 
                                                plot = myplots_bar[[i]], 
                                                device = "png",
                                                width = 5.75, 
                                                height = 5.75,
                                                units = "in")
                                        vector_plots <- c(vector_plots, paste("barplot_small", i, ".png", sep=""))
                                        
                                }
                                
                                # Zip them up
                                zip( file, vector_plots)
                        }
                )
        
        # SL.3. Multiple Countries Bar Plot tab ----
        
                # SL.3.1. Inputs and data processing ---- 
        
                        ## SL.3.1.1. Countries choice: update countries checkbox ====       
                        observeEvent(to_listen_data(),{
                                to_listen_data()
                                z <- unique(paste(gb_df_all_data$Country, gb_df_all_data$Ctry_group, sep=" | "))
                                datfra <- data.frame(z)
                                datfra <- separate(data = datfra,
                                        col = z,
                                        into = c("A", "B"),
                                        sep  =  " \\| ",
                                        extra = "merge")
                                
                                ctry <- datfra$A
                                group <- datfra$B
                                ctry_list <- as.list(ctry)
                                names(ctry_list) <- z
                                y <- c(gb_in_countries_ctry, gb_in_countries_asp, gb_in_countries_str, gb_in_countries_reg)
                                
                                if(is.null(x)) {x <- character(0)}
                                updatePickerInput(
                                        inputId = 'gr_mult_id_countries',
                                        session = session,
                                        choices = ctry_list,
                                        selected = y,
                                        options = list(size = 15,
                                                `actions-box` = TRUE))

                        })

                        output$gr_mult_message_select_ctries <- renderUI(
                                if(is.null(gb_df_all_data$Country)){helpText("Upload data using Load Data tab")}
                        )                        
                        
                        ## SL.3.1.2. Variable choice: update variable checkbox ====
                        observeEvent(to_listen_data(),{
                                x <- unique(paste(gb_df_all_data$Var_name, gb_df_all_data$Var_code, sep=" | "))
                                if(is.null(x)) {x <- character(0)}
                                updatePickerInput(
                                        inputId = 'gr_mult_id_vars',
                                        label = "Select variables",
                                        session = session,
                                        choices = x,
                                        selected = x,
                                        options = list(size = 15,
                                                `actions-box` = TRUE))
                        }
                        )
                        
                        ## SL.3.1.3. Error message if there is no data ====
                        output$gr_mult_out_query_result <- renderUI({
                                to_listen_data()
                                
                                # No data downloaded from WDI
                                if(empty(gb_df_all_data)){
                                        return(p("No data has been loaded.", 
                                                style = "color:red")
                                        )
                                }
                                
                                # Data is downloaded, but no values
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)) {
                                        return(p("Data has been loaded, but the variable Values are all missing.", 
                                                style = "color:red")
                                        )
                                }
                        })
                        
                        ## SL.3.1.4. Year choice: update selectInput ====
                        observeEvent(input$in_id_time_update,{
                                updateSliderInput(
                                        session= session,
                                        inputId = "gr_mult_id_time_range",
                                        min = gb_in_time_range_start,
                                        max = gb_in_time_range_end,
                                        step = 1,
                                        value = gb_in_time_range)}
                        )
                        
                        # Color palette
                        output$gr_mult_id_color_ui <- renderUI({
                                pickerInput(
                                        inputId = "gr_mult_id_color_pal",
                                        label = "Select color palette", 
                                        choices = colorblind_palette,
                                        selected = gb_ae_colour_palette_mult,
                                        multiple = FALSE
                                ) %>%
                                        shinyInput_label_embed(
                                                shiny_iconlink() %>%
                                                        bs_attach_modal(id_modal = "help_palette")
                                        )
                        })
                        
                        color_reactive <- eventReactive(input$gr_mult_id_color_pal,{
                                gb_ae_colour_palette_mult <<- input$gr_mult_id_color_pal
                                }
                        )
                        
                        
                # SL.3.2. Create plots ----
                        
                        ## SL.3.2.1. Prep data ====
                        prepped_data_mult <- reactive({
                                to_listen_data()
                                if(!is.null(input$gr_mult_id_vars)){
                                        datfra <- data.frame(input$gr_mult_id_vars)
                                        datfra <- separate(data = datfra,
                                                col = input.gr_mult_id_vars,
                                                into = c("A", "Var_code"),
                                                sep  =  " \\| ",
                                                extra = "merge")
                                        gr_mult_id_vars_var <- sort(datfra[,2])
                                        gb_df_all_data_raw %>%
                                                as_tibble() %>%
                                                filter(Country %in% input$gr_mult_id_countries &
                                                                Var_code %in% gr_mult_id_vars_var &
                                                                Year >= min(input$gr_mult_id_time_range) &
                                                                Year <= max(input$gr_mult_id_time_range)) %>%
                                                select(Var_name, Year, Value, Country, Ctry_iso, Ctry_group, Ctry_group_num, Units, Database, Period, Period_num) %>%
                                                group_split(Var_name)
                                }
                        })

                        ## SL.3.2.2. Create plots ====
                        createUI_mult <- function(table) {

                                # Parameters for plot
                                # Input for number of digits to be used in y-axis and data labels
                                digits_num <- input$gr_mult_id_digits
                                # Input for trillions/billions/millions/thousands transformation
                                accuracy_yaxis <- 1/10^(digits_num)
                                units_zeros <- c("Trillions", "Billions", "Millions", "Thousands")
                                # Input for title, subtitle Y-axis and source
                                title_text <- ""
                                title_text <- if(input$gr_mult_id_title){
                                        unique(table$Var_name)}
                                subtitle_text <- ""
                                yaxis_units <- if(input$gr_mult_id_yaxis){
                                        unique(table$Units)} else {NULL}
                                graph_source <- c("")
                                if(input$gr_mult_id_source){
                                        graph_source <- unique(table$Database)}
                                if(graph_source=="WDI"){graph_source<- "World Development Indicators"}
                                # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                                intervals <- ifelse(input$gr_mult_id_time_range[2] - input$gr_mult_id_time_range[1] < 30, 1, 2)

                                # Plot counter and plot names
                                gb_counter_mult <<- gb_counter_mult + 1
                                plot_name <- paste0("plot_mult_", as.character(gb_counter_mult))


                                # Transform data: divide by trillions/billions/millions/thousands
                                for (i in 4:1){
                                        if(!all(is.na(table$Value))){
                                                if(input$gr_mult_id_transform_zeros & max(abs(table$Value), na.rm =TRUE)>(10^(3*i))){
                                                        if(input$gr_mult_id_title) {
                                                                subtitle_text <- paste(
                                                                        c(subtitle_text, units_zeros[5-i]),
                                                                        collapse = "")
                                                                separator <- if(subtitle_text==""){""} else {", "}
                                                                if(input$gr_mult_id_yaxis){
                                                                        yaxis_units <- paste(
                                                                                c(yaxis_units, units_zeros[5-i]),
                                                                                collapse = separator)
                                                                }
                                                        }
                                                        table$Value <- table$Value/(10^(3*i))
                                                }
                                        }
                                }

                                # Log transform
                                if(!all(is.na(table$Value))){
                                        if(input$gr_mult_id_transform_log & min(table$Value, na.rm =TRUE)>0){
                                                subtitle_text <- paste(
                                                        c(subtitle_text, " (Log transformation)"),
                                                        collapse = "")
                                                if(input$gr_mult_id_yaxis){
                                                        yaxis_units <- paste(
                                                                c(yaxis_units, " (Log transformation)"),
                                                                collapse = "")
                                                }

                                                table$Value <- log(table$Value)
                                        }
                                }
                                
                                # Add years to Period variable
                                if(input$gr_mult_id_time_subper){
                                        # Period 1
                                        if(input$gr_mult_id_time_range[1] > gb_in_time_range_start){
                                                x <- paste(gb_in_time_name_1, " (", input$gr_mult_id_time_range[1], sep="")
                                                table$Period[table$Period_num == 1] <- x
                                        }else{
                                                x <- paste(gb_in_time_name_1, " (", as.character(gb_in_time_range_start), sep="")
                                                table$Period[table$Period_num == 1] <- x
                                        }
                                        if(input$gr_mult_id_time_range[2] < gb_in_time_limit_1){
                                                table$Period[table$Period_num == 1] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                        }else{
                                                table$Period[table$Period_num == 1] <- paste(x, "-", as.character(gb_in_time_limit_1), ")",sep="")
                                        }
                                        # Period 2
                                        if(gb_in_time_subper_num==2){
                                                if(input$gr_mult_id_time_range[1] > gb_in_time_limit_1+1){
                                                        x <- paste(gb_in_time_name_2, " (", input$gr_mult_id_time_range[1], sep="")
                                                        table$Period[table$Period_num == 2] <- x
                                                }else{
                                                        x <- paste(gb_in_time_name_2, " (", as.character(gb_in_time_limit_1+1), sep="")
                                                        table$Period[table$Period_num == 2] <- x
                                                }
                                                if(input$gr_mult_id_time_range[2] < gb_in_time_range_end){
                                                        table$Period[table$Period_num == 2] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                                }else{
                                                        table$Period[table$Period_num == 2] <- paste(x, "-", as.character(gb_in_time_range_end), ")", sep="")
                                                }
                                        }else{
                                                if(input$gr_mult_id_time_range[1] > gb_in_time_limit_1+1){
                                                        x <- paste(gb_in_time_name_2, " (", input$gr_mult_id_time_range[1], sep="")
                                                        table$Period[table$Period_num == 2] <- x
                                                }else{
                                                        x <- paste(gb_in_time_name_2, " (", as.character(gb_in_time_limit_1+1), sep="")
                                                        table$Period[table$Period_num == 2] <- x
                                                }
                                                if(input$gr_mult_id_time_range[2] < gb_in_time_limit_2){
                                                        table$Period[table$Period_num == 2] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                                }else{
                                                        table$Period[table$Period_num == 2] <- paste(x, "-", as.character(gb_in_time_limit_2), ")", sep="")
                                                }
                                        }

                                        
                                        # Period 3
                                        if(gb_in_time_subper_num==3){
                                                if(input$gr_mult_id_time_range[1] > gb_in_time_limit_2+1){
                                                        x <- paste(gb_in_time_name_3, " (", input$gr_mult_id_time_range[1], sep="")
                                                        table$Period[table$Period_num == 3] <- x
                                                }else{
                                                        x <- paste(gb_in_time_name_3, " (", as.character(gb_in_time_limit_2+1), sep="")
                                                        table$Period[table$Period_num == 3] <- x
                                                }
                                                if(input$gr_mult_id_time_range[2] < gb_in_time_range_end){
                                                        table$Period[table$Period_num == 3] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                                }else{
                                                        table$Period[table$Period_num == 3] <- paste(x, "-", as.character(gb_in_time_range_end), ")", sep="")
                                                }
                                        }else{
                                                if(input$gr_mult_id_time_range[1] > gb_in_time_limit_2+1){
                                                        x <- paste(gb_in_time_name_3, " (", input$gr_mult_id_time_range[1], sep="")
                                                        table$Period[table$Period_num == 3] <- x
                                                }else{
                                                        x <- paste(gb_in_time_name_3, " (", as.character(gb_in_time_limit_2+1), sep="")
                                                        table$Period[table$Period_num == 3] <- x
                                                }
                                                if(input$gr_mult_id_time_range[2] < gb_in_time_limit_3){
                                                        table$Period[table$Period_num == 3] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                                }else{
                                                        table$Period[table$Period_num == 3] <- paste(x, "-", as.character(gb_in_time_limit_3), ")", sep="")
                                                }
                                        }

                                        # Period 4
                                        if(gb_in_time_subper_num==4){
                                                if(input$gr_mult_id_time_range[1] > gb_in_time_limit_3+1){
                                                        x <- paste(gb_in_time_name_4, " (", input$gr_mult_id_time_range[1], sep="")
                                                        table$Period[table$Period_num == 4] <- x
                                                }else{
                                                        x <- paste(gb_in_time_name_4, " (", as.character(gb_in_time_limit_3+1), sep="")
                                                        table$Period[table$Period_num == 4] <- x
                                                }
                                                if(input$gr_mult_id_time_range[2] < gb_in_time_range_end){
                                                        table$Period[table$Period_num == 4] <- paste(x, "-", input$gr_mult_id_time_range[2], ")", sep="")
                                                }else{
                                                        table$Period[table$Period_num == 4] <- paste(x, "-", as.character(gb_in_time_range_end), ")", sep="")
                                                }
                                        }
                                }

                                # Warning sign if no data for this variable
                                if(all(is.na(table$Value))) {subtitle_text <- "No data available"}
                                
                                # Calculate variable to plot based on selected stat
                                detach(package:plyr)                                    # To avoid problems
                                
                                if(!input$gr_mult_id_time_subper){
                                        table$Period <- NA
                                        table$Period_num <- NA
                                }
                                
                                if(input$gr_mult_id_stat=="Average"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                mutate(sum_value = mean(Value, na.rm=T)) %>%
                                                ungroup()
                                        table <- table %>% distinct(Country, Period, .keep_all = TRUE) 
                                        if(subtitle_text==""){subtitle_text <- paste(
                                                c(subtitle_text, "Period average"),
                                                collapse = "")}
                                        else{subtitle_text <- paste(
                                                c(subtitle_text, ", period average"),
                                                collapse = "")}
                                }
                                
                                if(input$gr_mult_id_stat=="Median"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                mutate(sum_value = median(Value, na.rm=T)) %>%
                                                ungroup()
                                        table <- table %>% distinct(Country, Period, .keep_all = TRUE) 
                                        if(subtitle_text==""){subtitle_text <- paste(
                                                c(subtitle_text, "Period median"),
                                                collapse = "")}
                                        else{subtitle_text <- paste(
                                                c(subtitle_text, ", period median"),
                                                collapse = "")}
                                }
                                
                                if(input$gr_mult_id_stat=="Standard deviation"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                mutate(sum_value = sd(Value, na.rm=T)) %>%
                                                ungroup()
                                        table <- table %>% distinct(Country, Period, .keep_all = TRUE) 
                                        if(subtitle_text==""){subtitle_text <- paste(
                                                c(subtitle_text, "Period standard deviation"),
                                                collapse = "")}
                                        else{subtitle_text <- paste(
                                                c(subtitle_text, ", period standard deviation"),
                                                collapse = "")}
                                        }
                                
                                if(input$gr_mult_id_stat=="Maximum"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                mutate(sum_value = my_max(Value)) %>%
                                                ungroup()
                                        table <- table %>% distinct(Country, Period, .keep_all = TRUE) 
                                        if(subtitle_text==""){subtitle_text <- paste(
                                                c(subtitle_text, "Period maximum"),
                                                collapse = "")}
                                                else{subtitle_text <- paste(
                                                        c(subtitle_text, ", period maximum"),
                                                        collapse = "")}
                                        }
                                
                                if(input$gr_mult_id_stat=="Minimum"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                mutate(sum_value =  my_min(Value)) %>%
                                                ungroup()
                                        table <- table %>% distinct(Country, Period, .keep_all = TRUE)
                                        if(input$gr_mult_id_title){
                                                if(subtitle_text==""){subtitle_text <- paste(
                                                c(subtitle_text, "Period minimum"),
                                                collapse = "")}
                                                else{subtitle_text <- paste(
                                                        c(subtitle_text, ", period minimum"),
                                                        collapse = "")}}

                                }
                                
                                if(input$gr_mult_id_stat=="Most recent value"){
                                        table <- table %>%
                                                group_by(Country, Period) %>%
                                                # drop_na(Value) %>%
                                                mutate(sum_value = Value[which.max(Year)]) %>%
                                                ungroup()
                                        if(input$gr_mult_id_title){
                                                if(subtitle_text==""){subtitle_text <- paste(
                                                        c(subtitle_text, "Period most recent value"),
                                                        collapse = "")}
                                                else{subtitle_text <- paste(
                                                        c(subtitle_text, ", period most recent value"),
                                                        collapse = "")}}
                                        
                                }
                                
                                # Include years in subtitle if no sub-periods
                                if(input$gr_mult_id_title & !input$gr_mult_id_time_subper){
                                        year_start <- input$gr_mult_id_time_range[1]
                                        year_end <- input$gr_mult_id_time_range[2]
                                        subtitle_text <- paste(
                                                c(subtitle_text, " (", year_start, "-", year_end, ")"),
                                                collapse = "")
                                }
                                
                                table$Period_num <- factor(table$Period_num,
                                        levels = unique(table$Period_num),
                                        labels = unique(table$Period)
                                        )
                                
                                table$Ctry_group_num <- factor(table$Ctry_group_num,
                                        levels = unique(table$Ctry_group_num),
                                        labels = unique(table$Ctry_group))
                                
                                detach(package:dplyr)                                    # To avoid problems
                                library(plyr, warn.conflicts = FALSE)
                                library(dplyr, warn.conflicts = FALSE)
                                
                                # Short country names
                                if(input$gr_mult_id_ctry_short){
                                        table$Country <- table$Ctry_iso
                                }

                                # Actually create plot
                                p <- ggplot(data = table,
                                                aes(x = Country, 
                                                        y = sum_value,
                                                        fill = if(!input$gr_mult_id_time_subper){input$gr_mult_id_color} else {Period_num},
                                                        order = Period_num
                                                        )
                                        )+
                                        
                                        # Define type of graph: bar
                                        geom_bar(stat="identity", 
                                                position=position_dodge(),
                                                colour="black"
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
                                        labs(title = title_text,
                                                subtitle = subtitle_text,
                                                caption = if(input$gr_mult_id_source){paste("Source: ", graph_source, ".", sep="")},
                                                y = if(input$gr_mult_id_yaxis){yaxis_units} else {NULL}
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
                                                text = element_text(size=12, family=input$gr_mult_id_font),
                                                legend.title = element_blank(),
                                                axis.title.y = element_text()
                                        )

                                # Include thousands separating comma in Y-axis
                                p <- p + scale_y_continuous(name = yaxis_units,
                                        labels = comma_format(accuracy = accuracy_yaxis, big.mark = ","),
                                        breaks = pretty_breaks()
                                )
                                
                                # Include data labels        
                                if(input$gr_mult_id_data_labels){p <- p + geom_text(
                                        aes(x = Country,
                                                y = sum_value,
                                                label = format(round(as.numeric(sum_value), digits_num), 
                                                        nsmall=digits_num, big.mark=",")),
                                        vjust= ifelse(table$sum_value <0 , 1.5, -0.5),
                                        hjust= 0.5,
                                        size = 3,
                                        position = position_dodge(0.9),
                                        family=input$gr_mult_id_font)
                                }

                                # Color palette
                                color_reactive()
                                if(input$gr_mult_id_time_subper){
                                        p <- p + scale_fill_brewer(palette=gb_ae_colour_palette_mult)
                                }
                                
                                # Hide legend if no time period break
                                if(!input$gr_mult_id_time_subper){p <- p + theme(legend.position = "none")}

                                
                                # Set legend position
                                if(input$gr_mult_id_time_subper){
                                        p <- p + theme(legend.position=input$gr_mult_id_legend_pos)
                                }

                                # Append plot to the list of plots
                                myplots_mult <<- list.append(myplots_mult, p)
                                names(myplots_mult)[gb_counter_mult] <<- plot_name

                                # Show plot
                                renderPlot(p)
                        }
                        
                        ## SL.2.2.3. Call Prep data and Create plots ====
                        output$gr_mult_out_plots <- renderUI({
                                myplots_mult <<- list()
                                gb_counter_mult <<- 0
                                pd <- req(prepped_data_mult())
                                tagList(map(
                                        pd,
                                        ~ createUI_mult(.)
                                )
                                )
                        })

                # SL.3.3. Plot download handlers ----
                        
                        ## SL.3.3.1. Download plots as png zipped - large ====
                        output$gr_mult_download_large <- downloadHandler(
                                filename = 'gr_mult_plots_large.zip',
                                content = function( file){
                                        
                                        # Set temporary working directory
                                        owd <- setwd( tempdir())
                                        on.exit( setwd( owd))
                                        
                                        # Save the plots
                                        vector_plots <- vector()
                                        for (i in 1:length(myplots_mult)){
                                                name <- paste("multplot_large", i, ".png", sep="")
                                                ggsave(name, 
                                                        plot = myplots_mult[[i]], 
                                                        device = "png",
                                                        width = 11.5, 
                                                        height = 5.75,
                                                        units = "in")
                                                vector_plots <- c(vector_plots, paste("multplot_large", i, ".png", sep=""))
                                                
                                        }
                                        
                                        # Zip them up
                                        zip( file, vector_plots)
                                }
                        )
                        
                        ## SL.3.3.2. Download plots as png zipped - small ====
                        output$gr_mult_download_small <- downloadHandler(
                                filename = 'gr_mult_plots_small.zip',
                                content = function( file){
                                        
                                        # Set temporary working directory
                                        owd <- setwd( tempdir())
                                        on.exit( setwd( owd))
                                        
                                        # Save the plots
                                        vector_plots <- vector()
                                        for (i in 1:length(myplots_mult)){
                                                
                                                name <- paste("multplot_small", i, ".png", sep="")
                                                ggsave(name, 
                                                        plot = myplots_mult[[i]], 
                                                        device = "png",
                                                        width = 5.75, 
                                                        height = 5.75,
                                                        units = "in")
                                                vector_plots <- c(vector_plots, paste("multplot_small", i, ".png", sep=""))
                                                
                                        }
                                        
                                        # Zip them up
                                        zip( file, vector_plots)
                                }
                        )

        # SL.4. Line Plots tab ----

                # SL.4.1. Inputs and data processing ---- 
                
                        ## SL.4.1.1. Countries choice: update countries checkbox ====
                        observeEvent(to_listen_data(),{
                                to_listen_data()
                                z <- unique(paste(gb_df_all_data$Country, gb_df_all_data$Ctry_group, sep=" | "))
                                datfra <- data.frame(z)
                                datfra <- separate(data = datfra,
                                        col = z,
                                        into = c("A", "B"),
                                        sep  =  " \\| ",
                                        extra = "merge")
                                ctry <- datfra$A
                                group <- datfra$B
                                ctry_list <- as.list(ctry)
                                names(ctry_list) <- z
                                y <- c(gb_in_countries_ctry, gb_in_countries_asp, gb_in_countries_str, gb_in_countries_reg)
                                
                                
                                updatePickerInput(
                                        inputId = 'gr_line_id_countries',
                                        session = session,
                                        choices = ctry_list,
                                        selected = y,
                                        options = list(size = 15,
                                                `actions-box` = TRUE))
                        })
                        
                        output$gr_line_message_select_ctries <- renderUI(
                                if(is.null(gb_df_all_data$Country)){helpText("Upload data using Load Data tab")}
                        )
                
                        
                        ## SL.4.1.2. Variable choice: update variable checkbox ====
                        observeEvent(to_listen_data(),{
                                x <- unique(paste(gb_df_all_data$Var_name, gb_df_all_data$Var_code, sep=" | "))
                                if(is.null(x)) {x <- character(0)}
                                updatePickerInput(
                                        inputId = 'gr_line_id_vars',
                                        label = "Select variables",
                                        session = session,
                                        choices = x,
                                        selected = x,
                                        options = list(size = 15,
                                                `actions-box` = TRUE))
                                }
                        )
                        
                        output$gr_line_message_select_vars <- renderUI(
                                if(is.null(gb_df_all_data$Var_name)){helpText("Upload data using Load Data tab")}
                        )
                        
                        
                        ## SL.4.1.3. Error message if there is no data ====
                        output$gr_line_out_query_result <- renderUI({
                                to_listen_data()
                                
                                # No data downloaded from WDI
                                if(empty(gb_df_all_data)){
                                        return(p("No data has been loaded.", 
                                                style = "color:red")
                                        )
                                }
                                
                                # Data is downloaded, but no values
                                if(sum(is.na(gb_df_all_data$Value)) == nrow(gb_df_all_data)) {
                                        return(p("Data has been loaded, but the variable Values are all missing.", 
                                                style = "color:red")
                                        )
                                }
                        })
                        
                        
                        ## SL.4.1.4. Year choice: update selectInput ====
                        observeEvent(input$in_id_time_update,{
                                updateSliderInput(
                                        session= session,
                                        inputId = "gr_line_id_time_range",
                                        min = gb_in_time_range_start,
                                        max = gb_in_time_range_end,
                                        step = 1,
                                        value = gb_in_time_range)}
                                )
                        
                        ## SL.4.1.5. Highlight country choice: update selectInput ====
                        observe({
                                x <- input$gr_line_id_countries 
                                if(is.null(x)) {x <- character(0)}
                                updateSelectInput(
                                        inputId = 'gr_line_highlight_ctry',
                                        choices = x,
                                        selected = gb_in_countries_ctry )}
                        )
                
                
                # SL.4.2. Create plots ----
                
                        ## SL.4.2.1. Prep data ====
                        prepped_data_line <- reactive({
                                to_listen_data()
                                if(! is.null(input$gr_line_id_vars)){
                                        datfra <- data.frame(input$gr_line_id_vars)
                                        datfra <- separate(data = datfra, 
                                                col = input.gr_line_id_vars,
                                                into = c("A", "Var_code"), 
                                                sep  =  " \\| ",
                                                extra = "merge")
                                        gr_line_id_vars_var <- sort(datfra[,2])
                                        gb_df_all_data %>%
                                                as_tibble() %>%
                                                filter(Country %in% input$gr_line_id_countries & 
                                                                Var_code %in% gr_line_id_vars_var &
                                                                Year >= min(input$gr_line_id_time_range) &
                                                                Year <= max(input$gr_line_id_time_range)) %>%
                                                select(Var_name, Year, Value, Country, Ctry_iso, Units, Database, Period) %>%
                                                group_split(Var_name)
                                }
                        })
                        
                        
                        ## SL.4.2.2. Create plots ====
                        createUI_line <- function(table) {
                                
                                # Parameters for plot
                                        # Input for number of digits to be used in y-axis and data labels
                                        digits_num <- input$gr_line_id_digits
                                        # Input for trillions/billions/millions/thousands transformation
                                        accuracy_yaxis <- 1/10^(digits_num)
                                        units_zeros <- c("Trillions", "Billions", "Millions", "Thousands")
                                        # Input for title, subtitle Y-axis and source
                                        title_text <- ""
                                        title_text <- if(input$gr_line_id_title){
                                                paste0(unique(table$Var_name))}
                                        subtitle_text <- ""
                                        yaxis_units <- if(input$gr_line_id_yaxis){
                                                unique(table$Units)} else {NULL}
                                        graph_source <- c("")
                                        if(input$gr_line_id_source){
                                                graph_source <- unique(table$Database)}
                                        if(graph_source=="WDI"){graph_source<- "World Development Indicators"}
                                        # Year axis intervals: if less than 30 years covered, then include all years, otherwise every 2 years
                                        intervals <- ifelse(max(input$gr_line_id_time_range)-min(input$gr_line_id_time_range)<30, 1, 2)
                                
                                # Plot counter and plot names
                                        gb_counter_line <<- gb_counter_line + 1
                                        plot_name <- paste0("plot_line_", as.character(gb_counter_line))
                                
                                # Transform data: divide by trillions/billions/millions/thousands
                                for (i in 4:1){
                                        if(input$gr_line_id_transform_zeros & max(abs(table$Value), na.rm =TRUE)>(10^(3*i))){
                                                if(input$gr_line_id_title) {
                                                        subtitle_text <- paste(
                                                                c(subtitle_text, units_zeros[5-i]), 
                                                                collapse = "")
                                                        separator <- if(subtitle_text==""){""} else {", "}
                                                        if(input$gr_line_id_yaxis){
                                                                yaxis_units <- paste(
                                                                        c(yaxis_units, units_zeros[5-i]), 
                                                                        collapse = separator)
                                                        }
                                                }
                                                table$Value <- table$Value/(10^(3*i))
                                        }
                                }
                                
                                # Log transformation
                                if(input$gr_line_id_transform_log & min(table$Value, na.rm =TRUE)>0){
                                        subtitle_text <- paste(
                                                c(subtitle_text, " (Log transformation)"), 
                                                collapse = "")
                                        if(input$gr_line_id_yaxis){
                                                yaxis_units <- paste(
                                                        c(yaxis_units, " (Log transformation)"), 
                                                        collapse = "")
                                        }
                                        table$Value <- log(table$Value) 
                                }
                                
                                # Warning sign if no data for this variable
                                if(all(is.na(table$Value))) {subtitle_text <- "No data available"}
                                
                                # Create subperiod variables to include rectangles
                                if(gb_in_time_subper & input$gr_line_id_time_subper){
                                        
                                        # If 2 subperiods, then there are 3 cases
                                        if(gb_in_time_subper_num == 2){
                                                # Case 1 
                                                if(input$gr_line_id_time_range[1] < gb_in_time_limit_1 & input$gr_line_id_time_range[2] > gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 2 
                                                if(input$gr_line_id_time_range[2]<=gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                        }
                                        # If 3 subperiods, then there are 6 cases
                                        if(gb_in_time_subper_num == 3){
                                                # Case 1
                                                if(input$gr_line_id_time_range[1]<gb_in_time_limit_1 & input$gr_line_id_time_range[2]>gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5,input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 2
                                                if(input$gr_line_id_time_range[1]<gb_in_time_limit_1 & input$gr_line_id_time_range[2]<=gb_in_time_limit_2 & input$gr_line_id_time_range[2]>gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1 & input$gr_line_id_time_range[1]<gb_in_time_limit_2 & input$gr_line_id_time_range[2]>gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 4
                                                if(input$gr_line_id_time_range[2]<=gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 5
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1 & input$gr_line_id_time_range[2]<=gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 6
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3)
                                                        )
                                                }
                                        }
                                        # If 4 subperiods, then there are 10 cases
                                        if(gb_in_time_subper_num == 4){
                                                # Case 1
                                                if(input$gr_line_id_time_range[1]<gb_in_time_limit_1 & input$gr_line_id_time_range[2]>gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5,input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 2
                                                if(input$gr_line_id_time_range[1]<gb_in_time_limit_1 & input$gr_line_id_time_range[2]<=gb_in_time_limit_3 & input$gr_line_id_time_range[2]>gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, gb_in_time_limit_2+0.5,input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 3
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1 & input$gr_line_id_time_range[1]<gb_in_time_limit_2 & input$gr_line_id_time_range[2]>gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, gb_in_time_limit_3+0.5,input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 4
                                                if(input$gr_line_id_time_range[1]<gb_in_time_limit_1 & input$gr_line_id_time_range[2]<=gb_in_time_limit_2 &  input$gr_line_id_time_range[2]>gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_1+0.5), 
                                                                Year_end=c(gb_in_time_limit_1+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1, gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 5
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1 & input$gr_line_id_time_range[1]<gb_in_time_limit_2 & input$gr_line_id_time_range[2]<=gb_in_time_limit_3 & input$gr_line_id_time_range[2]>gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_2+0.5), 
                                                                Year_end=c(gb_in_time_limit_2+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2, gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 6
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_2 & input$gr_line_id_time_range[1]<gb_in_time_limit_3& input$gr_line_id_time_range[2]>gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5, gb_in_time_limit_3+0.5), 
                                                                Year_end=c(gb_in_time_limit_3+0.5, input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3, gb_in_time_name_4)
                                                        )
                                                }
                                                # Case 7
                                                if(input$gr_line_id_time_range[2]<=gb_in_time_limit_1){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_1)
                                                        )
                                                }
                                                # Case 8
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_1 & input$gr_line_id_time_range[2]<=gb_in_time_limit_2){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_2)
                                                        )
                                                }
                                                # Case 9
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_2 & input$gr_line_id_time_range[2]<=gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_3)
                                                        )
                                                }
                                                # Case 10
                                                if(input$gr_line_id_time_range[1]>=gb_in_time_limit_3){
                                                        rectangle_text <- data.frame(
                                                                Year_start=c(input$gr_line_id_time_range[1]-0.5), 
                                                                Year_end=c(input$gr_line_id_time_range[2]+0.5),
                                                                Period=c(gb_in_time_name_4)
                                                        )
                                                }
                                        }
                                        
                                        vertical_lines <- rectangle_text$Year_end
                                        vertical_lines <- vertical_lines[1:(length(vertical_lines)-1)]
                                }
                                
                                # Short country names
                                if(input$gr_line_id_ctry_short){
                                        table$Country2 <- table$Country
                                        table$Country <- table$Ctry_iso
                                }
                                        
                                
                                # Actually Create plot
                                
                                # Plot if no single country highlight option
                                if(!input$gr_line_highlight){
                                        p <- ggplot(data=table, aes(x = Year, y = Value)) +
                                                geom_line(data=table, aes(colour = Country), size = 0.75) +
                                                geom_point(data=table, aes(colour = Country), size = 0.75)
                                } else{
                                # Plot if highlight option selected
                                        # Filter data
                                        if(input$gr_line_id_ctry_short){
                                                highlighted_country_df <- filter(table, 
                                                        Country2 %in% input$gr_line_highlight_ctry)
                                                rest_df <- filter(table, 
                                                        !Country2 %in% input$gr_line_highlight_ctry)
                                        }else{
                                                highlighted_country_df <- filter(table, 
                                                        Country %in% input$gr_line_highlight_ctry)
                                                rest_df <- filter(table, 
                                                        !Country %in% input$gr_line_highlight_ctry)}

                                        # Obtain Y-value of last observation to locate label
                                        highlighted_country_last_obs <- highlighted_country_df %>% drop_na(Value)
                                        highlighted_country_last_obs <- subset(highlighted_country_last_obs, Year==max(highlighted_country_last_obs$Year))
                                        highlighted_country_last_obs <- highlighted_country_last_obs %>% pull(Value)
                                        lab <- unique(highlighted_country_df$Country)
                                        # Plot
                                        p <- ggplot(data=table, aes(x = Year, y = Value)) +
                                                # Grey lines (rest of the countries)
                                                geom_line(data=rest_df, aes(x = Year, y = Value, group = Country), color="grey", size = 0.75) +
                                                geom_point(data=rest_df, aes(x = Year, y = Value, group = Country), color="grey", size = 0.75) +
                                                # Highlighted line
                                                geom_line(data=highlighted_country_df, aes(x = Year, y = Value), color=input$gr_line_highlight_color, size = 1.5) +
                                                geom_point(data=highlighted_country_df, aes(x = Year, y = Value), fill=input$gr_line_highlight_color, size = 4, stroke = 1, colour=input$gr_line_highlight_color, shape = 21) +
                                                # Label with country name for highlighted
                                                annotate("text", 
                                                        x = input$gr_line_id_time_range[2]+0.25, 
                                                        y = highlighted_country_last_obs, 
                                                        label = lab,
                                                        color=input$gr_line_highlight_color,
                                                        family = input$gr_line_id_font,
                                                        fontface="bold",
                                                        hjust = "left",
                                                        vjust = "center"
                                                        ) +
                                                coord_cartesian(clip = "off")
                                }
                                
                                # Settings for both types (with/without highlight) of graphs
                                # Increase X axis half a year to both sides
                                x_min <- ggplot_build(p)$layout$panel_params[[1]]$x.range[1]
                                x_max <- ggplot_build(p)$layout$panel_params[[1]]$x.range[2]
                                expand_param <- (0.5/(x_max-x_min))
                                
                                p <- p + 
                                        # Include title, subtitle, source and Y-axis title
                                        labs(title  = title_text,
                                                subtitle = subtitle_text,
                                                caption = if(input$gr_line_id_source){paste("Source: ", graph_source, ".", sep="")},
                                                y = if(input$gr_line_id_yaxis){yaxis_units}) +
                                        # X-axis format
                                        scale_x_continuous(name="",
                                                breaks = seq(min(input$gr_line_id_time_range),
                                                        max(input$gr_line_id_time_range),
                                                        by = intervals),
                                                expand = c(expand_param,expand_param)) +
                                        # Aesthetics
                                        theme(
                                                panel.background = element_blank(),
                                                panel.border = element_blank(),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                plot.title = element_text(size = 12, face = "bold"),
                                                plot.margin = if(input$gr_line_highlight_legend) {margin(0.25, 3, 1, 0.25, "cm")} else {margin(0.25, 0.25, 1, 0.25, "cm")},
                                                plot.caption = element_text(hjust = 0, size = 10),
                                                axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                axis.text.x = element_text(colour = "black"),
                                                axis.text.y = element_text(colour = "black"),
                                                text = element_text(size=12,  family=input$gr_line_id_font),
                                                legend.key = element_rect(fill = "white"),
                                                legend.title = element_blank())
                                
                                # Include data labels 
                                if(input$gr_line_id_data_labels){p <- p + 
                                        geom_text(data = table,
                                        aes(x = Year, y = Value, label = format(round(as.numeric(Value), digits_num), nsmall=digits_num, big.mark=",")),
                                        vjust= ifelse(table$Value <0 , 1.5, -0.5),
                                        hjust= 0.5,
                                        size = 3,
                                        family=input$gr_line_id_font)
                                }
                                
                                # Include legend
                                if(!input$gr_line_highlight_legend){
                                        p <- p + theme(legend.position = "none")
                                }
                                
                                # Increase margins inside graph by extending the range 15% (7.5% above and below)
                                y_min <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                                y_max <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                                y_range <- y_max - y_min
                                p <- p +scale_y_continuous(name=  yaxis_units,
                                        labels = comma_format(accuracy = accuracy_yaxis, big.mark = ","),
                                        breaks=pretty_breaks(),
                                        limits = c(y_min, y_max+(y_range*0.1)))
                                
                                # To use in vertical lines
                                y_min_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
                                y_max_new <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
                                y_range_new <- y_max - y_min
                
                                size_factor <- 0.08
                                ALPHA<- 0.15
                                
                                # Subperiod rectangles, their labels and the dotted lines separating 
                                if(gb_in_time_subper & input$gr_line_id_time_subper & !all(is.na(table$Value))){
                                        p <- p + 
                                                geom_rect(data = rectangle_text,
                                                        aes(NULL, NULL, xmin=-Inf, xmax=Inf),
                                                        ymin = y_max_new - y_range_new*size_factor ,
                                                        ymax = y_max_new,
                                                        colour = NA,
                                                        fill="grey",
                                                        alpha = 0.5)+
                                                geom_label(data=rectangle_text,
                                                        aes(x = Year_start+(Year_end-Year_start)/2, 
                                                                y = y_max_new*ALPHA + (y_max_new - y_range_new*size_factor)*(1-ALPHA), 
                                                                label = Period,
                                                                family = input$gr_line_id_font
                                                        ),
                                                        size=3.3,
                                                        fill = "grey",
                                                        alpha = 0,
                                                        label.size = NA,
                                                        hjust = "center", 
                                                        vjust = "bottom")
                                        
                                        for (i in vertical_lines) {
                                                p <- p + geom_segment(x=i, 
                                                        y=y_max_new,
                                                        xend=i, 
                                                        yend=y_max_new - y_range_new*size_factor,
                                                        colour = "white",
                                                        size=1,
                                                        alpha=1) +
                                                        geom_segment(x=i, 
                                                                y=y_max_new - y_range_new*size_factor,
                                                                xend=i, 
                                                                yend=-Inf,
                                                                colour = "grey",
                                                                linetype = "dotted")
                                        }
                                }
                                
                                # Append plot to the list of plots   
                                myplots_line <<- list.append(myplots_line, p) 
                                names(myplots_line)[gb_counter_line] <<- plot_name
                                
                                # Show plot
                                renderPlot(p)
                                
                        }
                        
                        ## SL.4.2.3. Call prep data and create plots ====
                        output$gr_out_line_plots <- renderUI({
                                
                                myplots_line <<- list()
                                gb_counter_line <<- 0
                                
                                pd <- req(prepped_data_line())
                                tagList(map(
                                        pd,
                                        ~ createUI_line(.)
                                )
                                )
                        })
                        
                # SL.4.3. Plot download handlers ----
                        
                        ## SL.4.3.1. Download plots as png zipped - large ====
                        output$gr_line_download_large <- downloadHandler(
                                filename = 'gr_line_plots_large.zip',
                                content = function( file){
                                        
                                        # Set temporary working directory
                                        owd <- setwd( tempdir())
                                        on.exit( setwd( owd))
                                        
                                        # Save the plots
                                        vector_plots <- vector()
                                        for (i in 1:length(myplots_line)){
                                                name <- paste("lineplot_large", i, ".png", sep="")
                                                ggsave(name, 
                                                        plot = myplots_line[[i]], 
                                                        device = "png",
                                                        width = 11.5, 
                                                        height = 5.75,
                                                        units = "in")
                                                vector_plots <- c(vector_plots, paste("lineplot_large", i, ".png", sep=""))
                                                
                                        }
                                        
                                        # Zip them up
                                        zip( file, vector_plots)
                                }
                        )
                        
                        ## SL.4.3.2. Download plots as png zipped - small ====
                        output$gr_line_download_small <- downloadHandler(
                                filename = 'gr_line_plots_small.zip',
                                content = function( file){
                                        
                                        # Set temporary working directory
                                        owd <- setwd( tempdir())
                                        on.exit( setwd( owd))
                                        
                                        # Save the plots
                                        vector_plots <- vector()
                                        for (i in 1:length(myplots_line)){
                                                
                                                # Increase intervals in X axis in small plots
                                                intervals <- ifelse(max(input$gr_line_id_time_range)-min(input$gr_line_id_time_range)<30, 2, 4)
                                                myplots_line[[i]] <- myplots_line[[i]] + 
                                                        scale_x_continuous(name="",
                                                                breaks = seq(min(input$gr_line_id_time_range),
                                                                        max(input$gr_line_id_time_range),
                                                                        by = intervals))
                                                
                                                name <- paste("lineplot_small", i, ".png", sep="")
                                                ggsave(name, 
                                                        plot = myplots_line[[i]], 
                                                        device = "png",
                                                        width = 5.75, 
                                                        height = 5.75,
                                                        units = "in")
                                                vector_plots <- c(vector_plots, paste("lineplot_small", i, ".png", sep=""))
                                                
                                        }
                                        
                                        # Zip them up
                                        zip( file, vector_plots)
                                }
                        )

                                                
                        # SL.5. Scatter plots tab ----
                        
                        # SL.5.1. Inputs and data processing ---- 
                        
                        ## SL.5.1.1. Countries choice: update countries checkbox ====        
                        
                        ## SL.5.1.2. Variable choice: update variable checkbox ====
                        
                        ## SL.5.1.3. Error message if there is no data ====
                        
                        ## SL.5.1.4. Year choice: update selectInput ====
                        
                        # SL.5.2. Create plots ----
                        
                        ## SL.5.2.1. Prep data ====                        
                        
                        ## SL.5.2.2. Create plots ====
                        
                        ## SL.5.2.3. Call prep data and create plots ====
                        
                        # SL.5.3. Plot download handlers ----
                        
                        ## SL.5.3.1. Download plots as png zipped - large ====
                        
                        ## SL.5.3.2. Download plots as png zipped - small ====                        
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")

