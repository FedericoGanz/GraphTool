# global.r ----

## Load libraries ----

library(shiny)
library(dipsaus)
library(WDI)
library(dplyr)
library(RColorBrewer)


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
var_df <- var_df[order(var_df$name_indicator), ]
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
        ctries_ctry = "Pakistan",
        ctries_str = c("Bangladesh", "Egypt, Arab Rep.", "Ethiopia", "India"),
        ctries_asp = c("Indonesia", "Mexico", "Turkey", "Vietnam"),
        ctries_reg = "South Asia",
        ctries_all = FALSE,
        time_range_start = 2000,
        time_range_end = 2020,
        time_subper = TRUE,
        time_subper_num = 3,
        time_limit_1 = 2008,
        time_limit_2 = 2014,
        time_limit_3 = 2018,
        time_name_1 = "Historical",
        time_name_2 = "Recent past",
        time_name_3 = "Present",
        time_name_4 = "Period 4",
        wdi_vars = c("GDP growth (annual %) | NY.GDP.MKTP.KD.ZG",
                "Inflation, consumer prices (annual %) | FP.CPI.TOTL.ZG",
                "GDP per capita, PPP (current international $) | NY.GDP.PCAP.PP.CD"),
        imf_vars = NULL
)

# Clear input parameters
reset_list <- list(
        ctries_ctry = character(0),
        ctries_str = character(0),
        ctries_asp = character(0),
        ctries_reg = character(0),
        ctries_all = FALSE,
        time_range_start = 2000,
        time_range_end = 2020,
        time_subper = FALSE,
        time_subper_num = 2,
        time_limit_1 = numeric(length = 0L),
        time_limit_2 = numeric(length = 0L),
        time_limit_3 = numeric(length = 0L),
        time_name_1 = "Period 1",
        time_name_2 = "Period 2",
        time_name_3 = "Period 3",
        time_name_4 = "Period 4",
        wdi_vars = character(0),
        imf_vars = character(0)
)


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
                                                        multiple = TRUE),
                                                
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
                                                uiOutput("in_id_ext_mes_ui3"),
                                        
                                        )
                                ),
                                
                                mainPanel(
                                        
                                        tabsetPanel(type = "tabs",
                                                
                                                # Output: Table
                                                tabPanel("Table",
                                                        
                                                ),
                                                
                                                # Output: Metadata
                                                tabPanel("Metadata",
                                                        
                                                ),
                                                
                                                # Output: Summary
                                                tabPanel("Summary",
                                                
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
        
        # WDI variables
        
        updateSelectizeInput(
                session = session,
                inputId = 'in_id_wdi_vars',
                choices = var_vec,
                selected = def_list$wdi_vars,
                server = TRUE)
        
        # Upload external data
        
        output$in_id_ext_ui <- renderUI({
                fileInput('in_id_ext', label = "Select file") %>%
                        shinyInput_label_embed(
                                shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "help_text")
                        )
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
                imf_vars = def_list$imf_vars
        )
        
        # Create reactive values initially containing empty dataframes
        
        rv_df <- reactiveValues(

                dat_wdi = empty_data_df,
                dat_imf = empty_data_df,
                dat_ext = empty_data_df,
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
                        input$in_id_imf_vars
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
                                setequal(rv_input$imf_vars, input$in_id_imf_vars)
                                
                        )
                        
                        # If all elements in vector no_changes_vec are TRUE, then return FALSE, 
                        # else TRUE
                        return(!all(no_changes_vec))
                        
                }, ignoreInit = TRUE)
        
        # Show update button
        observeEvent(listen_inputs(), {
                
                print(listen_inputs())
                # If at least one input changed, show update button
                if(listen_inputs()){
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
                rv_input$imf_vars <- input$in_id_imf_vars

        })
        
        # Hide update button after updating
        observeEvent(input$in_id_update, {
                
                shinyjs::hide("in_id_update")
                
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
                
                # Reset reactive value list
                
                        # Country inputs
                        rv_input$ctries_ctry <- reset_list$ctries_ctry
                        rv_input$ctries_str <- reset_list$ctries_str
                        rv_input$ctries_asp <- reset_list$ctries_asp
                        rv_input$ctries_reg <- reset_list$ctries_reg
                        rv_input$ctries_all <- reset_list$ctries_all
                        
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
                        rv_input$imf_vars <- reset_list$imf_vars
                        
                        # Dataframes
                        rv_df$dat_wdi <- empty_data_df
                        rv_df$dat_imf <- empty_data_df
                        rv_df$dat_ext <- empty_data_df
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
                                selected = rv_input$ctries_ctry,
                                options = list(size = 15, `live-search` = TRUE))
                        
                        # Structural comparators
                        updateSelectizeInput(
                                session = session,
                                inputId = 'in_id_ctries_str',
                                choices = ctry_vec[!ctry_vec %in% c(rv_input$ctries_ctry , rv_input$ctries_asp)],
                                selected = rv_input$ctries_str)
                        
                        # Aspirational comparators
                        updateSelectizeInput(
                                session = session,
                                inputId = 'in_id_ctries_asp',
                                choices = ctry_vec[!ctry_vec %in% c(rv_input$ctries_ctry , rv_input$ctries_str)],
                                selected = rv_input$ctries_asp)
                        
                        # Regions
                        updateSelectizeInput(
                                session = session,
                                inputId = 'in_id_ctries_reg',
                                choices = reg_vec,
                                selected = rv_input$ctries_reg)
                        
                        # All
                        updateMaterialSwitch(
                                session = session,
                                inputId = "in_id_ctries_all",
                                value = rv_input$ctries_all)
                        
                        # Time variables
                        
                        # Time range
                        updateSliderInput(
                                session = session,
                                inputId = "in_id_time_range",
                                min = fix_list$time_range_min,
                                max = fix_list$time_range_max,
                                step = 1,
                                value = c(rv_input$time_range_start, rv_input$time_range_end))
                        
                        # Time subperiods
                        updateMaterialSwitch(
                                session = session,
                                inputId = "in_id_time_subper",
                                value = rv_input$time_subper)
                        
                        # Time subperiods number
                        updateNumericInput(
                                session = session,
                                inputId = "in_id_time_subper_num",
                                label = "Number of subperiods (min=2; max=4)",
                                min = 2,
                                max = 4,
                                value = rv_input$time_subper_num)
                        
                        # Time subperiods name 1
                        updateTextInput(
                                session = session,
                                inputId = "in_id_time_name_1",
                                label="Period 1 name",
                                value = rv_input$time_name_1)
                        
                        # Time subperiods name 2
                        updateTextInput(
                                session = session,
                                inputId = "in_id_time_name_2",
                                label="Period 2 name",
                                value = rv_input$time_name_2)

                        # Time limit 1
                        updateSliderInput(
                                session = session,
                                inputId = "in_id_time_limit_1",
                                min = rv_input$time_range_start,
                                max = rv_input$time_limit_2 - 1,
                                step = 1,
                                value = rv_input$time_limit_1)
                        
                        
                        # Time subperiods name 3
                        updateTextInput(
                                session = session,
                                inputId = "in_id_time_name_3",
                                label="Period 2 name",
                                value = rv_input$time_name_3)
                        
                        
                        # Time limit 2
                        updateSliderInput(
                                session = session,
                                inputId = "in_id_time_limit_2",
                                min = rv_input$time_limit_1 + 1,
                                max = rv_input$time_limit_3 - 1,
                                step = 1,
                                value = rv_input$time_limit_2)
                        
                        # Time subperiods name 4
                        updateTextInput(
                                session = session,
                                inputId = "in_id_time_name_4",
                                label="Period 4 name",
                                value = rv_input$time_name_4)
                        
                        # Time limit 3
                        updateSliderInput(
                                session = session,
                                inputId = "in_id_time_limit_3",
                                min = rv_input$time_limit_2,
                                max = rv_input$time_range_end - 1,
                                step = 1,
                                value = rv_input$time_limit_3)
                        
                        # WDI variables
                        updateSelectizeInput(
                                session = session,
                                inputId = "in_id_wdi_vars",
                                choices = var_vec,
                                server = TRUE,
                                selected = rv_input$wdi_vars)
                        
                        # IMF variables
                        updateSelectizeInput(
                                session = session,
                                inputId = "in_id_imf_vars",
                                choices = NULL,
                                server = TRUE,
                                selected = rv_input$imf_vars)
                        
                        # External data
                        output$in_id_ext_ui <- renderUI({
                                fileInput('in_id_ext', label = "Select file") %>%
                                        shinyInput_label_embed(
                                                shiny_iconlink() %>%
                                                        bs_attach_modal(id_modal = "help_text")
                                        )
                        })
                        
                        
                
        })
        
        dat_wdi <- reactive(
                
                
                
        ) 
        
        
        
        observeEvent(input$in_id_update, {
                
                # Data
                rv_df$dat_wdi <- dat_wdi()
                rv_df$dat_imf <- dat_imf()
                rv_df$dat_ext <- dat_ext()
                # rv_df$dat_all <- BIND
                
                # Metadata
                rv_df$met_wdi <- met_wdi()
                rv_df$met_imf <- met_imf()
                rv_df$met_ext <- met_ext()
                # rv_df$met_all <-  BIND
        })
        
        
        
        
}

shinyApp(ui = ui, server = server)