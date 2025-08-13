# load libraries
library(shiny)
library(shinydashboard)

# Updated Tables
update_table <- data.frame()
flag <- FALSE
IAP_flag <- 0

# Inflation Data Table
IAP_data <- data.frame(
    Year = 2000:2025,
    CPI = c(172.2, 177.1, 179.9, 184.0, 188.9, 195.3, 201.6, 207.3, 215.3, 214.5,
            218.1, 224.9, 229.6, 233.0, 236.7, 237.0, 240.0, 245.1, 251.1, 255.7,
            258.8, 271.0, 292.7, 304.7, 313.7,322.3)
)

# Inflation Adjusted Price Calculation
IAP <- function(plot_data, IAP_data) {
    # Get the base year CPI (2025 = 322.3)
    base_year_cpi <- IAP_data$CPI[IAP_data$Year == max(IAP_data$Year)]

    for (i in 1:nrow(plot_data)) {
        year_index <- which(IAP_data$Year == plot_data$Year[i])
        if (length(year_index) > 0) {
            # Calculate inflation factor
            current_year_cpi <- IAP_data$CPI[year_index]
            inflation_factor <- base_year_cpi / current_year_cpi
            
            # Apply the factor to the ORIGINAL Data (not the already adjusted one)
            plot_data$InflationAdjustedAvgPrice[i] <- plot_data$AvgPrice[i] * inflation_factor
            plot_data$InflationAdjustedMaxPrice[i] <- plot_data$MaxPrice[i] * inflation_factor
            plot_data$InflationAdjustedMinPrice[i] <- plot_data$MinPrice[i] * inflation_factor
        } else { 
            if (IAP_flag == 1) {
                plot_data$InflationAdjustedAvgPrice[i] <- NA
            } else if (IAP_flag == 2) {
                plot_data$InflationAdjustedMaxPrice[i] <- NA
            } else if (IAP_flag == 3) {
                plot_data$InflationAdjustedMinPrice[i] <- NA
            }
        }
    }
    plot_data$InflationAdjustedAvgPrice <- as.numeric(plot_data$InflationAdjustedAvgPrice)
    plot_data$InflationAdjustedMaxPrice <- as.numeric(plot_data$InflationAdjustedMaxPrice)
    plot_data$InflationAdjustedMinPrice <- as.numeric(plot_data$InflationAdjustedMinPrice)
    return(plot_data)
}

# Summary
load_summary <- read.csv("Data/Summary.csv")
data_summary <- data.frame(load_summary)
data_summary$Category <- tolower(data_summary$Category) 
data_summary$Category <- gsub("sawlog","sawlogs", data_summary$Category)
data_summary$Category <- gsub("sawlogss","sawlogs", data_summary$Category)
data_summary$Category <- tools::toTitleCase(tolower(data_summary$Category))
data_summary$Species <- gsub("all_species","all species", data_summary$Species)
data_summary$Species <- tools::toTitleCase(tolower(data_summary$Species))
data_summary$Species <- gsub("all Species", "All species", data_summary$Species)
data_summary$PerUnit <- tools::toTitleCase(tolower(data_summary$PerUnit))

# Tree Species
load_tree_species <- read.csv("Data/Tree_Species_Pricing.csv")
data_tree_species <- data.frame(load_tree_species)
data_tree_species$Species <- tools::toTitleCase(tolower(data_tree_species$Species))
data_tree_species$County <- tools::toTitleCase(tolower(data_tree_species$County))
data_tree_species$Category <- tools::toTitleCase(tolower(data_tree_species$Category))
data_tree_species$PerUnit <- tools::toTitleCase(tolower(data_tree_species$PerUnit))

# Biomass Data
load_biomass <- read.csv("Data/Biomass.csv")
data_biomass <- data.frame(load_biomass)
data_biomass$County <- tools::toTitleCase(tolower(data_biomass$County))
data_biomass$Species <- gsub("all species","all_species", data_biomass$Species)
data_biomass$Species <- gsub("all_species","All species", data_biomass$Species)

# Firewood Data
load_firewood <- read.csv("Data/Firewood.csv")
data_firewood <- data.frame(load_firewood)
data_firewood$County <- tools::toTitleCase(tolower(data_firewood$County))
data_firewood$Species <- gsub("all species","all_species", data_firewood$Species)
data_firewood$Species <- gsub("all_species","All species", data_firewood$Species)

# Palletwood Data
load_palletwood <- read.csv("Data/Palletwood.csv")
data_palletwood <- data.frame(load_palletwood)
data_palletwood$County <- tools::toTitleCase(tolower(data_palletwood$County))
data_palletwood$Species <- gsub("all_species","all species", data_palletwood$Species)
data_palletwood$Species <- tools::toTitleCase(tolower(data_palletwood$Species))
data_palletwood$Species <- gsub("all Species", "All species", data_palletwood$Species)

# Studwood Data
load_studwood <- read.csv("Data/Studwood.csv")
data_studwood <- data.frame(load_studwood)
data_studwood$County <- tools::toTitleCase(tolower(data_studwood$County))
data_studwood$Species <- gsub("spruce_fir","Spruce & Fir", data_studwood$Species)
data_studwood$Species <- gsub("other_species","other species", data_studwood$Species)
data_studwood$Species <- gsub("all_species","all species", data_studwood$Species)
data_studwood$Species <- tools::toTitleCase(tolower(data_studwood$Species))
data_studwood$Species <- gsub("all Species", "All species", data_studwood$Species)

# Create UI 
ui <- dashboardPage(
    skin = "green",
    title = "🪵Stumpage Dashboard",

    dashboardHeader(
        title = tags$div(
            style = "display: flex; align-items: center; text-align: left; width: 100%;",
            icon("tree", class = "fa-lg"),
            span("Stumpage Dashboard", style = "margin-left: 8px; font-size: 18px; ")
        ),
        # Navigation buttons
        tags$li(
            class = "dropdown",
            style = "margin-top: 4px; margin-bottom: 4px;",
            default = TRUE,
            tags$div(
                style = "display: flex; gap: 15px; align-items: center;",
                actionButton("nav_dashboard", "Dashboard", 
                           class = "btn-link",
                           style = "color: white; border: none; background: none; font-size: 18px; padding: 8px 15px;"),
                actionButton("nav_description", "Description", 
                           class = "btn-link",
                           style = "color: white; border: none; background: none; font-size: 18px; padding: 8px 15px;")
            )
        )
    ),
    dashboardSidebar(
        sidebarMenuOutput("menu"),
        # Reference Section Header
        tags$div(
            style = "text-align: center; margin-top: 20px; font-size: 20px; font-weight: bold; color: #ffffff; border-bottom: 2px solid #ffffff; padding-bottom: 10px; margin-bottom: 15px;",
            "Reference Information"
        ),
        
        # Main Reference Content
        tags$div(
            style = "padding: 0 15px; color: #ffffff; font-size: 13px; line-height: 1.4;",
            
            # Stumpage Reports Description
            tags$div(
            style = "margin-bottom: 20px; padding: 10px; background-color: rgba(255,255,255,0.1); border-radius: 5px;",
            tags$p(
                style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px;",
                "Stumpage Reports"
            ),
            tags$p(
                style = "margin: 0 0 10px 0;",
                "These reports contain the annual stumpage prices landowners received by county, product, and species."
            ),
            tags$a(
                href = "https://www.maine.gov/dacf/mfs/publications/annual_reports.html", 
                target = "_blank",
                style = "color: #87CEEB; text-decoration: underline;",
                "Maine Stumpage Reports"
            )
            ),
            
            # County Grouping Changes (Post-2010)
            tags$div(
            style = "margin-bottom: 20px; padding: 10px; background-color: rgba(255,255,255,0.1); border-radius: 5px;",
            tags$p(
                style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px; color: #FFD700;",
                "County Grouping Changes (2010 Onwards)"
            ),
            tags$p(
                style = "margin: 0 0 8px 0;",
                "Starting in 2010, counties were grouped into regions:"
            ),
            tags$div(
                style = "margin-left: 10px;",
                tags$p(
                style = "margin: 5px 0; font-weight: bold;",
                "🌊 CASCO BAY"
                ),
                tags$p(
                style = "margin: 2px 0 10px 15px; font-size: 12px;",
                "Includes: Androscoggin, Cumberland, Sagadahoc, and York counties"
                ),
                tags$p(
                style = "margin: 5px 0; font-weight: bold;",
                "🏛️ CAPITAL AREA"
                ),
                tags$p(
                style = "margin: 2px 0; font-size: 12px;",
                "Includes: Kennebec, Knox, Lincoln, and Waldo counties"
                )
            )
            ),
            
            # Product Category Changes
            tags$div(
            style = "margin-bottom: 20px; padding: 10px; background-color: rgba(255, 255, 255, 0.1); border-radius: 5px;",
            tags$p(
                style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px; color: #98FB98;",
                "Product Category Evolution"
            ),
            # Change in Units
            tags$div(
                style = "margin-bottom: 12px;",
                tags$p(
                style = "margin: 0 0 5px 0; font-weight: bold; font-size: 13px;",
                "⚖️ Change in Units (2000-2023)"
                ),
                tags$p(
                style = "margin: 0 0 5px 10px; font-size: 12px;",
                "Change in standard unit across two product class:"
                ),
                tags$ul(
                style = "margin: 0 0 0 20px; padding: 0;",
                tags$li(style = "margin: 2px 0;", "Boltwood: from 'cord (2000 - 2003)' to 'mbf (2004 - 2023)'"),
                tags$li(style = "margin: 2px 0;", "Pulpwood: from 'cord (2000 - 2003)' to 'ton (2004 - 2023)'"),
                )
            ),
            
            # Palletwood Changes
            tags$div(
                style = "margin-bottom: 12px;",
                tags$p(
                style = "margin: 0 0 5px 0; font-weight: bold; font-size: 13px;",
                "📦 Palletwood (2009+)"
                ),
                tags$p(
                style = "margin: 0 0 5px 10px; font-size: 12px;",
                "Divided into separate categories:"
                ),
                tags$ul(
                style = "margin: 0 0 0 20px; padding: 0;",
                tags$li(style = "margin: 2px 0;", "Hardwood"),
                tags$li(style = "margin: 2px 0;", "Softwood")
                )
            ),
            
            # Studwood Changes
            tags$div(
                tags$p(
                style = "margin: 0 0 5px 0; font-weight: bold; font-size: 13px;",
                "🏗️ Studwood (2005+)"
                ),
                tags$p(
                style = "margin: 0 0 5px 10px; font-size: 12px;",
                "Divided into separate categories:"
                ),
                tags$ul(
                style = "margin: 0 0 0 20px; padding: 0;",
                tags$li(style = "margin: 2px 0;", "Spruce & Fir"),
                tags$li(style = "margin: 2px 0;", "Other Species")
                )
            )
            ),
            
            # Report Structure Changes
            tags$div(
            style = "margin-bottom: 20px; padding: 10px; background-color: rgba(255,255,255,0.1); border-radius: 5px;",
            tags$p(
                style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px; color: #FFA07A;",
                "📊 Report Structure Timeline"
            ),
            tags$div(
                style = "margin-left: 10px;",
                tags$p(
                style = "margin: 5px 0; font-weight: bold; font-size: 13px;",
                "Until 2009:"
                ),
                tags$p(
                style = "margin: 2px 0 10px 15px; font-size: 12px;",
                "Each county had individual report pages"
                ),
                tags$p(
                style = "margin: 5px 0; font-weight: bold; font-size: 13px;",
                "2010-2023:"
                ),
                tags$p(
                style = "margin: 2px 0; font-size: 12px;",
                "Counties grouped into Casco Bay and Capital Area regions, resulting in consistent graphical structure for all counties within each region"
                )
            )
            ),
            
            # Note
            tags$div(
            style = "margin-top: 15px; padding: 8px; background-color: rgba(255,255,0,0.1); border-left: 3px solid #FFD700; font-size: 11px; font-style: italic;",
            "💡 Note: These structural changes affect data visualization and analysis approaches for different time periods."
            )
        )
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML("
                .btn-link:hover {
                    background-color: rgba(255, 255, 255, 0) !important;
                    color: white !important;
                    border-radius: 4px !important;
                }
                .btn-link:focus {
                    background-color: rgba(255, 255, 255, 0) !important;
                    color: white !important;
                    border-radius: 4px !important;
                    box-shadow: none !important;
                }
            "))
        ),
        
        # Content panels
        uiOutput("page_content")
    )
)
# Create Server
server <- function(input, output, session) {
    # Reactive value to track current page
    current_page <- reactiveVal("Dashboard")
    
    # Handle navigation
    observeEvent(input$nav_dashboard, {
        current_page("Dashboard")
    })
    
    observeEvent(input$nav_description, {
        current_page("Description")
    })
    
    # Render content based on current page
    output$page_content <- renderUI({
        if(current_page() == "Description") {
            div(
                tags$h2(
                    style = "margin: 0; padding-bottom: 10px;",
                    strong("Maine Stumpage Price Dashboard Content")
                ),
                
                tags$p("Welcome to the Stumpage Dashboard! This dashboard provides insights into various categories of stumpage data, including different Tree Species and categories like Firewood, Palletwood, Studwood, and Biomass. Use the navigation tabs to explore different datasets and visualize trends."),

                div(style = 'background-color: #f8f9fa; padding: 15px; margin: 20px 0; border-left: 4px solid #007bff;',
                    tags$h4("Reference Information"),
                    tags$p(tags$strong("Source: "), "All prices obtained from the Maine Stumpage Price Reports, 2000-2023"),
                    tags$p(tags$strong("Note: "), "Unit measurements for volume and weight differ by product class and species throughout time. Casco Bay and Capital Region counties data are included in the regional analysis. Please ensure to select at least one value from each filter to display results.")
                ),

                tags$p("Each section allows you to select a county and view the corresponding stumpage data. Please note that you must select at least one value to view the data."),
                tags$p("The dashboard includes a connected scatterplot that visualizes the average, maximum, and minimum prices of selected species over the years. You can also download the data displayed in the table for further analysis."),
                tags$p("The dashboard also includes inflation-adjusted prices for better comparison over time."),
                
                div(style = 'margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;',
                    tags$p(tags$strong("Developed by: "), "Tejas Phanse, Innovate for Maine Fellow, 2025 (", 
                        tags$a(href = 'mailto:tejas.phanse1205@gmail.com', "tejas.phanse1205@gmail.com"), ")"),
                    tags$p(tags$strong("Questions? "), "Contact Matt Russell, Arbor Analytics (", 
                        tags$a(href = 'mailto:matt@arbor-analytics.com', "matt@arbor-analytics.com"), ")")
                ),

                # Logo section with side-by-side layout
                div(style = 'margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 8px;',
                    tags$div(
                    style = "margin: 0; padding-bottom: 10px; text-align: center; font-size: 14px; color: #000000;",
                    strong("Partners")
                    ),
                    div(style = 'display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 20px;',
                        
                        # Arbor Analytics logo - left side
                        div(style = 'flex: 1; min-width: 250px; text-align: center;',
                            tags$img(src = 'https://arbor-analytics.com/images/arbor_horiz.png', 
                                    style = 'max-width: 100%; max-height: 80px; height: auto;', 
                                    alt = 'Arbor Analytics Logo')
                        ),
                        
                        # Innovate for Maine logo - right side
                        div(style = 'flex: 1; min-width: 250px; text-align: center;',
                            tags$img(src = 'https://umaine.edu/innovation/wp-content/uploads/sites/67/2015/11/IFM.jpg', 
                                    style = 'max-width: 100%; max-height: 120px; height: auto;', 
                                    alt = 'Innovate for Maine Logo')
                        )
                    )
                )
            )
        }
        else if(current_page() == "Dashboard") {

            # Main dashboard content
            div(
                # Main dashboard layout
                style = "display: flex; flex-direction: column; gap: 5px;", 

                tags$h2(
                    style = "margin: 0; padding-bottom: 10px;",
                    strong("Stumpage Dashboard from 2000 to 2023")
                ),
                
                div(
                    style = "display: flex; gap: 10px; justify-content: space-between; flex-wrap: wrap;",
                    # Box 1 - Product Class Category
                    div(
                        style = "flex: 1;",
                        box(
                            title = "Select Product Class:",
                            selectInput(
                                "ProductClass", NULL,
                                choices = c(sort(unique(data_summary$Category))),
                                selected = "Sawlogs",
                                width = "100%"
                            ),
                            width = NULL
                        )
                    ),

                    # Bosx 2 - State Summary and County
                    div(
                        style = "flex: 1;",
                        box(
                            title = "Select County:",
                            selectInput(
                                "SWCounty", NULL,
                                choices = c("Statewide", sort(unique(data_tree_species$County))),
                                selected = "Statewide",
                                width = "100%"
                            ),
                            width = NULL 
                        )
                    ),

                    # Box 3 - Species
                    div(
                    style = "flex: 1;",
                    box(
                        title = "Select Species:",
                        selectInput(
                            "Species", NULL,
                            choices = NULL,
                            selected = NULL,
                            width = "100%"
                        ),
                        width = NULL
                        )
                    ),

                    # Connected Scatterplot
                    box(
                        title = "Connected Scatterplot",
                        solidHeader = TRUE,
                        width = 12,
                        div(
                            style = "display: flex; gap: 20px; margin-bottom: 10px; align-items: center; margin-top: -30px;",
                            checkboxInput(
                                "AvgPriceTag", "Average Price",
                                value = TRUE
                            ),
                            checkboxInput(
                                "MaxPriceTag", "Maximum Price",
                                value = FALSE
                            ),
                            checkboxInput(
                                "MinPriceTag", "Minimum Price",
                                value = FALSE
                            ),
                            checkboxInput(
                                "IAP_AvgPrice", "Inflation Adjusted Price on Average Price",
                                value = FALSE,
                            ),
                            checkboxInput(
                                "IAP_MaxPrice", "Inflation Adjusted Price on Maximum Price",
                                value = FALSE,
                            ),
                            checkboxInput(
                                "IAP_MinPrice", "Inflation Adjusted Price on Minimum Price",
                                value = FALSE,
                            )
                        ),
                        plotOutput("ConnectedScatterplot", height = "500px")
                    ),
                ),
                # Graph table
                div(
                    style = "margin-top: 5px;",
                    box(
                        title = "Graph Table",
                        solidHeader = TRUE,
                        width = 12,
                        tableOutput("graphTable")
                    ),
                    # Dowload button
                    box(
                        title = "Download Data",
                        solidHeader = TRUE,
                        width = 12,
                        downloadButton("downloadData", "Download Data", class = "btn-primary"),
                        tags$div(
                            style = "margin-top: 10px; font-size: 14px; color: #000000;",
                            "Click the button to download the data displayed in the table above."
                        ),
                        tags$div(
                            style = "margin-top: 10px; font-size: 14px; color: #000000;",
                            "Download Original Data Files by clicking on the links below:",
                           tags$ul(
                                tags$li(downloadLink("download_summary", "Summary Data")),
                                tags$li(downloadLink("download_tree_species", "Tree Species Pricing Data")), 
                                tags$li(downloadLink("download_biomass", "Biomass Data")),
                                tags$li(downloadLink("download_firewood", "Firewood Data")),
                                tags$li(downloadLink("download_palletwood", "Palletwood Data")),
                                tags$li(downloadLink("download_studwood", "Studwood Data"))
                            )
                        )

                    )

                ),
            )

        }
    })

    # Update Species input based on selected Product Class
    observeEvent(input$ProductClass, {
        if (input$ProductClass == "Biomass"){
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_biomass$Species)),
                              selected = sort(unique(data_biomass$Species))[1])
        } 
        else if (input$ProductClass == "Firewood") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_firewood$Species)),
                              selected = sort(unique(data_firewood$Species))[1])
        } 
        else if (input$ProductClass == "Palletwood") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_palletwood$Species)),
                              selected = sort(unique(data_palletwood$Species))[1])
        } 
        else if (input$ProductClass == "Studwood") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_studwood$Species)),
                              selected = sort(unique(data_studwood$Species))[1])
        } 
        else if (input$ProductClass == "Boltwood") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_tree_species$Species[data_tree_species$Category == "Boltwood" ])),
                              selected = sort(unique(data_tree_species$Species[data_tree_species$Category == "Boltwood"]))[1])
        }
        else if (input$ProductClass == "Sawlogs") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_tree_species$Species[data_tree_species$Category == "Sawlogs"])),
                              selected = "White Pine")
        } 
        else if (input$ProductClass == "Veneer") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_tree_species$Species[data_tree_species$Category == "Veneer"])),
                              selected = sort(unique(data_tree_species$Species[data_tree_species$Category == "Veneer"]))[1])
        } 
        else if (input$ProductClass == "Pulpwood") {
            updateSelectInput(session, "Species", 
                              choices = sort(unique(data_tree_species$Species[data_tree_species$Category == "Pulpwood"])),
                              selected = sort(unique(data_tree_species$Species[data_tree_species$Category == "Pulpwood"]))[1])
        }
    })

    # Connected Scatterplot
    output$ConnectedScatterplot <- renderPlot({
        req (input$ProductClass, input$Species, input$SWCounty)
        if (input$ProductClass == "Biomass"){
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Biomass" & data_summary$Species == input$Species, ]
            } else {
                plot_data <- data_biomass[data_biomass$County == input$SWCounty & data_biomass$Species == input$Species, ]
            }
        }
        else if (input$ProductClass == "Firewood") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Firewood" & data_summary$Species == input$Species, ]
            } else {
                plot_data <- data_firewood[data_firewood$County == input$SWCounty & data_firewood$Species == input$Species, ]
            }
        } 
        else if (input$ProductClass == "Palletwood") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Palletwood" & data_summary$Species == input$Species, ]
            } else {
                plot_data <- data_palletwood[data_palletwood$County == input$SWCounty & data_palletwood$Species == input$Species, ]
            }
        } 
        else if (input$ProductClass == "Studwood") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Studwood" & data_summary$Species == input$Species, ]
            } else {
                plot_data <- data_studwood[data_studwood$County == input$SWCounty & data_studwood$Species == input$Species, ]
            }
        } 
        else if (input$ProductClass == "Boltwood") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Boltwood" & data_summary$Species == input$Species, ]
            }
            else {
                plot_data <- data_tree_species[data_tree_species$County == input$SWCounty & 
                                               data_tree_species$Species == input$Species & 
                                               data_tree_species$Category == "Boltwood", ]
            }
        }
        else if (input$ProductClass == "Sawlogs") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Sawlogs" & data_summary$Species == input$Species, ]
            }
            else {
                plot_data <- data_tree_species[data_tree_species$County == input$SWCounty & 
                                               data_tree_species$Species == input$Species & 
                                               data_tree_species$Category == "Sawlogs", ]
            }
        } 
        else if (input$ProductClass == "Veneer") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Veneer" & data_summary$Species == input$Species, ]
            }
            else {
                plot_data <- data_tree_species[data_tree_species$County == input$SWCounty & 
                                               data_tree_species$Species == input$Species & 
                                               data_tree_species$Category == "Veneer", ]
            }
        } 
        else if (input$ProductClass == "Pulpwood") {
            if (input$SWCounty == "Statewide"){
                plot_data <- data_summary[data_summary$Category == "Pulpwood" & data_summary$Species == input$Species, ]
            }
            else {
                plot_data <- data_tree_species[data_tree_species$County == input$SWCounty & 
                                               data_tree_species$Species == input$Species & 
                                               data_tree_species$Category == "Pulpwood", ]
            }
        }

        # Check Tag Conditions
        req(input$AvgPriceTag || input$MaxPriceTag || input$MinPriceTag)
    
        # Clean and convert data to numeric
        plot_data_clean <- plot_data
        plot_data_clean$Year <- as.numeric(as.character(plot_data_clean$Year))
        plot_data_clean$AvgPrice <- as.numeric(as.character(plot_data_clean$AvgPrice))
        plot_data_clean$MaxPrice <- as.numeric(as.character(plot_data_clean$MaxPrice))
        plot_data_clean$MinPrice <- as.numeric(as.character(plot_data_clean$MinPrice))
        
        # Remove rows with NA in Year (essential for plotting)
        plot_data_clean <- plot_data_clean[!is.na(plot_data_clean$Year), ]
        
        # Check if we still have data
        if(nrow(plot_data_clean) == 0) {
            plot(1, 1, type = "n", xlab = "Year", ylab = "Price ($)", 
                main = "No valid data to display")
            text(1, 1, "No valid numeric data available", cex = 1.2)
            flag <<- TRUE
            update_table <<- data.frame(Year = NA, County = NA, Category=NA, Species=NA, PerUnit=NA, AvgPrice = NA, MaxPrice = NA,
                                        MinPrice = NA, InflationAdjustedAvgPrice = NA, NumOfRecords = 0)
            return()
        }
        
        # Reset flag
        flag <<- FALSE

        # Inflation adjusted average price column
        plot_data_clean$InflationAdjustedAvgPrice <- plot_data_clean$AvgPrice
        plot_data_clean$InflationAdjustedMaxPrice <- plot_data_clean$MaxPrice
        plot_data_clean$InflationAdjustedMinPrice <- plot_data_clean$MinPrice
        if (input$IAP_AvgPrice) {
            IAP_flag <<- 1
        } else if (input$IAP_MaxPrice) {
            IAP_flag <<- 2
        } else if (input$IAP_MinPrice) {
            IAP_flag <<- 3
        }
        plot_data_clean <- IAP(plot_data_clean, IAP_data)
    
        # Calculate y limits only from selected and valid data
        y_values <- c()
        if(input$AvgPriceTag) y_values <- c(y_values, plot_data_clean$AvgPrice[!is.na(plot_data_clean$AvgPrice)])
        if(input$MaxPriceTag) y_values <- c(y_values, plot_data_clean$MaxPrice[!is.na(plot_data_clean$MaxPrice)])
        if(input$MinPriceTag) y_values <- c(y_values, plot_data_clean$MinPrice[!is.na(plot_data_clean$MinPrice)])
        if(input$IAP_AvgPrice) y_values <- c(y_values, plot_data_clean$InflationAdjustedAvgPrice[!is.na(plot_data_clean$InflationAdjustedAvgPrice)])
        if(input$IAP_MaxPrice) y_values <- c(y_values, plot_data_clean$InflationAdjustedMaxPrice[!is.na(plot_data_clean$InflationAdjustedMaxPrice)])
        if(input$IAP_MinPrice) y_values <- c(y_values, plot_data_clean$InflationAdjustedMinPrice[!is.na(plot_data_clean$InflationAdjustedMinPrice)])


        # Calculate limits with proper padding
        y_max <- max(y_values, na.rm = TRUE)
        y_min <- 0 

        # Add 10% padding to the top for better visualization
        y_padding <- y_max * 0.1
        y_limits <- c(y_min, y_max + y_padding)

        units <- unique(plot_data_clean$PerUnit)

        # Create title with unit-specific year ranges
        if (length(units) == 1) {
            unit_data <- plot_data_clean[plot_data_clean$PerUnit == units[1], ]
            min_year <- min(unit_data$Year, na.rm = TRUE)
            max_year <- max(unit_data$Year, na.rm = TRUE)
            title_text <- paste("Price of", input$Species, "per", units[1], "in", input$SWCounty, "from", min_year, "to", max_year)
        } else {
            # Handle multiple units
            unit_ranges <- sapply(units, function(unit) {
                unit_data <- plot_data_clean[plot_data_clean$PerUnit == unit, ]
                min_year <- min(unit_data$Year, na.rm = TRUE)
                max_year <- max(unit_data$Year, na.rm = TRUE)
                paste(unit, "(", min_year, "-", max_year, ")")
            })
            title_text <- paste("Price of", input$Species, "in", input$SWCounty, "per Unit:", paste(unit_ranges, collapse = ", "))
        }

        plot(c(2000, 2024), y_limits,
            type = "n", xlab = "Year", ylab = "Price ($)",
            main = title_text,
            ylim = y_limits,
            xlim = c(2000, 2024),
            xaxt = "n")
        
        # Add lines only if data is valid
        if (input$AvgPriceTag && any(!is.na(plot_data_clean$AvgPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$AvgPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$AvgPrice[valid_indices], 
                    type = "o", col = "#0000FF", pch = 19, lwd = 2)
            }
        }
        
        if (input$MaxPriceTag && any(!is.na(plot_data_clean$MaxPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$MaxPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$MaxPrice[valid_indices], 
                    type = "o", col = "#FF0000", pch = 17, lwd = 2)
            }
        }
        
        if (input$MinPriceTag && any(!is.na(plot_data_clean$MinPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$MinPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$MinPrice[valid_indices], 
                    type = "o", col = "#008000", pch = 15, lwd = 2)
            }
        }
        if (input$IAP_AvgPrice && any(!is.na(plot_data_clean$InflationAdjustedAvgPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$InflationAdjustedAvgPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$InflationAdjustedAvgPrice[valid_indices], 
                    type = "o", col = "#000000", pch = 17, lwd = 2)
            }
        }
        if (input$IAP_MaxPrice && any(!is.na(plot_data_clean$InflationAdjustedMaxPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$InflationAdjustedMaxPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$InflationAdjustedMaxPrice[valid_indices], 
                    type = "o", col = "#00664bff", pch = 17, lwd = 2)
            }
        }
        if (input$IAP_MinPrice && any(!is.na(plot_data_clean$InflationAdjustedMinPrice))) {
            valid_indices <- !is.na(plot_data_clean$Year) & !is.na(plot_data_clean$InflationAdjustedMinPrice)
            if(any(valid_indices)) {
                lines(plot_data_clean$Year[valid_indices], plot_data_clean$InflationAdjustedMinPrice[valid_indices], 
                    type = "o", col = "#f6b000ff", pch = 17, lwd = 2)
            }
        }
        
        # Add grid lines
        axis(1, at = 2000:2024, labels = 2000:2024, las = 1)
        grid(col = "lightgray", lty = "dotted")

        
        # Add legend (only for valid data)
        legend_items <- c()
        legend_colors <- c()
        legend_pch <- c()
        
        if (input$AvgPriceTag && any(!is.na(plot_data_clean$AvgPrice))) {
            legend_items <- c(legend_items, "Average Price")
            legend_colors <- c(legend_colors, "#0000FF")
            legend_pch <- c(legend_pch, 19)
        }
        if (input$MaxPriceTag && any(!is.na(plot_data_clean$MaxPrice))) {
            legend_items <- c(legend_items, "Maximum Price")
            legend_colors <- c(legend_colors, "#FF0000")
            legend_pch <- c(legend_pch, 17)
        }
        if (input$MinPriceTag && any(!is.na(plot_data_clean$MinPrice))) {
            legend_items <- c(legend_items, "Minimum Price")
            legend_colors <- c(legend_colors, "#008000")
            legend_pch <- c(legend_pch, 15)
        }
        if (input$IAP_AvgPrice && any(!is.na(plot_data_clean$InflationAdjustedAvgPrice))) {
            legend_items <- c(legend_items, "Inflation Adjusted Avg Price")
            legend_colors <- c(legend_colors, "#000000")
            legend_pch <- c(legend_pch, 17)
        }
        if (input$IAP_MaxPrice && any(!is.na(plot_data_clean$InflationAdjustedMaxPrice))) {
            legend_items <- c(legend_items, "Inflation Adjusted Max Price")
            legend_colors <- c(legend_colors, "#00664bff")
            legend_pch <- c(legend_pch, 17)
        }
        if (input$IAP_MinPrice && any(!is.na(plot_data_clean$InflationAdjustedMinPrice))) {
            legend_items <- c(legend_items, "Inflation Adjusted Min Price")
            legend_colors <- c(legend_colors, "#f6b000ff")
            legend_pch <- c(legend_pch, 17)
        }  
        
        if (length(legend_items) > 0) {
            legend("topleft", legend = legend_items, 
                col = legend_colors, pch = legend_pch, 
                lwd = 2, bg = "white")
        }

        update_table <<- plot_data_clean

    })

    # Render graph table
    output$graphTable <- renderTable({
        req(input$ProductClass, input$Species, input$SWCounty)
        
        # Cross check if we have valid data
        if(flag == TRUE) {
            return(data.frame(Year = NA, County = NA, Category=NA,Species=NA, PerUnit=NA,  AvgPrice = NA, MaxPrice = NA,
                              MinPrice = NA, IA_AvgPrice = NA, IA_MaxPrice = NA, IA_MinPrice = NA, NumOfRecords = 0))
        }
        else{
            # Display the updated table
            table_data <- data.frame(
                Year = as.character(update_table$Year),
                County = input$SWCounty,
                Category = input$ProductClass,
                Species = input$Species,
                PerUnit = update_table$PerUnit,
                AvgPrice = if(input$AvgPriceTag) update_table$AvgPrice else NA,
                MaxPrice = if(input$MaxPriceTag) update_table$MaxPrice else NA,
                MinPrice = if(input$MinPriceTag) update_table$MinPrice else NA,
                IA_AvgPrice = if(input$IAP_AvgPrice) update_table$InflationAdjustedAvgPrice else NA,
                IA_MaxPrice = if(input$IAP_MaxPrice) update_table$InflationAdjustedMaxPrice else NA,
                IA_MinPrice = if(input$IAP_MinPrice) update_table$InflationAdjustedMinPrice else NA,
                NumOfRecords = update_table$NumReports
            )
            return(table_data)
        }

    })
    # Download Data
    output$downloadData <- downloadHandler(
        filename = function() {
                paste("stumpage_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        # Check if data exists when download is triggered
        if (exists("update_table") && nrow(update_table) > 0 && !all(is.na(update_table$Year))) {
            write.csv(update_table, file, row.names = FALSE)
        } else {            
            # Show notification
            showNotification("No data available to download.", 
                           type = "error", 
                           duration = 5)
            return()
        }
    })

    # Download Original Data Files
    output$download_summary <- downloadHandler(
        filename = function() {
            "Summary.csv"
        },
        content = function(file) {
            write.csv(data_summary, file, row.names = FALSE) 
        }
    )
    output$download_tree_species <- downloadHandler(
        filename = function() {
            "Tree_Species_Pricing.csv"
        },
        content = function(file) {
            write.csv(data_tree_species, file, row.names = FALSE) 
        }
    )
    output$download_biomass <- downloadHandler(
        filename = function() {
            "Biomass.csv"
        },
        content = function(file) {
            write.csv(data_biomass, file, row.names = FALSE) 
        }
    )
    output$download_firewood <- downloadHandler(
        filename = function() {
            "Firewood.csv"
        },
        content = function(file) {
            write.csv(data_firewood, file, row.names = FALSE)
        }
    )
    output$download_palletwood <- downloadHandler(
        filename = function() {
            "Palletwood.csv"
        },
        content = function(file) {
            write.csv(data_palletwood, file, row.names = FALSE)
        }
    )
    output$download_studwood <- downloadHandler(
        filename = function() {
            "Studwood.csv"
        },
        content = function(file) {
            write.csv(data_studwood, file, row.names = FALSE)
        }
    )

}

shiny::runApp(
    shinyApp(ui = ui, server = server)
)