# ============================================================
# Washington Water Use Explorer
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(DT)
library(tibble)
library(htmltools)
library(shinycssloaders)


GAL_PER_ACRE_FT <- 325851.42857143

# Days per calendar month (Feb = 28.25 accounts for leap years)
DAYS_IN_MONTH <- c(
  "01" = 31, "02" = 28.25, "03" = 31, "04" = 30,
  "05" = 31, "06" = 30,    "07" = 31, "08" = 31,
  "09" = 30, "10" = 31,    "11" = 30, "12" = 31
)

# Water year order Oct -> Sep
WY_MONTH_ORDER  <- c("10","11","12","01","02","03","04","05","06","07","08","09")
WY_MONTH_LABELS <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")

# ---- Variable definitions ----
nwaa_vars <- tribble(
  ~label,                                     ~model_id,              ~var_id,
  "Public supply - Total withdrawals",        "wu-public-supply-wd",  "pswdtot",
  "Public supply - Groundwater withdrawals",  "wu-public-supply-wd",  "pswdgw",
  "Public supply - Surface water withdrawals","wu-public-supply-wd",  "pswdsw",
  "Irrigation - Total withdrawals",           "wu-irrigation-wd",     "irrwdtot",
  "Irrigation - Surface water withdrawals",   "wu-irrigation-wd",     "irrwdsw",
  "Irrigation - Consumptive use",             "wu-irrigation-cu",     "irrcutot"
)

# ---- Load data ----
# NWAA water-use data, pre-downloaded by download_nwaa_wa.R
nwaa_local     <- readRDS("nwaa_wa_2010_2020.rds")
wbd_huc12_geom <- readRDS("wbd_huc12_wa_geom.rds")
wbd_huc8_geom  <- readRDS("wbd_huc8_wa_geom.rds")
wa_border <- st_read(
  "https://github.com/jeffmarti/snodas-huc8/raw/main/cache/WA_State_Boundary.geojson",
  quiet = TRUE
) %>% st_transform(4326)
wa_huc8        <- read.csv("wa_huc8_lookup.csv", stringsAsFactors = FALSE)
wbd_huc8_geom <- wbd_huc8_geom %>%
  left_join(
    wa_huc8 %>% mutate(huc8 = as.character(huc8)) %>% select(huc8, huc8_label),
    by = "huc8"
  )

# ---- Helper functions ----

# Pre-aggregate monthly data to basin totals by year-month
# Runs once at fetch time so the pivot renders instantly
aggregate_monthly <- function(monthly_df) {
  monthly_df %>%
    mutate(month = substr(year_month, 6, 7)) %>%
    group_by(water_use_type, year_month, month) %>%
    summarise(mgd = sum(mgd, na.rm = TRUE), .groups = "drop") %>%
    group_by(water_use_type, month) %>%
    summarise(mgd = mean(mgd, na.rm = TRUE), .groups = "drop")
}

# Pull one variable for one HUC8 out of the pre-downloaded NWAA tables
# (built by download_nwaa_wa.R). Returns the same columns the API did:
# huc12_id, <year|year_month>, <var_id>_mgd
local_nwaa_huc8 <- function(tbl, var_id, huc8) {
  val_col  <- paste0(var_id, "_mgd")
  time_col <- intersect(c("year_month", "year"), names(tbl))
  tbl %>%
    filter(substr(huc12_id, 1, 8) == huc8, !is.na(.data[[val_col]])) %>%
    select(huc12_id, all_of(time_col), all_of(val_col))
}

# Annual summary: one row per HUC12 x water use type
summarise_huc12_annual <- function(df, var_id, var_label) {
  val_col <- names(df)[str_detect(names(df), paste0("^", var_id))][1]
  
  huc12_names <- wbd_huc12_geom %>%
    st_drop_geometry() %>%
    select(huc12, name) %>%
    mutate(huc12 = as.character(huc12))
  
  df %>%
    mutate(huc12_id = as.character(huc12_id)) %>%
    group_by(huc12_id) %>%
    summarise(
      mean_mgd = mean(.data[[val_col]], na.rm = TRUE),
      acre_ft  = mean_mgd * 365.25 * 1e6 / GAL_PER_ACRE_FT,
      .groups  = "drop"
    ) %>%
    left_join(huc12_names, by = c("huc12_id" = "huc12")) %>%
    mutate(water_use_type = var_label) %>%
    select(
      `HUC12`                 = huc12_id,
      `Watershed Name`        = name,
      `Water Use Type`        = water_use_type,
      `Mean Annual (acre-ft)` = acre_ft,
      `Mean Annual (MGD)`     = mean_mgd
    ) %>%
    mutate(
      `Mean Annual (acre-ft)` = round(`Mean Annual (acre-ft)`, 1),
      `Mean Annual (MGD)`     = round(`Mean Annual (MGD)`,     3)
    )
}
# Annual summary: one row per water use type for the entire HUC-8
summarise_huc8_annual <- function(annual_df, huc8_id) {
  
  huc8_name <- wa_huc8 %>%
    mutate(huc8 = as.character(huc8)) %>%
    filter(huc8 == huc8_id) %>%
    pull(huc8_label)
  
  annual_df %>%
    group_by(`Water Use Type`) %>%
    summarise(
      `Mean Annual (acre-ft)` = round(sum(`Mean Annual (acre-ft)`, na.rm = TRUE), 1),
      `Mean Annual (MGD)`     = round(sum(`Mean Annual (MGD)`,     na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    mutate(
      `HUC-8`          = huc8_id,
      `Watershed Name` = huc8_name
    ) %>%
    select(
      `HUC-8`,
      `Watershed Name`,
      `Water Use Type`,
      `Mean Annual (acre-ft)`,
      `Mean Annual (MGD)`
    ) %>%
    # Sort: totals first, then subdivisions
    arrange(
      desc(`Water Use Type` %in% c(
        "Public supply - Total withdrawals",
        "Irrigation - Total withdrawals"
      )),
      `Water Use Type`
    )
}

# Monthly pivot: one row per water use type, columns = water year months
make_monthly_pivot <- function(agg_df, units) {
  
  agg_df %>%
    mutate(
      value = if (units == "acre_ft") {
        mgd * DAYS_IN_MONTH[month] * 1e6 / GAL_PER_ACRE_FT
      } else {
        mgd
      }
    ) %>%
    mutate(month = factor(month, levels = WY_MONTH_ORDER)) %>%
    select(water_use_type, month, value) %>%
    pivot_wider(names_from = month, values_from = value) %>%
    rename(`Water Use Type` = water_use_type) %>%
    rename_with(~ WY_MONTH_LABELS, all_of(WY_MONTH_ORDER)) %>%
    mutate(
      `Annual Total` = if (units == "acre_ft") {
        rowSums(select(., all_of(WY_MONTH_LABELS)), na.rm = TRUE)
      } else {
        rowMeans(select(., all_of(WY_MONTH_LABELS)), na.rm = TRUE)
      }
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
}

ui <- fluidPage(
  tags$style(HTML("
    details > summary {
      cursor: pointer;
      list-style: disclosure-closed;
    }
    details[open] > summary {
      list-style: disclosure-open;
    }
  ")),
  
  tags$div(
    style = paste(
      "background: linear-gradient(135deg, #1a4a6e, #2980b9);",
      "color: white;",
      "padding: 18px 24px;",
      "margin-bottom: 16px;",
      "border-radius: 4px;"
    ),
    tags$h2(
      "Washington Water Use Explorer",
      style = "margin: 0 0 6px 0; font-size: 24px; font-weight: 700;"
    ),
    tags$p(
      paste(
        "A tool for exploring modeled water use data from the",
        "USGS National Water Availability Assessment Data Companion (NWDC)"
      ),
      style = "margin: 0; font-size: 13px; opacity: 0.88;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("huc8", "Select a major watershed (HUC-8) from the below list or by clicking the map",
                  choices = setNames(wa_huc8$huc8, wa_huc8$huc8_label)),
    
      hr(),                          # <-- inside sidebarPanel
      tags$details(                  # <-- inside sidebarPanel
        tags$summary(tags$strong("\u25b6 Water Use Variables")),
        tags$dl(
          style = "font-size: 12px; margin-top: 8px;",
          tags$dt(tags$strong("Public Supply — Total Withdrawals")),
          tags$dd("Water withdrawn from ground and surface sources by public and private water systems for delivery to cities, towns, rural water districts, and other community water systems serving at least 25 people or 15 connections."),
          tags$dt(tags$strong("Public Supply — Groundwater Withdrawals")),
          tags$dd("The portion of total public supply withdrawals sourced specifically from groundwater."),
          tags$dt(tags$strong("Public Supply — Surface Water Withdrawals")),
          tags$dd("The portion of total public supply withdrawals sourced specifically from surface water such as rivers, lakes, and reservoirs."),
          tags$dt(tags$strong("Irrigation — Total Withdrawals")),
          tags$dd("Water applied by an irrigation system to sustain plant growth in agricultural and horticultural settings. Includes pre-irrigation, frost protection, chemical application, weed control, field preparation, crop cooling, harvesting, dust suppression, and leaching of salts from the root zone."),
          tags$dt(tags$strong("Irrigation — Surface Water Withdrawals")),
          tags$dd("The portion of total irrigation withdrawals sourced specifically from surface water. In Washington State, surface water is the dominant source for crop irrigation, particularly in eastern Washington."),
          tags$dt(tags$strong("Irrigation — Consumptive Use")),
          tags$dd("The portion of irrigation water permanently removed from the local water environment — primarily through crop evapotranspiration — and therefore not available for return to streams or groundwater.")
        )
      ),
      br(),
      tags$details(
        tags$summary(tags$strong("\u25b6 Key Terms")),
        tags$dl(
          style = "font-size: 12px; margin-top: 8px;",
          tags$dt(tags$strong("Withdrawal")),
          tags$dd("The quantity of water removed from a groundwater source or diverted from a surface-water source to the point of use."),
          tags$dt(tags$strong("Consumptive use")),
          tags$dd("The portion of withdrawn water that evaporates, transpires, is incorporated into products or crops, or is otherwise permanently removed from the immediate water environment."),
          tags$dt(tags$strong("Return flow")),
          tags$dd("The quantity of water discharged after use or wastewater treatment back to surface water or groundwater, where it becomes available for reuse."),
          tags$dt(tags$strong("HUC-8 / HUC-12")),
          tags$dd("Hydrologic Unit Codes at the subbasin (8-digit) and subwatershed (12-digit) level. Washington State contains 71 HUC-8 basins, each subdivided into multiple HUC-12 subwatersheds."),
          tags$dt(tags$strong("MGD")),
          tags$dd("Millions of gallons per day, the unit in which the NWAA API reports water use rates."),
          tags$dt(tags$strong("Acre-feet")),
          tags$dd("A volume unit common in the western U.S. One acre-foot equals approximately 325,851 gallons.")
        )
      )
    ),  
    mainPanel(                           # <-- was missing entirely
      withSpinner(
        leafletOutput("map", height = 450),
        type  = 4,        # spinning circle style
        color = "#2980b9" # matches your title banner blue
      ),
      tags$p(
        style = "font-size: 12px; color: #555; margin-top: 6px;",
        "HUC-12 map colors represent the sum of ",
        tags$em("Public Supply — Total Withdrawals"),
        " and ",
        tags$em("Irrigation — Total Withdrawals"),
        ", which together represent the two major withdrawal categories."
      ),
      hr(),
      radioButtons("units", "Choose Display Units",
                   choices  = c("Acre-feet (acre-ft)" = "acre_ft",
                                "Million gallons per day (MGD)" = "mgd"),
                   selected = "acre_ft",
                   inline   = TRUE),    # <-- horizontal layout saves vertical space
      tabsetPanel(
        id = "tabs",
        
        # ---- Tab 1 ----
        tabPanel(
          "Annual Summary (HUC\u201112)",
          br(),
          DTOutput("tbl_annual"),
          br(),
          downloadButton("download_monthly", "Download Monthly Data (CSV)")
        ),
        
        # ---- Tab 2 ----
        tabPanel(
          "Annual Summary (HUC\u20118)",
          br(),
          DTOutput("tbl_huc8_annual"),
          br(),
          downloadButton("download_huc8_annual", "Download HUC-8 Summary (CSV)")
        ),
        
        # ---- Tab 3 ----
        tabPanel(
          "Monthly Pivot (HUC\u20118)",
          br(),
          DTOutput("tbl_monthly_pivot")
        ),
        
        # ---- Tab 4: NEW ----
        tabPanel(
          "Irrigation Data Quality",
          br(),
          tags$div(
            style = "max-width: 860px;",
            
            tags$h4("How Well Does the NWAA Model Detect Irrigation in Washington?"),
            
            tags$p(
              "The irrigation water use estimates in this application are modeled using ",
              "the USGS ",
              tags$a("Landsat-based Irrigation Dataset (LANID)",
                     href   = "https://www.usgs.gov/data/lanid-landsat-based-irrigation-dataset-conterminous-us-1997-2017",
                     target = "_blank"),
              ", which uses satellite imagery to detect actively irrigated fields. ",
              "However, the ability of satellite-based algorithms to detect irrigation varies ",
              "significantly depending on landscape type. ",
              "Washington State is characterized by two contrasting landscapes: the arid, irrigated ",
              "agricultural lands of eastern Washington and the wetter, forested lands of western Washington. ",
              "In eastern Washington, the stark green-on-brown contrast between irrigated fields ",
              "and dry, unirrigated land makes irrigation relatively easy to detect from space. ",
              "In western Washington, the naturally green landscape with abundant pasture and forest edge ",
              "makes it much harder for satellite algorithms to distinguish irrigated fields from non-irrigated land. ",
              "This leads to significant undercounting of irrigation demand in western Washington basins."
            ),
            
            tags$p(
              "The Columbia River Plateau and Snake River Plain Aquifer were both among the regions used to train ",
              "the LANID model algorithms for the detection of irrigation (",
              tags$a("Xie et al., 2019",
                     href   = "https://www.sciencedirect.com/science/article/am/pii/S0924271619301728",
                     target = "_blank"),
              "). ",
              "These areas represent some of the most heavily irrigated regions in the country, ",
              "so it is understandable that they were prioritized for model training. However, the LANID model ",
              "was not trained on landscapes like those found in western Washington, which are characterized by ",
              "a mix of forest, pasture, and smaller-scale agriculture with less distinct spectral signatures. ",
              "This lack of training data from wetter, more heterogeneous landscapes likely contributes to the ",
              "systematic under-detection of irrigation in western Washington basins."
            ),
           
            tags$div(
              style = "background: #fff8e1; border-left: 4px solid #f0a500;
           padding: 12px 16px; margin: 16px 0; border-radius: 2px;",
              tags$strong("\u26a0 Key finding: "),
              tags$p(
                style = "margin: 8px 0 4px 0;",
                tags$strong("Measured by number of fields: "),
                "LANID detects approximately 77% of irrigated fields in eastern Washington ",
                "and 39% in western Washington."
              ),
              tags$p(
                style = "margin: 4px 0;",
                tags$strong("Measured by irrigated acreage: "),
                "LANID detects approximately 81% of irrigated acreage in eastern Washington ",
                "and 38% in western Washington, with a statewide average detection rate of 77% — ",
                "represented by the dashed reference line in the chart below."
              )
            ),
            tags$h5("How Was This Measured?"),
            tags$p(
              "The ",
              tags$a("LANID 2020 coverage",
                     href   = "https://data.usgs.gov/datacatalog/data/USGS:63b5dbd9d34e92aad3caa517",
                     target = "_blank"),
              " was compared to the ",
              tags$a("Washington State Department of Agriculture (WSDA) Crop Distribution layer",
                     href   = "https://agr.wa.gov/departments/land-and-water/natural-resources/agricultural-land-use",
                     target = "_blank"),
              ". This layer, which is based on a combination of remote sensing, survey, and field observations, ",
              "contains an attribute indicating the presence and manner of irrigation. ",
              "The WSDA layer was spatially compared against LANID 2020 using zonal statistics in QGIS, ",
              "across approximately 92,000 irrigated fields statewide after filtering out non-irrigated records. ",
              "The results suggest substantial underestimates of irrigated acreage and, consequently, ",
              "irrigation water use in the NWAA model — particularly in western Washington, ",
              "where the model detects less than half of the irrigated area reported by WSDA."
            ),
            
            
            tags$p(
              "This analysis compared both the number of irrigated fields and total irrigated acreage ",
              "detected by LANID against those reported by WSDA. The two metrics tell a consistent story ",
              "but differ slightly in magnitude, reflecting the fact that field sizes vary considerably ",
              "across the state. The close agreement between the two metrics strengthens the finding — ",
              "it indicates the result is not an artifact of a few very large or very small fields ",
              "skewing one measure."
            ),
            
          
            tags$h5("LANID Detection Rate by County"),
            tags$p(
              style = "font-size: 12px; color: #555;",
              "Hover over any bar for LANID and WSDA acreage details. ",
              "Orange bars = eastern WA counties; blue bars = western WA counties. ",
              "Dashed line = statewide average detection rate."
            ),
            tags$iframe(
              src    = "irrigation_detection_rate_by_county.html",
              width  = "100%",
              height = "1380px",
              style  = "border: none;"
            ),
            
            tags$div(
              style = "background: #e8f4f8; border-left: 4px solid #2980b9;
               padding: 12px 16px; margin: 16px 0; border-radius: 2px;",
              tags$strong("\u2139 A note on data vintage: "),
              "The LANID dataset used here reflects 2020 imagery. The WSDA Crop Distribution ",
              "layer reflects 2024 reported acreage. Year-to-year variability in cropped area ",
              "means a small portion of the detection gap may reflect genuine changes in ",
              "irrigated acreage between those years, rather than model limitations alone."
            ),
            
            br(),
            tags$hr(),
            tags$p(
              style = "font-size: 12px; color: #777;",
              tags$em(
                "Analysis by Jeff Marti. Data sources: ",
                tags$a("WSDA 2024 Crop Distribution",
                       href   = "https://agr.wa.gov/departments/land-and-water/natural-resources/agricultural-land-use",
                       target = "_blank"),
                "; ",
                tags$a("LANID 2020 (Landsat Irrigation Dataset, USGS)",
                       href   = "https://data.usgs.gov/datacatalog/data/USGS:63b5dbd9d34e92aad3caa517",
                       target = "_blank"),
                ". Analysis performed in QGIS using zonal statistics."
              )
            )
          )  # closes tags$div max-width
        ),   # closes tabPanel "Irrigation Data Quality"
        # ---- Tab 5 ----
        tabPanel(
          "About",
          br(),
          tags$div(
            style = "max-width: 860px;",
            tags$h4("About This Application"),
            tags$p("Washington Water Use Explorer retrieves modeled water use estimates from the U.S. Geological Survey's ",
                   tags$a("National Water Availability Assessment (NWAA) Data Companion",
                          href   = "https://water.usgs.gov/nwaa-data/",
                          target = "_blank"),
                   " (data downloaded from the NWAA REST API, api.water.usgs.gov/nwaa-data/data, on ",
                   format(nwaa_local$downloaded, "%B %d, %Y"), ")."
            ),
            tags$p("The National Water Availability Assessment Data Companion (NWDC) is a centralized website providing ",
                   "U.S. Geological Survey (USGS) model-derived water availability, supply, and use information that underlies the ",
                   tags$a("National Water Availability Assessment Report",
                          href   = "https://www.usgs.gov/special-topics/integrated-water-availability-assessments/national-water-availability-assessment",
                          target = "_blank"),
                   "."
            ),
            tags$p("When a user selects a HUC-8 watershed, the application queries the API for all HUC-12 subwatersheds ",
                   "within that basin across six water use variables, covering the period 2010-2020. Both annual and monthly ",
                   "data are retrieved. Annual values displayed in the table and map represent the mean daily withdrawal rate ",
                   "averaged across all years of record, expressed in either acre-feet per year or millions of gallons per day ",
                   "(MGD). Monthly values in the pivot table represent the mean daily withdrawal rate for each calendar month ",
                   "averaged across all years of record."),
            
            tags$div(
              style = "background: #fff8e1; border-left: 4px solid #f0a500; padding: 12px 16px; margin: 16px 0; border-radius: 2px;",
              tags$strong("\u26a0 A note on double-counting: "),
              "The six water use variables shown in this application are not all independent — some are subdivisions of others. ",
              tags$em("Public Supply — Groundwater Withdrawals"),
              " and ",
              tags$em("Public Supply — Surface Water Withdrawals"),
              " are components of ",
              tags$em("Public Supply — Total Withdrawals"),
              ". Similarly, ",
              tags$em("Irrigation — Surface Water Withdrawals"),
              " and ",
              tags$em("Irrigation — Consumptive Use"),
              " are subsets of ",
              tags$em("Irrigation — Total Withdrawals"),
              ". Adding all six variables together would significantly overcount actual water use. ",
              "For this reason, the map colors HUC-12 subwatersheds based only on the sum of ",
              tags$em("Public Supply — Total Withdrawals"),
              " and ",
              tags$em("Irrigation — Total Withdrawals"),
              ". In the Annual Summary table and Monthly Pivot, all six variables are shown as individual rows ",
              "so users can explore the breakdown — but care should be taken not to sum across all rows as a ",
              "measure of total water use."
            ),
            
            tags$div(
              style = "background: #e8f4f8; border-left: 4px solid #2980b9; padding: 12px 16px; margin: 16px 0; border-radius: 2px;",
              tags$strong("\u2139 A note on inter-basin water transfers: "),
              "A caveat of the NWAA modeling approach is that water-use estimates are made where water is ",
              tags$em("used"),
              ", not where water is ",
              tags$em("withdrawn"),
              ". For example, water moved through irrigation canals from a reservoir source in Basin A and ",
              "applied to fields in Basin B is accounted for as a withdrawal in Basin B. Thus, reported ",
              "locations of some withdrawals may be in the incorrect physical basin, which can affect the ",
              "reliability of water-availability assessments in areas where water is moved across basin ",
              "boundaries. This is particularly relevant in Washington State, where large inter-basin ",
              "transfer projects move significant volumes of water across watershed boundaries, at the HUC8 or HUC12 level. ",
              "Agricultural examples include the Yakima Project and Columbia Basin Project. ",
              "Municipal examples include Seattle's water supply drawn from the Cedar River watershed ",
              "and delivered across basin boundaries, Tacoma's supply from the Howard Hanson Reservoir ",
              "on the Green River, and even Olympia's system which moves water from the Nisqually to ",
              "the Deschutes watershed.",
              " (Source: ",
              tags$a("USGS Water Use in the United States",
                     href   = "https://www.usgs.gov/special-topics/integrated-water-availability-assessments/national-water-availability-assessment",
                     target = "_blank"),
              ")"
            ),
            
            tags$hr(),
            tags$p(
              tags$em("Data source: U.S. Geological Survey ",
                      tags$a("National Water Availability Assessment Data Companion",
                             href   = "https://water.usgs.gov/nwaa-data/",
                             target = "_blank"),
                      ". Variable definitions adapted from the ",
                      tags$a("USGS Washington Water Science Center Water Use Concepts and Terms",
                             href   = "https://wa.water.usgs.gov/data/wuse/concepts.htm",
                             target = "_blank"),
                      "."
              )
            ),
            tags$p(
              tags$em("Contact: jeffjmarti at gmail.com (No affiliation with USGS)")
            )
          )
        )   # closes tabPanel "About"
        
      )             # closes tabsetPanel
    )               # closes mainPanel
  )                 # closes sidebarLayout
)                   # closes fluidPage

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  annual_data      <- reactiveVal(NULL)
  monthly_data     <- reactiveVal(NULL)
  monthly_agg_data <- reactiveVal(NULL)
  huc8_annual_data <- reactiveVal(NULL)
  
  # ---- Initial map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      #addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -120.5, lat = 47.5, zoom = 6) %>%
      addPolygons(
        data        = wbd_huc8_geom,
        layerId     = ~huc8,
        group       = "huc8_base",
        fillColor   = "#4a90d9",
        fillOpacity = 0.2,
        color       = "#2c5f8a",
        weight      = 1.5,
        label       = ~huc8_label,
        highlightOptions = highlightOptions(
          weight       = 3,
          color        = "#e05c00",
          fillOpacity  = 0.4,
          bringToFront = FALSE
        )
      ) %>%
      addPolygons(
        data    = wa_border,
        fill    = FALSE,
        color   = "#ffffff",
        weight  = 2,
        options = pathOptions(interactive = FALSE)
      )
    
  })
  
  # ---- Sync map click to selectInput ----
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id)) return()
    updateSelectInput(session, "huc8", selected = click$id)
  })
  
  # ---- observeEvent 1: load all variables for selected HUC8 ----
  # Data come from the local pre-downloaded file, so this is instant
  observeEvent(
    input$huc8,
    {
      req(input$huc8)
      
      all_annual  <- list()
      all_monthly <- list()
      
      for (i in seq_len(nrow(nwaa_vars))) {
        var_row <- nwaa_vars[i, ]
        
        raw_annual      <- local_nwaa_huc8(nwaa_local$annual, var_row$var_id, input$huc8)
        all_annual[[i]] <- summarise_huc12_annual(
          raw_annual, var_row$var_id, var_row$label
        )
        
        # Standardize the value column to "mgd" for consistent processing
        all_monthly[[i]] <- local_nwaa_huc8(nwaa_local$monthly, var_row$var_id, input$huc8) %>%
          rename(mgd = all_of(paste0(var_row$var_id, "_mgd"))) %>%
          mutate(water_use_type = var_row$label)
      }
      
      annual_df   <- bind_rows(all_annual)
      monthly_df  <- bind_rows(all_monthly)
      monthly_agg <- tryCatch(
        aggregate_monthly(monthly_df),
        error = function(e) {
          message("aggregate_monthly ERROR: ", conditionMessage(e))
          NULL
        }
      )
      
      annual_data(annual_df)
      huc8_annual_data(summarise_huc8_annual(annual_df, input$huc8))
      monthly_data(monthly_df)
      monthly_agg_data(monthly_agg)
    },
    ignoreInit = TRUE
  )
  

  # ---- observeEvent 2: update map when annual data changes ----
  observeEvent(annual_data(), {
    req(annual_data(), input$huc8)
    
    tryCatch({
      
      # Sum Total Withdrawal categories per HUC12 for map coloring
      annual_df <- annual_data() %>%
        filter(`Water Use Type` %in% c(
          "Public supply - Total withdrawals",
          "Irrigation - Total withdrawals"
        )) %>%
        group_by(huc12 = `HUC12`) %>%
        summarise(
          `Mean Annual (acre-ft)` = sum(`Mean Annual (acre-ft)`, na.rm = TRUE),
          .groups = "drop"
        )
      
      huc8_geom <- wbd_huc8_geom %>% filter(huc8 == input$huc8)
      
      # Filter to just this HUC8's HUC12s before joining -- much faster
      map_df <- wbd_huc12_geom %>%
        filter(substr(huc12, 1, 8) == input$huc8) %>%
        mutate(huc12 = as.character(huc12)) %>%
        left_join(annual_df, by = "huc12")
      
      pal <- colorNumeric("YlGnBu", map_df$`Mean Annual (acre-ft)`, na.color = "#EEEEEE")
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s<br/>Total water use: %s acre-ft/yr",
        map_df$name,
        map_df$huc12,
        formatC(map_df$`Mean Annual (acre-ft)`, format = "f", digits = 1, big.mark = ",")
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        clearGroup("huc8_selection") %>%
        clearGroup("huc12_data") %>%
        removeControl("legend") %>%
        addPolygons(
          data   = huc8_geom,
          group  = "huc8_selection",
          fill   = FALSE,
          color  = "#e05c00",
          weight = 3,
          options = pathOptions(interactive = FALSE)  # <-- add this
        ) %>%
        addPolygons(
          data         = map_df,
          group        = "huc12_data",
          fillColor    = ~pal(`Mean Annual (acre-ft)`),
          fillOpacity  = 0.75,
          color        = "#666666",
          weight       = 0.4,
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position  = "bottomright",
          pal       = pal,
          values    = map_df$`Mean Annual (acre-ft)`,
          title     = "Mean Annual<br/>Water Use (acre-ft)",
          labFormat = labelFormat(digits = 0, big.mark = ","),
          opacity   = 0.75,
          layerId   = "legend"
        )
      
    }, error = function(e) {
      message("Map update error: ", conditionMessage(e))
    })
    
  }, ignoreInit = TRUE)
  
  # ---- Annual summary table ----
  output$tbl_annual <- renderDT({
    req(annual_data())
    annual_data() %>% arrange(desc(`Mean Annual (acre-ft)`))
  },
  server   = FALSE,
  rownames = FALSE,# <-- removes the row number column
  filter   = "top", 
  options  = list(
        
    pageLength = 15,
    columnDefs = list(
      list(
        targets = 3,         # still correct with rownames = FALSE
        render  = JS("function(data, type, row) {
          if (type === 'display') {
            return parseFloat(data).toLocaleString('en-US',
              {minimumFractionDigits: 1, maximumFractionDigits: 1});
          }
          return data;
        }")
      ),
      list(
        targets = 4,
        render  = JS("function(data, type, row) {
          if (type === 'display') {
            return parseFloat(data).toLocaleString('en-US',
              {minimumFractionDigits: 3, maximumFractionDigits: 3});
          }
          return data;
        }")
      )
    )
  ))
  
 
  # ---- HUC-8 annual summary table ----
  output$tbl_huc8_annual <- renderDT({
    req(huc8_annual_data())
    huc8_annual_data()
  },
  server   = FALSE,
  rownames = FALSE,
  filter   = "top",
  options  = list(
    pageLength = 10,
    dom        = "ftp",
    columnDefs = list(
      list(
        targets = 3,    # Mean Annual (acre-ft)
        render  = JS("function(data, type, row) {
          if (type === 'display') {
            return parseFloat(data).toLocaleString('en-US',
              {minimumFractionDigits: 1, maximumFractionDigits: 1});
          }
          return data;
        }")
      ),
      list(
        targets = 4,    # Mean Annual (MGD)
        render  = JS("function(data, type, row) {
          if (type === 'display') {
            return parseFloat(data).toLocaleString('en-US',
              {minimumFractionDigits: 3, maximumFractionDigits: 3});
          }
          return data;
        }")
      )
    ),
    # Bold and shade the two total rows
    rowCallback = JS("
      function(row, data) {
        var waterUseType = data[2];
        if (waterUseType === 'Public supply - Total withdrawals' ||
            waterUseType === 'Irrigation - Total withdrawals') {
          $('td', row).css({
            'font-weight': 'bold',
            'background-color': '#e8f0f7'
          });
        }
      }
    ")
  ))
  
  # ---- Download HUC-8 annual summary CSV ----
  output$download_huc8_annual <- downloadHandler(
    filename = function() {
      paste0("huc8_annual_water_use_", input$huc8, ".csv")
    },
    content = function(file) {
      req(huc8_annual_data())
      write.csv(huc8_annual_data(), file, row.names = FALSE)
    }
  )
  # ---- Monthly pivot table ----
  output$tbl_monthly_pivot <- renderDT({
    req(monthly_agg_data())
    
    message("Pivot input rows: ", nrow(monthly_agg_data()))
    
    df <- tryCatch(
      make_monthly_pivot(monthly_agg_data(), input$units),
      error = function(e) {
        message("make_monthly_pivot ERROR: ", conditionMessage(e))
        NULL
      }
    )
    
    req(df)
    
    # Rename Annual Total column to reflect current units
    annual_col <- if (input$units == "acre_ft") "Annual Total (acre-ft)" else "Annual Total (MGD)"
    names(df)[names(df) == "Annual Total"] <- annual_col
    
    df
  },
  server  = FALSE,
  options = list(
    pageLength = 10,
    dom        = "t"    # table only -- no search/pagination needed
  ))
  
  # ---- Download full monthly CSV ----
  output$download_monthly <- downloadHandler(
    filename = function() {
      paste0("monthly_water_use_", input$huc8, ".csv")
    },
    content = function(file) {
      req(monthly_data())
      
      huc12_meta <- wbd_huc12_geom %>%
        st_drop_geometry() %>%
        select(huc12, name, areaacres) %>%
        mutate(huc12 = as.character(huc12))
      
      monthly_data() %>%
        mutate(huc12_id = as.character(huc12_id)) %>%
        left_join(huc12_meta, by = c("huc12_id" = "huc12")) %>%
        mutate(
          month    = substr(year_month, 6, 7),
          acre_ft  = round(mgd * DAYS_IN_MONTH[month] * 1e6 / GAL_PER_ACRE_FT, 1)
        ) %>%
        select(
          huc12_id,
          name,
          areaacres,
          year_month,   
          water_use_type,
          mgd,
          acre_ft
        ) %>%
        mutate(areaacres = round(areaacres, 1)) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
}   # closes server

shinyApp(ui, server)