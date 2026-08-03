# ----- 1. LIBRARIES -----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(spsComps)
library(dplyr, warn.conflicts = FALSE)
library(forcats)
library(tidyr)
library(DT)
library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(maps)
library(mapproj)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafpop)
library(leaflegend)
library(mapview)
library(RColorBrewer)
library(randomcoloR)
library(this.path)
library(stringr)
library(readr)
library(scales)
library(RColorBrewer)
library(httr2)
library(jsonlite)
library(vegan)
library(httr)
  


# ----- 2. LOAD AND PREPARE DATA -----
setwd(this.path::here())
# dashboard_data <- read.csv("data/all_deployments.csv") %>%
#   mutate(Date = ymd_hms(captureDTFormatted))
# locations <- read.csv("data/camera_stats.csv") %>%
#   filter(DeploymentLabel %in% unique(dashboard_data$DeploymentLabel))

# connecting from supabase

anon_key <- Sys.getenv("SUPABASE_KEY")

supabase_url <- Sys.getenv("SUPABASE_URL")

locations_url <- paste0("https://aeckuwcnusvrhrpbthgs.supabase.co/rest/v1/mahaica_camera_traps")

observations_url <- paste0("https://aeckuwcnusvrhrpbthgs.supabase.co/rest/v1/camera_trap_observations")

species_url <- paste0("https://aeckuwcnusvrhrpbthgs.supabase.co/rest/v1/species_info")

waterways_url <- paste0(supabase_url, "/rest/v1/mahaica_waterways")

# get_supabase_table <- function(url,
#                                key = anon_key,
#                                page_size = 1000) {
#   start <- 0
#   results <- list()
#   
#   repeat {
#     end <- start + page_size - 1
#     
#     resp <- request(url) |>
#       req_headers(
#         apikey = key,
#         Authorization = paste("Bearer", key),
#         Range = paste0(start, "-", end)
#       ) |>
#       req_perform()
#     chunk <- resp_body_json(resp, simplifyVector = TRUE)
#     if (nrow(chunk) == 0)
#       break
#     results[[length(results) + 1]] <- chunk
#     if (nrow(chunk) < page_size)
#       break
#     start <- start + page_size
#   }
#   
#   bind_rows(results)
# }

get_supabase_table <- function(url,
                               key = anon_key,
                               page_size = 1000,
                               return_type = c("data.frame", "sf"),
                               geom_col = "geom",
                               crs = 4326) {
  
  return_type <- match.arg(return_type)
  
  start <- 0
  results <- list()
  
  repeat {
    end <- start + page_size - 1
    resp <- httr2::request(url) |>
      httr2::req_headers(
        apikey = key,
        Authorization = paste("Bearer", key),
        Range = paste0(start, "-", end)
      ) |>
      httr2::req_perform()
    chunk <- httr2::resp_body_json(
      resp,
      simplifyVector = TRUE
    )
    if (length(chunk) == 0)
      break
    results[[length(results) + 1]] <- chunk
    if (nrow(chunk) < page_size)
      break
    start <- start + page_size
  }
  
  data <- dplyr::bind_rows(results)
  
  # Return a regular data frame
  if (return_type == "data.frame") {
    return(data)
  }
  
  # Convert to sf
  if (return_type == "sf") {
    
    if (!geom_col %in% names(data)) {
      stop(
        paste0(
          "The geometry column '",
          geom_col,
          "' was not found in the returned data."
        )
      )
    }
    
    return(
      sf::st_as_sf(
        data,
        wkt = geom_col,
        crs = crs
      )
    )
  }
}

dashboard_data <- get_supabase_table(observations_url, return_type = "data.frame") %>%
  rename(DDLat = latitude, DDLon = longitude) %>%
  mutate(Date = ymd_hms(capture_datetime))

locations <- get_supabase_table(locations_url, return_type = "data.frame") %>%
  rename(DDLat = latitude, DDLon = longitude) %>%
  filter(DeploymentLabel %in% unique(dashboard_data$DeploymentLabel))

species_info <- get_supabase_table(species_url, return_type = "data.frame")

waterways <- get_supabase_table(waterways_url, return_type = "data.frame")

# Waterways ----
# River line features fetched as GeoJSON using the PostgREST Accept header,
# which tells Supabase to return geometry as GeoJSON rather than WKB hex

waterways_geojson    <- NULL
waterways_load_error <- NULL

tryCatch({
  url  <- paste0(supabase_url, "/rest/v1/mahaica_waterways?select=*")
  resp <- GET(url, add_headers(
    "apikey"        = anon_key,
    "Authorization" = paste("Bearer", anon_key),
    "Accept"        = "application/geo+json"
  ))
  if (http_error(resp))
    stop("HTTP ", status_code(resp))
  waterways_geojson <<- content(resp, as = "text", encoding = "UTF-8")
}, error = function(e) {
  waterways_load_error <<- e$message
  message("Could not load waterways: ", e$message)
})

# Merge tables using DeploymentID
# dashboard_data <- deployments %>%
#   inner_join(locations %>% select(DeploymentID, DDLat, DDLon), by = "DeploymentID")

# lists for selectors
species_list <- sort(unique(dashboard_data$SpeciesCommonName))
camera_list <- unique(dashboard_data$DeploymentID)
taxa_list <- sort(unique(dashboard_data$Taxa))
year_list <- unique(year(dashboard_data$captureDTFormatted))
year_list <- sort(year_list[!is.na(year_list)])
dates <- sort(unique(dashboard_data$Date))

sort_by_selector <- function(id) {
  selectizeInput(
    id,
    "Sort by",
    choices = c(
      "Most recent" = 1,
      "Least recent" = 2,
      "Number of detections" = 3,
      "Alphabetical" = 4
    ),
    selected = "Most recent"
  )
}

ui <- page_navbar(
  title = tags$span(
    tags$a(
      href = "https://emcguyana.com/",
      tags$img(
        src = "https://i0.wp.com/www.emcguyana.com/wp-content/uploads/2025/09/icon.png?fit=500%2C500&ssl=1",
        # src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Flag_of_Guyana.svg/40px-Flag_of_Guyana.svg.png",
        height = "30px",
        style = "margin-right:8px; vertical-align:middle;"
      )
    ),
    "Mahaica Watershed Camera Trap Program"
  ),
  
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#2c7bb6",
    base_font   = font_google("Roboto"),
    # heading_font = font_google("DM Sans")
    heading_font = font_google("Open Sans")
  ),
  
  bg = "#1a3a5c",
  inverse = TRUE,
  
  # Explorer tab -----
  
  nav_panel(
    "Explorer",
    icon = icon("map"),
    layout_sidebar(
      # sidebar ----
      sidebar = sidebar(
        width = 270,
        bg = "#f0f4f8",
        
        tags$h6("Filters", class = "text-muted fw-bold mt-1 mb-2"),
        
        selectInput(
          "taxa",
          "Select Taxa:",
          choices = c("All", taxa_list),
          selected = "All"
        ),
        
        selectInput(
          "species",
          "Select Species:",
          choices = c("All", species_list),
          selected = "All"
        ),
        
        selectInput(
          "camera",
          "Select Camera:",
          choices = c("All", camera_list),
          selected = "All"
        ),
        
        hr(),
        
        tags$h6("Date Range", class = "text-muted fw-bold"),
        # dates <- sort(unique(readings$Date_parsed))
        sliderInput(
          "date_range",
          NULL,
          min   = min(dates),
          max   = max(dates),
          value = c(min(dates), max(dates)),
          timeFormat = "%b %Y",
          step  = 30
        ),
        
        hr(),
        
        tags$h6("Map Content", class = "text-muted fw-bold"),
        radioButtons(
          "map_content",
          NULL,
          choices = c("Species Heatmap" = 1, "Camera Status Map" = 2)
        ),
        
        hr(),
        
        tags$h6("Map Style", class = "text-muted fw-bold"),
        radioButtons(
          "map_base",
          NULL,
          choices = c(
            "Satellite" = "Esri.WorldImagery",
            "Street"    = "OpenStreetMap",
            "Terrain"   = "Esri.WorldTopoMap"
          ),
          selected = "Esri.WorldImagery"
        ),
        
        checkboxInput("show_labels", "Show camera labels", value = FALSE),
        checkboxInput("show_waterways", "Show river features",  value = TRUE),
        
        hr(),
        
        tags$small(
          class = "text-muted",
          icon("circle-info"),
          " Click a map marker to inspect a camera."
        )
        
      ),
      
      # Main content panels ----
      div(
        layout_columns(
          # col_widths = c(3, 3, 3, 3),
          uiOutput("vbox_unique_species"),
          uiOutput("vbox_cameras"),
          uiOutput("vbox_detections"),
          uiOutput("vbox_trap_days"),
          uiOutput("vbox_shannon")
        ),
        
        tags$br(),
        
        # map + lists -----
        layout_columns(
          col_widths = c(6, 3, 3),
          card(
            full_screen = TRUE,
            card_header(icon("map-location-dot"), "Camera Trap Locations"),
            leafletOutput("map", height = "400px")
          ),
          card(
            full_screen = TRUE,
            card_header(
              icon("chart-line"),
              "Species in extent",
              sort_by_selector("spec_list_sorter")
            ),
            uiOutput("species_list", height = "400px")
          ),
          card(
            full_screen = TRUE,
            card_header(
              icon("chart-line"),
              "Cameras in extent",
              sort_by_selector("cam_list_sorter")
            ),
            uiOutput("cameras_list", height = "400px")
          )
        ),
        
        tags$br(),
        
        tags$h4("Location Analysis"),
        
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            full_screen = TRUE,
            card_header(icon("chart-area"), "Taxa Chart"),
            plotlyOutput("taxa_plot", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("chart-area"), "Camera Chart"),
            plotOutput("camera_plot", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("chart-area"), "Species Chart"),
            plotOutput("species_plot", height = "380px")
          )
        ),
        
        tags$br(),
        
        layout_columns(
          col_widths = c(9, 3),
          card(
            full_screen = TRUE,
            card_header(icon("chart-line"), "Species Accumulation Curve"),
            plotOutput("cumulative_species", height = "380px")
          ),
          card(
            full_screen = TRUE,
            card_header(icon("clock"), "Diel Chart"),
            plotOutput("activity_radial_plot", height = "380px")
          )
        )
      )
    )
  ),
  
  # Species detail tab ----
  nav_panel(
    "Species detail",
    icon = icon("paw"),
    layout_sidebar(sidebar = sidebar(
      width = 250,
      bg = "#f0f4f8",
      selectInput(
        "detail_species",
        "Select Species:",
        choices = c("All", species_list),
        selected = "All"
      ),
      
      hr()
      #uiOutput("species_info_card")
    ))
  ),
  
  # Camera detail tab ----
  nav_panel("Camera trap detail", icon = icon("camera"), layout_sidebar())
  
)


# ----- 4. SERVER LOGIC -----
server <- function(input, output, session) {
  # Reactive dataframe based on sidebar selections
  # filtered_data <- reactive({
  #   data <- dashboard_data
  #
  #   if (!is.null(input$year_select)) {
  #     data <- data %>% filter(year(captureDTFormatted) %in% input$year_select)
  #   }
  #
  #   if (!is.null(input$species_select)) {
  #     data <- data %>% filter(SpeciesCommonName %in% input$species_select)
  #   }
  #
  #   if (!is.null(input$camera_select)) {
  #     data <- data %>% filter(DeploymentID == input$camera_select)
  #   }
  #
  #   return(data)
  # })
  
  ## Filtered Reactives ----
  
  # Debounce all inputs by 400ms so charts don't re-render on every
  # intermediate value during rapid slider drags or dropdown changes
  
  taxa_d              <- reactive(input$taxa)             %>% debounce(400)
  species_d           <- reactive(input$species)          %>% debounce(400)
  camera_d            <- reactive(input$camera)           %>% debounce(400)
  date_range_d        <- reactive(input$date_range)       %>% debounce(400)
  spec_list_sorter_d  <- reactive(input$spec_list_sorter) %>% debounce(400)
  cam_list_sorter_d   <- reactive(input$cam_list_sorter)  %>% debounce(400)
  
  # Map bounds — updated on zoom/pan with 600ms debounce to avoid thrashing
  map_bounds <- reactive({
    input$map_bounds
  }) %>% debounce(600)
  
  # Sites whose coordinates fall within the current map extent
  visible_sites <- reactive({
    bounds <- map_bounds()
    if (is.null(bounds))
      return(unique(locations$SiteID))
    locations %>%
      filter(
        DDLat >= bounds$south,
        DDLat <= bounds$north,
        DDLon >= bounds$west,
        DDLon <= bounds$east
      ) %>%
      pull(DeploymentLabel)
  })
  
  fdata_full <- reactive({
    req(date_range_d())
    d <- dashboard_data %>%
      filter(Date >= date_range_d()[1], Date <= date_range_d()[2])
    if (taxa_d() != "All")
      d <- d %>% filter(Taxa == taxa_d())
    if (species_d() != "All")
      d <- d %>% filter(SpeciesCommonName == species_d())
    if (camera_d() != "All")
      d <- d %>% filter(DeploymentLabel == camera_d())
    d
  })
  
  fdata <- reactive({
    d <- fdata_full() %>% filter(DeploymentLabel %in% visible_sites())
    d
  })
  
  # Update site filter dropdown when a map marker is clicked;
  # clicking the same site again resets to "All sites"
  observeEvent(input$map_base, {
    leafletProxy("map") %>%
      addProviderTiles(input$map_base)
  })
  
  observeEvent(input$map_marker_click, {
    clicked <- input$map_marker_click$id
    if (!is.null(clicked)) {
      current <- isolate(input$camera)
      new_val <- if (identical(current, clicked))
        "All"
      else
        clicked
      updateSelectInput(session, "camera", selected = new_val)
    }
  })
  
  # when a different camera is selected, zoom to it
  observeEvent(input$camera, {
    if (input$camera == "All") {
      leafletProxy("map") %>% fitBounds(
        min(locations$DDLon),
        min(locations$DDLat),
        max(locations$DDLon),
        max(locations$DDLat)
      )
    } else {
      loc <- locations %>% filter(DeploymentLabel == input$camera)
      if (nrow(loc) > 0) {
        leafletProxy("map") %>%
          setView(lng = loc$DDLon,
                  lat = loc$DDLat,
                  zoom = 14)
      }
    }
  }, ignoreInit = TRUE)
  
  ## Value Boxes ----
  
  make_vbox <- function(label, value, icon_name, color) {
    value_box(
      title    = label,
      value    = value,
      showcase = icon(icon_name, style = "font-size:2rem;"),
      theme    = color
      # height   = "120px"
    )
  }
  
  output$vbox_unique_species <- renderUI({
    v <- length(unique(fdata()$SpeciesCommonName))
    make_vbox("Unique Species", v, "paw", "primary")
  })
  
  output$vbox_cameras <- renderUI({
    v <- length(unique(fdata()$DeploymentLabel))
    make_vbox("Camera Traps", v, "camera", "primary")
  })
  
  output$vbox_detections <- renderUI({
    v <- length(fdata()$ImageID)
    make_vbox("Detections", v, "photo-film", "primary")
  })
  
  output$vbox_trap_days <- renderUI({
    num_cams <- length(unique(fdata()$DeploymentLabel))
    days_active <- fdata() %>% group_by(day = as_date(Date)) %>% count(day) %>% nrow()
    v <- num_cams * days_active
    make_vbox("Trap days", v, "calendar-day", "primary")
  })
  
  output$vbox_shannon <- renderUI({
    s_data <- fdata() %>% mutate(dummy_cam = "All")
    v <- table(s_data$dummy_cam, s_data$SpeciesCommonName) %>% unclass() %>%
      diversity() %>% round(2)
    make_vbox("Shannon index", v, "calendar-day", "primary")
  })
  
  ## Map ----
  
  camera_summary <- reactive({
    fdata_full() %>%
      group_by(DeploymentLabel, DDLat, DDLon) %>%
      summarise(
        detections = n(),
        unique_species = length(unique(SpeciesCommonName)),
        .groups = "drop"
      )
    # %>% left_join(locations %>% select(DeploymentLabel, start_date, end_date, shannon))
  })
  
  # leaflet(locations) %>%
  
  
  output$map <- renderLeaflet({
    leaflet(locations) %>%
      fitBounds( ~ min(DDLon), ~ min(DDLat), ~ max(DDLon), ~ max(DDLat)) %>%
      addResetMapButton() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      # Report bounds back to Shiny on every move so charts can filter by extent

      htmlwidgets::onRender(
        "
        function(el, x) {
          var map = this;
          function reportBounds() {
            var b = map.getBounds();
            Shiny.setInputValue('map_bounds', {
              north: b.getNorth(), south: b.getSouth(),
              east:  b.getEast(),  west:  b.getWest()
            });
          }
          map.on('moveend', reportBounds);
          reportBounds();
        }
      "
      )
  })
  
  observe({
    cs <- camera_summary()
    req(nrow(cs) > 0)
    
    pal <- colorNumeric("RdYlBu",
                        domain = cs$unique_species,
                        reverse = TRUE)
    
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      clearGroup("waterways")
    
    # # River features drawn first so they sit underneath all point markers
    # if (isTRUE(input$show_waterways) && !is.null(waterways_geojson)) {
    #   proxy %>%
    #     addGeoJSON(
    #       geojson    = waterways_geojson,
    #       group      = "waterways",
    #       color      = "#4A90D9",
    #       weight     = 2,
    #       opacity    = 0.7,
    #       fill       = FALSE
    #     )
    # }
    
    proxy %>%
      addCircleMarkers(
        data = cs,
        lng = ~ DDLon,
        lat = ~ DDLat,
        layerId = ~ DeploymentLabel,
        radius = ~ rescale(detections, c(3, 15)),
        fillColor = ~ pal(unique_species),
        color = "white",
        weight = 1.5,
        fillOpacity = 0.85,
        label = ~ lapply(
          paste0(
            "<h6>",
            DeploymentLabel,
            "</h6><br>",
            "<b>Number of detections: </b>",
            detections,
            "<br>",
            "<b>Number of unique species: </b>",
            unique_species,
            "<br>"
          ),
          # "<b>Shannon Index: </b>", round(shannon, 2), "<br>"),
          htmltools::HTML
        ),
        labelOptions = labelOptions(
          style = list(
            "font-size"        = "12px",
            "background-color" = "rgba(255,255,255,0.92)",
            "border"           = "1px solid #ccc",
            "border-radius"    = "4px",
            "padding"          = "6px 10px",
            "box-shadow"       = "2px 2px 4px rgba(0,0,0,0.15)"
          ),
          direction = "top",
          textsize = "12px"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = cs$unique_species,
        title = "Unique Species",
        opacity = 9.85
      )
    
    if (isTRUE(input$show_labels)) {
      proxy %>% addLabelOnlyMarkers(
        data = cs,
        lng = ~ DDLon,
        lat = ~ DDLat,
        label = ~ DeploymentLabel,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textsize = "11px",
          style     = list(
            "font-weight"      = "bold",
            "background-color" = "rgba(255,255,255,0.6)",
            "border"           = "none",
            "box-shadow"       = "none",
            "padding"          = "1px 4px"
          )
        )
      )
    }
  })
  
  # Lists ----
  
  # Unique species list ----
  output$species_list <- renderUI({
    sort_by <- spec_list_sorter_d()
    
    
    f_uniqueSpecies <- fdata() %>% group_by(SpeciesCommonName) %>%
      summarise(
        maxDate = max(Date),
        minDate = min(Date),
        count = n()
      )
    
    feed_data <- if (sort_by == 1)
      f_uniqueSpecies %>% arrange(desc(maxDate))
    else if (sort_by == 2)
      f_uniqueSpecies %>% arrange(minDate)
    else if (sort_by == 3)
      f_uniqueSpecies %>% arrange(desc(count))
    else
      f_uniqueSpecies %>% arrange(SpeciesCommonName)
    
    feed_boxes <- lapply(1:nrow(feed_data), function(i) {
      # JavaScript action payload sent to Shiny server when clicked
      click_js <- sprintf(
        "Shiny.setInputValue('species_feed_click', '%s', {priority: 'event'});",
        feed_data$SpeciesCommonName[i]
      )
      
      tags$div (
        style = "background-color: #f8f9fa; border-left: 4px solid #007bff; margin-bottom: 10px; padding: 12px; border-radius: 4px; cursor: pointer; transition: background 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
        onclick = click_js,
        # Listens for browser click
        
        # Simple CSS hover effect via inline attributes
        onmouseover = "this.style.backgroundColor='#e9ecef';",
        onmouseout = "this.style.backgroundColor='#f8f9fa';",
        
        tags$strong(feed_data$SpeciesCommonName[i], style = "color: #6c757d; font-size: 1em; display: block; margin-bottom: 4px;"),
        tags$span(lapply(
          paste0(
            "First spotted: ",
            format(feed_data$minDate[i], "%B %d %Y, %H:%M"),
            "<br> Last spotted: ",
            format(feed_data$maxDate[i], "%B %d %Y, %H:%M"),
            "<br> Number of detections: ",
            feed_data$count[i]
          ),
          htmltools::HTML
        ), style = "color: #333333; font-size: 0.85em;")
      )
    })
    
    # Wrap all list items into an unordered list tag
    # html_list <- tags$ul(list_items)
    
    tags$div(
      style = "max-height: 400px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
      # html_list
      feed_boxes
    )
  })
  
  # cameras list ----
  output$cameras_list <- renderUI({
    sort_by <- cam_list_sorter_d()
    
    
    f_uniqueCameras <- fdata() %>% group_by(DeploymentLabel) %>%
      summarise(
        maxDate = max(Date),
        minDate = min(Date),
        count = n()
      )
    
    feed_data <- if (sort_by == 1)
      f_uniqueCameras %>% arrange(desc(maxDate))
    else if (sort_by == 2)
      f_uniqueCameras %>% arrange(minDate)
    else if (sort_by == 3)
      f_uniqueCameras %>% arrange(desc(count))
    else
      f_uniqueCameras %>% arrange(DeploymentLabel)
    
    feed_boxes <- lapply(1:nrow(feed_data), function(i) {
      # JavaScript action payload sent to Shiny server when clicked
      click_js <- sprintf(
        "Shiny.setInputValue('camera_feed_click', '%s', {priority: 'event'});",
        feed_data$DeploymentLabel[i]
      )
      
      tags$div (
        style = "background-color: #f8f9fa; border-left: 4px solid #007bff; margin-bottom: 10px; padding: 12px; border-radius: 4px; cursor: pointer; transition: background 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
        onclick = click_js,
        # Listens for browser click
        
        # Simple CSS hover effect via inline attributes
        onmouseover = "this.style.backgroundColor='#e9ecef';",
        onmouseout = "this.style.backgroundColor='#f8f9fa';",
        
        tags$strong(feed_data$DeploymentLabel[i], style = "color: #6c757d; font-size: 1em; display: block; margin-bottom: 4px;"),
        tags$span(lapply(
          paste0(
            "First detection: ",
            format(feed_data$minDate[i], "%B %d %Y, %H:%M"),
            "<br> Most recent detection: ",
            format(feed_data$maxDate[i], "%B %d %Y, %H:%M"),
            "<br> Number of detections: ",
            feed_data$count[i]
          ),
          htmltools::HTML
        ), style = "color: #333333; font-size: 0.85em;")
      )
    })
    
    # Wrap all list items into an unordered list tag
    # html_list <- tags$ul(list_items)
    
    tags$div(
      style = "max-height: 400px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
      # html_list
      feed_boxes
    )
  })
  
  # when a feed box is clicked, select the corresponding species
  observeEvent(input$species_feed_click, {
    clicked = input$species_feed_click
    if (!is.null(clicked)) {
      current = isolate(input$species)
      newVal = if (current == clicked)
        "All"
      else
        clicked
      updateSelectInput(session = session,
                        inputId = "species",
                        selected = newVal)
    }
  })
  
  observeEvent(input$camera_feed_click, {
    clicked = input$camera_feed_click
    if (!is.null(clicked)) {
      current = isolate(input$camera)
      newVal = if (current == clicked)
        "All"
      else
        clicked
      updateSelectInput(session = session,
                        inputId = "camera",
                        selected = newVal)
    }
  })
  
  # Plots ----
  
  # Render Taxa bar chart ----
  output$taxa_plot <- renderPlotly({
    colors <- c(
      'rgb(211,94,96)',
      'rgb(128,133,133)',
      'rgb(144,103,167)',
      'rgb(171,104,87)',
      'rgb(114,147,203)'
    )
    fdata() %>%
      group_by(Taxa) %>% count(Taxa) %>%
      # ggplot(aes(x = "", y = n, fill = Taxa)) +
      # geom_bar(stat = "identity", width = 1, color = "white") +
      # coord_polar("y", start = 0) +
      # theme_minimal()
      plot_ly(
        labels = ~ Taxa,
        values = ~ n,
        type = "pie",
        textposition = "inside",
        textinfo = "label+percent",
        insidetextfont = list(color = "#FFFFFF"),
        hoverinfo = "text",
        text = ~ paste(n, "detections"),
        marker = list(
          colors = colors,
          line = list(color = "white", width = 1)
        ),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
  })
  
  # Render Camera Column Chart ----
  output$camera_plot <- renderPlot({
    fdata() %>%
      mutate(DeploymentLabel = fct_lump_n(DeploymentLabel, 10)) %>%
      count(DeploymentLabel) %>%
      ggplot(aes(
        x = reorder(DeploymentLabel, -n),
        y = n,
        fill = DeploymentLabel
      )) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      labs(x = "Camera Deployment ID", y = "No. of detections") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render Species Column Chart ----
  output$species_plot <- renderPlot({
    fdata() %>%
      mutate(SpeciesCommonName = fct_lump_n(SpeciesCommonName, 10)) %>%
      count(SpeciesCommonName) %>%
      ggplot(aes(x = reorder(SpeciesCommonName, -n), y = n)) +
      theme_minimal() +
      geom_col() +
      coord_flip() +
      labs(x = "Species", y = "No. of detections") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # Render species accumulation chart ----
  output$cumulative_species <- renderPlot({
    fdata() %>%
      arrange(Date) %>%
      mutate(cum_unique = cumsum(!duplicated(SpeciesCommonName))) %>%
      group_by(day = as_date(Date)) %>%
      summarise(total_cum_unique = if_else(all(is.na(cum_unique)), NA_real_, max(cum_unique, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(cum_days = row_number()) %>%
      ggplot(aes(x = cum_days, y = total_cum_unique)) +
      geom_line() +
      geom_point() +
      labs(x = "Cumulative trap days", y = "Cumulative unique species") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
  })
  
  # Render diel activity radial plot -----
  
  output$activity_radial_plot <- renderPlot({
    fdata() %>%
      mutate(Hour = hour(Date)) %>%
      count(Hour) %>%
      # Ensure all 24 hours are represented even if counts are 0
      complete(Hour = 0:23, fill = list(n = 0)) %>%
      ggplot(aes(x = Hour, y = n)) +
      geom_bar(
        stat = "identity",
        fill = "steelblue",
        color = "white",
        width = 1
      ) +
      geom_line() +
      # Crucial: Bend the Cartesian x-axis into a full circular clock
      coord_polar(start = 0) +
      # Force x-axis to accept precisely a 24-hour clock layout
      scale_x_continuous(
        breaks = 0:23,
        labels = sprintf("%02d:00", 0:23),
        limits = c(-0.5, 23.5)
      ) +
      theme_minimal() +
      labs(xlab = NULL, ylab = NULL) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 16,
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 11,
          hjust = 0.5,
          color = "gray40"
        ),
        axis.text.x = element_text(
          size = 10,
          face = "bold",
          color = "darkslategrey"
        ),
        axis.title.y = element_blank(),
        # Circular layout grid line numbers act as visual scale
        panel.grid.minor = element_blank()
      )
    
  })
}

# ----- 5. RUN APPLICATION -----
shinyApp(ui = ui, server = server)
