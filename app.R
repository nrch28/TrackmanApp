library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ellipse)
library(ggplot2)
library(gridExtra)
library(lightgbm)
library(jsonlite)
library(zoo)
library(TTR)
library(RColorBrewer)

# Data Source (Trackman File)
combined_data <- read.csv("combined_ftp_data - Copy.csv")

#cleans date col
combined_data$Date <- as.Date(combined_data$Date, format = "%m/%d/%Y")

# Cleans Pitch Type Choices (for sidebar)
pitch_type_choices <- sort(unique(
  combined_data$TaggedPitchType[
    !is.na(combined_data$TaggedPitchType) &  
      combined_data$TaggedPitchType != "" &   
      !grepl("^\\s*$", combined_data$TaggedPitchType)  
  ]
))

# ------------------------------ UI -------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("player_name", "Select Player:",
                  choices = unique(combined_data$Pitcher)),
      
      # Dates toggle button + checkboxes
      h4("Select Dates:"),
      actionButton("toggle_dates", "Select All / Deselect All"),
      uiOutput("date_checkboxes"),  # dynamic date checkboxes here
      
      # Pitch Types toggle button + checkboxes
      h4("Include Pitch Types:"),
      actionButton("toggle_pitch_types", "Select All / Deselect All"),
      uiOutput("include_pitch_types"),
      
      #checkboxGroupInput("include_pitch_types", NULL,
                         #choices = pitch_type_choices,
                         #selected = pitch_type_choices),
      
      actionButton("update", "Update"),
      actionButton("select_all_btn", "Select All Pitches"),
      actionButton("toggle_selection", "Toggle Selection Group", icon = icon("toggle-on")),
      actionButton("show_comparison", "Show Comparison", icon = icon("columns")),
      
      # Download Options (PDF, CSV, Live Report)
      downloadButton("downloadCSV", "Download Raw Data (csv)",
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;"),
      downloadButton("downloadReportFull", "Download Bullpen Report",
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;"),
      downloadButton("downloadLive", "Download Live Report",
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;")
    ),
    mainPanel(
      tabsetPanel(
        # Tab 1, Movement Plot and Metrics Table
        tabPanel("Movement & Metrics",
                 fluidRow(
                   column(width = 12,
                          plotlyOutput("movement_plot", height = "500px")
                    )
                 ),
                 fluidRow(
                   column(width = 12,
                          br(), br(), br(), br(), br(), br(), br(),
                          tableOutput("metrics_table")
                    )
                 )
        ),
        # Tab 2, stuff, velo, spin rate, ivb, and hb distributions
        tabPanel("Distributions",
                 fluidRow(
                   column(width = 12,
                          plotlyOutput("stuff_dist_plot", height = "500px")
                          )
                 ),
                 fluidRow(
                   column(width = 12,
                          plotlyOutput("velo_dist_plot", height = "500px")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          br(),
                          plotlyOutput("spin_dist_plot")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          br(),
                          plotlyOutput("ivb_dist_plot")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          br(),
                          plotlyOutput("hb_dist_plot")
                   )
                 )
        ),
        # Tab 3, location heat maps
        tabPanel("Location",
                 br(), br(),
                 plotlyOutput("pitch_loc_chart")
        ),
        # Tab 4, release point plot and release consistency table
        tabPanel("Release Point",
                 fluidRow(
                   column(width = 12,
                          plotlyOutput("release_point_plot", height = "500px")
                   )
                 ),
                 fluidRow(
                   column(width = 6,
                          br(), br(), br(), br(), br(), br(), br(), br(), br(),
                          h4("Within-Pitch Variance"),
                          tableOutput("release_point_table")
                          ),
                   column(width = 6,
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                          h4("Between-Pitch Distances"),
                          tableOutput("centroid_dist_table")
                      )
                   ),
                 ),
        # Tab 5, spin clock
        tabPanel("Spin Clock",
                 br(), br(),
                 plotlyOutput("clock")
        ),
        # Tab 6, Velo over the course of a bullpen
        tabPanel("Velo Endurance",
                 br(), br(),
                 verbatimTextOutput("date_debug"),
                 plotlyOutput("velo_over_time")
        ),
        # Tab 7, Velo, Spin, IVB, HB trends w/ EWMA 
        tabPanel("Trends",
                 br(), br(),
                 checkboxGroupInput("plot_elements", "Show Elements:",
                                    choices = c("Boxplots", "Weighted Averages", "Daily Avg"),
                                    selected = c("Boxplots", "Weighted Averages", "Daily Avg")
                                      ),
                 br(),
                 plotlyOutput("Trends_velo"),
                 br(),
                 plotlyOutput("Trends_spin"),
                 br(),
                 checkboxGroupInput("break_show", "Break:",
                                    choices = c("IVB", "HB"),
                                    selected = c("IVB", "HB")
                                    ),
                 br(), br(), br(),
                 plotlyOutput("Trends_break")
        ),
        # Tab 7, data from live abs
        tabPanel("Live Data",
                 br(), br(),
                 plotlyOutput("LiveLoc"),
                 br(), br(),
                 plotlyOutput("SprayChart"),
                 br(), br(),
                 tableOutput("results_table")
        ),
      )
    )
  )
)

# --------------------- server--------------------------------------------------
server <- function(input, output, session){
  
  # Stores calculated stuff+ data for use in other parts of the app
  data_with_stuff <- reactiveVal(NULL)
  
  # Initialize toggle states (FALSE means not all selected)
  vals <- reactiveValues(
    dates_all = TRUE,    # assume all initially selected
    pitches_all = TRUE
  )
  
  # Dynamic Date Options/Filtering 
  observeEvent(input$toggle_dates, {
    vals$dates_all <- !vals$dates_all
    
    # Recompute player_dates based on currently selected player
    player_dates <- sort(unique(combined_data$Date[combined_data$Pitcher == input$player_name]))
    
    updateCheckboxGroupInput(
      session,
      inputId = "selected_dates",     
      choices = player_dates,          
      selected = if (vals$dates_all) player_dates else character(0)
    )
  })
  
  # Toggle Pitch Types
  observeEvent(input$toggle_pitch_types, {
    vals$pitches_all <- !vals$pitches_all
    updateCheckboxGroupInput(
      inputId = "include_pitch_types",
      selected = if (vals$pitches_all) pitch_type_choices else character(0)
    )
  })
  
  #Date Checkboxes
  output$date_checkboxes <- renderUI({
    req(input$player_name)
    player_dates <- sort(unique(combined_data$Date[combined_data$Pitcher == input$player_name]))
    
    checkboxGroupInput("selected_dates", "Select Dates:",
                       choices = player_dates,
                       selected = player_dates)  # or leave empty to require selection
  })
  
  #Pitch Type Checkboxes
  output$include_pitch_types <- renderUI({
    req(input$player_name)
    player_pitch_types <- sort(unique(combined_data$TaggedPitchType[combined_data$Pitcher == input$player_name]))
    
    checkboxGroupInput("include_pitch_types", "Select Pitch Types:",
                       choices = player_pitch_types,
                       selected = player_pitch_types)  # or leave empty to require selection
  })
  
  
  #Consistent Colors For Pitch Types
  pitch_color_map <- reactive({
    data <- filtered_data()
    pitch_types <- unique(data$TaggedPitchType)
    setNames(
      RColorBrewer::brewer.pal(length(pitch_types), "Set2"),
      pitch_types
    )
  })

  # Keep Track of Tables (for download)
  tables <- reactiveValues(
    metrics_table = NULL,
    metrics_html = NULL
  )
  
  # Track selected pitches (starts as NULL)
  selected_pitches <- reactiveVal(NULL)
  
  # Initialize selected_pitches when data loads - but only if it's not already set
  observe({
    req(filtered_data())
    current_selected <- selected_pitches()
    if (is.null(current_selected)) {
      print("Initializing selected_pitches with all pitch IDs")
      selected_pitches(filtered_data()$pitch_id)  # Start with all pitches selected
    }
  })
  
  selections <- reactiveValues(
    first = NULL,
    second = NULL,
    active = "first"  # Tracks which selection we're currently making
  )
  
  # Toggle between first/second selection
  observeEvent(input$toggle_selection, {
    selections$active <- ifelse(selections$active == "first", "second", "first")
  })
  
  # Observe Select all pitches button
  observeEvent(input$select_all_btn, {
    req(filtered_data())
    #print("Select All button clicked") #Debug
    # Set selected_pitches reactiveVal to all pitch_ids
    selected_pitches(filtered_data()$pitch_id)
  })
  
  # Updated Lasso Observer - Store selections
  observeEvent(event_data("plotly_selected", source = "pitch_click"), {
    lasso_data <- event_data("plotly_selected", source = "pitch_click")
    req(lasso_data$customdata)
    
    # Store in the active slot (either "first" or "second")
    selections[[selections$active]] <- lasso_data$customdata
    
    # Optional: Show which group was just updated
    showNotification(
      paste("Stored in", selections$active, "group:", length(lasso_data$customdata), "pitches"),
      type = "message"
    )
  })
  
  #Observe plotly clicks (to select/deselect individual pitches)
  observeEvent(event_data("plotly_click", source = "pitch_click"), {
    click_data <- event_data("plotly_click", source = "pitch_click")
    req(click_data$customdata)  
    
    clicked_id <- click_data$customdata
    
    current_selected <- selected_pitches()
    if (clicked_id %in% current_selected) {
      new_selected <- setdiff(current_selected, clicked_id)
    } else {
      new_selected <- c(current_selected, clicked_id)
    }
    
    selected_pitches(new_selected)
  })
  
  
  # Main Filtering Step. Uses all options selected from SideBar and creates filtered_data()
  # filtered_data() is where we start for every table/chart
  filtered_data <- eventReactive(input$update, {
    print("Update button clicked - starting data filtering")
    req(input$player_name, input$selected_dates)
    
    print(paste("Player selected:", input$player_name))
    print(paste("Dates selected:", paste(input$selected_dates, collapse = ", ")))
    print(paste("Pitch types selected:", paste(input$include_pitch_types, collapse = ", ")))
    
    if (!inherits(combined_data$Date, "Date")) {
      combined_data$Date <- as.Date(combined_data$Date)
    }
    
    # Name and Date Filter using selected checkboxes
    filtered <- combined_data %>%
      mutate(pitch_id = row_number()) %>%
      filter(
        Pitcher %in% input$player_name,
        PitchSession == "Live",
        Date %in% as.Date(input$selected_dates),
        !is.na(TaggedPitchType),
        TaggedPitchType != "",
        TaggedPitchType != "Undefined"
      )
    
    # Pitch Type Filter
    if (length(input$include_pitch_types) == 0) {
      print("Please Select Pitch Type(s)")
      return(filtered[0, ])
    }
    
    filtered <- filtered %>%
      filter(TaggedPitchType %in% input$include_pitch_types)
    
    filtered
  })
  
  
  # Lasso Comparison Pop Up (Movement Plot (first Tab))
  observeEvent(input$show_comparison, {
    req(selections$first, selections$second)
    
    prepare_data <- function(ids) {
      data <- filtered_data() %>%
        filter(pitch_id %in% ids)
      
      if(nrow(data) == 0) return(NULL)
      
      details <- data %>%
        select(
          Pitch = TaggedPitchType,
          Velo = RelSpeed,
          Spin = SpinRate,
          HB = HorzBreak,
          IVB = InducedVertBreak,
          Date
        ) %>%
        mutate(
          across(c(Velo, Spin, HB, IVB), ~round(., 1)),
          Date = format(as.Date(Date), "%m/%d/%y") # Consistent compact date format
        )
      
      # Summary Data Pop Up 
      summary <- data %>%
        summarise(
          n = n(),
          Velo = round(mean(RelSpeed, na.rm = TRUE), 1), 
          Spin = round(mean(SpinRate, na.rm = TRUE), 0),  
          HB = round(mean(HorzBreak, na.rm = TRUE), 1),   
          IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1)  
        )
      
      list(details = details, summary = summary)
    }
    
    data1 <- prepare_data(selections$first)
    data2 <- prepare_data(selections$second)
    
    #Sets Layout for Popup (Averages On Top, raw data On bottom)
    showModal(
      modalDialog(
        title = "Pitch Group Comparison",
        tags$style(HTML("
        .compact-table td, .compact-table th {
          padding: 4px 8px;
          white-space: nowrap;
        }
        .scroll-box {
          max-height: 300px;
          overflow-y: auto;
          margin-bottom: 15px;
        }
      ")),
        fluidRow(
          column(12,
                 h4("Averages"),
                 HTML(paste0(
                   "<table class='table table-bordered compact-table' style='width:100%; margin-bottom:20px;'>",
                   "<tr><th>Metric</th><th>Selection 1 (n=", data1$summary$n, ")</th>",
                   "<th>Selection 2 (n=", data2$summary$n, ")</th><th>Difference</th></tr>",
                   "<tr><td>Velo (mph)</td><td>", data1$summary$Velo, "</td><td>", data2$summary$Velo, "</td>",
                   "<td>", round(data1$summary$Velo - data2$summary$Velo, 1), "</td></tr>",
                   "<tr><td>Spin (rpm)</td><td>", data1$summary$Spin, "</td><td>", data2$summary$Spin, "</td>",
                   "<td>", round(data1$summary$Spin - data2$summary$Spin, 0), "</td></tr>",
                   "<tr><td>HB (in)</td><td>", data1$summary$HB, "</td><td>", data2$summary$HB, "</td>",
                   "<td>", round(data1$summary$HB - data2$summary$HB, 1), "</td></tr>",
                   "<tr><td>IVB (in)</td><td>", data1$summary$IVB, "</td><td>", data2$summary$IVB, "</td>",
                   "<td>", round(data1$summary$IVB - data2$summary$IVB, 1), "</td></tr>",
                   "</table>"
                 )),
                 
                 h4("Raw Data"),
                 fluidRow(
                   column(6,
                          h5(paste("Selection 1 (n =", data1$summary$n, ")")),
                          div(class = "scroll-box",
                              renderTable(data1$details, width = "100%")
                          )
                   ),
                   column(6,
                          h5(paste("Selection 2 (n =", data2$summary$n, ")")),
                          div(class = "scroll-box",
                              renderTable(data2$details, width = "100%")
                          )
                   )
                 )
          )
        ),
        size = "l",
        footer = modalButton("Close"),
        easyClose = TRUE
      )
    )
  })
  # ------------------ Movement Plot (tab 1) -----------------------------------
  
  # Plots IVB and HB
  # Points are clickable, can unselect outliers and remove them from metrics table 
  # (and all other charts on different tabs)
  # Also can lasso select two portions of a table and compare them using "show comparison" button
  
  
  output$movement_plot <- renderPlotly({
    data <- filtered_data()
    selected <- selected_pitches()
    
    req(data, selected)
    
    color_map <- pitch_color_map()
    
    data$selected_flag <- data$pitch_id %in% selected
    selected_data <- data[data$selected_flag, ]
    unselected_data <- data[!data$selected_flag, ]
    
    
    # Fix Range. A square movement plot is important to visualize the pitch shapes
    x_range <- range(data$HorzBreak, na.rm = TRUE)
    y_range <- range(data$InducedVertBreak, na.rm = TRUE)
    range <- range(c(x_range +5, y_range+5))
    max <- max(range)
    
    
    # Create Unselected (Grayed Out) Points
    p <- plot_ly(source = "pitch_click") %>%
      add_trace(
        data = unselected_data,
        x = ~HorzBreak,
        y = ~InducedVertBreak,
        type = "scatter",
        mode = "markers",
        marker = list(color = "lightgray", size = 5, opacity = 0.5),
        text = ~paste("Pitch:", TaggedPitchType,
                      "<br>Velo:", round(RelSpeed,1), "mph",
                      "<br>Spin:", round(SpinRate,0), "rpm",
                      "<br>HB:", round(HorzBreak,1), "in",
                      "<br>IVB:", round(InducedVertBreak,1), "in",
                      "<br>Date:", Date),
        hoverinfo = "text",
        customdata = ~ pitch_id,
        showlegend = FALSE) %>% 
      
      # Create Selected Points (use pitch type colors) 
      add_trace(
        data = selected_data,
        x = ~HorzBreak,
        y = ~InducedVertBreak,
        color = ~TaggedPitchType,
        colors = color_map,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10),
        text = ~paste("Pitch:", TaggedPitchType,
                      "<br>Velo:", round(RelSpeed,1), "mph",
                      "<br>Spin:", round(SpinRate,0), "rpm",
                      "<br>HB:", round(HorzBreak,1), "in",
                      "<br>IVB:", round(InducedVertBreak,1), "in",
                      "<br>Date:", Date),
        hoverinfo = "text",
        customdata = ~ pitch_id) %>%
      layout(
        dragmode = "lasso",
        showlegend = TRUE,
        title = paste(input$player_name, "Pitch Movement"),
        xaxis = list(
          title = "Horizontal Break (inches)", 
          scaleanchor = "y",
          scaleration = 1,
          range = c(-max,max),
          autorange = FALSE
        ),
        yaxis = list(
          title = "Horizontal Break (inches)",
          range = c(-max,max),
          autorange = FALSE
        ),
        autosize = FALSE,
        width = 800,
        height = 600
      )
    event_register(p, 'plotly_click')
    event_register(p, 'plotly_selected')
    
    p
  })
  
  # -------------------------- Movement Plot Static (ggplot) - for PDF --------
  
  # Same thing as above but in ggplot format (static) for use in pdf download
  # This is not displayed in the UI, just  used in the PDF Output 
  
  make_movement_plot_gg <- function(data, selected_ids, player_name = "Player", color_map = NULL) {
    data$selected_flag <- data$pitch_id %in% selected_ids
    
    # Compute fixed range - *Important for visualizing pitch shapes*
    x_range <- range(data$HorzBreak, na.rm = TRUE)
    y_range <- range(data$InducedVertBreak, na.rm = TRUE)
    combined_range <- range(c(x_range + 5, y_range + 5))
    max_range <- max(combined_range)
    
    gg <- ggplot(data, aes(x = HorzBreak, y = InducedVertBreak)) +
      # Create Unselected (grayed out) points
      geom_point(
        data = data[!data$selected_flag, ],
        color = "lightgray",
        alpha = 0.5,
        size = 1
      ) +
      # Create selected points (color by pitch type)
      geom_point(
        data = data[data$selected_flag, ],
        aes(color = TaggedPitchType),
        size = 2
      ) +
      
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      
      coord_cartesian(
        xlim = c(-max_range, max_range),
        ylim = c(-max_range, max_range)
      ) +
      labs(
        title = paste("Pitch Movement"),
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)",
        color = "Pitch Type"
      ) +
      theme_minimal()+ 
      theme(plot.title = element_text(size = 10, hjust = 0.5))
    
    # Use color map if provided
    if(!is.null(color_map)){
      gg <- gg + scale_color_manual(values = color_map)
    } else {
      gg <- gg + scale_color_brewer(palette = "Set1")
    }
    
    return(gg)
  }
  
  #---------------------------- Metrics Table (bottom of tab 1) ----------------
  
  # --------------------------- Calculate Values -------------------------------
  observeEvent({ filtered_data(); selected_pitches() }, {
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    # if no data, just return empty table
    if (nrow(data) == 0) {
      tables$metrics_table <- NULL
      tables$metrics_html <- NULL
      return()
    }
    
    # Stuff+ Calculations
    # Time Consuming but need to be done every time because pitcher averages change
    
    # Uses a saved model (also available on my github)
    scaler_params <- fromJSON("robust_scaler_params.json")

    # 2. Feature order to match how model was trained (in python)
    PYTHON_FEATURE_ORDER <- c(
      "release_speed", "release_spin_rate", "release_extension",
      "pfx_x", "pfx_z", "release_pos_x", "release_pos_z",
      "vx0", "vy0", "vz0", "ax", "ay",
      "spin_axis_sin", "spin_axis_cos",
      "velo_diff", "ivb_diff", "hb_diff"
    )
    
    stuff_features <- c("RelSpeed", "SpinRate", "Extension", "RelSide",
                        "RelHeight", "ax0", "ay0", "HorzBreak", "InducedVertBreak", 
                        "SpinAxis")

    # Modified get_pitch_metrics with proper scaling
    get_scaled_pitch_metrics <- function(data) {
      
      # Calculate primary fb type (most common fastball variant)
      fb_types <- c("Fastball", "Sinker", "Cutter", "Splitter")
      fb_only <- data %>%
        filter(TaggedPitchType %in% fb_types) %>%
        filter(!is.na(RelSpeed), !is.na(InducedVertBreak), !is.na(HorzBreak))

      primary_fb <- names(which.max(table(fb_only$TaggedPitchType)))

      # Calculate FB averages for pitcher's primary fastball
      fb_avgs <- fb_only %>%
        filter(TaggedPitchType == primary_fb) %>%
        summarize(
          fb_velo = mean(RelSpeed, na.rm = TRUE),
          fb_ivb = mean(InducedVertBreak, na.rm = TRUE) / 12,
          fb_hb = mean(HorzBreak, na.rm = TRUE) / 12
        )

      # Calculate all features (UNSCALED)
      features <- data %>%
        mutate(
          # Differential vs pitcher's primary FB
          velo_diff = RelSpeed - fb_avgs$fb_velo,
          ivb_diff = (InducedVertBreak / 12) - fb_avgs$fb_ivb,
          hb_diff = (HorzBreak / 12) - fb_avgs$fb_hb,

          # Spin axis components
          spin_axis_rad = pi * SpinAxis / 180,
          spin_axis_sin = sin(spin_axis_rad),
          spin_axis_cos = cos(spin_axis_rad),

          # Movement in feet
          pfx_x = HorzBreak / 12,
          pfx_z = InducedVertBreak / 12,

          # Rename other features to match Python
          release_speed = RelSpeed,
          release_spin_rate = SpinRate,
          release_extension = Extension,
          release_pos_x = RelSide,
          release_pos_z = RelHeight,
          ax = ax0,
          ay = ay0
        ) %>%
        select(all_of(PYTHON_FEATURE_ORDER))

      # Apply RobustScaler transformation
      scaled_features <- as.data.frame(scale(
        features,
        center = scaler_params$center_,
        scale = scaler_params$scale_
      ))

      return(scaled_features)
    }

    calculate_stuff_plus <- function(pitch_features, model_path) {
      # Load model
      model <- lgb.load(model_path)

      # Ensure correct feature order
      pitch_features <- pitch_features[, PYTHON_FEATURE_ORDER]

      # League normalization constants from Python
      # Need to be updated everytime model is retrained
      league_avg <- 0.2754554166236993
      league_std <- 0.07958256262837364

      # Predict (LightGBM expects matrix)
      pred_data <- as.matrix(pitch_features)
      pred_csw <- predict(model, pred_data)

      # Calculate Stuff+
      stuff_plus <- 100 + 10 * ((pred_csw - league_avg) / league_std)

      return(as.numeric(stuff_plus))
    }

    # Actually Calculate Stuff+ on our data
    stuff_plus_data_clean <- data %>%
      filter(across(all_of(stuff_features), ~ !is.na(.)))

    # Get SCALED features
    scaled_metrics <- get_scaled_pitch_metrics(stuff_plus_data_clean)

    # Calculate Stuff+
    stuff_plus_data_clean$stuff_plus <- calculate_stuff_plus(
      scaled_metrics,
      model_path = "lightboost_stuff_model.txt" #Model saved as txt
    )

    # Merge back with original data
    data <- data %>%
      left_join(
        stuff_plus_data_clean %>% select(pitch_id, stuff_plus),
        by = "pitch_id"
      )
    
    data_with_stuff(data)

    # Compute summary table 
    pitch_type_summary <- tryCatch({
      data %>%
        filter(!is.na(TaggedPitchType)) %>%
        group_by(PitchType = TaggedPitchType) %>%
        summarize(
          Pitches = n(),
          `Avg Velo (mph)` = round(mean(RelSpeed, na.rm = TRUE), 2),
          `Max Velo (mph)` = round(max(RelSpeed, na.rm = TRUE), 2),
          `Avg Spin Rate (rpm)` = round(mean(SpinRate, na.rm = TRUE), 2),
          `Avg HB (in)` = round(mean(HorzBreak, na.rm = TRUE), 2),
          `Avg IVB (in)` = round(mean(InducedVertBreak, na.rm = TRUE), 2),
          `Avg Extension (ft)` = round(mean(Extension, na.rm = TRUE), 2),
          `Avg Release Height (ft)` = round(mean(RelHeight, na.rm = TRUE), 2),
          `Avg Release Side (ft)` = round(mean(RelSide, na.rm = TRUE), 2),
          `Avg Stuff+` = round(mean(stuff_plus, na.r=TRUE),0),
          #`Max Stuff+` = round(max(stuff_plus, na.rm = TRUE),0),
          .groups = "drop"
        ) %>%
        arrange(desc(Pitches))
    }, error = function(e) {
      message("Error summarizing pitch data: ", e$message)
      # fallback empty data.frame to avoid app crash
      data.frame()
    })
    
    # For now don't include Stuff+ in PDF Export
    # Create new table withot Stuff+ col
    
    pitch_type_summary_pdf <- pitch_type_summary %>%
      select(-`Avg Stuff+`)
    
    tables$metrics_table_pdf <- pitch_type_summary_pdf
    
    # Add to tables for display in pdf/html 
    tables$metrics_table <- pitch_type_summary
    tables$metrics_html <- htmltools::tagList(
      htmltools::tags$h4("Pitch Type Summary"),
      htmlTable::htmlTable(pitch_type_summary_pdf)
    )
  })
  
  
  # ------------------------- Render Table (UI) --------------------------------
  
  # Create output for display in UI
  output$metrics_table <- renderTable({
    req(tables$metrics_table)
    tables$metrics_table
  })
  
  # ------------------------- Stuff+ dist plot (tab 2) -------------------------
  
  # Shows Distribution of Stuff+ for each pitch type
  
  output$stuff_dist_plot <- renderPlotly({
    req(filtered_data(), selected_pitches())
    
    data <- data_with_stuff()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids,]
    
    pitch_types <- unique(data$TaggedPitchType)
    pitch_types <- pitch_types[!is.na(pitch_types)]
    
    plot_list <- list()
    
    for (pt in pitch_types){
      pt_data <- filter(data, TaggedPitchType == pt) %>%
        filter(!is.na(stuff_plus))
      
      if (nrow(pt_data) > 1){
        min_stuff <- min(pt_data$stuff_plus)
        max_stuff <- max(pt_data$stuff_plus)
        
        dens <- density(pt_data$stuff_plus, na.rm = TRUE, from = min_stuff, to = max_stuff)
        
        p <- plot_ly() %>%
          add_trace(
            x = dens$x,
            y = dens$y,
            type = 'scatter',
            mode = 'lines',
            name = pt,
            fill = 'tozeroy',
            line = list(shape = "spline", smoothing = 0.5),
            hoverinfo = "text",
            text = paste0(
              "Pitch: ", pt, 
              "<br>Stuff+: ", round(dens$x, 1),
              "<br>Density: ", round(dens$y, 2),
              "<br>Observed Range: ", round(min_stuff,1), "-", round(max_stuff,1)
            )
          ) %>%
          layout(
            title = list(
              text = paste("<b>Stuff+ Distribution</b>"),
              x = 0.05,  
              font = list(size = 14)
            ),
            xaxis = list(title= "Stuff+"),
            showlegend = FALSE
          )
        plot_list[[pt]] <- p
      }
    }
    
    final_plot <- subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
    final_plot
    
  })
  
  # ------------------------ velo dist plot (tab 2) ----------------------------
  
  # Shows Distribution of velocity 
  # Gives you a sense of how consistent  a pitcher is 
  
    output$velo_dist_plot <- renderPlotly({
      req(filtered_data(), selected_pitches())
      
      data <- filtered_data()
      selected_ids <- selected_pitches()
      data <- data[data$pitch_id %in% selected_ids,]
      
      pitch_types <- unique(data$TaggedPitchType)
      pitch_types <- pitch_types[!is.na(pitch_types)]
      
      plot_list <- list()
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          filter(!is.na(RelSpeed))
        
        if (nrow(pt_data) > 1){
          min_velo <- min(pt_data$RelSpeed)
          max_velo <- max(pt_data$RelSpeed)
          
          dens <- density(pt_data$RelSpeed, na.rm = TRUE, from = min_velo, to = max_velo)
        
        p <- plot_ly() %>%
          add_trace(
            x = dens$x,
            y = dens$y,
            type = 'scatter',
            mode = 'lines',
            name = pt,
            fill = 'tozeroy',
            line = list(shape = "spline", smoothing = 0.5),
            hoverinfo = "text",
            text = paste0(
              "Pitch: ", pt, 
              "<br>Velocity: ", round(dens$x, 1), " mph",
              "<br>Density: ", round(dens$y, 3),
              "<br>Observed Range: ", round(min_velo,1), "-", round(max_velo,1), " mph"
            )
          ) %>%
          layout(
            title = list(
              text = paste("<b>Velocity Distribution</b>"),
              x = 0.05,  
              font = list(size = 14)
            ),
            xaxis = list(title= "Velocity (mph)"),
            showlegend = FALSE
        )
        plot_list[[pt]] <- p
        }
      }
      
      final_plot <- subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
      final_plot
      
      })
  
    # -------------------Spin dist plot (tab 2) --------------------------------
    
    # Same Idea as above, just with spin rate instead of Velo
  
    output$spin_dist_plot <- renderPlotly({
      req(filtered_data(), selected_pitches())
      
      data <- filtered_data()
      selected_ids <- selected_pitches()
      data <- data[data$pitch_id %in% selected_ids,]
      
      pitch_types <- unique(data$TaggedPitchType)
      pitch_types <- pitch_types[!is.na(pitch_types)]
      
      plot_list <- list()
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          filter(!is.na(SpinRate))
        
        if (nrow(pt_data) > 1){
          min_spin <- min(pt_data$SpinRate)
          max_spin <- max(pt_data$SpinRate)
          
          dens <- density(pt_data$SpinRate, na.rm = TRUE, from = min_spin, to = max_spin)
          
          p <- plot_ly() %>%
            add_trace(
              x = dens$x,
              y = dens$y,
              type = 'scatter',
              mode = 'lines',
              name = pt,
              fill = 'tozeroy',
              line = list(shape = "spline", smoothing = 0.5),
              hoverinfo = "text",
              text = paste0(
                "Pitch: ", pt, 
                "<br>Spin Rate: ", round(dens$x, 1), " rpm",
                "<br>Density: ", round(dens$y, 3),
                "<br>Observed Range: ", round(min_spin,0), "-", round(max_spin,0), " rpm"
              )
            ) %>%
            layout(
              title = list(
                text = paste("<b>Spin Rate Distribution</b>"),
                x = 0.05,  
                font = list(size = 14)
              ),
              xaxis = list(title= "Spin Rate (rpm)"),
              showlegend = FALSE
            )
          plot_list[[pt]] <- p
        }
      }
      
      final_plot <- subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
      
    
      final_plot
    })
  
    # ------------------- ivb dist plot (tab 2) --------------------------------
    
    # Once again, same idea as above
  
    output$ivb_dist_plot <- renderPlotly({
      req(filtered_data(), selected_pitches())
      
      data <- filtered_data()
      selected_ids <- selected_pitches()
      data <- data[data$pitch_id %in% selected_ids,]
      
      pitch_types <- unique(data$TaggedPitchType)
      pitch_types <- pitch_types[!is.na(pitch_types)]
      
      plot_list <- list()
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          filter(!is.na(InducedVertBreak))
        
        if (nrow(pt_data) > 1){
          min_ivb <- min(pt_data$InducedVertBreak)
          max_ivb <- max(pt_data$InducedVertBreak)
          
          dens <- density(pt_data$InducedVertBreak, na.rm = TRUE, from = min_ivb, to = max_ivb)
          
          p <- plot_ly() %>%
            add_trace(
              x = dens$x,
              y = dens$y,
              type = 'scatter',
              mode = 'lines',
              name = pt,
              fill = 'tozeroy',
              line = list(shape = "spline", smoothing = 0.5),
              hoverinfo = "text",
              text = paste0(
                "Pitch: ", pt, 
                "<br>IVB: ", round(dens$x, 1), "in",
                "<br>Density: ", round(dens$y, 3),
                "<br>Observed Range: ", round(min_ivb,0), "-", round(max_ivb,0), "in"
              )
            ) %>%
            layout(
              title = list(
                text = paste("<b>Induced Verticle Break Distribution</b>"),
                x = 0.05,  
                font = list(size = 14)
              ),
              xaxis = list(title= "IVB (in)"),
              showlegend = FALSE
            )
          plot_list[[pt]] <- p
        }
      }
      
      final_plot <- subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
      
      final_plot
    })
    
    # ------------------ create HB dist plot (tab 2)  ---------------------------
    
    # Same idea as above, but for HB 
  
    output$hb_dist_plot <- renderPlotly({
      req(filtered_data(), selected_pitches())
      
      data <- filtered_data()
      selected_ids <- selected_pitches()
      data <- data[data$pitch_id %in% selected_ids,]
      
      pitch_types <- unique(data$TaggedPitchType)
      pitch_types <- pitch_types[!is.na(pitch_types)]
      
      plot_list <- list()
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          filter(!is.na(HorzBreak))
        
        if (nrow(pt_data) > 1){
          min_hb <- min(pt_data$HorzBreak)
          max_hb <- max(pt_data$HorzBreak)
          
          dens <- density(pt_data$HorzBreak, na.rm = TRUE, from = min_hb, to = max_hb)
          
          p <- plot_ly() %>%
            add_trace(
              x = dens$x,
              y = dens$y,
              type = 'scatter',
              mode = 'lines',
              name = pt,
              fill = 'tozeroy',
              line = list(shape = "spline", smoothing = 0.5),
              hoverinfo = "text",
              text = paste0(
                "Pitch: ", pt, 
                "<br>HB: ", round(dens$x, 1), " mph",
                "<br>Density: ", round(dens$y, 3),
                "<br>Observed Range: ", round(min_hb,0), "-", round(max_hb,0), "in"
              )
            ) %>%
            layout(
              title = list(
                text = paste("<b>Horizontal Break Distribution</b>"),
                x = 0.05,  
                font = list(size = 14)
              ),
              xaxis = list(title= "HB (in)"),
              showlegend = FALSE
            )
          plot_list[[pt]] <- p
        }
      }
      
      final_plot <- subplot(plot_list, nrows = length(plot_list), shareX = TRUE, shareY = TRUE)
      
      final_plot
    })
    
    # --------------------- Heat Maps (tab 3) ----------------------------------
    
    # Creates heat maps to show locations of pitches, grouped by pitch type 
  
    output$pitch_loc_chart <- renderPlotly({
      
      #Color vector from Gabe Appelbam 
      #(https://medium.com/@gabriel.appelbaum/how-to-create-a-shiny-app-in-r-using-pitching-trackman-data-a9919b29d2c3)
      
      color_vec <- c(
        "#f7f7f7", 
        "#fff7bc",
        "#fee391",
        "#fec44f",
        "#fe9929",
        "#ec7014",
        "#cc4c02",
        "#993404",
        "#662506"
      )
    
      color_scale <- lapply(seq_along(color_vec), function(i) {
        list((i - 1) / (length(color_vec) - 1), color_vec[i])
      })
      
      data <- filtered_data()
      plot_list <- list()
      pitch_types <- unique(data$TaggedPitchType) %>% na.omit()
      
      x_range <- c(-3,3)
      y_range <- c(0,6)
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
        
        #print(paste("Pitch type:", pt, "nrow:", nrow(pt_data)))
        
        if(nrow(pt_data) > 0){
          p <- plot_ly(
            pt_data,
            x = ~PlateLocSide,
            y = ~PlateLocHeight,
            type = "histogram2dcontour",
            colorscale = color_scale,
            hoverinfo = "none",
            showscale = FALSE)
          p <- add_markers(
            p,
            x = ~PlateLocSide,
            y = ~PlateLocHeight,
            marker = list(size = 4, opacity = 0.5, color = "black"),
            hoverinfo = "text",
            text = ~paste("<br>X:", PlateLocSide, "in",
                          "<br>Y:", PlateLocHeight, "in"),
            showlegend = FALSE)
          
          p <- layout(
            p,
            annotations = list(
              list(
                text = pt,
                x = 0.3,
                y = .9,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size =14),
                color = "black"
              )
            ),
            xaxis = list(visible = FALSE, title = "", range = x_range),
            yaxis = list(title = "", range = y_range),
            shapes = list(
              list(
                type = "rect",
                x0 = -0.7, x1 = 0.7,
                y0 = 1.6, y1 = 3.4,
                line = list(color = "black"),
                fillcolor = "transparent"
                )
              ),
              autosize = FALSE,
              width = 400,
              height = 400
            )
        
        #plot_list[[pt]] <- p
        plot_list[[length(plot_list) + 1]] <- p
        }
      }
      
      n_plots <- length(plot_list)
      if(n_plots == 0){
        return(plotly::plot_ly() %>%
                 layout(title = "No data available"))
      }
      
      n_cols <- min (2, n_plots)
      n_rows <- ceiling(n_plots / n_cols)
      
      
      result <- subplot(
        plot_list,
        nrows = n_rows,
        shareX = FALSE,
        shareY = FALSE,
        titleX = FALSE,
        titleY = FALSE
      )
      
      result <- layout(
        result,
      height = 400 * n_rows,
      width = 400 * n_cols
      )
      
      result
      
    })
  # --------------------------- Static Version - pitch locations ---------------
  
  # ggplot has a slightly different look, but conveys same information
  make_pitch_loc_plot_gg <- function(data, selected_ids) {
    # Filter data similar to your movement plot
    plot_data <- data %>%
      filter(pitch_id %in% selected_ids,
             !is.na(TaggedPitchType),
             !is.na(PlateLocSide),
             !is.na(PlateLocHeight))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               ggtitle("No data available") + 
               theme_minimal())
    }
    
    # Create plot following movement plot's structure
    ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..level..), 
                      geom = "polygon",
                      color = NA, 
                      alpha = 0.8) +
      scale_fill_gradientn(
        colors = c("#f7f7f7", "#fff7bc", "#fee391", "#fec44f", "#fe9929",
                   "#ec7014", "#cc4c02", "#993404", "#662506"),
        name = "Density"
      ) +
      geom_point(alpha = 0.5, size = 1, color = "black") +
      facet_wrap(~TaggedPitchType, nrow = 1,
                 labeller = labeller(TaggedPitchType = function(x){
                   paste(x, "Location (Pitcher's View)")
                 })) +
      geom_rect(aes(xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.4),
                color = "black", fill = NA, inherit.aes = FALSE) +
      coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 6),
        axis.title = element_blank()
      )
  }
  
  #------------------------ release point plot (4th tab) ---------------------
  # 3D release point plot w/ centroids 
  output$release_point_plot <- renderPlotly({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    
    data$selected_flag <- data$pitch_id %in% selected_ids
    selected_data <- data[data$selected_flag, ]
    unselected_data <- data[!data$selected_flag, ]
    
    # calculate centroids
    centroids <- selected_data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        mean_side = mean(RelSide, na.rm = TRUE),
        mean_ext = mean(Extension, na.rm=TRUE),
        mean_height = mean(RelHeight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    color_map <- pitch_color_map()
    
    p <- plot_ly(source = "pitch_click")
    
    # print unseected (greyed out) points
    if(nrow(unselected_data) >0){
      p <- p %>%
        add_trace(
          data = unselected_data,
          x = ~RelSide,
          y = ~Extension,
          z = ~RelHeight,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 4, color = "lightgrey", opacity = 0.5),
          text = ~paste("Pitch:", TaggedPitchType,
                        "<br>Date:", Date,
                        "<br>Velo:", round(RelSpeed,1) , "mph",
                        "<br>Release Height:", round(RelHeight,1), "ft",
                        "<br>Release Side:", round(RelSide,1), "ft",
                        "<br>Extension:", round(Extension,1), "ft"
          ),
          hoverinfo = "text",
          customdata = ~pitch_id,
          showlegend = FALSE
        )
    }
    
    # print selected (colored by pitch type) points
    p <- p %>%
      add_trace(
        data = selected_data,
        x = ~RelSide,
        y = ~Extension,
        z = ~RelHeight,
        color = ~TaggedPitchType,
        colors = color_map,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 6),
        text = ~paste("Pitch:", TaggedPitchType,
                      "<br>Date:", Date,
                      "<br>Velo:", round(RelSpeed,1) , "mph",
                      "<br>Release Height:", round(RelHeight,1), "ft",
                      "<br>Release Side:", round(RelSide,1), "ft",
                      "<br>Extension:", round(Extension,1), "ft"
        ),
        hoverinfo = "text",
        customdata = ~pitch_id
      )
    
    # print pitching "mound" 
    # helps visualize the chart
    
    x_seq <- seq(-2, 2, length.out = 40)       # width of the mound
    y_seq <- seq(0, 10, length.out = 40)       # length of the mound/ramp
    
    ramp_grid <- expand.grid(x = x_seq, y = y_seq)
    ramp_grid$z <- with(ramp_grid, 0.833 * (1 - y / 10))  # linear slope from 10 in to 0
    
    # Convert to matrix format for surface
    x_mat <- matrix(ramp_grid$x, nrow = length(x_seq))
    y_mat <- matrix(ramp_grid$y, nrow = length(x_seq))
    z_mat <- matrix(ramp_grid$z, nrow = length(x_seq))
    
    # Add ramp surface to plot
    p <- p %>%
      add_surface(
        x = ~x_mat,
        y = ~y_mat,
        z = ~z_mat,
        showscale = FALSE,
        opacity = 0.6,
        surfacecolor = matrix(rep(0.5, length(z_mat)), nrow = nrow(z_mat)),
        colorscale = list(c(0, "saddlebrown"), c(1, "sienna")),
        hoverinfo = "none",
        name = "Mound Ramp"
      )

    rubber_x <- c(-1, 1, 1, -1, -1)         # 2 ft wide
    rubber_y <- c(-0.25, -0.25, 0.25, 0.25, -0.25)  # 0.5 ft deep
    rubber_z <- rep(0.833, 5)              # Constant height at top of mound
    
    # add pitching rubber to plot
    p <- p %>%
      add_trace(
        x = rubber_x,
        y = rubber_y,
        z = rubber_z,
        type = "scatter3d",
        mode = "lines",
        line = list(color = "white", width = 8),
        fill = "toself",
        fillcolor = "white",
        opacity = 1,
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
      # plot centroids
      for (pt in unique(centroids$TaggedPitchType)){
      centroid <- filter(centroids, TaggedPitchType == pt)
      
      p <- p %>%
        add_trace(
          x = centroid$mean_side,
          y = centroid$mean_ext,
          z = centroid$mean_height,
          type = "scatter3d",
          mode = "markers",
          marker = list(
            size = 8, 
            color = color_map[pt],
            symbol = "diamond",
            line = list(color = "black", width = 1)
          ),
          name = paste(pt, "Centroid"),
          legendgroup = pt, 
          showlegend = TRUE,
          text = paste(pt, "Centroid"),
          hoverinfo = "text"
        )
    }
    
    p %>% layout(
      title = paste("Release Point & Centroids"),
      showlegend = TRUE,
      scene = list(
        xaxis = list(title = "Release Side (ft)"),
        yaxis = list(title = "Extension (ft)"),
        zaxis = list(title = "Release Height (ft)")
      ),
      width = 1000,
      height = 700,
      margin = list(l = 0, r = 0, b = 0, t = 50)
    )
    event_register(p, 'plotly_click')
  
    p
    
  })
  
  # ------------------------- Static (2D) Release Point Plot -------------------
  # Since the PDF output has to be static, I figured a 2d release plot made more sense
  # TO make it easier to see, this only plots the centroids for each pitch
  
  make_release_point_gg <- function(data, selected_ids, color_map = NULL) {
    # Filter data for selected pitches
    plot_data <- data %>%
      filter(pitch_id %in% selected_ids)
    
    # Handle empty data case
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               ggtitle("No data available") + 
               theme_minimal())
    }
    
    centroids <- plot_data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        mean_side = mean(RelSide, na.rm = TRUE),
        mean_height = mean(RelHeight, na.rm = TRUE),
        .groups = 'drop'
      )

    
    color_map <- pitch_color_map()
    
    xmax <- max(abs(centroids$mean_side)) + 1
    ymax <- max(centroids$mean_height) + 1
    
    # Base plot
    gg <- ggplot(centroids, aes(x = mean_side, y = mean_height, color = TaggedPitchType)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        title = "Release Points",
        x = "Release Side (ft)",
        y = "Release Height (ft)", 
        color = "Pitch Type"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 10, hjust = 0.5)
      ) + 
      xlim(-xmax, xmax) + 
      ylim(-.5, ymax) + 
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black")
    
    
    # Apply color map if provided, otherwise use default colors
    if (!is.null(color_map)) {
      gg <- gg + scale_color_manual(values = color_map)
    } else {
      gg <- gg + scale_color_brewer(palette = "Set1")
    }
    
    return(gg)
  }
    # ------------release point table (within pitch variance) - 4th tab -----------------
    # This calculates the sd of distances from centroid for each pitch type (within pitch variance)
  
    output$release_point_table <- renderTable({
      data <- filtered_data()
      selected_ids <- selected_pitches()
      data <- data[data$pitch_id %in% selected_ids,]
      
      pitch_types <- unique(data$TaggedPitchType) %>% na.omit()
      
      results <- list()
      
      for (pt in pitch_types){
        pt_data <- filter(data, TaggedPitchType == pt) %>%
          select(RelSide,Extension,RelHeight) %>%
          na.omit()
        
        centroid <- c(mean(pt_data$RelSide), mean(pt_data$Extension), mean(pt_data$RelHeight))
    
        distances <- sqrt(
          (pt_data$RelSide - centroid[1])^2 +
            (pt_data$Extension - centroid[2])^2 +
            (pt_data$RelHeight - centroid[3])^2
        )
        
        results[[pt]] <- sd(distances)
      }
      results_df <- data.frame(
        PitchType = names(results),
        StandardDeviation = unlist(results) %>% round(3),
        stringsAsFactors = FALSE
      )
      
      results_df
    })
  
  # --------------------------- Centroid Distance Matrix -----------------------
  
  # Calculates distances between each centroid. Shows between pitch variance
  output$centroid_dist_table <- renderTable({
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    # Step 1: Compute centroids
    centroids <- data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        mean_side = mean(RelSide, na.rm = TRUE),
        mean_ext = mean(Extension, na.rm = TRUE),
        mean_height = mean(RelHeight, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      na.omit()
    
    if (nrow(centroids) < 2) {
      return(data.frame(Message = "Not enough pitch types selected to compute distances."))
    }
    
    # Step 2: Pairwise Euclidean distances
    centroid_coords <- centroids %>%
      select(mean_side, mean_ext, mean_height) %>%
      as.matrix()
    
    pitch_types <- centroids$TaggedPitchType
    rownames(centroid_coords) <- pitch_types
    
    dist_matrix <- as.matrix(dist(centroid_coords, method = "euclidean"))
    
    # Step 3: Long form
    dist_df <- as.data.frame(as.table(dist_matrix)) %>%
      rename(PitchType1 = Var1, PitchType2 = Var2, Distance = Freq) %>%
      filter(PitchType1 != PitchType2) %>%
      rowwise() %>%
      mutate(Pair = paste(sort(c(PitchType1, PitchType2)), collapse = " vs ")) %>%
      ungroup() %>%
      distinct(Pair, .keep_all = TRUE) %>%
      mutate(`Distance (in)` = round(Distance * 12, 1)) %>% # convert to inches, round to 1 decimal
      select(PitchType1, PitchType2, `Distance (in)`) 
    
    dist_df
  })
  
  
#----------------------------Spin Clock (30min bins) Tab 5----------------------
  # Bins Spin into 30 min bins
  # average spin within the bin dictates how long the bar is
  
  output$clock <- renderPlotly({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    color_map <- pitch_color_map()
    
    # Clean Tilt values
    data$Tilt[data$Tilt == ""] <- NA
    data <- data[!is.na(data$Tilt), ]
    
    # Map to 30-minute bins
    tilt_15_to_30 <- c(
      "12:00" = "12:00", "12:15" = "12:30", "12:30" = "12:30", "12:45" = "1:00",
      "1:00" = "1:00", "1:15" = "1:30", "1:30" = "1:30", "1:45" = "2:00",
      "2:00" = "2:00", "2:15" = "2:30", "2:30" = "2:30", "2:45" = "3:00",
      "3:00" = "3:00", "3:15" = "3:30", "3:30" = "3:30", "3:45" = "4:00",
      "4:00" = "4:00", "4:15" = "4:30", "4:30" = "4:30", "4:45" = "5:00",
      "5:00" = "5:00", "5:15" = "5:30", "5:30" = "5:30", "5:45" = "6:00",
      "6:00" = "6:00", "6:15" = "6:30", "6:30" = "6:30", "6:45" = "7:00",
      "7:00" = "7:00", "7:15" = "7:30", "7:30" = "7:30", "7:45" = "8:00",
      "8:00" = "8:00", "8:15" = "8:30", "8:30" = "8:30", "8:45" = "9:00",
      "9:00" = "9:00", "9:15" = "9:30", "9:30" = "9:30", "9:45" = "10:00",
      "10:00" = "10:00", "10:15" = "10:30", "10:30" = "10:30", "10:45" = "11:00",
      "11:00" = "11:00", "11:15" = "11:30", "11:30" = "11:30", "11:45" = "12:00"
    )
    
    data$Tilt <- tilt_15_to_30[data$Tilt]
    data <- data[!is.na(data$Tilt), ]
    
    # Create angle mapping
    tilt_levels <- c(
      "12:00", "12:30", "1:00", "1:30", "2:00", "2:30", "3:00", "3:30",
      "4:00", "4:30", "5:00", "5:30", "6:00", "6:30", "7:00", "7:30",
      "8:00", "8:30", "9:00", "9:30", "10:00", "10:30", "11:00", "11:30"
    )
    
    tilt_degrees <- seq(0, 345, by = 15)
    tilt_lookup <- setNames(tilt_degrees, tilt_levels)
    data$theta <- tilt_lookup[data$Tilt]
    
    data <- data[complete.cases(data$TaggedPitchType, data$Tilt, data$SpinRate, data$theta), ]
    
    # Calculate summary statistics
    avg_data <- data %>%
      group_by(TaggedPitchType, Tilt) %>%
      summarize(
        avg_spin = mean(SpinRate, na.rm = TRUE),
        count = n(),
        theta = first(theta),
        .groups = "drop"
      )
    
    # Create plot
    p <- plot_ly()
    
    # Add traces for each pitch type
    for (pt in unique(avg_data$TaggedPitchType)) {
      subset <- avg_data %>% filter(TaggedPitchType == pt) %>% arrange(theta)
      
      if (nrow(subset) > 0) {
        # Create hover text with explicit pitch type
        hover_text <- paste0(
          "Pitch: ", pt, "<br>",
          "Tilt: ", subset$Tilt, "<br>",
          "Avg Spin: ", round(subset$avg_spin, 0), " rpm<br>",
          "Count: ", subset$count
        )
        
        p <- p %>%
          add_trace(
            data = subset,
            type = 'barpolar',
            r = ~avg_spin,
            theta = ~theta,
            name = pt,
            text = hover_text,  
            hoverinfo = 'text',
            hoverlabel = list(align = "left"),
            marker = list(color = color_map[pt], line = list(width = 1)),
            showlegend = TRUE
          )
      }
    }
    
    # Configure layout
    p <- p %>%
      layout(
        polar = list(
          angularaxis = list(
            direction = "clockwise",
            rotation = 90,
            tickmode = "array",
            tickvals = seq(0, 330, by = 30),
            ticktext = c("12:00", "1:00", "2:00", "3:00", "4:00", "5:00", 
                         "6:00", "7:00", "8:00", "9:00", "10:00", "11:00"),
            showticklabels = TRUE
          ),
          radialaxis = list(
            title = "Avg Spin Rate (rpm)",
            ticksuffix = " rpm",
            showline = TRUE
          )
        ),
        title = list(
          text = "Average Spin Rate by Clock Angle and Pitch Type",
          y = 0.95
        ),
        margin = list(t = 100)
      )
    p
  })
  
  # -------------------- Static Version - spin clock ---------------------------
  make_spin_clock_gg <- function(data, selected_ids, color_map = NULL) {
    # Filter data for selected pitches
    plot_data <- data %>%
      filter(pitch_id %in% selected_ids)
    
    # Handle empty data case
    if (nrow(plot_data) == 0) {
      return(ggplot() + ggtitle("No data available") + theme_minimal())
    }
    
    # Clean Tilt values
    data$Tilt[data$Tilt == ""] <- NA
    data <- data[!is.na(data$Tilt), ]
    
    # Map to 30-minute bins
    tilt_15_to_30 <- c(
      "12:00" = "12:00", "12:15" = "12:30", "12:30" = "12:30", "12:45" = "1:00",
      "1:00" = "1:00", "1:15" = "1:30", "1:30" = "1:30", "1:45" = "2:00",
      "2:00" = "2:00", "2:15" = "2:30", "2:30" = "2:30", "2:45" = "3:00",
      "3:00" = "3:00", "3:15" = "3:30", "3:30" = "3:30", "3:45" = "4:00",
      "4:00" = "4:00", "4:15" = "4:30", "4:30" = "4:30", "4:45" = "5:00",
      "5:00" = "5:00", "5:15" = "5:30", "5:30" = "5:30", "5:45" = "6:00",
      "6:00" = "6:00", "6:15" = "6:30", "6:30" = "6:30", "6:45" = "7:00",
      "7:00" = "7:00", "7:15" = "7:30", "7:30" = "7:30", "7:45" = "8:00",
      "8:00" = "8:00", "8:15" = "8:30", "8:30" = "8:30", "8:45" = "9:00",
      "9:00" = "9:00", "9:15" = "9:30", "9:30" = "9:30", "9:45" = "10:00",
      "10:00" = "10:00", "10:15" = "10:30", "10:30" = "10:30", "10:45" = "11:00",
      "11:00" = "11:00", "11:15" = "11:30", "11:30" = "11:30", "11:45" = "12:00"
    )
    
    data$Tilt <- tilt_15_to_30[data$Tilt]
    data <- data[!is.na(data$Tilt), ]
    
    # Create angle mapping
    tilt_levels <- c(
      "12:00", "12:30", "1:00", "1:30", "2:00", "2:30", "3:00", "3:30",
      "4:00", "4:30", "5:00", "5:30", "6:00", "6:30", "7:00", "7:30",
      "8:00", "8:30", "9:00", "9:30", "10:00", "10:30", "11:00", "11:30"
    )
    
    tilt_degrees <- seq(0, 345, by = 15)
    tilt_lookup <- setNames(tilt_degrees, tilt_levels)
    data$theta <- tilt_lookup[data$Tilt]
    
    # Clean data
    data <- data[complete.cases(data$TaggedPitchType, data$Tilt, data$SpinRate, data$theta), ]
    
    # Calculate summary statistics
    avg_data <- data %>%
      group_by(TaggedPitchType, Tilt) %>%
      summarize(
        avg_spin = mean(SpinRate, na.rm = TRUE),
        count = n(),
        theta = first(theta),
        .groups = "drop"
      )
    
    gg <- ggplot(avg_data, aes(x = reorder(Tilt, theta), y = avg_spin, fill = TaggedPitchType)) +
      geom_col(position = position_dodge(width = 0.9), width = 0.8) +
      coord_polar(start = -pi/24) +  # Adjusted to match plotly's 12:00 position
      scale_x_discrete(limits = tilt_levels) +  # Ensure proper order
      labs(
        title = "Spin Rate by Clock Angle and Pitch Type",
        x = "Tilt Angle",
        y = "Avg Spin Rate (rpm)",
        fill = "Pitch Type"
      ) +
      scale_fill_manual(values = color_map) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = seq(0, 330, by = 30), hjust = 0.5, vjust = 0.5)
      )
    
    # Apply color map if provided, otherwise use brewer palette
    if (!is.null(color_map)) {
      gg <- gg + scale_color_manual(values = color_map)
    } else {
      gg <- gg + scale_color_brewer(palette = "Dark2")
    }
      return(gg)
    }
    
  
  # --------------------- Velo Over Time (Tab 6) ---------------------------------------
  # Shows plot of velo overtime of pen (for all pitch types)
  # As shown on USA baseball report
  # Will display error messsage if more than one day is selected
  
  output$velo_over_time <- renderPlotly({
    
    req(filtered_data(), selected_pitches(), input$selected_dates)
    
    selected_dates <- input$selected_dates
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    
    if (length(selected_dates) > 1) {
      return(
        plotly_empty() %>%
          layout(
            title = "Please select exactly one day",
            plot_bgcolor = "white",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          ) %>%
          add_annotations(
            text = "Date range must be exactly one day",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(size = 16)
          )
      )
    }
    
    # Since pitch number counts all pitches thrown in trackman session,
    # need to perform this transformation so the first pitch is pitch 1
    
    start_pitch <- data$PitchNo[1] - 1
    
    data$PitchNo <- data$PitchNo - start_pitch
    
    p <- plot_ly()
    
    print("Pitch Types:")
    print(data$TaggedPitchType)
    
    pitch_types <- unique(data$TaggedPitchType)
    print(pitch_types)
    
    for(pt in unique(data$TaggedPitchType)){
      data_pt <- data %>% filter(TaggedPitchType == pt)
      
      p <- p%>% add_trace(
        data = data_pt,
        x = ~ PitchNo,
        y = ~ RelSpeed,
        type = 'scatter', 
        mode = 'lines+markers',
        name = pt,
        text = ~paste("Pitch Type:", TaggedPitchType, "<br>Velo:", round(RelSpeed,2), "<br>Spin", round(SpinRate,0),
                      "<br>HB:", round(HorzBreak,1), "<br>IVB:", round(InducedVertBreak,1)),
        hoverinfo = "text"
        
      )
      
      p <- p %>% layout(
        xaxis = list(title = "Pitch Number"),
        yaxis = list(title = "Velocity (mph)", range = c(60,100))
      )
      
    }
    p
  })
  
  # ------------------ Static Version - velo over time -------------------------
  
  make_velo_over_time_gg <- function(data, selected_ids, color_map = NULL, selected_dates) {
    # Filter data for selected pitches
    plot_data <- data %>%
      filter(pitch_id %in% selected_ids)
    
    # Handle empty data case
    if (nrow(plot_data) == 0) {
      return(ggplot() + ggtitle("No data available") + theme_minimal())
    }
    
    # Adjust pitch numbers to start at 1
    start_pitch <- plot_data$PitchNo[1] - 1
    plot_data$PitchNo <- plot_data$PitchNo - start_pitch
    
    # Base plot
    gg <- ggplot(plot_data, aes(x = PitchNo, y = RelSpeed, color = TaggedPitchType)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2.5) +
      labs(
        title = "Velocity by Pitch",
        x = "Pitch Number",
        y = "Velocity (mph)",
        color = "Pitch Type"
      ) +
      ylim(60, 100) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 10)
      )
    
    # Apply color map if provided, otherwise use brewer palette
    if (!is.null(color_map)) {
      gg <- gg + scale_color_manual(values = color_map)
    } else {
      gg <- gg + scale_color_brewer(palette = "Dark2")
    }
    
    if(length(selected_dates) > 1){
      return(NULL)
    } else {
      return(gg)
    }
    
  }
# ----------------------------- trends (tab 7) -----------------------------------------
  
# ------------------------------ Velo Trends -----------------------------------
  output$Trends_velo <- renderPlotly({ 
    
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    color_map <- pitch_color_map()
    
    daily_velo <- data %>%
      group_by(TaggedPitchType, Date) %>%
      summarise(
        AvgVelo = mean(RelSpeed, na.rm = TRUE),
        MinVelo = min(RelSpeed, na.rm = TRUE),
        MaxVelo = max(RelSpeed, na.rm = TRUE),
        SDVelo = sd(RelSpeed, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>% arrange(TaggedPitchType, Date)
    
    alpha <- 0.3  # Smoothing factor between 0 and 1, adjust as needed
    
    daily_velo <- daily_velo %>%
      group_by(TaggedPitchType) %>%
      filter(!is.na(AvgVelo)) %>%
      mutate(
        EWMA = if (n() >= 2) {
          EMA(AvgVelo, n = 2, ratio = alpha)
        } else {
          rep(NA_real_, n())
        }
      ) %>%
      ungroup()
    
    p <- plot_ly() %>%
      layout(
        title = "Velo Trend", 
        xaxis = list(title = "Date"),
        yaxis = list(title = "Velocity (mph)"),
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    data <- left_join(data, daily_velo %>% select(TaggedPitchType, Date, EWMA, AvgVelo),
                      by = c("TaggedPitchType", "Date"))
    
    for(pt in unique(data$TaggedPitchType)){
      pitch_data <- data %>% filter(TaggedPitchType == pt)
      color <- color_map[pt]
      
      
      if ("Daily Avg" %in% input$plot_elements) {
      # Add Actual Daily Averages
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~AvgVelo,
        type = "scatter",
        name = ~paste0(TaggedPitchType, " Actual Daily Avg"),
        marker = list(color = color),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Pitch: ", TaggedPitchType, "<br>",
          "Date: ", Date, "<br>",
          "Actual Daily Avg: ", round(AvgVelo, 2)
        )
      )
      
    }
      
      if ("Boxplots" %in% input$plot_elements) {
        
      # add box plots
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~RelSpeed,
        type = "box",
        showlegend = FALSE,
        marker = list(color = color),
        line = list(color = color),
        fillcolor = paste0("rgba(", col2rgb(color)[1], ",", 
                           col2rgb(color)[2], ",", 
                           col2rgb(color)[3], ",0.3)"),
        boxpoints = FALSE,  # or "all" if you want individual pitch dots
        hoverinfo = "none"
      )
      
      }
      
      if ("Weighted Averages" %in% input$plot_elements){
        
      # Add weighted moving average line
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~EWMA,
        type = "scatter",
        mode = "lines",
        name = paste0(pt, " EWMA"),
        showlegend = TRUE,
        line = list(width = 3, color = color),
        hoverinfo = "text",
        hovertext = ~paste0(
          "EWMA: ", round(EWMA, 2), "<br>"
        )
      )
      }
    }
    p
  })
  
  # ------------------------ Spin Rate Trends ----------------------------------
  
  output$Trends_spin <- renderPlotly({ 
    
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    color_map <- pitch_color_map()
    
    daily_spin <- data %>%
      group_by(TaggedPitchType, Date) %>%
      summarise(
        AvgSpin = mean(SpinRate, na.rm = TRUE),
        MinSpin = min(SpinRate, na.rm = TRUE),
        MaxSpin = max(SpinRate, na.rm = TRUE),
        SDSpin = sd(SpinRate, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>% arrange(TaggedPitchType, Date)
    
    alpha <- 0.3  # Smoothing factor between 0 and 1, adjust as needed
    
    daily_spin <- daily_spin %>%
      group_by(TaggedPitchType) %>%
      filter(!is.na(AvgSpin)) %>%
      mutate(
        EWMA = if (n() >= 2) {
          EMA(AvgSpin, n = 2, ratio = alpha)
        } else {
          rep(NA_real_, n())
        }
      ) %>%
      ungroup()
    
    p <- plot_ly() %>%
      layout(
        title = "Spin Trend", 
        xaxis = list(title = "Date"),
        yaxis = list(title = "Spin Rate (rpm)"),
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    data <- left_join(data, daily_spin %>% select(TaggedPitchType, Date, EWMA, AvgSpin),
                      by = c("TaggedPitchType", "Date"))
    
    for(pt in unique(data$TaggedPitchType)){
      pitch_data <- data %>% filter(TaggedPitchType == pt)
      color <- color_map[pt]
      
      if ("Daily Avg" %in% input$plot_elements) {
        # Add Actual Daily Averages
        p <- p %>% add_trace(
          data = pitch_data,
          x = ~Date,
          y = ~AvgSpin,
          type = "scatter",
          name = ~paste0(TaggedPitchType, " Actual Daily Avg"),
          marker = list(color = color),
          hovertext = ~paste0(
            "Pitch: ", TaggedPitchType, "<br>",
            "Date: ", Date, "<br>",
            "Actual Daily Avg: ", round(AvgSpin, 0)
          ),
          hoverinfo = "text"
        )
        
      }
      
      if ("Boxplots" %in% input$plot_elements) {
        
      # add box plots
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~SpinRate,
        type = "box",
        marker = list(color = color),
        line = list(color = color),
        fillcolor = paste0("rgba(", col2rgb(color)[1], ",", 
                           col2rgb(color)[2], ",", 
                           col2rgb(color)[3], ",0.3)"),
        boxpoints = FALSE,  # or "all" if you want individual pitch dots
        hoverinfo = "none",
        showlegend = FALSE
      )
      
      }
      
      if ("Weighted Averages" %in% input$plot_elements) {
        
      # Add weighted moving average line
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~EWMA,
        type = "scatter",
        mode = "lines",
        name = ~paste0(TaggedPitchType, " EWMA"),
        line = list(width = 3, color = color),
        showlegend = TRUE,
        legendgroup = pt,
        hoverinfo = "text",
        hovertext = ~paste0(
          "EWMA: ", round(EWMA, 0)
        )
      )
      }
    }
    p
  })
  
  # --------------------------- HB/IVB ----------------------------------------

  output$Trends_break <- renderPlotly({ 
    
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    color_map <- pitch_color_map()
    
    daily_break <- data %>%
      group_by(TaggedPitchType, Date) %>%
      summarise(
        AvgIVB = mean(InducedVertBreak, na.rm = TRUE),
        MinIVB = min(InducedVertBreak, na.rm = TRUE),
        MaxIVB = max(InducedVertBreak, na.rm = TRUE),
        SDIVB = sd(InducedVertBreak, na.rm = TRUE),
        AvgHB = mean(HorzBreak, na.rm = TRUE),
        MinHB = min(HorzBreak, na.rm = TRUE),
        MaxHB = max(HorzBreak, na.rm = TRUE),
        SDHB = sd(HorzBreak, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>% arrange(TaggedPitchType, Date)
    
    alpha <- 0.3  # Smoothing factor 
    
    daily_break <- daily_break %>%
      group_by(TaggedPitchType) %>%
      filter(!is.na(AvgIVB)) %>%
      filter(!is.na(AvgHB)) %>%
      mutate(
        EWMAHB = if (n() >= 2) {
          EMA(AvgHB, n = 2, ratio = alpha)
        } else {
          rep(NA_real_, n())
        },
        EWMAIVB = if (n() >= 2) {
          EMA(AvgIVB, n = 2, ratio = alpha)
        } else {
          rep(NA_real_, n())
        }
      ) %>%
      ungroup()
    
    p <- plot_ly() %>%
      layout(
        title = "Break Trend", 
        xaxis = list(title = "Date"),
        yaxis = list(title = "Break (in)"),
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    data <- left_join(data, daily_break %>% select(TaggedPitchType, Date, EWMAIVB, EWMAHB, AvgIVB, AvgHB),
                      by = c("TaggedPitchType", "Date"))
    
    for(pt in unique(data$TaggedPitchType)){
      pitch_data <- data %>% filter(TaggedPitchType == pt)
      color <- color_map[pt]
      
      if ("Daily Avg" %in% input$plot_elements) {
        # Add Actual Daily Averages
        
        if ("HB" %in% input$break_show){
        p <- p %>% add_trace(
          data = pitch_data,
          x = ~Date,
          y = ~AvgHB,
          type = "scatter",
          name = ~paste0(TaggedPitchType, " HB Actual Daily Avg"),
          marker = list(color = color),
          hovertext = ~paste0(
            "Pitch: ", TaggedPitchType, "<br>",
            "Date: ", Date, "<br>",
            "Actual Daily Avg HB: ", round(AvgHB, 1), "in"
          ),
          hoverinfo = "text"
        )
        
        }
        
        if ("IVB" %in% input$break_show){
          
        p <- p %>% add_trace(
          data = pitch_data,
          x = ~Date,
          y = ~AvgIVB,
          type = "scatter",
          mode = "markers",
          marker = list(color = color,
                        line = list(
                          color = "black",
                          width = 1
                        )
                        ),
          name = ~paste0(TaggedPitchType, " IVB Actual Daily Avg"),
          hovertext = ~paste0(
            "Pitch: ", TaggedPitchType, "<br>",
            "Date: ", Date, "<br>",
            "Actual Daily Avg IVB: ", round(AvgIVB, 1), "in"
          ),
          hoverinfo = "text"
        )
        
        }
      }
      
      if ("Boxplots" %in% input$plot_elements) {
        if("IVB" %in% input$break_show){
        
      # add box plots for IVB
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~InducedVertBreak,
        type = "box",
        marker = list(color = color),
        line = list(color = "black"),
        fillcolor = paste0("rgba(", col2rgb(color)[1], ",", 
                           col2rgb(color)[2], ",", 
                           col2rgb(color)[3], ",0.3)"),
        boxpoints = FALSE,  
        hoverinfo = "none",
        showlegend = FALSE
      )
        }
        
        if("HB" %in% input$break_show){
      # add box plot for HB
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~HorzBreak,
        type = "box",
        marker = list(color = color),
        line = list(color = color),
        fillcolor = paste0("rgba(", col2rgb(color)[1], ",", 
                           col2rgb(color)[2], ",", 
                           col2rgb(color)[3], ",0.3)"),
        boxpoints = FALSE,  
        hoverinfo = "none",
        showlegend = FALSE
      )
        }
      }
      
      if ("Weighted Averages" %in% input$plot_elements) {
      
        if("IVB" %in% input$break_show){
      # Add weighted moving average line - IVB
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~EWMAIVB,
        type = "scatter",
        mode = "lines",
        line = list(width = 3, color = color),
        name = paste0(pt, " IVB EWMA"),
        legendgroup = pt,
        hoverinfo = "text",
        hovertext = ~paste0(
          TaggedPitchType, "<br>",
          "Date: ", Date, "<br>",
          "EWMA (IVB): ", round(EWMAIVB, 1), " mph<br>"
        )
      )
        }
        
        if("HB" %in% input$break_show){
      
      p <- p %>% add_trace(
        data = pitch_data,
        x = ~Date,
        y = ~EWMAHB,
        type = "scatter",
        mode = "lines",
        line = list(width = 3, color = color, dash = 'dot'),
        name = paste0(pt, " HB EWMA"),
        legendgroup = pt,
        hoverinfo = "text",
        hovertext = ~paste0(
          TaggedPitchType, "<br>",
          "Date: ", Date, "<br>",
          "EWMA (HB): ", round(EWMAHB, 1), " mph<br>" 
        )
      )
        }
      }
    }
    p
  })
  
  # --------------------- Create Live Location Report for UI -------------------
  
  output$LiveLoc <- renderPlotly({
    
    req(filtered_data(), selected_pitches(), input$selected_dates)

    selected_dates <- input$selected_dates
    data <- filtered_data()

    if (length(selected_dates) > 1 || !all(data$PracticeType == "LiveBpPitching")) {
      return(
        plotly_empty() %>%
          layout(
            title = "Please Select Live AB Session (For Demo: 2025-08-09)",
            plot_bgcolor = "white",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
           )
      )
    }
    
    pitch_data <- data %>%
      filter(PracticeType == "LiveBpPitching", !is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        hover_text = paste0(
          "Velo: ", round(RelSpeed, 1), " mph<br>",
          "Spin Rate: ", round(SpinRate, 0), " rpm<br>",
          "HB: ", round(HorzBreak, 1), " in<br>",
          "IVB: ", round(InducedVertBreak, 1), " in"
        )
      )
    
    if (nrow(pitch_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "text", text = "No pitch location data available"))
    }
    
    swing_data <- data %>%
      filter(pitch_classification %in% c("Strike Swinging", "Foul Ball", "H", "In Play Out")) %>%
      mutate(
        hover_text = paste0(
          "Velo: ", round(RelSpeed, 1), " mph<br>",
          "Spin Rate: ", round(SpinRate, 0), " rpm<br>",
          "HB: ", round(HorzBreak, 1), " in<br>",
          "IVB: ", round(InducedVertBreak, 1), " in"
        )
      )
    
    bip_data <- data %>%
      filter(pitch_classification %in% c("H", "In Play Out")) %>%
      mutate(
        hover_text = paste0(
          "Velo: ", round(RelSpeed, 1), " mph<br>",
          "Spin Rate: ", round(SpinRate, 0), " rpm<br>",
          "HB: ", round(HorzBreak, 1), " in<br>",
          "IVB: ", round(InducedVertBreak, 1), " in<br>",
          "EV: ", round(ExitSpeed,1), "mph<br>", 
          "LA: ", round(Angle,0), "°"
        )
      )
    
    swing_miss <- data %>% filter(pitch_classification == "Strike Swinging")
    
    pitch_types <- sort(unique(pitch_data$TaggedPitchType))
    colors <- brewer.pal(max(length(pitch_types), 3), "Set1")
    pitch_colors <- setNames(colors[1:length(pitch_types)], pitch_types)
    
    pitch_data$TaggedPitchType <- factor(pitch_data$TaggedPitchType, levels = pitch_types)
    swing_data$TaggedPitchType <- factor(swing_data$TaggedPitchType, levels = pitch_types)
    bip_data$TaggedPitchType <- factor(bip_data$TaggedPitchType, levels = pitch_types)
    
    strike_zone <- data.frame(
      x = c(-0.83, 0.83, 0.83, -0.83, -0.83),
      y = c(1.50, 1.50, 3.50, 3.50, 1.50)
    )
    
    p1 <- ggplot(pitch_data, aes(x = PlateLocSide, y = PlateLocHeight,
                                 color = TaggedPitchType, text = hover_text)) +
      ylim(0,6)+
      xlim(-3,3) +
      geom_path(data = strike_zone, aes(x = x, y = y), color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_point(size = 3, alpha = 0.7) +
      coord_fixed(ratio = 1) + theme_minimal()
    
    p2 <- ggplot(swing_data, aes(x = PlateLocSide, y = PlateLocHeight,
                                 color = TaggedPitchType, text = hover_text)) +
      ylim(0,6)+
      xlim(-3,3) +
      geom_path(data = strike_zone, aes(x = x, y = y), color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_point(size = 3, alpha = 0.7) +
      geom_point(data = swing_miss, aes(x = PlateLocSide, y = PlateLocHeight),
                 shape = 21, size = 3, color = "black", fill = NA, stroke = 1, inherit.aes = FALSE) +
      coord_fixed(ratio = 1) + theme_minimal()
    
    p3 <- ggplot(bip_data, aes(x = PlateLocSide, y = PlateLocHeight,
                               color = TaggedPitchType, text = hover_text)) +
      ylim(0,6)+
      xlim(-3,3) +
      geom_path(data = strike_zone, aes(x = x, y = y), color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_point(size = 3, alpha = 0.7) +
      coord_fixed(ratio = 1) + theme_minimal()
    
    subplot(
      ggplotly(p1, tooltip = "text"),
      ggplotly(p2, tooltip = "text"),
      ggplotly(p3, tooltip = "text"),
      nrows = 1, shareX = TRUE, shareY = TRUE
    ) %>%
      layout(
        xaxis  = list(range = c(-3,3)),
        xaxis2 = list(range = c(-3,3)),
        xaxis3 = list(range = c(-3,3)),
        yaxis  = list(range = c(0,6)),
        yaxis2 = list(range = c(0,6)),
        yaxis3 = list(range = c(0,6)),
        annotations = list(
          list(x = 0.16, y = 1.05, text = "All Pitches", showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 14)),
          list(x = 0.5,  y = 1.05, text = "All Swings", showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 14)),
          list(x = 0.84, y = 1.05, text = "Balls In Play", showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 14))
        )
      )
  })
  
  
  
  #---------------------- Create Swing/Miss Plots for pdf ----------------------
  
  make_live_pitch_location_plots_gg <- function(data, color_map = NULL) {
    
    # Filter to only pitches with location data
    pitch_data <- data %>%
      filter(PracticeType == "LiveBpPitching", !is.na(PlateLocSide), !is.na(PlateLocHeight))
    
    if (nrow(pitch_data) == 0) {
      return(ggplot() + ggtitle("No pitch location data available") + theme_minimal())
    }
    
    # Subsets for plotting
    swing_data <- data %>% 
      filter(pitch_classification %in% c("Strike Swinging", "Foul Ball", "H", "In Play Out"))
    
    bip_data <- data %>% 
      filter(pitch_classification %in% c("H", "In Play Out"))
    
    swing_miss <- data %>% 
      filter(pitch_classification == "Strike Swinging")
    
    # Pitch types & colors
    pitch_types <- sort(unique(pitch_data$TaggedPitchType))
    if (is.null(color_map)) {
      palette_name <- "Set1"
      colors <- brewer.pal(max(length(pitch_types), 3), palette_name)
      pitch_colors <- setNames(colors[1:length(pitch_types)], pitch_types)
    } else {
      pitch_colors <- color_map
    }
    
    pitch_data$TaggedPitchType <- factor(pitch_data$TaggedPitchType, levels = pitch_types)
    
    # Strike zone
    strike_zone <- data.frame(
      x = c(-0.83,  0.83,  0.83, -0.83, -0.83),
      y = c( 1.50,  1.50,  3.50,  3.50,  1.50)
    )
    
    # All pitches
    all_pitches <- ggplot(pitch_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_path(data = strike_zone, aes(x = x, y = y),
                color = "black", linewidth = 1) +
      geom_point(aes(color = TaggedPitchType), size = 3, alpha = 0.7) +
      coord_fixed(ratio = 1) +
      ylim(0,6) + xlim(-3,3) +
      scale_color_manual(values = pitch_colors) + 
      labs(title = "All Pitches\n(Pitcher's View)", x = NULL, y = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Swings
    swings <- ggplot(swing_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_path(data = strike_zone, aes(x = x, y = y),
                color = "black", linewidth = 1) +
      geom_point(aes(color = TaggedPitchType), size = 3, alpha = 0.7) +
      geom_point(data = swing_miss, aes(x = PlateLocSide, y = PlateLocHeight),
                 shape = 21, size = 3, color = "black", fill = NA, stroke = 1) +
      coord_fixed(ratio = 1) +
      ylim(0,6) + xlim(-3,3) +
      scale_color_manual(values = pitch_colors) +
      labs(title = "All Swings\n(Swing + Miss in Black)", x = NULL, y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Balls in play
    bip <- ggplot(bip_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_path(data = strike_zone, aes(x = x, y = y),
                color = "black", linewidth = 1) +
      geom_point(aes(color = TaggedPitchType), size = 3, alpha = 0.7) +
      coord_fixed(ratio = 1) +
      ylim(0,6) + xlim(-3,3) +
      scale_color_manual(values = pitch_colors) + 
      labs(title = "Balls In Play", x = NULL, y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Return arranged plots
    return(gridExtra::arrangeGrob(all_pitches, swings, bip, ncol = 3))
    
  }

  #----------------------- Create Live Spray Chart + Stats for pdf -------------
  
  make_live_spray_chart_gg <- function(data) {
    
    # Filter to valid batted ball data
    df <- data %>%
      filter(PracticeType == "LiveBpPitching",
             !is.na(ExitSpeed), !is.na(Angle),
             !is.na(Direction), !is.na(Distance)) %>%
      mutate(
        radians = Direction * pi / 180,
        x = Distance * sin(radians),
        y = Distance * cos(radians),
        label_text = paste0(round(ExitSpeed, 1), " mph, ", round(Angle, 1), "°")
      )
    
    if (nrow(df) == 0) {
      return(ggplot() + ggtitle("No spray chart data available") + theme_minimal())
    }
    
    # Helper: draw arcs at given distance
    draw_arc <- function(r) {
      theta <- seq(-pi/4, pi/4, length.out = 300)
      data.frame(
        x = r * sin(theta),
        y = r * cos(theta),
        dist = paste0(r, " ft")
      )
    }
    
    arcs <- bind_rows(
      draw_arc(200),
      draw_arc(250),
      draw_arc(300),
      draw_arc(350)
    )
    
    foul_line_length <- 400
    
    foul_lines <- data.frame(
      angle = c(-45, 45),
      x = foul_line_length * sin(c(-45, 45) * pi / 180),
      y = foul_line_length * cos(c(-45, 45) * pi / 180)
    )
    
    basepaths <- data.frame(
      x = c(0, 90, 0, -90, 0),
      y = c(0, 90, 180, 90, 0)
    )
    
    # Spray chart
    spray_chart <- ggplot(df, aes(x = x, y = y)) +
      geom_path(data = arcs, aes(x = x, y = y, group = dist),
                color = "black", linetype = "dashed") +
      geom_segment(data = foul_lines, aes(x = 0, y = 0, xend = x, yend = y),
                   color = "black", linetype = "dotted", size = 1) +
      geom_path(data = basepaths, aes(x = x, y = y),
                color = "black", linetype = "dashed") +
      geom_point(size = 3, color = "black") +
      geom_text(data = df, aes(label = label_text),
                vjust = -1, size = 3, color = "blue") +
      coord_fixed() +
      labs(
        y = NULL,
        x = NULL,
        title = "Live AB Spray Chart"
      ) +
      theme_minimal()
    
    # Hitting summary table
    hitting_summary <- tryCatch({
      data %>%
        summarize(
          Pitches = n(),
          BF = sum(pa_outcome %in% c("BB", "SO", "In Play Out", "H", "HBP")),
          Hits = sum(pa_outcome == "H", na.rm = TRUE),
          BB = sum(pa_outcome == "BB", na.rm = TRUE),
          K = sum(pa_outcome == "SO", na.rm = TRUE),
          `Strike%` = paste0(round(
            sum(pitch_classification %in% c("Strike Swinging", "Strike Looking", "In Play Out", "H", "Foul Ball"), na.rm = TRUE) /
              Pitches * 100, 1), "%"),
          `Whiff%` = paste0(round(
            sum(pitch_classification == "Strike Swinging") /
              sum(pitch_classification %in% c("Strike Swinging", "In Play Out", "H", "Foul Ball")) * 100, 1), "%")
        )
    }, error = function(e) {
      message("Error summarizing pitch data: ", e$message)
      data.frame()
    })
    
    table_grob_hitting <- tableGrob(hitting_summary, rows = NULL)
    
    # Arrange vertically: spray chart on top, table below
    return(gridExtra::arrangeGrob(spray_chart, table_grob_hitting, ncol = 1, heights = c(3, 1)))
    
  }
  
  # ------------------- Spray chart for UI -------------------------------------
  output$SprayChart <- renderPlotly({
    
    req(filtered_data(), selected_pitches(), input$selected_dates)
    
    selected_dates <- input$selected_dates
    data <- filtered_data()
    
    if (length(selected_dates) > 1 || !all(data$PracticeType == "LiveBpPitching")) {
      return(
        plotly_empty() %>%
          layout(
            title = "Please Select Live AB Session (For Demo: 2025-08-09)",
            plot_bgcolor = "white",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          )
      )
    }
    
    data <- filtered_data() %>%
      filter(PracticeType == "LiveBpPitching",
             !is.na(ExitSpeed), !is.na(Angle),
             !is.na(Direction), !is.na(Distance)) %>%
      mutate(
        radians = Direction * pi / 180,
        x = Distance * sin(radians),
        y = Distance * cos(radians),
        hover_text = paste0(
          "EV: ", round(ExitSpeed, 1), " mph<br>",
          "LA: ", round(Angle, 1), "°<br>",
          "Distance: ", round(Distance, 1), " ft<br>",
          "Outcome: ", pa_outcome
        )
      )
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "text", text = "No spray chart data available"))
    }
    
    # Helper to draw arcs at given distance
    draw_arc <- function(r) {
      theta <- seq(-pi/4, pi/4, length.out = 300)
      data.frame(
        x = r * sin(theta),
        y = r * cos(theta),
        dist = paste0(r, " ft")
      )
    }
    
    arcs <- bind_rows(draw_arc(200), draw_arc(250), draw_arc(300), draw_arc(350))
    
    foul_line_length <- 400
    
     # foul_segments <- data.frame(
     #   x = c(0, 0),
     #   y = c(0, 0),
     #   xend = c(400 * sin(-pi/4), 400 * sin(pi/4)),
     #   yend = c(400 * cos(-pi/4), 400 * cos(pi/4))
     # )
     # 
    foul_lines <- data.frame(
      angle = c(-45, 45),
      x = foul_line_length * sin(c(-45, 45) * pi / 180),
      y = foul_line_length * cos(c(-45, 45) * pi / 180)
    )
    
    basepaths <- data.frame(
      x = c(0, 90, 0, -90, 0),   # feet from home
      y = c(0, 90, 180, 90, 0)   # feet from home
    )
    
    
    gg <- ggplot(data, aes(x = x, y = y, text = hover_text)) +
      geom_segment(data = foul_lines, aes(x = 0, y = 0, xend = x, yend = y), inherit.aes = FALSE,
                   color = "black", linetype = "dotted", size = 1) +
      geom_path(data = basepaths, aes(x = x, y = y), inherit.aes = FALSE, color = "black", linetype = "dashed") +
      
      geom_path(data = arcs, aes(x = x, y = y, group = dist),
                 color = "black", linetype = "dashed", inherit.aes = FALSE) +
      #geom_segment(data = foul_segments,
       #             aes(x = x, y = y, xend = xend, yend = yend),
        #            color = "black", linetype = "dotted", size = 1, inherit.aes = FALSE) + 
      geom_point(size = 3, color = "red") +
      coord_fixed() +
      theme_minimal() +
      labs(title = "Live AB Spray Chart", x = NULL, y = NULL)
    
    ggplotly(gg, tooltip = "text")
  })
  
  
  # ------------------------ results summary table ----------------------------
  output$results_table <- renderTable({
    req(filtered_data(), selected_pitches(), input$selected_dates)
    
    selected_dates <- input$selected_dates
    data <- filtered_data()
  
    if (length(selected_dates) > 1 || !all(data$PracticeType == "LiveBpPitching")) {
      return(data.frame(
        Message = "Please Select Live AB Session (For Demo: 2025-08-09)"
      ))
    }
    
    data %>%
      summarize(
        Pitches = n(),
        BF = sum(pa_outcome %in% c("BB","SO","In Play Out","H","HBP")),
        Hits = sum(pa_outcome == "H", na.rm = TRUE),
        BB = sum(pa_outcome == "BB", na.rm = TRUE),
        K = sum(pa_outcome == "SO", na.rm = TRUE),
        `Strike%` = paste0(
          round(
            sum(pitch_classification %in% c("Strike Swinging", "Strike Looking", "In Play Out", "H", "Foul Ball")) / n() * 100,
            1
          ), "%"
        ),
        `Whiff%` = paste0(
          round(
            sum(pitch_classification == "Strike Swinging") /
              sum(pitch_classification %in% c("Strike Swinging","In Play Out","H","Foul Ball")) * 100,
            1
          ), "%"
        )
      )
  }, rownames = FALSE)
  
  
# ------------------------ Full PDF Download -----------------------------------
  output$downloadReportFull <- downloadHandler(
    filename = function() {
      paste0(input$player_name, "-report-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      showModal(modalDialog("Generating PDF...", footer = NULL))
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("RDTrackmanReportPDF.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        player_name = input$player_name,
        logo_path = normalizePath("RDLogo.jpg"),
        selected_dates = input$selected_dates,
        movement_plot = tryCatch({
          make_movement_plot_gg(
            filtered_data(), 
            selected_pitches(),
            input$player_name, 
            pitch_color_map()
          )
        }, error = function(e) {
          warning("Failed to create movement plot: ", e$message)
          ggplot() + labs(title = "Movement plot unavailable")
        }),
        
        metrics_table = {
          mt <- tables$metrics_table_pdf
          if (is.null(mt)) data.frame(Error = "Metrics unavailable") else {
            colnames(mt)[colnames(mt) == "Pitches"] <- "#"  # replicate short version fix
            mt
          }
        },
        
        spin_clock = tryCatch({
          make_spin_clock_gg(filtered_data(), selected_pitches(), pitch_color_map())
        }, error = function(e) {
          warning("Pitch location error: ", e$message)
          ggplot() + labs(title = "Spin Clock unavailable")
        }),
        
        pitch_loc_chart_static = tryCatch({
          make_pitch_loc_plot_gg(filtered_data(), selected_pitches())
        }, error = function(e) {
          warning("Pitch location error: ", e$message)
          ggplot() + labs(title = "Location plot unavailable")
        }),
        
        release_plot = tryCatch({
          make_release_point_gg(
            filtered_data(), 
            selected_pitches(),
            pitch_color_map()
          )
        }, error = function(e) {
          warning("Release plot error: ", e$message)
          ggplot() + labs(title = "Release plot unavailable")
        }),
        
        velo_over_time = tryCatch({
          make_velo_over_time_gg(
            filtered_data(), 
            selected_pitches(), 
            pitch_color_map(),
            input$selected_dates
          )
        }, error = function(e) {
          warning("Velocity plot error: ", e$message)
          ggplot() + labs(title = "Velocity data unavailable")
        })
      )
      
      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          output_format = rmarkdown::pdf_document(  # Explicit package reference
            latex_engine = "xelatex",
            extra_dependencies = "grffile"
          ),
          params = params,
          envir = new.env()
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }, finally = {
        removeModal()
      })
    }
  )
  
# --------------------- Download CSV -------------------------------------------
  # Downloads currently selected data as a csv
  
  output$downloadCSV <- downloadHandler(filename = function(){
    paste0(input$player_name, "date created:", Sys.Date(), ".csv")
  }, 
  content = function(file){
    write.csv(filtered_data(),file,row.names = FALSE)
    }
  )

# ---------------------- Download Live Report ----------------------------------

  output$downloadLive <- downloadHandler(
    filename = function() {
      paste0("LivePitchingReport_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("RDLivePitchingReportTemplate.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        player_name = input$player_name,
        logo_path = normalizePath("RDLogo.jpg"),
        Date = input$selected_dates[1],
        live_loc = make_live_pitch_location_plots_gg(filtered_data(), pitch_color_map()),
        spray_chart = make_live_spray_chart_gg(filtered_data())
      )
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui = ui, server = server)