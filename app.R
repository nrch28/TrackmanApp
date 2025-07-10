## TO DO:
# 1) Stuff+ (model complete(ish), need to add to app)
# 2) Cluster movement plot, contextualize clustering for release plot 
# 3) Arm Angle (calculations + on movement/release plot) 
#     -> first need to see if approximation is valid at all
# 4) Make PDF output cleaner and complete (side by side layout + RD logo) -> need static versions of everything


# Description: 
# Shiny App with following tabs:
# 1) Pitch Movement Plot + Metrics Table (Similar to Pitch Profiler)
# 2) Velo, Spin, IVB, and HB distributions 
# 3) Location Heat Maps 
# 4) Release Point 3d Plot + within pitch sd table
# 5) Spin Clock
# 6) Velo Over Time Plot

# How to deploy on ShinyIO
#rsconnect::deployApp('C:/Users/nrch0/Downloads/RD Pitching App V1')
#showLogs(appName = "rd_pitching_app_public", streaming = TRUE)

library(shiny)
library(plotly)
library(dplyr)
library(reticulate)
library(tidyr)
library(ellipse)
library(ggplot2)
library(gridExtra)

# Data Source that contains all pitches thrown on trackman 
combined_data <- read.csv("combined_ftp_data.csv")
combined_data$Date <- as.Date(combined_data$Date)

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
      dateRangeInput("date_range", "Select Date Range:",                      
                     start = min(combined_data$Date, na.rm = TRUE),
                     end = max(combined_data$Date, na.rm = TRUE)),
      dateRangeInput("exclude_range", "Exclude Data Range (optional):",
                     start = as.Date("2020-01-01"),
                     end = as.Date("2020-01-01")),
      checkboxGroupInput("include_pitch_types", "Include Pitch Types:",
                         choices = pitch_type_choices,
                         selected = pitch_type_choices),
      actionButton("toggle_all", "Select All/Deselect All Pitch Types"),
      actionButton("update", "Update"),
      actionButton("select_all_btn", "Select All Pitches"),
      actionButton("toggle_selection", "Toggle Selection Group", icon = icon("toggel-on")),
      actionButton("show_comparison", "Show Comparison", icon = icon("columns")),
      downloadButton("downloadReport", "Download PDF Report", 
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;"),
      downloadButton("downloadHTML", "Download HTML Report",
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;"),
      downloadButton("downloadCSV", "Download Raw Data (csv)",
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;")
    ),
    mainPanel(
      tabsetPanel(
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
        tabPanel("Distributions",
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
        tabPanel("Location",
                 br(), br(),
                 plotlyOutput("pitch_loc_chart")
        ),
        tabPanel("Release Point",
                 fluidRow(
                   column(width = 12,
                          plotlyOutput("release_point_plot", height = "500px")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          br(), br(), br(), br(), br(), br(), br(), br(), br(),
                          tableOutput("release_point_table")
                          )),
                 # fluidRow(
                 #   column(width = 12,
                 #          br(), br(),
                 #          plotOutput("release_point_2d"))
                 # )
                 ),
        tabPanel("Spin Clock",
                 br(), br(),
                 plotlyOutput("clock")
        ),
        tabPanel("Velo Over Time",
                 br(), br(),
                 plotlyOutput("velo_over_time")
        ),
      )
    )
  )
)

# --------------------- server--------------------------------------------------
server <- function(input, output, session){
  
  #Consistent Colors For Pitch Types
  pitch_color_map <- reactive({
    data <- filtered_data()
    pitch_types <- unique(data$TaggedPitchType)
    setNames(
      RColorBrewer::brewer.pal(length(pitch_types), "Set2"),
      pitch_types
    )
  })
 
  
  # Keep Track of Plots and Tables (for HTML download)
  plots <- reactiveValues()
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
  
  # Toggle Select All / Deselect All Pitch Types
  observeEvent(input$toggle_all, {
    print("Button Clicked")
    current_choices <- sort(unique(
      combined_data$TaggedPitchType[
        !is.na(combined_data$TaggedPitchType) & 
          combined_data$TaggedPitchType != "" &
          !grepl("^\\s*$", combined_data$TaggedPitchType)
      ]
    ))
    
    if (is.null(input$include_pitch_types) || 
        length(input$include_pitch_types) < length(current_choices)) {
      # If nothing or only some are selected → Select All
      updateCheckboxGroupInput(
        session, 
        "include_pitch_types",
        selected = current_choices
      )
    } else {
      # If all are selected → Deselect All
      updateCheckboxGroupInput(
        session, 
        "include_pitch_types",
        selected = character(0)  # Empty selection
      )
    }
  })
  
  # Filter data based on sidebar selections
  filtered_data <- eventReactive(input$update, {
    print("Update button clicked - starting data filtering")
    req(input$player_name, input$date_range)
    
    print(paste("Player selected:", input$player_name))
    print(paste("Date range:", input$date_range[1], "to", input$date_range[2]))
    print(paste("Pitch types selected:", paste(input$include_pitch_types, collapse = ", ")))
    
    if (!inherits(combined_data$Date, "Date")){
      combined_data$Date <- as.Date(combined_data$Date)
    }
    
    # Name and Date Filter
    filtered <- combined_data %>%
      mutate(pitch_id = row_number()) %>%
      filter(
        Pitcher %in% input$player_name,
        PitchSession == "Live",
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        !is.na(TaggedPitchType),
        TaggedPitchType != "",
        TaggedPitchType != "Undefined"
      )
    
    #print(paste("Rows after initial filtering:", nrow(filtered)))
    
    #Date Exclude Filter
    if (!is.null(input$exclude_range) &&
        !is.na(input$exclude_range[1]) &&
        !is.na(input$exclude_range[2]) &&
        input$exclude_range[1] <= input$exclude_range[2]) {
      
      filtered <- filtered %>%
        filter(Date < input$exclude_range[1] | Date > input$exclude_range[2])
      
      print(paste("Rows after exclude:", nrow(filtered)))
    }
    
    if(length(input$include_pitch_types) == 0){
      print("Please Select Pitch Type(s)")
      return (filtered[0,])
    }
    
    #Pitch Type Filter
    if (length(input$include_pitch_types) > 0) {
      filtered <- filtered %>%
        filter(TaggedPitchType %in% input$include_pitch_types)
    } else {
      # Return empty dataframe if no pitch types selected
      return(filtered[0, ])
    }
    
    #print(paste("Final filtered rows:", nrow(filtered)))
    #print("Data filtering completed successfully")
    
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
  
  output$movement_plot <- renderPlotly({
    #print("Rendering movement plot...")
    data <- filtered_data()
    selected <- selected_pitches()
    
    req(data, selected)
    
    color_map <- pitch_color_map()
    
    #print(paste("Movement plot data rows:", nrow(data)))
    #print(paste("Selected pitches count:", length(selected)))
    
    data$selected_flag <- data$pitch_id %in% selected
    selected_data <- data[data$selected_flag, ]
    unselected_data <- data[!data$selected_flag, ]
    
    #print(paste("Selected data rows:", nrow(selected_data)))
    #print(paste("Unselected data rows:", nrow(unselected_data)))
    
    x_range <- range(data$HorzBreak, na.rm = TRUE)
    y_range <- range(data$InducedVertBreak, na.rm = TRUE)
    range <- range(c(x_range +1, y_range+1))
    max <- max(range)
    
    #print("Creating movement plot...")
    
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
    
    #print("Movement plot created successfully")
    plots$movement_plot <- p
    p
  })
  
  # -------------------------- Movement Plot Static (ggplot) - for PDF --------
  
  # This is not displayed in the UI, but is used in the PDF Output 
  make_movement_plot_gg <- function(data, selected_ids, player_name = "Player", color_map = NULL) {
    data$selected_flag <- data$pitch_id %in% selected_ids
    
    # Compute fixed range - *Important for visualizing pitch shapes*
    x_range <- range(data$HorzBreak, na.rm = TRUE)
    y_range <- range(data$InducedVertBreak, na.rm = TRUE)
    combined_range <- range(c(x_range + 1, y_range + 1))
    max_range <- max(combined_range)
    
    # Create Unselected (grayed out) points
    gg <- ggplot(data, aes(x = HorzBreak, y = InducedVertBreak)) +
      geom_point(
        data = data[!data$selected_flag, ],
        color = "lightgray",
        alpha = 0.5,
        size = 1
      ) +
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
        title = paste(player_name, "Pitch Movement"),
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)",
        color = "Pitch Type"
      ) +
      theme_minimal()
    
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
    
    # This is where we would do stuff+ caculations. I deleted the code because the
    # model was not accurate enough 
    
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
          .groups = "drop"
        ) %>%
        arrange(desc(Pitches))
    }, error = function(e) {
      message("Error summarizing pitch data: ", e$message)
      # fallback empty data.frame to avoid app crash
      data.frame()
    })
    
    # Add to tables for display in pdf/html 
    tables$metrics_table <- pitch_type_summary
    tables$metrics_html <- htmltools::tagList(
      htmltools::tags$h4("Pitch Type Summary"),
      htmlTable::htmlTable(pitch_type_summary)
    )
  })
  
  
  # ------------------------- Render Table -------------------------------------
  
  # Create output for display in UI.
  # Keeping the creation and Output seperate will matter when calculating stuff+
  output$metrics_table <- renderTable({
    req(tables$metrics_table)
    tables$metrics_table
  })
  
  # ------------------------ velo dist plot (tab 2) ----------------------------
  
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
  # --------------------- Static Version (for pdf) --------------------------------------

  plots$velo_dist_plot_static <- reactive({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    data <- data %>%
      filter(!is.na(RelSpeed), !is.na(TaggedPitchType))
    
    ggplot(data, aes(x = RelSpeed)) +
      geom_density(fill = "steelblue", alpha = 0.6) +
      facet_wrap(~ TaggedPitchType, scales = "free_y", ncol = 1) +
      labs(
        title = "Velocity Distribution by Pitch Type",
        x = "Velocity (mph)",
        y = "Density"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
    
    # -------------------Spin dist plot (tab 2) --------------------------------
    
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
  
  #-------------------------- Static Version (for pdf) -----------------------------------
  plots$spin_dist_plot_static <- reactive({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids,]
    
    data <- data %>%
      filter(!is.na(SpinRate), !is.na(TaggedPitchType)) %>%
      filter(TaggedPitchType %in% unique(TaggedPitchType))
    
    ggplot(data, aes(x = SpinRate, fill = TaggedPitchType)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~TaggedPitchType, scales = "free_y", ncol = 1) +
      labs(
        title = "Spin Rate Distribution by Pitch Type",
        x = "Spin Rate (rpm)",
        y = "Density"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

    
    # ------------------- ivb dist plot (tab 2) --------------------------------
    
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
      
      plots$ivb_dist_plot <- final_plot
      final_plot
    })
    
  # --------------------------- Static Version (for pdf)---------------------------------
  plots$ivb_dist_plot_static <- reactive({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    data <- data %>%
      filter(!is.na(InducedVertBreak), !is.na(TaggedPitchType))
    
    ggplot(data, aes(x = InducedVertBreak)) +
      geom_density(fill = "forestgreen", alpha = 0.6) +
      facet_wrap(~ TaggedPitchType, scales = "free_y", ncol = 1) +
      labs(
        title = "Induced Vertical Break Distribution by Pitch Type",
        x = "IVB (inches)",
        y = "Density"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
    # ------------------ create HB dist plot (tab 2)  ---------------------------
    
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
      
      plots$hb_dist_plot <- final_plot
      final_plot
    })
    
  # ------------------------------- Static Version (for pdf -----------------------------
  plots$hb_dist_plot_static <- reactive({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    data <- data %>%
      filter(!is.na(HorzBreak), !is.na(TaggedPitchType))
    
    ggplot(data, aes(x = HorzBreak)) +
      geom_density(fill = "purple", alpha = 0.6) +
      facet_wrap(~ TaggedPitchType, scales = "free_y", ncol = 1) +
      labs(
        title = "Horizontal Break Distribution by Pitch Type",
        x = "HB (inches)",
        y = "Density"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
    
    # --------------------- Heat Maps (tab 3) ----------------------------------
    
    output$pitch_loc_chart <- renderPlotly({
      
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
      
      plots$pitch_loc_chart <- result
      
      result
      
    })
  
  #----------------------- Static Version -------------------------------------
  # need to use ggplot for pdf. Might look better than plotly, so may replace in ui
  plots$pitch_loc_chart_static <- reactive({
    
    data <- filtered_data()
    
    # Filter and keep only rows with needed values
    data <- data %>%
      filter(!is.na(TaggedPitchType), !is.na(PlateLocSide), !is.na(PlateLocHeight))
    
    # If no data, return empty ggplot
    if (nrow(data) == 0) {
      return(ggplot() + ggtitle("No data available"))
    }
    
    # Base ggplot with 2D density fill + points
    p <- ggplot(data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", color = NA, alpha = 0.8) +
      scale_fill_gradientn(
        colors = c("#f7f7f7", "#fff7bc", "#fee391", "#fec44f", "#fe9929", 
                   "#ec7014", "#cc4c02", "#993404", "#662506"),
        name = "Density"
      ) +
      geom_point(alpha = 0.5, size = 1, color = "black") +
      facet_wrap(~ TaggedPitchType, ncol = 2) +
      coord_cartesian(xlim = c(-3, 3), ylim = c(0, 6)) +
      # Draw the strike zone rectangle
      geom_rect(aes(xmin = -0.7, xmax = 0.7, ymin = 1.6, ymax = 3.4),
                color = "black", fill = NA, inherit.aes = FALSE) +
      theme_minimal() +
      theme(
        legend.position = "right",
        strip.text = element_text(size = 14),
        axis.title = element_blank()
      ) +
      labs(title = "Pitch Location Heat Maps")
    
    p
  })
  
  
  
  #------------------------ release point plot (4th tab) ---------------------
  # 3D release point plot w/ centroids 
  output$release_point_plot <- renderPlotly({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    
    data$selected_flag <- data$pitch_id %in% selected_ids
    selected_data <- data[data$selected_flag, ]
    unselected_data <- data[!data$selected_flag, ]
    
    
    centroids <- selected_data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        mean_side = mean(RelSide, na.rm = TRUE),
        mean_ext = mean(Extension, na.rm=TRUE),
        mean_height = mean(RelHeight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    color_map <- pitch_color_map()
    
    #print(centroids)
    
    p <- plot_ly(source = "pitch_click")
    
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
    
    p <- p %>%
      add_trace(
        x = 0, y = 0, z = 0,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5, color = "black", symbol = "x"),
        name = "Orgin",
        showlegend = FALSE,
        hoverinfo = "none"
      ) 
    
    # An attempt at drawing a person to make the graph easier to interpret
    # May try a png image 
    # Back leg
    p <- p %>%
      add_trace(
        x = c(0, 0),
        y = c(0, 2.5),
        z = c(0, 2),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "black", width = 4),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
    # Front leg
    p <- p %>%
      add_trace(
        x = c(0, 0),
        y = c(2.5, 5),
        z = c(2, 0),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "black", width = 4),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
    # Torso
    p <- p %>%
      add_trace(
        x = c(0, 0),
        y = c(2.5, 2.5),
        z = c(2, 5),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "black", width = 5),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
    # Throwing arm
    p <- p %>%
      add_trace(
        x = c(0, 0),
        y = c(2.5, 6),
        z = c(4, 5),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "black", width = 4),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
    # Head
    p <- p %>%
      add_trace(
        x = 0,
        y = 2.5,
        z = 5.5,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 6, color = "black", symbol = "circle"),
        showlegend = FALSE,
        hoverinfo = "none"
      )
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
    
    # Original Plan was to just show centroids in html output (cleaner)
    # would use this plot if so
    centroid_plot <- plot_ly()
    
    for (pt in unique(centroids$TaggedPitchType)) {
      centroid <- filter(centroids, TaggedPitchType == pt)
      
      centroid_plot <- centroid_plot %>%
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
    
    centroid_plot <- centroid_plot %>%
      layout(
        title = paste("Release Point Centroids Only"),
        showlegend = TRUE,
        scene = list(
          xaxis = list(title = "Release Side (ft)"),
          yaxis = list(title = "Extension (ft)"),
          zaxis = list(title = "Release Height (ft)")
        ),
        width = 1000,
        height = 700,
        margin = list(l = 0, r = 0, b = 0, t = 50)
      ) %>%
      add_trace(
        x = 0, y = 0, z = 0,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5, color = "black", symbol = "x"),
        name = "Orgin",
        showlegend = FALSE,
        hoverinfo = "none"
      )
    
    # Add centroid only plot to plots (for html output)
    plots$release_point_plot <- centroid_plot
    
    # Return the full app plot to UI
    p
    
  })
  
  # ------------------------- Static (2D) Release Point Plot -------------------
  # Since the PDF output has to be static, I figured a 2d release plot made more sense
  # This plots all pitches for now, but could also do centroids only. 
  # Also could be a good place to plot arm angles (if arm angle calculations are accurate)
  
  output$release_point_2d <- renderPlot({
    req(filtered_data(), selected_pitches())
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids,]
    
    color_map <- pitch_color_map()
    
    ggplot(data=data, aes(x = RelSide, y = RelHeight, color = TaggedPitchType))+
      geom_point() + 
      scale_color_manual(values = color_map)+
      labs(
        x = "Release Side (ft)",
        y = "Release Height (ft)", 
        color = "Pitch Type"
      ) + 
      theme_minimal()
    
  })

    # ------------release point table (within pitch variance) - 4th tab -----------------
    # This calculates the sd of distances from centroid for each pitch type (within pitch variance)
  # Nice idea, need to contextualize to give more meaning (what does .21 mean?)
  # Also will do something similar for across pitch variance 
  
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
        #print(centroid)
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
      #print(results_df)
    })
  
#----------------------------Spin Clock (30min bins) Tab 5----------------------------
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
            text = hover_text,  # Use pre-constructed text
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
    
    plots$clock_plot <- p
    
    # Static Plot (for download)
    
    static_plot <- ggplot(avg_data, aes(x = Tilt, y = avg_spin, fill = TaggedPitchType)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_polar(start = -pi/2) +
      labs(
        title = "Average Spin Rate by Clock Angle and Pitch Type",
        x = "Tilt Angle",
        y = "Avg Spin Rate (rpm)",
        fill = "Pitch Type"
      ) +
      theme_minimal() +
      scale_color_manual(values = color_map)+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    plots$clock_plot_static <- static_plot
    
    p
  })
  
  # --------------------- Velo Over Time (Tab 6) ---------------------------------------
  # Shows plot of velo overtime of pen (for all pitch types)
  # As shown on USA baseball report
  # Will display error messsage if more than one day is selected
  
  output$velo_over_time <- renderPlotly({
    
    data <- filtered_data()
    selected_ids <- selected_pitches()
    data <- data[data$pitch_id %in% selected_ids, ]
    
    
    validate(
      need(length(input$date_range) == 2, "Please select a date range"),
      need(input$date_range[1] == input$date_range[2], "Please Select Only One Day")
    )
    
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
  
  ## ---------------------  FOR HTML -------------------------------------------
  # Download for HTML 
  # Just Downloads Movement plot and table for now (will add more sooner)
  # Uses RMD template 
  output$downloadHTML <- downloadHandler(
    filename = function() {
      paste0(input$player_name, "-report-", Sys.Date(), ".html")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Please wait",
        "Generating HTML report...",
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Copy Rmd to a temp location
      tempReport <- file.path(tempdir(), "RDTrackmanReportHTMLShort.Rmd")
      file.copy("RDTrackmanReportHTMLShort.Rmd", tempReport, overwrite = TRUE)

      # IMPORTANT: Render to our own .html path
      output_html <- file.path(tempdir(), paste0(input$player_name, "_report.html"))

      params <- list(
        player_name = input$player_name,
        date_range  = input$date_range,
        movement_plot = plots$movement_plot,
        metrics_table = tables$metrics_table
      )

      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_file = output_html,
          params = params,
          envir = new.env(parent = globalenv())
        )
        # Now copy to 'file' for shinyapps.io
        file.copy(output_html, file)
      }, error = function(e) {
        showNotification(paste("HTML generation failed:", e$message), type = "error")
      }, finally = {
        removeModal()
      })
    }
  )

  
  ## ----------------------- FOR PDF --------------------------------------------
  # Same content as HTML, but static and so can be sent easier
  # Will add more content/ better format soon
  # Also uses RMD template
  
  output$downloadReport <- downloadHandler(filename = function() {
    paste0(input$player_name, "-report-", Sys.Date(), ".pdf")
  }, content = function(file) {
    showModal(modalDialog(title = "Please wait", "Generating PDF report...", 
                          easyClose = FALSE, footer = NULL))
    tempReport <- file.path(tempdir(), "RDTrackmanReportShort.Rmd")
    file.copy("RDTrackmanReportShort.Rmd", tempReport, overwrite = TRUE)
    output_pdf <- file.path(tempdir(), paste0(input$player_name, 
                                              "_report.pdf"))
    colnames(tables$metrics_table)[colnames(tables$metrics_table) == 
                                     "Pitches"] <- "#"
    params <- list(player_name = input$player_name, date_range = input$date_range, 
                   movement_plot = make_movement_plot_gg(filtered_data(), 
                                                         selected_pitches(), input$player_name, color_map = pitch_color_map()), metrics_table = tables$metrics_table)
    tryCatch({
      rmarkdown::render(input = tempReport, output_file = output_pdf, 
                        output_format = "pdf_document", params = params, 
                        envir = new.env(parent = globalenv()))
      file.copy(output_pdf, file)
    }, error = function(e) {
      showNotification(paste("PDF generation failed:", e$message), 
                       type = "error")
    }, finally = {
      removeModal()
    })
  })
  
# --------------------- Download CSV -------------------------------------------
  # Downloads currently selected data as a csv
  
  output$downloadCSV <- downloadHandler(filename = function(){
    paste0(input$player_name, "date created:", Sys.Date(), ".csv")
  }, 
  content = function(file){
    write.csv(filtered_data(),file,row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)