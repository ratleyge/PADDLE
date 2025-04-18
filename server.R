## Server ##

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Search by chemical ----
    ## Selector behavior ----

    # Different models have different age stratifications
    observe({
      
      if (req(input$dataSource_chem) == "non_spatial") {
        ageChoices <<- c(
          "Youth (0-5 yrs)" = "Youth", 
          "Pediatric (6-17 yrs)" = "Pediatric_ns", 
          "Adult (18-55 yrs)" = "Adult_ns", 
          "Retirement (56-75 yrs)" = "Retirement",  
          "Geriatric (+76 yrs)" = "Geriatric")
      } else if (req(input$dataSource_chem) == "spatial") {
        ageChoices <<- c(
          "Adult" = "Over18",
          "Pediatric" = "Under18"
        )
      }
      
      updateSelectInput(session = session,
                        "ageGroup_chem",
                        choices = ageChoices,
                        selected = ageChoices[1])
    })
  
  
    # Generate the title for the data viewer
    output$currentlyViewing_chem <- renderText({
      
      ageGroup_chemText <- switch(req(input$ageGroup_chem),
                                  "Over18" = "an adult (over 18)",
                                  "Under18" = "a pediatric (under 18)",
                                  "Youth" = "a youth (ages 0-5)",  
                                  "Pediatric_ns" = "a pediatric (ages 6-17)",
                                  "Adult_ns" = "an adult (ages 18-55)",
                                  "Retirement" = "a retirement-age (ages 56-75)",
                                  "Geriatric" = "a geriatric (ages 75+)"
      )
      
      currentlyViewing_chem <- paste0(
        "Disease associations with ", 
        req(input$searchChemical_chem), 
        ", using a ", 
        gsub("_", "-", req(input$dataSource_chem)), 
        " model in ", 
        ageGroup_chemText,
        " population in the United States."
      )
      
      currentlyViewing_chem
      
    })
      
    # Update the search chemical selection to show only water chems for water and only air for air
    # Also there are no spatial models for the water models, so remove that option when water is selected
    observe({
      
      if (req(input$pollutionSource_chem) == "Water") {
        
        choices <- c("Start typing or select chemical from dropdown" = "", unique(non_spatial_Adult_ns_Water$Variable)[order(unique(non_spatial_Adult_ns_Water$Variable))])
        updateSelectizeInput(session = session,
                          "searchChemical_chem",
                          choices = choices,
                          selected = NULL,  # Ensure nothing is pre-selected
                          options = list(
                            placeholder = 'Start typing or select disease from dropdown',
                            onInitialize = I('function() { this.clear(); }'), # Force clearing on load
                            persist = FALSE
                          ),
                          server = TRUE
                        )
        
        updateSelectInput(session = session,
                          "dataSource_chem",
                          choices = c("Non-spatial" = "non_spatial"),
                          selected = "non_spatial")
      } else {
        
        choices <- c("Start typing or select chemical from dropdown" = "", unique(spatial_Over18_Air$Variable)[order(unique(spatial_Over18_Air$Variable))])
        updateSelectizeInput(session = session,
                          "searchChemical_chem",
                          choices = choices,
                          selected = NULL,  # Ensure nothing is pre-selected
                          options = list(
                            placeholder = 'Start typing or select disease from dropdown',
                            onInitialize = I('function() { this.clear(); }'), # Force clearing on load
                            persist = FALSE
                          ),
                          server = TRUE)
        
        updateSelectInput(session = session,
                          "dataSource_chem",
                          choices = c(
                            "Spatial" = "spatial",
                            "Non-spatial" = "non_spatial"
                            ),
                          selected = "spatial")
      }
    })
    
    
    ## Chem properties ----
    # Show the Chemical Class
    output$chem_class_chem <- renderText({
      classes <- chem_class %>% 
        filter(Chemical == req(input$searchChemical_chem))
      paste(classes$Class, collapse = ", ")
    })
    
    # Show whether the chemical is a carcinogen
    output$carcinogen_chem <- renderText({
      classes <- chem_cancer %>% 
        filter(Chemical == req(input$searchChemical_chem))
      p <- ifelse(nrow(classes) > 0, "Yes", "No")
    })
    
    # Show whether the chemical has known organ toxicities
    output$organ_tox_chem <- renderText({
      classes <- chem_organ_system %>% 
        filter(Chemical == req(input$searchChemical_chem))
      
      if (nrow(classes) == 0 ) {
        organ_text <- "Unknown"
      } else {
        organ_text <- paste(classes$`Organ System`, collapse = ", ")
      }
      
      organ_text
    })
    
    # Show whether the chemical has known acute or chronic toxicity 
    output$tox_timing_chem <- renderText({
      classes <- chem_timing %>% 
        filter(Chemical == req(input$searchChemical_chem))
      paste(classes$Timing, collapse = ", ")
    })
    
    
    ## Plot chem-disease associations ----
    
    # Get the data frame associated with the user input:
      # Source: either spatial or non-spatial
      # Age group: Ped or adult etc.
      # Pollution Source: Air or Water
    # These data frames are prepared in the Archive.R file from the model outputs
    dataToView <- reactive({
      
      req(exists(paste(req(input$dataSource_chem), req(input$ageGroup_chem), req(input$pollutionSource_chem), sep = "_")))
      
      get(paste(req(input$dataSource_chem), req(input$ageGroup_chem), req(input$pollutionSource_chem), sep = "_")) %>%
        filter(Variable == req(input$searchChemical_chem)) %>%
        filter(Disease != "NA") %>%
        mutate_if(is.numeric, ~ signif(.x, 5)) %>%
        select(-Variable) %>% 
        arrange(desc(abs(log(Odds))))
      
    })
    
    # Plot the top 20 Oddss Odds/odds relative to other variables for the same disease
    output$viewPlots_chem <- renderPlotly({
      
      validate(
        need(input$searchChemical_chem != "", "Please select a chemical")
      )
      
      # Define vline function
      vline <- function(x = 0, color = "black") {
        list(
          type = "line",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          x0 = x,
          x1 = x,
          line = list(color = color, width = 0.5, dash = "dash")
        )
      }
      
      data <- dataToView() %>% 
        mutate(ranking_metric = (log(Odds))) %>%
        as.data.frame() %>%
        left_join(
          icdKey[which(icdKey$`SHORT DESCRIPTION (VALID ICD-10 FY2025)` != icdKey$`LONG DESCRIPTION (VALID ICD-10 FY2025)`), ] %>%
            select(-`NF EXCL`, -Disease) %>%
            rename("Disease" = "SHORT DESCRIPTION (VALID ICD-10 FY2025)") %>%
            na.omit() , 
          by = "Disease"
          ) %>%
        mutate(Description = coalesce(`LONG DESCRIPTION (VALID ICD-10 FY2025)`, Disease)) %>%
        select(-`LONG DESCRIPTION (VALID ICD-10 FY2025)`) %>%
        slice_max(order_by = ranking_metric, n = 15, with_ties = FALSE) %>%
        mutate(Disease = reorder(Disease, Odds)) 
      
      p <- plot_ly(data) %>%
        # Error bars (Min to Max range)
        add_segments(
          x = ~Min, xend = ~Max, y = ~Disease, yend = ~Disease,
          color = I("black"), hoverinfo = "none", line = list(width = 1), name = 'Disease Odds Range'
        ) %>%
        
        # Min marker
        add_markers(
          x = ~Min, y = ~Disease, 
          marker = list(symbol = "line-ns-open", size = 6, color = "black"),  # Corrected symbol
          hoverinfo = "none", showlegend = FALSE
        ) %>%
        
        # Max marker
        add_markers(
          x = ~Max, y = ~Disease, 
          marker = list(symbol = "line-ns-open", size = 6, color = "black"),  # Corrected symbol
          hoverinfo = "none", showlegend = FALSE
        ) %>%
        
        # Standard deviation range
        add_segments(
          x = ~Mean - `Standard Deviation`, xend = ~Mean + `Standard Deviation`,
          y = ~Disease, yend = ~Disease,
          color = I("darkgrey"), hoverinfo = "none", line = list(width = 6), 
          opacity = 0.5, name = '± 1 Standard Deviation'
        ) %>%

        # Mean marker (cross-thin)
        add_markers(
          x = ~Mean, y = ~Disease, 
          marker = list(symbol = "x-thin-open", size = 6, color = "black"),  # Corrected symbol
          hoverinfo = "text",
          text = ~paste0(Description, 
                         "<br><b>Mean:</b> ", round(Mean, 2), 
                         "<br><b>± 1 Standard Deviation:</b> ", round(Mean - `Standard Deviation`, 2), "-", round(Mean + `Standard Deviation`, 2),
                         "<br><b>Range:</b> ", round(Min, 2), "-", round(Max, 2)
                         ),
          name = "Mean Disease Odds"
        ) %>%
        
        # Scatter plot for Odds ratios
        add_markers(
          x = ~Odds, y = ~Disease,
          marker = list(color = "red", size = 6),
          hoverinfo = "text",
          text = ~paste0(Description, "<br>association with<br>", req(input$searchChemical_chem),
                         "<br><b>Odds Ratio:</b> ", round(Odds, 2)),
          name = "Chemical Odds"
        ) %>%

        layout(
          title = "Odds Ratio for Top 15 Diseases",
          xaxis = list(title = "Odds Ratio", zeroline=FALSE),
          yaxis = list(title = "",  tickmode = "linear", dtick = 1),
          margin = list(l = 150, r = 50, t = 70, b = 70),  # Adjust left margin for long disease names
          shapes = list(vline(1, "black")),  # Add vertical dashed line at x = 1
          hoverlabel = list(align = "center")
        )
      
      p
    })
    
    # Display the data table 
    output$viewTable_chem <- DT::renderDataTable({ 
      DT::datatable(
        dataToView() %>% 
          arrange(desc(Odds)),
        selection = "none",
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:7))
        ))
    })
    
    # Download diseases associated with chemical
    output$download_diseases_chem <- downloadHandler(
      filename = function() {
        paste(req(input$searchChemical_chem), req(input$dataSource_chem), req(input$ageGroup_chem), req(input$pollutionSource_chem), "disease-associations.csv", sep = "_")
      },
      
      content = function(file) {
        write.csv(
          dataToView() %>%
            mutate(`Log Odds` = log(Odds)) %>%
            mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>%
          arrange(desc(Odds))
          ,
          file, 
          row.names = FALSE)
      }
    )
    
    
    ## Mapping exposure ----
    map_data <- reactive({
      
      mapping_chem <- rbind(
        read_csv(
          "Data/Chemical_mapping_data_part_1.csv",
          col_select = c("county", "state", "chemical" = req(input$searchChemical_chem))
        ) %>%
          na.omit() %>%
          filter(chemical != 0),
        read_csv(
          "Data/Chemical_mapping_data_part_2.csv",
          col_select = c("county", "state", "chemical" = req(input$searchChemical_chem))
        ) %>%
          na.omit() %>%
          filter(chemical != 0)
        )
      
      mapping_chem %>%
        inner_join(fips_codes, by = c("county", "state")) %>%
        mutate(fips = paste0(state_code, county_code))  %>%
        group_by(county, state, state_code, state_name, county_code, fips) %>%
        summarise(chemical = mean(chemical, na.rm = TRUE), .groups = "drop") %>% 
        mutate(chemical_scaled = signif((chemical - min(chemical)) / (max(chemical) - min(chemical)), 5))
    })
    
    output$US_map_chem <- renderPlotly({

      map_data <- req(map_data())
      
      # Get the rows with max and min (non-zero) scaled chemical values
      max_row <- map_data[which(map_data$chemical_scaled == max(map_data$chemical_scaled)), ]
      min_row <- map_data[which(map_data$chemical_scaled == min(map_data$chemical_scaled)), ]
      
      max_value <- max_row$chemical[[1]]
      min_value <- min_row$chemical[[1]]
      
      ratio <- signif(max_value / min_value, 3)
      
      # Build summary text 
      summary_text <- paste0(
        "<b>", max_row$county[[1]], ", ", max_row$state[[1]], "</b> had the <br>highest levels of <br>",
        req(input$searchChemical_chem), ". <br>This was <b>",
        ratio, " times higher</b> <br>than the levels in <br><b>",
        min_row$county[[1]], ", ", min_row$state[[1]],
        "</b>, which had <br>the lowest non-zero <br>detected levels of <br>",
        req(input$searchChemical_chem), "."
      )
      
      legend_Title <- split_middle_hyphen(as.character(req(input$searchChemical_chem)))
      
      myPlotly_map <- plot_ly(
        data = map_data,
        type = "choropleth",
        geojson = counties,
        locations = ~fips,
        z = ~chemical_scaled,
        colorscale = "Viridis",
        marker = list(line = list(width = 0)),
        hoverinfo = "text",
        text = ~paste0(county, ", ", state,
                       "<br>", chemical_scaled, "</br>")
      ) %>%
        colorbar(title = legend_Title) %>%
        layout(
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa"),
            showlakes = TRUE,
            lakecolor = toRGB("white")
          ),
          margin = list(r = 180, b = 30),  # Slightly smaller margin now
          annotations = list(
            list(
              text = summary_text,
              align = "left",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              x = 0.9,              # Nudge left (closer to legend)
              y = -0.05,              # Nudge up (closer to bottom of plot)
              xanchor = "left",
              yanchor = "bottom",
              bordercolor = "black",
              borderwidth = 1,
              bgcolor = "#dddddd",
              opacity = 0.9
            )
          )
        )
      
      myPlotly_map
    
    }) |> bindEvent(input$generate_map_chem) 
    
    
  
    ## At risk groups ----
    
      ### Race ----
      output$race_chem <- renderUI({
        
        req(exists(paste(req(input$dataSource_chem), "Race", req(input$pollutionSource_chem), sep = "_")))
        
        # Get the dataset dynamically
        dataset <- get(paste(req(input$dataSource_chem), "Race", req(input$pollutionSource_chem), sep = "_"))
        
        # Check if the chemical is in the dataset
        if (req(input$searchChemical_chem) %in% dataset$Variable) {
          
          tagList(
            div(
              class = "center-container",
              h4("Ethnicity"),
              column(8, DT::dataTableOutput("viewTable_race_chem")),
              br(),
            )
          )
          
        } else {
          return(NULL)  # Hide the section if the chemical is not in the dataset
        }
      })
      
      
      # Filter the data to be viewed
      dataToView_Race_chem <- reactive({
        
        get(paste(req(input$dataSource_chem), "Race", req(input$pollutionSource_chem), sep = "_")) %>%
          rename("Ethnicity" = "Disease") %>%
          filter(Variable == req(input$searchChemical_chem)) %>%
          mutate(Ethnicity = gsub("BlackAA", "Black", 
                             gsub("NativeAmerican", "Native American", 
                                  gsub("AAPI", "Asian & Pacific Islander", Ethnicity)))
          ) %>%
          mutate_if(is.numeric, ~ signif(.x, 5)) %>%
          arrange(desc(abs(log(Odds))))
        
      })
      
      
      # Display the data table 
      output$viewTable_race_chem <- DT::renderDataTable({ 
        
        DT::datatable(
          dataToView_Race_chem() %>% 
            mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
            mutate(`Log Odds` = signif(log(Odds), 5)) %>%
            select(Ethnicity, Odds, `Log Odds`, Risk),
            selection = "none",
            rownames= FALSE,
            options = list(
              pageLength = -1,
              dom = 't', 
              autoWidth = TRUE,
              columnDefs = list(list(className = 'dt-center', targets = 0:3))
            )
          )
      })
    
      
      
      ### Deprivation ----
      output$deprivation_chem <- renderUI({
        
        # Get the dataset dynamically
        dataset <- get(paste("non_spatial", "ADI", "Air", sep = "_"))
        
        # Check if the chemical is in the dataset
        if (req(input$searchChemical_chem) %in% dataset$Variable & req(input$pollutionSource_chem) == "Air") {
          
          tagList(
            div(
              class = "center-container",
              h4("Deprivation"),
              p("From a non-spatial model"),
              column(8, DT::dataTableOutput("viewTable_deprivation_chem")),
              br()
            )
          )
          
        } else {
          return(NULL)  # Hide the section if the chemical is not in the dataset
        }
      })
      
      
      # Filter the data to be viewed
      dataToView_deprivation_chem <- reactive({
        
        get(paste("non_spatial", "ADI", "Air", sep = "_")) %>%
          rename("Deprivation" = "Disease") %>%
          filter(Variable == req(input$searchChemical_chem)) %>%
          mutate_if(is.numeric, ~ signif(.x, 5)) %>%
          arrange(desc(abs(log(Odds))))
        
      })
      

      # Display the data table 
      output$viewTable_deprivation_chem <- DT::renderDataTable({ 
        
        DT::datatable(
          dataToView_deprivation_chem() %>% 
            mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
            mutate(`Log Odds` = signif(log(Odds), 5)) %>%
            select(Variable, Odds, `Log Odds`, Risk),
          selection = "none",
          rownames= FALSE,
          options = list(
            pageLength = -1,
            dom = 't', 
            autoWidth = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = 0:3))
          )
        )
      })
      
      ### Historic Red Lining ----
      
      output$hrs_chem <- renderUI({
        
        req(exists(paste("non_spatial", "HRS", req(input$pollutionSource_chem), sep = "_")))
        
        # Get the dataset dynamically
        dataset <- get(paste("non_spatial", "HRS", req(input$pollutionSource_chem), sep = "_"))
        
        # Check if the chemical is in the dataset
        if (req(input$searchChemical_chem) %in% dataset$Variable) {
          
          tagList(
            div(
              class = "center-container",
              h4("Historic Red Lining"),
              column(8, DT::dataTableOutput("viewTable_hrs_chem")),
              br(),
            )
          )
          
        } else {
          return(NULL)  # Hide the section if the chemical is not in the dataset
        }
        
      })
      
      
      # Filter the data to be viewed
      dataToView_hrs_chem <- reactive({
        get(paste("non_spatial", "HRS", req(input$pollutionSource_chem), sep = "_")) %>%
          mutate_if(is.numeric, ~ signif(.x, 5)) %>%
          filter(Variable == req(input$searchChemical_chem)) %>%
          select(-Disease) %>%
          arrange(desc(abs(log(Odds))))
      })
      
      
      # Display the data table 
      output$viewTable_hrs_chem <- DT::renderDataTable({ 
        DT::datatable(
          dataToView_hrs_chem() %>% 
            mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
            mutate(`Log Odds` = signif(log(Odds), 5)) %>%
            select(Variable, Odds, `Log Odds`, Risk),
          selection = "none",
          rownames= FALSE,
          options = list(
            pageLength = -1,
            dom = 't', 
            autoWidth = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = 0:3))
          )
        )
      })
      
      
    ## Products containing chemical ----
    output$product_table_chem <- DT::renderDataTable({ 
      DT::datatable(
        chem_product %>% 
          filter(Chemical == req(input$searchChemical_chem)) %>%
          select(Chemical, Product),
        selection = "none",
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:1))
        ))
    })
      
  
  # Search by disease ----

    # Server side select disease
    isolate({
      
      diseases <- icdKey %>% 
        select(`SHORT DESCRIPTION (VALID ICD-10 FY2025)`, `LONG DESCRIPTION (VALID ICD-10 FY2025)`, Disease) %>%
        mutate(Description = coalesce(`LONG DESCRIPTION (VALID ICD-10 FY2025)`, `SHORT DESCRIPTION (VALID ICD-10 FY2025)`)) %>%
        select(Description, `SHORT DESCRIPTION (VALID ICD-10 FY2025)`) %>%
        arrange(Description)
      
      disease_choices <- as.character(diseases$`SHORT DESCRIPTION (VALID ICD-10 FY2025)`)
      names(disease_choices) <- diseases$Description
        
      updateSelectizeInput(
        session, 
        'searchDisease_disease', 
        choices = c("", disease_choices), 
        selected = NULL,  # Ensure nothing is pre-selected
        options = list(
          placeholder = 'Start typing or select disease from dropdown',
          onInitialize = I('function() { this.clear(); }'), # Force clearing on load
          persist = FALSE
        ),
        server = TRUE
      )
    })
    
    ## UI ----
      
    output$strat_select_input_disease <- renderUI({
      
      if (req(input$comb_or_strat_disease) == "Stratified") {
        tagList(
          selectInput(
            "dataSource_disease",
            "Model type",
            choices = c("Spatial" = "spatial", "Non-spatial" = "non_spatial"),
            selected = "Spatial"
          ),
          
          selectInput(
            "ageGroup_disease",
            "Age group",
            choices = c("Adult" = "Over18", "Pediatric" = "Under18"),
            selected = "Adult"
          ),
        )
      }
    })
      
      
    output$comb_or_strat_disease <- renderUI({
      
      # Check if the chemical is in the dataset
      if (req(input$comb_or_strat_disease) == "Combined") {
        
        tagList(
          
          h3("Combined Analysis"),
          p("This graph displays chemicals which have strong associations with 
            the disease you selected. The odds ratio was derived using the beta 
            coefficients from a logistic elastic net model, and it conveys the 
            change in odds of having the disease for each unit increase in the 
            chemical of interest."),
          
          p("If a chemical is presented, then the interactions with the disease 
            you selected is potentially important.  If the dot is far to the right 
            of the given line, that indicates the chemical you selected is one of 
            the strongest associations with the diagnosis indicated.  However, 
            while negative associations (odds ratios less than one) might indicate 
            that the chemical selected “protects” against the diagnosis, the more 
            likely reason for negative associations is that the chemical selected 
            causes diagnoses which displace visits for the diagnosis listed.  For 
            example, a chemical which triggered asthma would drive more people 
            with asthma to see their health care provider; if enough people were 
            being seen for asthma, it may leave less clinic appointments for 
            people with other lung diseases.  Thus, any association presented 
            should be evaluated for molecular or epidemiologic connections beyond 
            this analysis alone."),
          
          p("The solid black line is at 1. A dot with an odds ratio of greater 
            than 1 conveys increased risk. The color of the dot aligns with the 
            age range and type of analysis performed."),
          
          div(
            class = "center-container", 
            uiOutput("viewHistogram_disease_ui_adults"),
            br()
          ), 
          
          div(
            class = "center-container", 
            uiOutput("viewHistogram_disease_ui_kids"),
            br()
          ),
          
          downloadButton("download_combined_disease_chem_disease"),
        )
        
      } else {
        
        tagList(
          h3("Stratified Analysis"),
          p(textOutput("currentlyViewing_disease")),
          p("If a chemical is presented, then the interactions with the disease 
            you selected is potentially important.  If the dot is far to the right 
            of the given line, that indicates the chemical you selected is one of 
            the strongest associations with the diagnosis indicated.  However, 
            while negative associations (odds ratios less than one) might indicate 
            that the chemical selected “protects” against the diagnosis, the more 
            likely reason for negative associations is that the chemical selected 
            causes diagnoses which displace visits for the diagnosis listed.  For 
            example, a chemical which triggered asthma would drive more people 
            with asthma to see their health care provider; if enough people were 
            being seen for asthma, it may leave less clinic appointments for 
            people with other lung diseases.  Thus, any association presented 
            should be evaluated for molecular or epidemiologic connections beyond 
            this analysis alone."),
          p("This bar graph shows how likely certain chemicals are to be linked 
            with either a higher (red) or lower (blue) chance of developing the 
            disease."),
          div(class="center-container", column(8, plotlyOutput("viewPlots_disease"),),),
          br(),
          DT::dataTableOutput("viewTable_disease"),
          br(),
          downloadButton("download_stratified_disease_chem_disease"),
        )
        
      }
    })
        
      
    
    ## Combined ----
      ### Pediatric ----
      result_kids <- reactive({
        
        validate(
          need(input$searchDisease_disease != "", "Please select a disease")
        )
        
        if (req(input$pollutionSource_disease) == "Air") {
          
          result <- map_dfr(chemDiseaseDatasetList_Air_kids, function(df_name) {
            get(df_name) %>%
              filter(Disease == req(input$searchDisease_disease)) %>%
              mutate(Source = df_name)
          })
          
        } else {
          
          result <- map_dfr(chemDiseaseDatasetList_Water_kids, function(df_name) {
            get(df_name) %>%
              filter(Disease == req(input$searchDisease_disease)) %>%
              mutate(Source = df_name)
          })
          
        }
        
        result
        
      })
      
      # Set dynamic height (base 400px, +30px per chemical)
      plot_height_kids <- reactive({
        
        result <- req(result_kids())
        
        plot_height_kids <- max(400, length(unique(result$Variable)) * 18) # Minimum height = 400px
        }) 
      
      output$viewHistogram_disease_kids <- renderPlotly({
        df <- req(result_kids()) %>%
          mutate(Source = recode(Source,
                                 "non_spatial_Pediatric_ns_Air" = "Non-Spatial Model of<br>Pediatric (6-17) Visits",
                                 "non_spatial_Youth_Air" = "Non-Spatial Model of<br>Youth (0-5) Visits",
                                 "spatial_Under18_Air"= "Spatial Model of<br>Pediatric (<18) Visits",
                                 "non_spatial_Pediatric_ns_Water" = "Non-Spatial Model of<br>Pediatric (6-17) Visits",
                                 "non_spatial_Youth_Water" = "Non-Spatial Model of<br>Youth (0-5) Visits"
                                 ))
        
        vline <- function(x = 0, color = "black") {
          list(
            type = "line",
            y0 = 0,
            y1 = 1,
            yref = "paper",
            x0 = x,
            x1 = x,
            line = list(color = color, width = 0.5)
          )
        }
        
        p <- plot_ly(
          data = df, 
          x = ~Odds, 
          y = ~reorder(Variable, abs(Odds)),  # Order by extremity
          color = ~Source,  # Different colors for each dataset
          text = ~paste("<b>Chemical:</b>", Variable, 
                        "<br><b>Source:</b>", Source, 
                        "<br><b>Odds Ratio:</b>", signif(Odds, 5)),
          hoverinfo = "text",
          type = "scatter",
          mode = "markers",
          marker = list(size = 6, opacity = 0.7),
          height = plot_height_kids(),
          width = 800
        ) %>%
          layout(
            title = "Pediatric",
            shapes = list(vline(1)),
            xaxis = list(title = "Odds Ratio"),
            yaxis = list(title = " ", tickmode = "linear", dtick = 1),
            showlegend = TRUE,
            margin = list(l = 50, r = 50, t = 50, b = 50),
            font = list(color = 'black'),
            hoverlabel = list(font = list(color = "black"))
          )
        
        p
        
      })
      
      output$viewHistogram_disease_ui_kids <- renderUI({
        plotlyOutput("viewHistogram_disease_kids", height = plot_height_kids())
      })
      
      
      ### Adult ----
      result_adults <- reactive({
        
        if (req(input$pollutionSource_disease) == "Air") {
          
          result <- map_dfr(chemDiseaseDatasetList_Air_adults, function(df_name) {
            get(df_name) %>%
              filter(Disease == req(input$searchDisease_disease)) %>%
              mutate(Source = df_name)
          })
          
        } else {
          
          result <- map_dfr(chemDiseaseDatasetList_Water_adults, function(df_name) {
            get(df_name) %>%
              filter(Disease == req(input$searchDisease_disease)) %>%
              mutate(Source = df_name)
          })
          
        }
        
        result
        
      })
      
      # Set dynamic height (base 400px, +30px per chemical)
      plot_height_adults <- reactive({
        
        result <- req(result_adults())
        
        plot_height_adults <- max(400, length(unique(result$Variable)) * 18) # Minimum height = 400px
      }) 
      
      output$viewHistogram_disease_adults <- renderPlotly({

        df <- req(result_adults()) %>%
          mutate(Source = recode(Source,
                                 "non_spatial_Adult_ns_Air" = "Non-Spatial Model of<br>Adult (18-55) Visits",
                                 "non_spatial_Geriatric_Air" = "Non-Spatial Model of<br>Retirement (56-75) Visits",
                                 "non_spatial_Retirement_Air" = "Non-Spatial Model of<br>Geriatric (76+) Visits",
                                 "spatial_Over18_Air" = "Spatial Model of<br>Adult (18+) Visits",
                                 "non_spatial_Adult_ns_Water" = "Non-Spatial Model of<br>Adult (18-55) Visits",
                                 "non_spatial_Geriatric_Water" = "Non-Spatial Model of<br>Retirement (56-75) Visits",
                                 "non_spatial_Retirement_Water" = "Non-Spatial Model of<br>Geriatric (76+) Visits"
          ))
        
        vline <- function(x = 0, color = "black") {
          list(
            type = "line",
            y0 = 0,
            y1 = 1,
            yref = "paper",
            x0 = x,
            x1 = x,
            line = list(color = color, width = 0.5)
          )
        }
        
        p <- plot_ly(
          data = df, 
          x = ~Odds, 
          y = ~reorder(Variable, abs(Odds)),  # Order by extremity
          color = ~Source,  # Different colors for each dataset
          text = ~paste("<b>Chemical:</b>", Variable, 
                        "<br><b>Source:</b>", Source, 
                        "<br><b>Odds Ratio:</b>", signif(Odds, 5)),
          hoverinfo = "text",
          type = "scatter",
          mode = "markers",
          marker = list(size = 6, opacity = 0.7),
          height = plot_height_adults(),
          width = 800
        ) %>%
          layout(
            title = "Adult",
            shapes = list(vline(1)),
            xaxis = list(title = "Odds Ratio"),
            yaxis = list(title = " ", tickmode = "linear", dtick = 1),
            showlegend = TRUE,
            margin = list(l = 50, r = 50, t = 50, b = 50),
            font = list(color = 'black'),
            hoverlabel = list(font = list(color = "black"))
          )
        
        p
        
      })
      
      output$viewHistogram_disease_ui_adults <- renderUI({
        plotlyOutput("viewHistogram_disease_adults", height = plot_height_adults())
      })
      
      
      # Download output combined analysis
      output$download_combined_disease_chem_disease <- downloadHandler(
        filename = function() {
          paste(req(input$searchDisease_disease), req(input$pollutionSource_disease), "Combined-Analysis", "chemical-associations.csv", sep = "_")
        },
        
        content = function(file) {
          write.csv(
            rbind(result_adults(), result_kids()) %>% 
              mutate(`Log Odds` = log(Odds)) %>%
              mutate(Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased")) %>%
              select(Variable, Odds, `Log Odds`, Risk,  Source),
            file, 
            row.names = FALSE)
        }
      )
      
      
      
    ## Stratified analysis ---- 
    # Generate the title for the data viewer
    output$currentlyViewing_disease <- renderText({
      
      validate(
        need(input$searchDisease_disease != "", "Please select a disease")
      )
      
      ageGroup_diseaseText <- switch(req(input$ageGroup_disease),
                                     "Over18" = "an adult (over 18)",
                                     "Under18" = "a pediatric (under 18)",
                                     "Youth" = "a youth (ages 0-5)",  
                                     "Pediatric_ns" = "a pediatric (ages 6-17)",
                                     "Adult_ns" = "a adult (ages 18-55)",
                                     "Retirement" = "a retirement-age (ages 56-75)",
                                     "Geriatric" = "a geriatric (ages 76+)"
      )
      
      paste0(
        "Chemical associations with ", 
        req(input$searchDisease_disease), 
        ", using a ", 
        gsub("_", "-", req(input$dataSource_disease)), 
        " model in ", 
        ageGroup_diseaseText,
        " age group in the United States."
      )
    })
    
    
    # Different models have different age stratifications
    observe({
      
      if (req(input$dataSource_disease) == "non_spatial") {
        ageChoices <<- c(
          "Youth (0-5 yrs)" = "Youth", 
          "Pediatric (6-17 yrs)" = "Pediatric_ns", 
          "Adult (18-55 yrs)" = "Adult_ns", 
          "Retirement (56-75 yrs)" = "Retirement", 
          "Geriatric (+76 yrs)" = "Geriatric")
      } else if (req(input$dataSource_disease) == "spatial") {
        ageChoices <<- c(
          "Adult" = "Over18",
          "Pediatric" = "Under18"
        )
      }
      
      updateSelectInput(session = session,
                        "ageGroup_disease",
                        choices = ageChoices,
                        selected = ageChoices[1])
      
    })
    
    
    
    # Different models have different age stratifications
    observe({
      
      if (req(input$pollutionSource_disease) == "Water") {
        
        updateSelectInput(session = session,
                          "dataSource_disease",
                          choices = c("Non-spatial" = "non_spatial"),
                          selected = "non_spatial")
      } else {
        
        updateSelectInput(session = session,
                          "dataSource_disease",
                          choices = c(
                            "Spatial" = "spatial",
                            "Non-spatial" = "non_spatial"
                          ),
                          selected = "spatial")
      }
      
      
    })
    
    
    # Filter the data to be viewed
    dataToView_disease <- reactive({
      
      req(exists(paste(req(input$dataSource_disease), req(input$ageGroup_disease), req(input$pollutionSource_disease), sep = "_")))
      
      get(paste(req(input$dataSource_disease), req(input$ageGroup_disease), req(input$pollutionSource_disease), sep = "_")) %>%
        filter(Disease == req(input$searchDisease_disease)) %>%
        mutate_if(is.numeric, ~ signif(.x, 5)) %>%
        select(-Disease) %>%
        arrange(desc((log(Odds))))
      
    })
    
    # Plot the top 20 Oddss Odds/odds relative to other variables for the same disease
    output$viewPlots_disease <- renderPlotly({

        # Process data
        data_plot <- dataToView_disease() %>%
          mutate(`Log Odds` = log(Odds)) %>%
          mutate(ranking_metric = abs(`Log Odds`)) %>%
          slice_max(order_by = `Log Odds`, n = 15, with_ties = FALSE) %>%
          mutate(
            Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased"),
            Variable = reorder(Variable, `Log Odds`)
          )
        
        # Plotly chart
        p_plotly <- plot_ly(
          data = data_plot,
          x = ~`Log Odds`,
          y = ~Variable,
          type = "bar",
          orientation = "h",
          color = ~Risk,
          colors = c("Decreased" = "lightblue", "Increased" = "red"),
          hoverinfo = "none"
        ) %>%
          layout(
            barmode = "relative",
            title = list(text = "", x = 0),
            xaxis = list(title = "Log Odds Ratio", zeroline = TRUE),
            yaxis = list(title = ""),
            shapes = list(
              list(
                type = "line",
                x0 = 0,
                x1 = 0,
                y0 = -0.5,
                y1 = length(data_plot$Variable) - 0.5,
                line = list(dash = "dash", width = 1, color = "black")
              )
            ),
            font = list(size = 15),
            showlegend = TRUE
          )
        
        p_plotly
        
      
    })
    
    # Display the data table 
    output$viewTable_disease <- DT::renderDataTable({ 
      DT::datatable(
        dataToView_disease() %>% 
          mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
          mutate(`Log Odds` = signif(log(Odds), 5)) %>%
          select(Variable, Odds, `Log Odds`, Risk),
        selection = "none",
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:3))
        ))
    })
    
    output$download_stratified_disease_chem_disease <- downloadHandler(
      
      filename = function() {
        paste(req(input$searchDisease_disease), req(input$dataSource_disease), req(input$ageGroup_disease), req(input$pollutionSource_disease), "chemical-associations.csv", sep = "_")
      },
      
      content = function(file) {
        write.csv(
          req(dataToView_disease()) %>% 
            mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
            mutate(`Log Odds` = signif(log(Odds), 5)) %>%
            select(Variable, Odds, `Log Odds`, Risk),
          file, 
          row.names = FALSE)
      }
    )
    
    ## Mapping exposure ----
    map_data_disease <- reactive({
      
      # Load your data
      mapping_disease <- read_csv(paste0("Data/Disease_mapping_data/", gsub("[^A-Za-z0-9_-]", "_", req(input$searchDisease_disease)), ".csv"))
      
      # Join to FIPS
      mapping_disease %>%
        inner_join(fips_codes, by = c("county", "state")) %>%
        mutate(fips = paste0(state_code, county_code)) 
      
    })
    
    output$top_10_disease <- DT::renderDataTable({ 
      
      validate(
        need(input$searchDisease_disease != "", "Please select a disease")
      )
      
      age_groups <- names(req(map_data_disease()))[-c(1:3, (ncol(map_data_disease())-3):ncol(map_data_disease()))]

      top_10 <- lapply(age_groups, function (group) {
        map_data_disease() %>%
          slice_max(order_by = .[group], n = 10, with_ties = FALSE) %>%
          mutate(Location = paste(county, state, sep = ", ")) %>%
          pull(Location)
      })
      
      top_10 <- as.data.frame(do.call(cbind, top_10))
      names(top_10) <- age_groups
      
      top_10 <- cbind(Rank = 1:10, top_10)
      
        DT::datatable(
          top_10,
          selection = "none",
          rownames = FALSE,
          options = list(
            columnDefs = list(
              list(className = 'dt-center', targets = 0:(length(age_groups)))),
            dom = 't'
          ))
    })
    
    output$US_map_disease <- renderPlotly({
      
      map_data <- req(map_data_disease())
      
      # List of age group columns
      age_groups <- names(map_data)[-c(1:3, (ncol(map_data_disease())-3):ncol(map_data_disease()))]
      
      # Create dropdown buttons for each age group
      buttons <- lapply(seq_along(age_groups), function(i) {
        visibilities <- rep(FALSE, length(age_groups) + 1)  # +1 for grey layer
        visibilities[1] <- TRUE  # always show the grey layer
        visibilities[i + 1] <- TRUE  # show the selected age group trace
        
        visibilities_leg <- rep(FALSE, length(age_groups) + 1)  # +1 for grey layer
        visibilities_leg[1] <- FALSE  # always show the grey layer
        visibilities_leg[i + 1] <- TRUE  # show the selected age group trace
        
        list(
          method = "update",
          args = list(list(
            visible = visibilities,
            showscale = visibilities_leg
          )),
          label = age_groups[i]
        )
      })
      
      disease_plot <- plot_ly() %>%
        
        add_trace(
          data = all_visit_zipcodes,
          type = "choropleth",
          geojson = counties,
          locations = ~ fips,
          z = I(rep(1, nrow(all_visit_zipcodes))),
          showscale = FALSE,
          colorscale = list(c(0, 1), c("#dddddd", "#dddddd")),
          marker = list(line = list(width = 0)),
          hoverinfo = "none"
        ) %>%
        
        # Conditional age group layers
        {
          if ("Youth" %in% age_groups)
            add_trace(
              .,
              data = map_data %>% filter(!is.na(Youth)),
              geojson = counties,
              type = "choropleth",
              locations = ~ fips,
              z = ~ Youth,
              colorscale = "Viridis",
              colorbar = list(title = "Percentile"),
              marker = list(line = list(width = 0)),
              hoverinfo = "text",
              text = ~paste0(county, ", ", state,
                             "<br><b>Percentile:</b> ", signif(Youth, 3), "</br>"),
              visible = age_groups[1] == "Youth",
              showscale = age_groups[1] == "Youth"
            )
          else
            .
        } %>%
        
        {
          if ("Pediatric" %in% age_groups)
            add_trace(
              .,
              data = map_data %>% filter(!is.na(Pediatric)),
              geojson = counties,
              type = "choropleth",
              locations = ~ fips,
              z = ~ Pediatric,
              colorscale = "Viridis",
              colorbar = list(title = "Percentile"),
              marker = list(line = list(width = 0)),
              hoverinfo = "text",
              text = ~paste0(county, ", ", state,
                             "<br><b>Percentile:</b> ", signif(Pediatric, 3), "</br>"),
              visible = age_groups[1] == "Pediatric",
              showscale = age_groups[1] == "Pediatric"
            )
          else
            .
        } %>%
        
        {
          if ("Adult" %in% age_groups)
            add_trace(
              .,
              data = map_data %>% filter(!is.na(Adult)),
              geojson = counties,
              type = "choropleth",
              locations = ~ fips,
              z = ~ Adult,
              colorscale = "Viridis",
              colorbar = list(title = "Percentile"),
              marker = list(line = list(width = 0)),
              hoverinfo = "text",
              text = ~paste0(county, ", ", state,
                             "<br><b>Percentile:</b> ", signif(Adult, 3), "</br>"),
              visible = age_groups[1] == "Adult",
              showscale = age_groups[1] == "Adult"
            )
          else
            .
        } %>%
        
        {
          if ("Retirement" %in% age_groups)
            add_trace(
              .,
              data = map_data %>% filter(!is.na(Retirement)),
              geojson = counties,
              type = "choropleth",
              locations = ~ fips,
              z = ~ Retirement,
              colorscale = "Viridis",
              colorbar = list(title = "Percentile"),
              marker = list(line = list(width = 0)),
              hoverinfo = "text",
              text = ~paste0(county, ", ", state,
                             "<br><b>Percentile:</b> ", signif(Retirement, 3), "</br>"),
              visible = age_groups[1] == "Retirement",
              showscale = age_groups[1] == "Retirement"
            )
          else
            .
        } %>%
        
        {
          if ("Geriatric" %in% age_groups)
            add_trace(
              .,
              data = map_data %>% filter(!is.na(Geriatric)),
              geojson = counties,
              type = "choropleth",
              locations = ~ fips,
              z = ~ Geriatric,
              colorscale = "Viridis",
              colorbar = list(title = "Percentile"),
              marker = list(line = list(width = 0)),
              hoverinfo = "text",
              text = ~paste0(county, ", ", state,
                             "<br><b>Percentile:</b> ", signif(Geriatric, 3), "</br>"),
              visible = age_groups[1] == "Geriatric",
              showscale = age_groups[1] == "Geriatric"
            )
          else
            .
        } %>%
        
        layout(
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa"),
            showlakes = TRUE,
            lakecolor = toRGB("white")
          ),
          updatemenus = list(
            list(
              buttons = buttons,
              direction = "down",
              showactive = TRUE,
              x = 0.45,
              xanchor = "left",
              y = 1.05,
              yanchor = "top"
            )
          )
        )
      
      disease_plot
      
    }) |> bindEvent(input$generate_map_disease) 
    
    
    
    ## Toxin classes ----
    
    output$chem_class_count_disease <- DT::renderDataTable({
      
      if (req(input$comb_or_strat_disease) == "Stratified") {
        filtered_chem_class <- chem_class %>%
          filter(Chemical %in% dataToView_disease()$Variable)
      } else {
        filtered_chem_class <- chem_class %>%
          filter(Chemical %in% rbind(result_adults(), result_kids())$Variable)
      }
      
      
      datatable(
        filtered_chem_class %>%
          group_by(Class) %>%
          summarise(
            `Chemical Class` = length(unique(Chemical)),
            `Chemicals` = paste(unique(Chemical), collapse = "; ")
          ) %>%
          arrange(desc(`Chemical Class`)) %>%
          left_join(class_descriptions, by = "Class") %>%
          select(-References)
        ,
        selection = "none",
        rownames= FALSE,
        options = list(
          columnDefs = list(
            list(visible=FALSE, targets=c(3)), 
            list(className = 'dt-center', targets = 0:2)),
          pageLength = -1,
          dom = 't',
          rowCallback = JS(
            "function(row, data) {",
            "var full_text = data[3]",
            "$('td', row).attr('title', full_text);",
            "}")
        )
      )
    })
    
    output$download_chem_class_count <- downloadHandler(
      filename = function() {
        paste(req(input$searchDisease_disease), req(input$pollutionSource_disease), req(input$comb_or_strat_disease), "chem-class-summary.csv", sep = "_")
      },
      
      content = function(file) {
        
        if (req(input$comb_or_strat_disease) == "Stratified") {
          filtered_chem_class <- chem_class %>%
            filter(Chemical %in% dataToView_disease()$Variable)
        } else {
          filtered_chem_class <- chem_class %>%
            filter(Chemical %in% rbind(result_adults(), result_kids())$Variable)
        }
        
        write.csv(
          filtered_chem_class %>%
            group_by(Class) %>%
            summarise(
              `Chemical Class` = length(unique(Chemical)),
              `Chemicals` = paste(unique(Chemical), collapse = "; ")
            ) %>%
            arrange(desc(`Chemical Class`)) %>%
            left_join(class_descriptions, by = "Class") %>%
            select(-References),
          file, 
          row.names = FALSE)
      }
    )
    
    
    output$chem_class_pie_disease <- renderPlotly({
      
      if (req(input$comb_or_strat_disease) == "Stratified") {
        filtered_chem_class <- chem_class %>%
          filter(Chemical %in% dataToView_disease()$Variable)
      } else {
        filtered_chem_class <- chem_class %>%
          filter(Chemical %in% rbind(result_adults(), result_kids())$Variable)
      }
      
      filtered_chem_class %>%
        group_by(Class) %>%
        summarise(
          `Chemical Class` = length(unique(Chemical)),
        ) %>%
        arrange(desc(`Chemical Class`)) %>% 
        plot_ly(labels = ~Class, values = ~`Chemical Class`) %>% 
        add_pie(hole = 0.6) %>%
        layout(showlegend = F,
               plot_bgcolor="black",
               paper_bgcolor="black",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })

    ## Toxin protein interactions ----
    
    interactions <- reactive({

      
      if (req(input$comb_or_strat_disease) == "Stratified") {
        interactions <- chem_prot_interactions %>% 
          filter(`Toxin Name` %in% dataToView_disease()$Variable)
      } else {
        interactions <- chem_prot_interactions %>% 
          filter(`Toxin Name` %in% rbind(result_adults(), result_kids())$Variable)
      }
      
      interactions
      
      })
    
    # Using toxin protein interaction data, use enrich R to identify interesting pathways
    output$toxin_protein_disease <- DT::renderDataTable({ 

      if(nrow(req(interactions())) > 0) {
        DT::datatable(
          interactions() %>% 
            select(
              `Toxin Name`,
              `Gene Name`,
              Name,
              `Mechanism of Action`,
              `General Function`
            ),
          selection = "none",
          rownames = FALSE,
          options = list(
            columnDefs = list(
              list(visible=FALSE, targets=c(4)), 
              list(className = 'dt-center', targets = 0:4)),
            rowCallback = JS(
              "function(row, data) {",
              "var full_text = data[4]",
              "$('td', row).attr('title', full_text);",
              "}")
          ))
      }
    })
    
    output$download_toxin_protein_interaction_disease <- downloadHandler(
      filename = function() {
        paste(req(input$searchDisease_disease), req(input$pollutionSource_disease), req(input$comb_or_strat_disease), "toxin-protein-interactions.csv", sep = "_")
      },
      
      content = function(file) {
        write.csv(
          interactions() %>% 
            select(
              `Toxin Name`,
              `Gene Name`,
              Name,
              `Mechanism of Action`,
              `General Function`
            ),
          file, 
          row.names = FALSE)
      }
    )
    
    
    # Counts the number of toxins that interact with proteins and runs
    toxin_enrichment <- reactive({
      
      if (input$comb_or_strat_disease == "Stratified") {
        req(input$searchDisease_disease)
        total_toxins_in_disease <- nrow(req(dataToView_disease()))
      } else {
        req(input$searchDisease_disease)
        comb_df <- as.data.frame(rbind(req(result_adults()), req(result_kids())))
        total_toxins_in_disease <- length(unique(comb_df$Variable))
      }
      
      total_toxins <- length(unique(chem_prot_interactions$`Toxin Name`))
      
      total_toxins_by_gene <- chem_prot_interactions %>%
        filter(!is.na(`Gene Name`)) %>%
        group_by(`Gene Name`) %>%
        summarise(
          `Number of Toxins` = length(unique(`Toxin Name`)),
          `Interacting Toxins` = paste(unique(`Toxin Name`), collapse = "; "),
        ) %>%
        arrange(desc(`Number of Toxins`))
      
      toxin_by_gene_in_disease <- interactions() %>%
        filter(!is.na(`Gene Name`)) %>%
        rename("Gene" = `Gene Name`) %>%
        group_by(Gene, `General Function`) %>%
        summarise(
          `Number of Toxins` = length(unique(`Toxin Name`)),
          `Interacting Toxins` = paste(unique(`Toxin Name`), collapse = "; "),
        ) %>%
        arrange(desc(`Number of Toxins`))
      
      toxin_enrichment <- sapply(toxin_by_gene_in_disease$Gene, function(eachGene) {
        
        # Number of disease toxins affecting this gene
        a <- toxin_by_gene_in_disease[[which(toxin_by_gene_in_disease$Gene == eachGene), "Number of Toxins"]]
        
        # Total number of toxins affecting this gene (across all toxins, not just in disease)
        total_affecting_gene <- total_toxins_by_gene[[which(total_toxins_by_gene$`Gene Name` == eachGene), "Number of Toxins"]]
        
        # Number of disease toxins NOT affecting the gene
        c <- total_toxins_in_disease - a
        
        # Number of non-disease toxins affecting the gene
        b <- total_affecting_gene - a
        
        # Number of non-disease toxins NOT affecting the gene
        d <- total_toxins - total_toxins_in_disease - b
        
        fishers_table <- matrix(c(a, b, c, d), nrow = 2)
        
        FET <- fisher.test(fishers_table)
        
        return(c(FET$p.value, FET$estimate))
        
      })
      
      toxin_enrichment <- as.data.frame(t(toxin_enrichment))
      
      if (nrow(toxin_enrichment) > 1) {
        
        names(toxin_enrichment) <- c("FET P-value", "Odds Ratio")
        toxin_enrichment$`Adjusted FET P-value` <- p.adjust(toxin_enrichment$`FET P-value`, method = "BH")
        
        toxin_enrichment <- merge(toxin_enrichment %>% rownames_to_column("Gene"), 
                                  interactions() %>%
                                    filter(!is.na(`Gene Name`)) %>%
                                    rename("Gene" = `Gene Name`) %>%
                                    group_by(Gene, `General Function`) %>%
                                    summarise(
                                      `Number of Toxins` = length(unique(`Toxin Name`)),
                                      `Interacting Toxins` = paste(unique(`Toxin Name`), collapse = "; "),
                                    ) %>%
                                    arrange(desc(`Number of Toxins`)), 
                                  by = "Gene")
      } else {
        toxin_enrichment <- NULL
      }
      
      toxin_enrichment
      
    })
    
    
    output$protein_count_disease <- DT::renderDataTable({
      
      if (!is.null(toxin_enrichment())) {
      datatable(
        req(toxin_enrichment() %>% 
              mutate(across(where(is.numeric), ~ signif(., 3))) %>%
              mutate_if(is.numeric, list(~na_if(., Inf))) %>%
              mutate(Enriched = ifelse(`FET P-value` <= 0.05 & `Odds Ratio` > 1, "Enriched", NA))
              ) %>%
          select(
            Gene, 
            `Number of Toxins`, 
            `Interacting Toxins`, 
            Enriched,
            `General Function`
          ) %>%
          # mutate(`Odds Interpretation` = ifelse(
          #   `Odds Ratio` < 1 & `Adjusted FET P-value` < 0.05,
          #       "Underrepresented",
          #   ifelse(`Odds Ratio` > 1 & `Adjusted FET P-value` < 0.05,
          #       "Overrepresented",
          #       "No significant enrichment")
          #   )) %>%
          arrange(desc(`Number of Toxins`))
        ,
        selection = "none",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(visible=FALSE, targets=c(4)), 
            list(width = '200px', targets = c(4)),
            list(className = 'dt-center', targets = 0:4)),
          rowCallback = JS(
            "function(row, data) {",
            "var full_text = data[4]",
            "$('td', row).attr('title', full_text);",
            "}")
        )
      )
        }
    })
    
    output$download_protein_count_disease <- downloadHandler(
      filename = function() {
        paste(req(input$searchDisease_disease), req(input$pollutionSource_disease), req(input$comb_or_strat_disease), "toxin-protein-interactions-summary.csv", sep = "_")
      },
      
      content = function(file) {
        write.csv(
          req(toxin_enrichment() %>% 
                mutate(across(where(is.numeric), ~ signif(., 3))) %>%
                mutate_if(is.numeric, list(~na_if(., Inf))) %>%
                mutate(`Odds Ratio` = as.character(`Odds Ratio`)) %>%
                mutate(`Odds Ratio` = replace_na(`Odds Ratio`, "Inf"))
          ) %>%
            select(
              Gene, 
              `Number of Toxins`, 
              `Interacting Toxins`, 
              `Odds Ratio`, 
              `FET P-value`, 
              `Adjusted FET P-value`, 
              `General Function`
            ) %>%
            arrange(desc(`Number of Toxins`)),
          file, 
          row.names = FALSE)
      }
    )

    
    ## Run Enrichment ----
    # Render protein-toxin interactions only if the data source is air
    output$pathway_enrichment_disease <- renderUI({
      
      if(req(input$pollutionSource_disease) == "Air") {
        tagList(
          fluidRow(
            h3("Toxin-Protein Interactions"),
            
            p("Hover mouse over row to see gene function."),
            
            div(
              class = "center-container",
              column(8, DT::dataTableOutput("toxin_protein_disease"),),),
            downloadButton("download_toxin_protein_interaction_disease"),
            br(),
            br(),
            h4("Summary of Affected Proteins"),
            div(class = "center-container",
                column(
                  6,
                  p(
                    "The table below summarizes the number of toxin-protein interactions
                    associated with your disease of interest. Because there are only 792
                    proteins listed in the database and the interactions are biased towards
                    a select number of these (e.g. 63 out of the 191 toxins in this database
                    interact with the estrogen receptor ESR1), we add some statistical measures
                    to help guage how surprising it would be that, say, 10 of the 21 toxins
                    associated with your disease interact with each protein."
                  ),
                  p("Hover mouse over row to see mechanism of action"),
                ), 
            ), 
            
            div(
              class = "center-container",
              column(8, DT::dataTableOutput("protein_count_disease"),),),
            downloadButton("download_protein_count_disease"),
          ),
          
          br(),
          h3("Pathway Enrichment of Interacting Proteins"),
          div(
            class = "center-container",
            column(6, 
                   p("This will return broader biological pathways by which chemical toxins 
            are associated with the disease you selected, which is determined by 
            the number of associated genes relevant to a particular pathway."),
            HTML("<p>Clicking the button below will upload a list of the interacting
               proteins as shown above to <a href='https://maayanlab.cloud/Enrichr/'>
               Enrichr</a>. This will check for enrichment of pathways from the KEGG, 
               Gene Ontology, and WikiPathways databases. This may take a minute 
               or 2.</p>"),
            ),),
          br(),
          actionBttn("run_enrichment_disease", "Run Enrichment"),
          br(),
          br(),
          div(
            class = "center-container", htmlOutput("toxin_pathway_text_disease"),),
          br(),
          div(
            class = "center-container", DT::dataTableOutput("toxin_pathway_disease"),),
          
          downloadButton("download_pathway_enrichment_disease"),
        )
      } else {
        return(NULL)
      }
    })
    
    
    
    # TODO: Add error handling if EnrichR fails -------------------------------------------
    
    # When button is pressed, run the enrichR code for the current settings
    enriched_df <- reactive({ 
    
      # When you click run enrichment, a modal will pop up and will disappear when Enrichr is done running
      showModal(modalDialog(div("Processing data...", style="font-size:160%"), footer=NULL)) 
      
      # Only send the data if the toxins associated with the disease have known protein interactions
      if(nrow(req(interactions())) > 0) {
        
        # This is the main code that sends the data to EnrichR
        enriched <- enrichr(unique(interactions()$`Gene Name`), dbs)
        
        # This formats the output. EnrichR returns a list of data frames, one for each of the databases searched
        # The list of the databases that I selected can be found in the global.R file
        # Also worth noting that we only display significant pathways (before hypothesis-correction)
        enriched_df <- do.call(rbind, lapply(enriched, function(x) subset(x, P.value <= 0.05))) %>% 
          rownames_to_column("Pathway Source") %>%
          mutate(`Pathway Source` = sub("\\..*", "", `Pathway Source`))
        
        # I add 2 columns. The pathway enrichment is basically a fisher's exact test that takes the 
        # proteins you submit and compares it to the proteins in the pathway.
        # The columns I added tell you how many toxins in the data have known relationships with the pathway of interest
        # And list the names of those toxins
        contributing_chems <- merge(enriched_df %>% separate_rows(Genes, sep = ";"), interactions(), by.x = "Genes", by.y = "Gene Name") %>%
          group_by(Term) %>%
          summarise(`Number of Toxins` = length(unique(`Toxin Name`)),
                    `Driving Toxins` = paste(unique(`Toxin Name`), collapse = "; ")
                    )
        
        enriched_df <- merge(enriched_df, contributing_chems, by = "Term")
        
        removeModal() # Remove the moda
        
        } else { # If there are no known toxin-protein interactions, then give an error message and don't run EnrichR
          removeModal() 
          showModal(modalDialog(
            div("Unable to complete analysis. None of the 
                associated pollutants could be found in the 
                toxin-protein interaction database.", 
                style="font-size:160%"), 
            easyClose = TRUE, footer=NULL))
          enriched_df <- NULL
        }
        
        
        
        enriched_df
        
      }) |> bindEvent(input$run_enrichment_disease) # This makes the code run if and only if the Run Enrichment button is pressedl
        
      output$toxin_pathway_disease <- DT::renderDataTable({ 
        # Display the table 
        DT::datatable(
          req(enriched_df()) %>% 
            mutate(across(where(is.numeric), ~ signif(., 3))) %>% # Make significant figures pretty
            select(
              Term,
              Overlap,
              P.value,
              Adjusted.P.value,
              Odds.Ratio,
              `Pathway Source`,
              `Number of Toxins`,
              `Driving Toxins`, 
              Genes
              ) %>%
            arrange(desc(`Number of Toxins`)) %>% # Sort by p value
            mutate( # This also makes significant figures pretty, it uses scientific format if it is less than 0.001
              P.value = as.numeric(ifelse(P.value < 0.001,
                                        formatC(P.value, format = "e", digits = 3),
                                        formatC(P.value, format = "f", digits = 3))),
              Adjusted.P.value = as.numeric(ifelse(Adjusted.P.value < 0.001,
                                        formatC(Adjusted.P.value, format = "e", digits = 3),
                                        formatC(Adjusted.P.value, format = "f", digits = 3))),
              Genes = gsub(";", "; ", Genes)) # More formatting, but this allows the genes to undergo wrap text in the table
          ,
          selection = "none", # don't highlight when you select a row
          options = list(
            scrollX = TRUE, # It the table is wider than the display, allow scrolling so the display doesn't get extended
            columnDefs = list(
              list(width = '200px', targets = c(1, 8, 9)), # Standardize width of a couple columns and force wrap text
              list(className = 'dt-center', targets = 0:9) # Center the column headings
              )
          )
        )
    }) 

    
    output$toxin_pathway_text_disease <- renderText({
      
      if (req(input$comb_or_strat_disease) == "Stratified") {
        comb_or_strat_text <- paste(" in a", req(input$dataSource_disease), "model of an", req(input$ageGroup_disease), "population.")
      } else {
        comb_or_strat_text <- paste(" across all models.")
      }
      
      paste0(
        "<h4>If you have changed the input settings, click the Run Enrichment button again to update the results.</h4>",
        "<p style='text-align: center'>The table below shows the pathway enrichment results for toxins associated with <b>",
        req(input$searchDisease_disease), "</b>",
        comb_or_strat_text, "</p>"
        )
    }) |> bindEvent(input$run_enrichment_disease) # This makes the code run if and only if the Run Enrichment button is pressed

    output$download_pathway_enrichment_disease <- downloadHandler(
      filename = function() {
        paste(req(input$searchDisease_disease), req(input$pollutionSource_disease), req(input$comb_or_strat_disease), "pathway_enrichment.csv", sep = "_")
      },
      
      content = function(file) {
        write.csv(
          req(enriched_df()) %>% 
            mutate(across(where(is.numeric), ~ signif(., 3))) %>% # Make significant figures pretty
            select(
              Term,
              Overlap,
              P.value,
              Adjusted.P.value,
              Odds.Ratio,
              `Pathway Source`,
              `Number of Toxins`,
              `Driving Toxins`, 
              Genes
            ) %>%
            arrange(desc(`Number of Toxins`)) %>% # Sort by p value
            mutate( # This also makes significant figures pretty, it uses scientific format if it is less than 0.001
              P.value = as.numeric(ifelse(P.value < 0.001,
                                          formatC(P.value, format = "e", digits = 3),
                                          formatC(P.value, format = "f", digits = 3))),
              Adjusted.P.value = as.numeric(ifelse(Adjusted.P.value < 0.001,
                                                   formatC(Adjusted.P.value, format = "e", digits = 3),
                                                   formatC(Adjusted.P.value, format = "f", digits = 3))),
              Genes = gsub(";", "; ", Genes)),
          file, 
          row.names = FALSE)
      }
    )
    
    
  # Social determinants of health ----

    ## Ethnicity ----
    
    # There is only spatial data for Air pollution, so modify the inputs for ethnicity accordingly 
    observe({
      
      if (req(input$pollutionSource_determinant) == "Water") {
        
        updateSelectInput(session = session,
                          "dataSource_determinant",
                          choices = c("Non-spatial" = "non_spatial"),
                          selected = "non_spatial")
      } else {
  
        updateSelectInput(session = session,
                          "dataSource_determinant",
                          choices = c(
                            "Non-spatial" = "non_spatial",
                            "Spatial" = "spatial"
                          ),
                          selected = "non_spatial")
      }
    })
    
    
    # Prepare data to view for different user inputs for ethnicity
    dataToView_Race_determinant <- reactive({
      
      req(exists(paste(req(input$dataSource_determinant), "Race", req(input$pollutionSource_determinant), sep = "_")))
      get(paste(req(input$dataSource_determinant), "Race", req(input$pollutionSource_determinant), sep = "_")) %>%
        rename("Ethnicity" = "Disease") %>%
        filter(Ethnicity == req(input$race_determinant)) %>%
        mutate_if(is.numeric, ~ signif(.x, 5)) %>%
        select(-Ethnicity) %>%
        mutate(`Log Odds` = signif(log(Odds), 5)) %>%
        arrange(desc(abs(`Log Odds`)))
      
    })
    
    # Bar plot for risk of exposure for different ethnicities
    output$viewPlots_race_determinant <- renderPlotly({
      
      # Process data
      data_plot <- dataToView_Race_determinant() %>%
        mutate(ranking_metric = abs(`Log Odds`)) %>%
        slice_max(order_by = ranking_metric, n = 15, with_ties = FALSE) %>%
        mutate(
          Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased"),
          Variable = reorder(Variable, `Log Odds`)
        )
      
      # Plotly chart
      p_plotly <- plot_ly(
        data = data_plot,
        x = ~`Log Odds`,
        y = ~Variable,
        type = "bar",
        orientation = "h",
        color = ~Risk,
        colors = c("Decreased" = "lightblue", "Increased" = "red"),
        hoverinfo = "none"
      ) %>%
        layout(
          barmode = "relative",
          title = list(text = "", x = 0),
          xaxis = list(title = "Log Odds Ratio", zeroline = TRUE),
          yaxis = list(title = ""),
          shapes = list(
            list(
              type = "line",
              x0 = 0,
              x1 = 0,
              y0 = -0.5,
              y1 = length(data_plot$Variable) - 0.5,
              line = list(dash = "dash", width = 1, color = "black")
            )
          ),
          font = list(size = 15),
          showlegend = TRUE
        )
      
      p_plotly
      
    })
    
    # Ethnicity table 
    output$viewTable_race_determinant <- DT::renderDataTable({ 
      DT::datatable(
        dataToView_Race_determinant() %>% 
          mutate(Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased")) %>% 
          select(Variable, Odds, `Log Odds`, Risk),
        selection = "none",
        rownames= FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:3))
        ))
    })
  
  
    
    
    ## Deprivation ----
    
    # Prepare the deprivation index data frame -- There is no user input for this one
    dataToView_deprivation_determinant <- reactive({
      
      get(paste("non_spatial", "ADI", "Air", sep = "_")) %>%
        mutate_if(is.numeric, ~ signif(.x, 5)) %>%
        select(-Disease) %>%
        mutate(`Log Odds` = signif(log(Odds), 5)) %>%
        arrange(desc(abs(`Log Odds`)))
      
    })
    
    # Bar plot for deprivation index - used log odds because it doesn't skew to positive as much
    output$viewPlots_deprivation_determinant <- renderPlotly({
      
      # Process data
      data_plot <- dataToView_deprivation_determinant() %>%
        mutate(ranking_metric = abs(`Log Odds`)) %>%
        slice_max(order_by = ranking_metric, n = 15, with_ties = FALSE) %>%
        mutate(
          Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased"),
          Variable = reorder(Variable, `Log Odds`)
        )
      
      # Plotly chart
      p_plotly <- plot_ly(
        data = data_plot,
        x = ~`Log Odds`,
        y = ~Variable,
        type = "bar",
        orientation = "h",
        color = ~Risk,
        colors = c("Decreased" = "lightblue", "Increased" = "red"),
        hoverinfo = "none"
      ) %>%
        layout(
          barmode = "relative",
          title = list(text = "", x = 0),
          xaxis = list(title = "Log Odds Ratio", zeroline = TRUE),
          yaxis = list(title = ""),
          shapes = list(
            list(
              type = "line",
              x0 = 0,
              x1 = 0,
              y0 = -0.5,
              y1 = length(data_plot$Variable) - 0.5,
              line = list(dash = "dash", width = 1, color = "black")
            )
          ),
          font = list(size = 15),
          showlegend = TRUE
        )
      
      p_plotly

    })
    
    # Deprivation index table
    output$viewTable_deprivation_determinant <- DT::renderDataTable({ 
      DT::datatable(
        dataToView_deprivation_determinant() %>% 
          mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
          select(Variable, Odds, `Log Odds`, Risk),
        selection = "none",
        rownames= FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:3))
        ))
    })
    
  
    
    
    
    ## Historic Red Lining ----
    
    # Prepare the historic red lining data frame from user input
    dataToView_hrs_determinant <- reactive({
      
      req(exists(paste("non_spatial", "HRS", req(input$pollutionSource_hrs_determinant), sep = "_")))
      get(paste("non_spatial", "HRS", req(input$pollutionSource_hrs_determinant), sep = "_")) %>%
        mutate_if(is.numeric, ~ signif(.x, 5)) %>%
        mutate(`Log Odds` = signif(log(Odds), 5)) %>%
        select(-Disease) %>%
        arrange(desc(abs(`Log Odds`)))
      
    })
    
    # Bar plot of Historic Red Lining
    output$viewPlots_hrs_determinant <- renderPlotly({

        # Process data
        data_plot <- dataToView_hrs_determinant() %>%
          mutate(ranking_metric = abs(`Log Odds`)) %>%
          slice_max(order_by = ranking_metric, n = 15, with_ties = FALSE) %>%
          mutate(
            Risk = ifelse(`Log Odds` > 0, "Increased", "Decreased"),
            Variable = reorder(Variable, `Log Odds`)
          )
        
        # Plotly chart
        p_plotly <- plot_ly(
          data = data_plot,
          x = ~`Log Odds`,
          y = ~Variable,
          type = "bar",
          orientation = "h",
          color = ~Risk,
          colors = c("Decreased" = "lightblue", "Increased" = "red"),
          hoverinfo = "none"
        ) %>%
          layout(
            barmode = "relative",
            title = list(text = "", x = 0),
            xaxis = list(title = "Log Odds Ratio", zeroline = TRUE),
            yaxis = list(title = ""),
            shapes = list(
              list(
                type = "line",
                x0 = 0,
                x1 = 0,
                y0 = -0.5,
                y1 = length(data_plot$Variable) - 0.5,
                line = list(dash = "dash", width = 1, color = "black")
              )
            ),
            font = list(size = 15),
            showlegend = TRUE
          )
        
        p_plotly
      
    })
    
    # Historic Red Lining Code
    output$viewTable_hrs_determinant <- DT::renderDataTable({ 
      DT::datatable(
        dataToView_hrs_determinant() %>% 
          mutate(Risk = ifelse(Odds > 1, "Increased", "Decreased")) %>% 
          select(Variable, Odds, `Log Odds`, Risk),
        selection = "none",
        rownames= FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = 0:3))
        ))
    })
  
}
