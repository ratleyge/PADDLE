## UI ##


# Sets up nav bar layout for a shiny app
ui <- navbarPage(
  
  # Set my custom Css to make sure that the nav bar layout is compatible with the side bar layout
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # Nice theme, but others can be found here: https://rstudio.github.io/shinythemes/
  theme = shinytheme("cyborg"),
  
  # Nav bar fixed to the top of the page
  position = c("fixed-top"),
  
  # Title in top left corner
  title = div(
    class = "customNavbar-title",
    img(src="White no background paddle logo.png", height = "30px", width = "auto")
  ),
  
  # About page ----
  tabPanel("Home", 
           
     fluidPage(
       align = "center",
       
       fluidRow(
         img(src = "White no background paddle logo.png", align = "center", width='650px'),
       ),
       br(),
       br(),
       fluidRow(
         column(6, 
                img(src = "Man in boat.jpg", align = "right"),
         ),
         column(6, 
                img(src = "vid placeholder.jpg", align = "left", height = "307px", width = "auto"),
         ),
       ),
       br(),
       h3(class = "ohio-state", "Wading through our toxic world"),
       br(),
       br(),
       div(class = "center-container",
           column(6, 
                  style = "background-color: #333333; padding: 0px 10px 15px 10px;",
                  HTML("<h3 class='ohio-state'>Citation</h3>
                       <p>Grace Ratley, Aditi Vijendra, Jalin Jordan, Pranav Thota, Jordan Zeldin, 
            Prem Prashant Chaudhary, Ian A Myles. <i>P.A.D.D.L.E. : A novel tool for 
            assessing pollution’s potential role in disease.</i> The Epithelial Therapeutics Unit. 
            Version 1.0, April 2025.</p>"),
                  ),
           ),
     ),
  ),
  
  tabPanel("Methods",
    fluidPage(
      align = "center",
      div(class='center-container',
        column(6,
          HTML("
            <h3 class='ohio-state'>Derivation of disease rates</h3>
            <p style='text-align: left'>&emsp;&emsp;Disease rates from the Agency for Healthcare Research and Quality 
            (AHRQ) <a href='https://www.ahrq.gov/data/innovations/syh-dr.html' target='_blank'>
            Synthetic Healthcare Database for Research (SyH-DR)</a>.  The database 
            collected all billable clinical visits in the United States which 
            occurred in 2016.  Rates for each ICD included in the SyH-DR were 
            calculated by taking the incidence of diagnosis and dividing by the 
            total billed diagnoses for each included zip code.  The SyH-DR 
            anonymizes diagnoses in zip codes with less than 10,000 people and the 
            locations for disease diagnoses occurring less than 10,000 times.  
            Therefore, these rare disorders and small towns were pre-excluded from 
            the analysis.  We further excluded ICD’s that occurred in less than 2% 
            of zip codes to reduce the risk that an ICD would be used only by a 
            few providers in a localized region.  Converting ICD codes to their 
            respective names was performed using the 
            <a href='https://www.cms.gov/medicare/coordination-benefits-recovery/overview/icd-code-lists' target='_blank'>
            ICD code list</a> provided by the Centers for Medicare and Medicaid 
            Services (CMS).</p>
            
            <p style='text-align: left'>&emsp;&emsp;For non-spatial analysis, billing visits were separated into categories 
            of “youth” (age 0-5 years of age), “pediatric” (6-17 years), “adult” 
            (18-54 years), “retirement age” (55-74 years), and “geriatric” 
            (75 years and older).  Each age cohort was modeled independently.  
            Because of the increased computational power required for spatial 
            analysis, subjects were grouped by adult (over 18) or pediatric 
            (under 18 years of age).</p>
            <br>
            
            <h3 class='ohio-state'>Identification of pollution exposures and modeling</h3>
            <p style='text-align: left'>&emsp;&emsp;Pollution exposure was derived from the EPA databases <a href='https://www.epa.gov/rsei' target='_blank'>
            Risk-Screening Environmental Indicators (RSEI)</a> and <a herf='https://www.epa.gov/toxics-release-inventory-tri-program' target='_blank'>
            Toxics Release Inventory (TRI)</a>.  Water pollution was also evaluated 
            using the <a href='https://www.epa.gov/dwucmr' target='_blank'>Monitoring Unregulated 
            Contaminants in Drinking Water (UCMR)</a> data from the EPA.  Features 
            matrixes were constructed by same method as previously described<sup>
            <a href='https://pubmed.ncbi.nlm.nih.gov/38637696/' target='_blank'>1</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/36608129/' target='_blank'>2</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/37692200/' target='_blank'>3</a>
            </sup>.  In brief, 30-mile catchment areas were created around point-
            source release emissions for air pollution.  Distance weights were 
            created so that pollution sources with catchment areas that expanded 
            beyond the borders to their zip code were counted towards all zip 
            codes that would be within the 30-mile area.  Water pollution was 
            counted only towards the zip code of the measurement device reported 
            by the EPA.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Nonspatial analysis was performed as previously described
            <sup><a href='https://pubmed.ncbi.nlm.nih.gov/36608129/' target='_blank'>2</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/37692200/' target='_blank'>3</a></sup>, 
            using the glmnet package in R17.  After beta-coefficients were derived, 
            because our non-spatial approach does not produce p-values, we eliminated 
            associations that were less than two standard deviations (2SD) from the mean 
            beta-values.  Partial model fit testing was performed by assessing the 
            means of the residuals and removing any model which was more than 2% removed 
            from a residual mean of 0.  Correlations were displayed for those with more 
            than 5SD from the mean to improve readability. Spatial modeling was performed 
            as previously described9, also using the glmnet package in R.  Spatial models 
            were screened for trend p values of <0.1.  Heatmaps were created using the 
            pheatmap package in R.  The exposures were collated from the years 2010-2016 
            to contrast with the AHRQ data from 2016.  The UCMR data from UCMR 3-5 was 
            combined to contrast against the 2016 AHRQ data as well.  If differing 
            measurements of the same chemical were reported in different UCMR databases, 
            the results were averaged prior to analysis.</p>
            <br>
            
            <h3 class='ohio-state'>Additional comparisons</h3>
            <p style='text-align: left'>&emsp;&emsp;Racial disparities were calculated by taking the percentage 
            representation of each race/ethnicity from the <a href='https://www.census.gov/' target='_blank'>
            US Census of 2020</a>.  Deprivation index was collected from the 
            <a href='https://www.neighborhoodatlas.medicine.wisc.edu/' target='_blank'>
            Neighborhood Altas</a> from the Center of Health Disparities Research at 
            the University of Wisconsin.  <a href='https://redivis.com/datasets/rnef-d56dafea8?v=1.0' target='_blank'>
            Historic redlining scores for 2020</a> US census tracts were used after 
            registration from the Environmental Impact Data Collective.  Exposure 
            rates for social determinants were collected from the years 2013-2019 
            to compare against the 2020 census.</p> 
            
            <p style='text-align: left'>&emsp;&emsp;Connecting which commercial products contain any indicated chemical 
            was taken from <a href='https://comptox.epa.gov/chemexpo/get_data/'>EPA 
            ChemExpo databases</a>.  Spatial and non-spatial modeling was performed 
            as for diseases.  Mapping functions were performed using the ggmap and 
            viridis packages in R.</p>
            <br>
            
            <h3 class='ohio-state'>Limitations</h3>
            <p style='text-align: left'>&emsp;&emsp;The major limitation of this work is that the AHRQ database was only 
            a pilot program encompassing visits in the US in 2016.  The inability to 
            assess disease-chemical associations over time likely limits our accuracy.  
            For example, our prior work on AD used a commercial database which 
            included data from 2017-2019 and identified slightly different specific 
            chemicals; although, the fact that the same chemical class was identified 
            speaks to the value of using pathway and chemical class aggregation in our
            assessments. However, one advantage of the AHRQ data is that, unlike most 
            commercial databases, it contains both inpatient and outpatient visit 
            information.  Furthermore, should the AHRQ or other agency collect updated 
            disease visit information, P.A.D.D.L.E. could be updated to incorporate 
            the new data.  A similar limitation of our work is that it is limited to 
            pollution data released in the United States only.  Our included databases 
            do not capture exposures that may cause harm as commercial products unless 
            the exposure also creates pathology in the areas surrounding their 
            manufacturing point sources.  Nations with centralized health records 
            could however mirror our approach by collecting pollution data in their 
            countries and/or product exposure surveys and performing similar 
            correlation assessments.</p>

            <p style='text-align: left'>&emsp;&emsp;Another limitation is that our databases assessing functional 
            consequences of chemical exposure are not congruent.  For example, the 
            database for known protein-pollutant interactions is only a subset of 
            the overall pollutant data and is enriched for gene and protein targets 
            that have received greater investigative attention.  However, the pathway 
            analysis offered by P.A.D.D.L.E. may still serve hypothesis generation 
            for researchers.  Finally, the authors stress that the data outputs 
            from P.A.D.D.L.E. are correlations and associations only and should not 
            be assumed to be causal, even when statistically significant.  Any 
            association identified should either be assessed against the established 
            literature or be experimentally modeled before drawing any conclusions 
            between the associations presented.  For example, although negative 
            associations could theoretically represent a protective effect of a 
            given chemical.  Because our clinical data is derived from visits to 
            healthcare providers rather than individual diagnose, it seems more 
            likely that the chemical indicated may generate other diseases which 
            displace visits for other ailments.  For example, areas with the 
            highest rates of COVID-19 infections saw a drop in outpatient visits 
            for non-COVID related ailments.<sup><a href='https://pubmed.ncbi.nlm.nih.gov/36893413/' 
            target='_blank'>4</a></sup>  
            Thus, both negative and positive associations should spur mechanistic 
            follow up studies rather than assumed causal relationships.</p>
          "),
         ),
      ),
    ),
  ),
  
  # Search chemicals ----
  tabPanel("Search Chemicals", # Make a page layout that contains a side panel for inputs  and a main panel for outputs
           
       sidebarLayout(
         # Inputs ----
         sidebarPanel(
           id = "searchByChem",
           style="padding: 0px 30px 0px 30px;",
           
           h3("Search by Chemical"),
           
           # If air - update selector to get air pollutants
           # If water update selector to get water pollutants & only allow non-spatial
           selectInput(
             "pollutionSource_chem",
             "Pollution Source:",
             choices = c(
               "Air",
               "Water - non-spatial only" = "Water"
             ),
             selected = "Air"
           ),
           
           # Which chemical to filter
           selectizeInput(
             "searchChemical_chem",
             "Chemical of Interest:",
             choices = NULL
           ),
           
           # Choose model architecture
           selectInput(
             "dataSource_chem",
             "Model Type:",
             choices = c(
              "Spatial" = "spatial",
              "Non-Spatial" = "non_spatial"
             ),
            selected = "Spatial"
           ),
           
           # Choose the age group 
           # To-do: offer an option to look at all at once?
           selectInput(
             "ageGroup_chem",
             "Age Group",
             choices = NULL
           )
           
         ),
         
         # Outputs ----
         mainPanel(
           align = "center",
           style="padding: 0px 50px 0px 30px;",
           
           # State which data we are looking at based on the inputs 
           h4(textOutput("currentlyViewing_chem")),
           br(), # Space beneath
           
           fluidRow(
             id = "chem_properties",
             tags$style('#chem_properties {
                             background-color: #333333;
              }'),
             
             column(
               3,
               class = "center-container",
               h5("Classes"),
               textOutput("chem_class_chem"), 
               br(),
              ),
             column(
               3,
               class = "center-container",
               h5("Carcinogen"),
               textOutput("carcinogen_chem"), 
               br(),
             ),
             column(
               3,
               class = "center-container",
               h5("Organ Toxicity"),
               textOutput("organ_tox_chem"), 
               br(),
             ),
             column(
               3,
               class = "center-container",
               h5("Timing of Toxicity"),
               textOutput("tox_timing_chem"), 
               br(),
             ),
           ),
           
           br(),
           
           # Explain the graphical output
           fluidRow(
             align = "left",
             HTML("<p>The graph below displays the 15 diseases with the strongest 
              associations with your selected chemical, based on the absolute value 
              of the beta coefficient from a logistic elastic net model. The odds 
              ratios were calculated by exponentiating the beta coefficients, 
              representing the change in odds of having the disease for each unit 
              increase in the chemical of interest.
              
              <div style='background-color: #333333'>
              <ul>
              <li>The <b>red dot</b> represents the specific odds ratio for the chemical-disease association.</li>
              <li>The <b>black line</b> indicates the full range of odds ratios observed for that disease across all chemical associations.</li>
              <li>The <b>x</b> marks the mean odds ratio for that disease.</li>
              <li>The <b>grey range</b> represents one standard deviation above and below the mean.</li>
              <li>A <b>dashed line</b> at 1 serves as a reference, indicating no association (odds ratio = 1 means no increased or decreased risk).</li>
              </ul></p>
              </div>
              
              <p>If a diagnosis is presented, then the chemical interactions with that disease are 
              potentially important.  If the red dot is far to the right of the given black line, 
              that indicates the chemical you selected is one of the strongest associations with the 
              diagnosis indicated.  However, while negative associations (odds ratios less than one) 
              might indicate that the chemical selected “protects” against the diagnosis, the more 
              likely reason for negative associations is that the chemical selected causes diagnoses 
              which displace visits for the diagnosis listed.  For example, a chemical which triggered 
              asthma would drive more people with asthma to see their health care provider; if enough 
              people were being seen for asthma, it may leave less clinic appointments for people with 
              other lung diseases.  Thus, any association presented should be evaluated for molecular 
              or epidemiologic connections beyond this analysis alone.</p>
              "),
           ),
           
           # Plot the output
           div(
             class = "center-container",
             plotlyOutput("viewPlots_chem")
             ),
           
           br(), # Space beneath
           
           # Explain the table
           p("The table below shows all the diseases with which your chemical of interest was associated.
             The total predictors column shows how many chemicals were associated with a change in risk in each disease.
             The mean, standard deviation, max, and min columns show summary statistics for the chemicals associated with 
             each disease, so you can guage the relative importance of the chemical in moderating disease risk."),
           
           # Display all the odds and the disease ranges
           DT::dataTableOutput("viewTable_chem"),
           downloadButton("download_diseases_chem"),
           br(),
           
           # Distribution of chemical release across the US
           h3("Distribution Across the US"),
           actionBttn("generate_map_chem", "Generate Map"),
           br(),
           br(),
           p("If you have changed the input settings, click the Generate Map button again to update the results."),
           p("Values are min-max scaled."),
           plotlyOutput("US_map_chem") %>% withSpinner(color = "#666666", type = 6),
           
           # At risk
           h3("At Risk Groups"),
           p("Associations between the selected chemical and deprivation index, ethnicity, and historic red-lining, 
             if present, are shown below."),
           uiOutput("deprivation_chem"),
           uiOutput("race_chem"),
           uiOutput("hrs_chem"),
           
           h3("Products Containing the Chemical"),
           HTML("<p>This table includes products that are known to contain the chemical of interest, 
             per the EPA’s <a href='https://comptox.epa.gov/chemexpo/get_data/'>ChemExpo database</a>.  
             However, most products have not been assessed for 
             chemical content and thus failure to see any products listed here does not indicate 
             that no commercial product has the chemical of interest.</p>"),
           div(class = "center-container", column(8, DT::dataTableOutput("product_table_chem"),),),
         ),
       ), 
  ),
  
  # Search by disease ----
  tabPanel(
    "Search Diseases",
    # Make a page layout that contains a side panel for inputs  and a main panel for outputs
    
    sidebarLayout(
      
      sidebarPanel(
        style="padding: 0px 30px 0px 30px;",
        
        h4("Search by Disease"),
        
        selectInput(
          "pollutionSource_disease",
          "Pollution Source:",
          choices = c("Air", "Water - non-spatial only" = "Water"),
          selected = "Air"
        ),
        
        selectizeInput(
          "searchDisease_disease",
          "Disease of Interest:",
          choices = NULL,
          selected = NULL,  # Ensure nothing is pre-selected
          options = list(
            placeholder = 'Start typing or select chemical from dropdown',
            onInitialize = I('function() { this.clear(); }') # Force clearing on load
          )
        ),
        
        selectizeInput(
          "comb_or_strat_disease",
          "View Combined or Stratified Data:",
          choices = c("Combined", "Stratified"),
          selected = "Combined"
        ),
        
        uiOutput("strat_select_input_disease"),
        
      ),
      
      mainPanel(
        align = "center",
        style="padding: 0px 50px 0px 30px;",
        uiOutput("comb_or_strat_disease"),
      ),
    ),
    
    fluidPage(
      align = "center",
      
        fluidRow(
          # Distribution of chemical release across the US
          h3("Distribution Across the US"),
          div(class="center-container",
            column(6, style="background-color: #333333; padding: 10px 10px 5px 10px;",
                     p("Be advised that the generation of this figure may take several 
                        minutes and may reduce the performace of other sections of the website."),
                     ),
            ),
          br(),
          actionBttn("generate_map_disease", "Generate Map"),
          br(),
          br(),
          p("If you have changed the input settings, click the Generate Map button again to update the results."),
          div(class = "center-container",
              column(8, 
                     p("Note that the rates are 
                       derived from the providers location, not the patient's. This means that
                       for diseases that require a specialist, the specialist's county
                       may be artificially inflated."),
                     plotlyOutput("US_map_disease") %>% withSpinner(color = "#666666", type = 6),
                     br(),
                     h4("Top 10 Counties in Each Age Group"),
                     p("The following counties had the highest clinical visit rates
                       for the selected diesease for each age group. If an age group does not
                       appear in the table, that means no visits were recorded for
                       the selected disease in that age group."),
                     DT::dataTableOutput("top_10_disease"),
                     ),
              ),
          
        ),
      
        fluidRow(
          h3("Chemical Class Summary"),
          div(class = "center-container",
              column(
                6,
                p(
                  "The donut chart displays the chemical classes of the compounds
                             this disease is associated with. The table lists the classes,
                             from most to least common, and the chemicals found within that class."
                ),
                p(
                  "Note: There are overlapping chemicals between the chemical
                             classes, as certain groups of chemicals fall under broader
                             categories. For example, Phthalates are often described as
                             Endocrine disruptors and BTEX substances are often described
                             as Volatile Organic Compounds.
                           "
                ),
                p("Hover mouse over chart or table for more chemical class information."),
              ), ), 
          br(),
          column(5, 
                 plotlyOutput("chem_class_pie_disease"), 
          ),
          column(7, 
                 DT::dataTableOutput("chem_class_count_disease"),
                 br(),
                 downloadButton("download_chem_class_count"),
                 br(),
          ),
        ),
        

        uiOutput("pathway_enrichment_disease"),
    ),
  ), 
  
  # Social determinants of health ----
  tabPanel("Search Determinants of Health",
           # Make a page layout that contains a side panel for inputs  and a main panel for outputs
           
           fluidPage(
             tags$head(
               tags$style('
                          ul.nav-pills{
                            display: flex !important;
                            justify-content: center !important;
                          }')
             ),
             align = "center",
             
             h3("Search Determinants"),
             
             
             tabsetPanel(
               type="pills",
               tabPanel(
                 "Ethnicity",
                 br(),
                 div(class = 'center-container',
                     column(
                       6,
                       p(
                         "This table displays the risk of chemical exposures, through
              an odds ratio, based on the ethnicity you have selected.
              “Longitude” or “latitude” may appear in the table. While they
              are not chemical exposures, they are included to express that
              certain chemicals have greater exposure in certain regions of
              the country."
                       ),
              p(
                "Longitude and latitude may also be associated with racial
              demographics given that US populations are not evenly
              distributed – for example, Black American populations represent a 
              higher percentage of zip codes in the South Eastern US than other areas."
              ),
                     ), ),
              
              fluidRow(
                column(
                  3,
                  selectInput(
                    "pollutionSource_determinant",
                    "Pollution Source",
                    choices = c("Air",
                                "Water - non-spatial only" = "Water"),
                    selected = "Air"
                  ),
                  
                  selectInput(
                    "dataSource_determinant",
                    "Model Type",
                    choices = c("Non-spatial" = "non_spatial",
                                "Spatial" = "spatial"),
                    selected = "non_spatial"
                  ),
                  
                  selectInput(
                    "race_determinant",
                    "Percent _______ in zip code:",
                    choices = c(
                      "White" = "White",
                      "Black" = "BlackAA",
                      "Native American" = "NativeAmerican",
                      "Asian & Pacific Islander" = "AAPI",
                      "Hispanic" = "Hispanic"
                    ),
                    selected = "White"
                  )
                ),
                column(6, plotlyOutput("viewPlots_race_determinant"),),
              ),
              br(),
              div(class = "center-container",
                  column(
                    6, DT::dataTableOutput("viewTable_race_determinant")
                  ), ),
              br(),
               ),
              
              tabPanel(
                "Deprivation",
                br(),
                div(class = 'center-container',
                    column(
                      6,
                      p(
                        "The Area Deprivation Index (ADI) is a widely used and well-validated
           composite measure of 17 different variables of education, employment,
           housing-quality, and income. The ADI uses data from the American Community
           Survey to assign census block groups a number denoting its level of
           neighborhood socioeconomic disadvantage. Higher numbers on the ADI indicate
           greater disadvantage. It is currently maintained and regularly updated by
           the University of Wisconsin’s School of Medicine and Public Health"
                      ),
                    ), ),
           
           div(class = "center-container",
               column(
                 6, plotlyOutput("viewPlots_deprivation_determinant")
               ), ),
           
           br(),
           
           div(class = "center-container",
               column(
                 6, DT::dataTableOutput("viewTable_deprivation_determinant")
               ), ),
           
           br(),
           
              ),
           
           tabPanel(
             "Historic Red Lining",
             br(),
             div(class = 'center-container',
                 column(
                   6,
                   p(
                     "The Historic Redlining Score is a metric to express the
                   extent to which historic redlining practices in the 20th
                   century have affected concentrated inequality and racial
                   disparities today. Redlining was a discriminatory practice
                   describing the government sanctioned denial of financial
                   services, such as mortgage loans and insurance, to minority
                   communities, especially Black communities. Redlining practices
                   sequestered minority communities into neighborhoods deemed
                   “hazardous” and were  a key mechanism by which the United
                   States maintained and deepened social inequities over time.
                   The Historic Redling Score was determined by overlaying
                   historic Home Owners’ Loan Corporation (HOLC) redlining maps
                   with the 2020 census tracts and assigning the neighborhood
                   classification grades a numerical score. A higher HRS conveys
                   more redlining in a given census tract. The scores are weighted
                   to account for spatial discrepancies between historic maps
                   and the 2020 census tracts. "
                   ),
                 ),),
             
             fluidRow(column(
               3,
               
               selectInput(
                 "pollutionSource_hrs_determinant",
                 "Pollution Source",
                 choices = c("Air",
                             "Water - non-spatial only" = "Water"),
                 selected = "Air"
               ),
             ),
             
             column(
               6, plotlyOutput("viewPlots_hrs_determinant")
             ),),
             
             br(),
             
             div(class = "center-container",
                 column(
                   6, DT::dataTableOutput("viewTable_hrs_determinant")
                 ),),
           ),
             ),
           ),),
)
