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
    img(src="PADDLE Blue Background.png", margin-top = "-5px",
    height = "45px", width = "auto")
  ),
  
  # About page ----
  tabPanel("Home", 
           
     fluidPage(
       align = "center",
       
       fluidRow(
         img(src = "PADDLE Black Background.png", align = "center", width='650px'),
       ),
       br(),
       br(),
       fluidRow(
            HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/CksOqC-zP9s?si=B3fuRay7T1ufTuey" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'
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
                       Prem Prashant Chaudhary, Ian A Myles. <a href='https://doi.org/10.1038/s41598-026-39836-2'><i>P.A.D.D.L.E.: a hypothesis generation tool for 
                       assessing pollution’s potential role in disease.</i></a> Scientific Reports. 16, 8808 (2026). </p>"),
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
            <p style='text-align: left'>&emsp;&emsp;Disease rates were taken from the Agency for Healthcare Research and Quality
            (AHRQ) <a href='https://www.ahrq.gov/data/innovations/syh-dr.html' target='_blank'>
            Synthetic Healthcare Database for Research (SyH-DR)</a>. The database
            collected all billable clinical visits in the United States which
            occurred in 2016. Rates for each International Classification of Diseases (ICD) included in the SyH-DR were
            calculated by taking the incidence of diagnosis and dividing by the
            total billed diagnoses for each included zip code. The SyH-DR
            anonymizes diagnoses in zip codes with less than 10,000 people and the
            locations for disease diagnoses occurring less than 10,000 times.
            Therefore, these rare disorders and small towns were pre-excluded from
            the analysis. We further excluded ICD codes that occurred in less than 2% of zip codes to reduce the risk that an ICD
            would be used only by a few providers in a localized region. Converting ICD codes to their
            respective names was performed using the
            <a href='https://www.cms.gov/medicare/coordination-benefits-recovery/overview/icd-code-lists' target='_blank'>
            ICD code list</a> provided by the Centers for Medicare and Medicaid Services (CMS).</p>
            
            <p style='text-align: left'>&emsp;&emsp;For any given healthcare visit, up to 10 ICD diagnoses could be assigned in addition to a primary diagnosis. Because the ordering of diagnoses can sometimes reflect administrative rather than clinical priorities, we counted each diagnosis separately rather than relying solely on the primary diagnosis. This approach makes the models more resistant to arbitrary coding decisions and captures disease prevalence more completely. However, we acknowledge this may inflate the apparent frequency of common, chronic, or multi-system diseases (such as diabetes) that generate multiple related diagnostic codes per visit. Multiple visits by the same patient were treated as separate events, as this may reflect greater symptom severity.</p>
            
            <p style='text-align: left'>&emsp;&emsp;For non-spatial analysis, billing visits were separated into categories
            of 'Pre-K' (ages 0–5), 'pediatric' (6–17 years), 'adult'
            (18–54 years), 'retirement age' (55–74 years), and 'geriatric'
            (75 years and older). Each age cohort was modeled independently.
            Because of the increased computational power required for spatial
            analysis, subjects were grouped as either adult (18 and over) or pediatric
            (under 18 years of age).</p>
            <br>
            
            <h3 class='ohio-state'>Identification of pollution exposures and modeling</h3>
            <p style='text-align: left'>&emsp;&emsp;Air pollution exposure was derived from the EPA databases <a href='https://www.epa.gov/rsei' target='_blank'>
            Risk-Screening Environmental Indicators (RSEI)</a> and <a href='https://www.epa.gov/toxics-release-inventory-tri-program' target='_blank'>
            Toxics Release Inventory (TRI)</a>. Outdoor concentrations of O<sub>3</sub>, CO, SO<sub>2</sub>, NO<sub>2</sub>, PM<sub>10</sub>, and PM<sub>2.5</sub> were derived from the <a href='https://www.caces.us/' target='_blank'>Center for Air, Climate, &amp; Energy Solutions (CACES)</a> using their Land Use Regression (LUR) model, with census tract-level data averaged for overlapping zip codes. Water pollution was separately evaluated
            using the <a href='https://www.epa.gov/dwucmr' target='_blank'>Monitoring Unregulated
            Contaminants in Drinking Water (UCMR)</a> data from the EPA. The UCMR data from UCMR 3–5 was
            combined to contrast against the 2016 AHRQ data. If differing
            measurements of the same chemical were reported in different UCMR databases,
            the results were averaged prior to analysis. Exposures were collated from the years 2010–2016
            to contrast with the AHRQ data from 2016.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Feature matrices were constructed by the same method as previously described<sup>
            <a href='https://pubmed.ncbi.nlm.nih.gov/38637696/' target='_blank'>1</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/36608129/' target='_blank'>2</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/37692200/' target='_blank'>3</a>
            </sup>. In brief, for each zip code in the AHRQ database, a 30-mile catchment area was defined around the zip code centroid. The total amount of each pollutant released by facilities within that catchment area was summed. A Gaussian distance-weighting function was applied to account for the fact that the zip code reflects the location of the healthcare provider, not necessarily the patient's residence. Water pollution was attributed only to the zip code of the EPA measurement device, as cross-zip dissemination patterns are not captured in the UCMR data.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Two complementary modeling approaches were used to assess associations between environmental toxicant exposures and disease diagnosis rates: a non-spatial penalized regression and a spatial penalized regression. Both approaches were applied separately to each of 5,984 disease diagnoses. The predictor matrix for air pollution models included 592 variables: 571 environmental exposures and 21 sociodemographic covariates (census age distributions, deprivation index, population density, and for non-spatial models, latitude and longitude). The water pollution model contained 42 environmental exposures alongside the same sociodemographic covariates. All predictors were standardized prior to modeling to allow comparison across variables with different measurement scales.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Nonspatial analysis was performed as previously described<sup>
            <a href='https://pubmed.ncbi.nlm.nih.gov/36608129/' target='_blank'>2</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/37692200/' target='_blank'>3</a></sup>,
            using the glmnet package in R. For each disease-age combination, an elastic net regression model was fit (alpha = 0.5) with the regularization parameter tuned via 10-fold cross-validation. Because elastic net regression does not produce p-values, we filtered associations to those with beta-coefficients more than two standard deviations (2SD) from the mean. Correlations more than 5SD from the mean are displayed on the website to improve readability, but all correlations are available in the underlying data.</p>
            
            <p style='text-align: left'>&emsp;&emsp;For spatial modeling, a negative binomial generalized linear mixed effects model was fit with nested spatial random effects, applied only to air pollution data and the two collapsed age strata (pediatric and adult) due to computational demands. A four-level nested spatial hierarchy was constructed using hierarchical clustering on distances between zip code centroids, generating clusters of approximately 81, 27, 9, and 3 zip codes at each successive level. These clusters were included as random effects to capture spatial autocorrelation at multiple geographic scales.</p>
            <br>
            
            <h3 class='ohio-state'>Additional comparisons</h3>
            <p style='text-align: left'>&emsp;&emsp;Racial disparities were calculated by taking the percentage
            representation of each race/ethnicity from the <a href='https://www.census.gov/' target='_blank'>
            US Census of 2020</a>. Deprivation index was collected from the
            <a href='https://www.neighborhoodatlas.medicine.wisc.edu/' target='_blank'>
            Neighborhood Atlas</a> from the Center of Health Disparities Research at
            the University of Wisconsin. <a href='https://redivis.com/datasets/rnef-d56dafea8?v=1.0' target='_blank'>
            Historic redlining scores for 2020</a> US census tracts were used after
            registration from the Environmental Impact Data Collective. Exposure
            rates for social determinants were collected from the years 2013–2019
            to compare against the 2020 census.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Connecting which commercial products contain any indicated chemical
            was taken from the <a href='https://comptox.epa.gov/chemexpo/get_data/' target='_blank'>EPA
            ChemExpo databases</a>. Spatial and non-spatial modeling was performed
            as for diseases. Mapping functions were performed using the ggmap and
            viridis packages in R.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Protein-toxicant interactions were accessed from the <a href='http://www.t3db.ca/' target='_blank'>Toxin-Target Database (T3DB)</a>. Protein-level enrichment analysis was performed using Fisher's exact test to identify proteins disproportionately targeted by disease-associated toxins. Pathway enrichment analysis was performed using the enrichR package, referencing GO Biological Process, GO Molecular Function, KEGG, and WikiPathways databases. Enriched proteins are indicated on the website where FDR-corrected p-values are less than 0.05.</p>
            <br>
            
            <h3 class='ohio-state'>Limitations</h3>
            <p style='text-align: left'>&emsp;&emsp;The major limitation of this work is that the AHRQ database was only
            a pilot program encompassing visits in the US in 2016. The inability to
            assess disease-chemical associations over time likely limits our accuracy.
            For example, our prior work on Alzheimer's disease used a commercial database spanning 2017–2019 and identified slightly different specific
            chemicals; although the fact that the same chemical class was identified
            speaks to the value of using pathway and chemical class aggregation in our
            assessments. One advantage of the AHRQ data is that, unlike most
            commercial databases, it contains both inpatient and outpatient visit
            information. Furthermore, should the AHRQ or other agency collect updated
            disease visit information, P.A.D.D.L.E. could be updated to incorporate
            the new data.</p>
            
            <p style='text-align: left'>&emsp;&emsp;The use of zip code-aggregated data introduces important caveats, including the ecological fallacy (population-level associations may not reflect individual risk), the possibility that a patient's zip code of care differs from their home zip code, and potential inflation of results from densely zip-coded urban areas. Despite these limitations, aggregated data enables nationwide analysis while protecting individual privacy, and our penalized regression approach and spatial smoothing terms help mitigate some of these effects. This approach is consistent with our goal of designing a hypothesis-generating tool to identify associations warranting further investigation with individual-level data.</p>
            
            <p style='text-align: left'>&emsp;&emsp;Another limitation is that our databases assessing functional
            consequences of chemical exposure are not fully congruent. For example, the
            database for known protein-pollutant interactions covers only a subset of
            the overall pollutant data and is enriched for targets that have received greater investigative attention. The pathway
            analysis offered by P.A.D.D.L.E. may still serve hypothesis generation
            for researchers. Finally, the authors stress that the data outputs
            from P.A.D.D.L.E. are correlations and associations only and should not
            be assumed to be causal, even when statistically significant. Any
            association identified should either be assessed against the established
            literature or be experimentally modeled before drawing any conclusions.
            Notably, negative associations could theoretically represent a protective effect, but because our clinical data derives from healthcare visits rather than individual diagnoses, it is more likely that a given chemical generates other diseases that displace visits for certain ailments — similar to how areas with the highest rates of COVID-19 saw a drop in outpatient visits for non-COVID conditions.<sup><a href='https://pubmed.ncbi.nlm.nih.gov/36893413/'
            target='_blank'>4</a></sup>
            Both negative and positive associations should spur mechanistic
            follow-up studies rather than be assumed to reflect causal relationships.</p>
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
             HTML("<p>The graph below displays the 15 diagnoses with the strongest 
              associations with your selected chemical, based on the absolute value 
              of the beta coefficient from a logistic elastic net model. The odds 
              ratios were calculated by exponentiating the beta coefficients, 
              representing the change in odds of having the diagnosis for each unit 
              increase in the chemical of interest.
              
              <div style='background-color: #333333'>
              <ul>
              <li>The <b>red dot</b> represents the specific odds ratio for the chemical-diagnosis association.</li>
              <li>The <b>black line</b> indicates the full range of odds ratios observed for that diagnosis across all chemical associations.</li>
              <li>The <b>x</b> marks the mean odds ratio for that diagnosis.</li>
              <li>The <b>grey range</b> represents one standard deviation above and below the mean.</li>
              <li>A <b>dashed line</b> at 1 serves as a reference, indicating no association (odds ratio = 1 means no increased or decreased risk).</li>
              </ul></p>
              </div>
              
              <p>If a diagnosis is presented, then the chemical interactions with that diagnosis are 
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
           p("The table below shows all the diagnoses with which your chemical of interest was associated.
             The total predictors column shows how many chemicals were associated with a change in risk in each diagnosis.
             The mean, standard deviation, max, and min columns show summary statistics for the chemicals associated with 
             each diagnosis, so you can guage the relative importance of the chemical in moderating diagnosis risk."),
           
           # Display all the odds and the diagnosis ranges
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
  
  # Search by diagnosis ----
  tabPanel(
    "Search Diagnoses",
    # Make a page layout that contains a side panel for inputs  and a main panel for outputs
    
    sidebarLayout(
      
      sidebarPanel(
        style="padding: 0px 30px 0px 30px;",
        
        h4("Search by Diagnosis"),
        
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
                       for diagnoses that require a specialist, the specialist's county
                       may be artificially inflated."),
                     plotlyOutput("US_map_disease") %>% withSpinner(color = "#666666", type = 6),
                     br(),
                     h4("Top 10 Counties in Each Age Group"),
                     p("The following counties had the highest clinical visit rates
                       for the selected diesease for each age group. If an age group does not
                       appear in the table, that means no visits were recorded for
                       the selected diagnosis in that age group."),
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
                             this diagnosis is associated with. The table lists the classes,
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
           ),
),


# Summary Data ----
tabPanel("Summary Data",
         
         fluidPage(
           tags$head(
             tags$style('
                          ul.nav-pills{
                            display: flex !important;
                            justify-content: center !important;
                          }')
           ),
           align = "center",
           
           h3("Summary Data"),
           
           
           tabsetPanel(
             type="pills",
             tabPanel(
               "Adult Diagnosis Data",
               br(),
               div(class = 'center-container',
                   column(
                     6,
                     p(
                       "Download heat maps (nonspatial only) or full spread sheets of the various associations identified by P.A.D.D.L.E."
                     ),
                     p(
                       "Note that heat maps are limited to only the associations that are >5 standard deviations removed from 
                       the mean of all associations."
                     ),
                   ), 
               ),
               
               fluidPage(
                 align = "center",
                 
                 fluidRow(
                   h3(class = "ohio-state", "Air Pollution and Adults (18-54 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Adult_Air_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water Pollution and Adults (18-54 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Adult_Water_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Air Pollution and Adults (55-74 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Retirement_Air_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water Pollution and Adults (55-74 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Retirement_Water_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Air Pollution and Adults (75+ yrs)"),
                   img(src = "Summary Data Images/Diseases_hm/Geriatric_Air_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water Pollution and Adults (75+ yrs)"),
                   img(src = "Summary Data Images/Diseases_hm/Geriatric_Water_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 
                 
                 fluidRow(
                   h3(class = "ohio-state", "Download Adult Nonspatial Data"),
                   downloadButton("downloadNonspatialad", "Download")
                 ),
               ),
               
               br(),
               br(),
               
               fluidRow(
                 h3(class = "ohio-state", "Download Adult/Peds Spatial Data"),
                 downloadButton("downloadSpatialad", "Download")
               )
             ),
             
             
             
             tabPanel(
               "Pediatric Diagnosis Data",
               br(),
               div(class = 'center-container',
                   column(
                     6,
                     p(
                       "Download heat maps (nonspatial only) or full spread sheets of the various associations identified by P.A.D.D.L.E."
                     ),
                     p(
                       "Note that heat maps are limited to only the associations that are >5 standard deviations removed from 
                       the mean of all associations."
                     ),
                   ), 
               ),
               
               fluidPage(
                 align = "center",
                 
                 fluidRow(
                   h3(class = "ohio-state", "Air Pollution and Children (0-5 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Youth_Air_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water Pollution and Children (0-5 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Youth_Water_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Air Pollution and Children (6-17 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Pediatric_Air_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water Pollution and Children (6-17 yrs old)"),
                   img(src = "Summary Data Images/Diseases_hm/Pediatric_Water_nonspatial_5SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 br(),

                 fluidRow(
                   h3(class = "ohio-state", "Download Pediatric Nonspatial Data"),
                   downloadButton("downloadNonspatial", "Download")
                 ),
               ),
               
               br(),
               br(),
               
               fluidRow(
                 h3(class = "ohio-state", "Download Adult/Peds Spatial Data"),
                 downloadButton("downloadSpatial", "Download")
               ),
             ),
             
             
             tabPanel(
               "Social Determinants Data",
               br(),
               
               div(class = 'center-container',
                   column(
                     6,
                     p(
                       "Download heat maps or full spread sheets of the various associations identified by P.A.D.D.L.E."
                     ),
                     p(
                       "Note that top hits heat maps are limited to only the associations that are >2 standard deviations removed from 
                       the mean of all associations."
                     ),
                   ), 
               ),
               
               fluidPage(
                 align = "center",
                 
                 fluidRow(
                   h3(class = "ohio-state", "Top Hits for Air & Water Pollution and Deprivation (nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Deprivation/Deprivation_combined_nonspatial_2SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Full set for Air & Water Pollution and Deprivation (nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Deprivation/Deprivation_combined_nonspatial_Full.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Pollution linked to Historic Redlining Score (HRS)(nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/HRS_air_water_nonspatial.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Top hits for air pollutants associated with increased % of population of given ethnicity (nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Ethnicity/Ethnicity_air_nonspatial_2SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Full set for air pollutants associated with increased % of population of given ethnicity  (nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Ethnicity/Ethnicity_air_nonspatial.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Water pollutants associated with increased % of population of given ethnicity (nonspatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Ethnicity/Ethnicity_water_nonspatial.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Top hits for air pollutants associated with increased % of population of given ethnicity  (spatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Ethnicity/Ethnicity_air_spatial_2SD.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Full set for air pollutants associated with increased % of population of given ethnicity  (spatial)"),
                   img(src = "Summary Data Images/Social_Determinants/Ethnicity/Ethnicity_air_spatial.png", align = "center", width='850px'),
                 ),
                 
                 br(),
                 br(),
                 
                 fluidRow(
                   h3(class = "ohio-state", "Download Social Determinants Data"),
                   downloadButton("downloadSDOH", "Download"))
               ),
             ),
           ),
         ),
    ),

# Summary Data ----
tabPanel("Additional Links",
         
         fluidPage(
           tags$head(
             tags$style('
                          ul.nav-pills{
                            display: flex !important;
                            justify-content: center !important;
                          }')
           ),
           align = "center",
           
           h3(class = "ohio-state", "EPA Where You Live"),
           
           div(class = 'center-container',
               column(
                 6,
                 HTML("<p><a href = 'https://www.epa.gov/trinationalanalysis/where-you-live'>https://www.epa.gov/trinationalanalysis/where-you-live</a></p>"),
                 p(
                   "This site reports the specific chemicals released by factories, 
                   and the amounts released.  Search for your state or zip code to 
                   see the sources of industrial pollution in your area.  Note, 
                   this site does not collect data on road/automobile exhaust.  
                   Using this site would allow one to assess the pollutants in 
                   their area, which can then be searched in PADDLE for disease 
                   associations of concern." 
                   ), 
               ), 
           ),
           
           h3(class = "ohio-state","EPA TRI Toxin Tracker"),
           
           div(class = 'center-container',
               column(
                 6,
                 HTML("<p><a href = 'https://edap.epa.gov/public/extensions/TRIToxicsTracker/TRIToxicsTracker.html'>https://edap.epa.gov/public/extensions/TRIToxicsTracker/TRIToxicsTracker.html</a></p>"),
                 p("This site reports the exact locations for factories releasing 
                   toxic substances.  Search by address, state, or zip code to see 
                   the facilities in your area.  Note, this site does not collect
                   data on road/automobile exhaust."
                   ), 
               ), 
           ),
           

           
         ),
  ),
)
