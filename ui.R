## UI ##

# Sets up nav bar layout for a shiny app
ui <- navbarPage(
  
  windowTitle = "PADDLE",
  
  # Set my custom Css to make sure that the nav bar layout is compatible with the side bar layout
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(
      HTML(
        "
          function toggleLegend(id) {
            var el = document.getElementById(id || 'chart-legend');
            el.classList.toggle('collapsed');
          }
      
          function toggleSection(id) {
            var body   = document.getElementById(id);
            var arrow  = document.getElementById(id + '-arrow');
            var header = body.previousElementSibling;
            body.classList.toggle('hidden');
            if (header) header.classList.toggle('open');
          }
      
          function updateDeterminantPills() {
            var tab = document.querySelector('input[name=\"determinant_tab\"]:checked');
            var src = document.querySelector('input[name=\"pollutionSource_determinant\"]:checked');
      
            var tabVal = tab ? tab.value : 'Ethnicity';
            var srcVal = src ? src.value : 'Air';
      
            var spatialInput = document.getElementById('dt_spatial');
            var spatialLabel = document.querySelector('label[for=\"dt_spatial\"]');
            var waterInput   = document.getElementById('pd_water');
            var waterLabel   = document.querySelector('label[for=\"pd_water\"]');
      
            var disableSpatial = (tabVal === 'Deprivation' || tabVal === 'HRS' || srcVal === 'Water');
            spatialInput.disabled = disableSpatial;
            spatialLabel.style.opacity = disableSpatial ? '0.35' : '1';
            spatialLabel.style.cursor  = disableSpatial ? 'not-allowed' : 'pointer';
      
            if (disableSpatial && spatialInput.checked) {
              spatialInput.checked = false;
              document.getElementById('dt_nonspatial').checked = true;
              Shiny.setInputValue('dataSource_determinant', 'non_spatial', {priority: 'event'});
            }
      
            var disableWater = (tabVal === 'Deprivation');
            waterInput.disabled = disableWater;
            waterLabel.style.opacity = disableWater ? '0.35' : '1';
            waterLabel.style.cursor  = disableWater ? 'not-allowed' : 'pointer';
      
            if (disableWater && waterInput.checked) {
              waterInput.checked = false;
              document.getElementById('pd_air').checked = true;
              Shiny.setInputValue('pollutionSource_determinant', 'Air', {priority: 'event'});
            }
          }
          
          function toggleAbout(el, id) {
            el.querySelector('.icon-filter').classList.toggle('active');
            var target = document.getElementById(id);
            var isAlreadyOpen = target.classList.contains('open');
            document.querySelectorAll('.about-hidden.open').forEach(function(div) { div.classList.remove('open'); });
            document.querySelectorAll('.image-cropper .icon-filter.active').forEach(function(img) {
              if (img !== el.querySelector('.icon-filter')) img.classList.remove('active');
            });
            if (!isAlreadyOpen) { target.classList.add('open'); }
          }
                
          document.addEventListener('DOMContentLoaded', function() {
      
            // Sync all toggle pills to Shiny
            document.querySelectorAll('.toggle-pill-input').forEach(function(radio) {
              radio.addEventListener('change', function() {
                var target = this.getAttribute('data-target');
                if (target) Shiny.setInputValue(target, this.value, {priority: 'event'});
              });
            });
      
            // Attach determinant pill logic
            document.querySelectorAll(
              'input[name=\"determinant_tab\"], input[name=\"pollutionSource_determinant\"], input[name=\"dataSource_determinant\"]'
            ).forEach(function(radio) {
              radio.addEventListener('change', updateDeterminantPills);
            });
      
            updateDeterminantPills();
          });
          "
      )
    )
  ), 
  
  # Nice theme, but others can be found here: https://rstudio.github.io/shinythemes/
  theme = shinytheme("cyborg"),
  
  # Nav bar fixed to the top of the page
  position = c("fixed-top"),
  
  # Title in top left corner
  title = div(
    class = "customNavbar-title",
    img(src="PADDLE Blue Background.png", style="margin: -3px -20px",
        height = "55px")
  ),
  
  
  tabPanel("Home", fluidPage(style = "max-width: fit-content; margin-left: auto; margin-right: auto;",
                             
     # Contents Section -----------------------------------------------------------------
     column(3,
        class = "paddle-sidebar",
        style = "position: fixed; top: 70px; padding: 10px;",
        
        div(class = "sidebar-header",
            h3("Contents", class = "sidebar-title", style = "font-size: 25px !important; margin-top: 5px; margin-bottom: 5px;")
        ),
        a("To Top", href="#top", style = "font-size:14px;"),
        br(),
        
        hr(class = "sidebar-divider"),
        
        p(style="margin-top: 20px; margin-bottom: 10px;", a("Methods", href="#methods", class = "h4", style="font-size: 21px"),),
        a("Derivation of Disease Rates", href="#Derivation_of_disease_rates", style = "font-size:14px;"),
        br(),
        a("Pollution Exposures and Modeling", href="#Identification_of_pollution_exposures_and_modeling", style = "font-size:14px;"),
        br(),
        a("Additional Comparisons", href="#Additional_comparisons", style = "font-size:14px;"),
        br(),
        a("Limitations", href="#Limitations", style = "font-size:14px;"),
        br(),
        a("Download Data", href="#download_data",  style = "font-size:14px;"),
        
        p(style="margin-top: 20px; margin-bottom: 10px", a("Additional Links", href="#additional_links", class = "h4", style="font-size: 21px")),
        a(HTML("EPA Where You Live &#8599;"), target="_blank", href="https://www.epa.gov/trinationalanalysis/where-you-live", style = "font-size:14px;"),
        br(),
        a(HTML("TRI Toxics Tracker &#8599;"), target="_blank", href="https://edap.epa.gov/public/extensions/TRIToxicsTracker/TRIToxicsTracker.html", style = "font-size:14px; margin-bottom: 20px"),
        br(),
        a("Citation", href="#citation", style = "font-size:14px;"),
        br(),
        a("About Us", href="#about_us", style = "font-size:14px;"),
        br(),
     ),
     
     column(4),
     
     
     # Main Panel -----------------------------------------------------------------
     column(8,
        align = "center",
        class = 'center-container',
        
        fluidRow(
          img(src = "PADDLE Black Background.png", align = "center", width='650px'),
        ),
        
        # Intro Section ----------------------------------------------------------------------
        HTML(
          "
        <div style='background-color: #00425A; width: 100%; margin-top: 10px; height: 20px'>
        </div>
        <div style='background-color: white; color: black; width: 100%; padding: 10px 40px 10px 40px; margin-top: 0px;'>
        <div>
          <h4 style='color: black'>A Tool For Assessing Pollution’s Potential Role in Disease</h4>
          <p>Since the 1960s, tens of thousands of chemicals have been added to the global market, yet the vast majority
          lack comprehensive health risk assessments. During this same period, industrialized nations have experienced 
          dramatic increases in inflammatory diseases, raising concerns about environmental contributors. We aim to 
          provide a tool for researchers to explore associations between environmental toxicant releases and diseases
          of interest, assess impacts of the route of exposure, connect findings to protein targets and biological 
          pathways, map geographic “hot spots”, and identify at-risk populations. </p>
        </div>
        
        <img style='width: 90%; margin-bottom: -20px; height: 45px' src='Paddle divider.png'>
        
        <div id='summary' class = 'col-sm-12' style='margin-top: 20px;'>
          <div>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px'>61.9M</p>
          <p style='font-size: 15px;'>Healthcare Visits</p>
          </div>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px;'>4,533</p>
          <p style='font-size: 15px;'>Diagnoses</p>
          </div>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px;'>16,451</p>
          <p style='font-size: 15px;'>Zip Codes</p>
          </div>
          </div>
          
          <div style='margin-top: 10px'>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px'>571</p>
          <p style='font-size: 15px;'>Air Pollutants</p>
          </div>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px;'>42</p>
          <p style='font-size: 15px;'>Water Pollutants</p>
          </div>
          <div class='col-sm-4'>
          <p class='syncopate-bold' style='font-size: 30px; color: #026285; margin-bottom: 0px;'>21</p>
          <p style='font-size: 15px;'>Sociodemographic Covariates</p>
          </div>
          </div>
        </div>
        
        </div>
        
        
        <div id='how_to_paddle' style='margin-bottom: 0px; background-color: #1f1f1f; width: 100%; padding-bottom: 20px; padding-top: 10px;'>
        <h4 style='color: white'>An Introduction to Using PADDLE</h4>
        <iframe width='560' height='315' src='https://www.youtube.com/embed/CksOqC-zP9s?si=B3fuRay7T1ufTuey' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share' referrerpolicy='strict-origin-when-cross-origin' allowfullscreen></iframe>
        </div>
        "),
        
        # Methods Section -----------------------------------------------------------------------
        HTML("
        <div>
        <h3 id='methods' class='syncopate-bold' style='font-size: 50px; color: white; margin-top: 60px'>Methods</h3>
        </div>
        
        <div style='background-color: #00425A; width: 100%;  height: 20px; margin-top: 0px;'></div>
        <div style='background-color: white; color: black; padding: 10px 40px 10px 40px; margin-top: 0px; margin-bottom: 40px'>
        "),
        
        # Disease Rates
        HTML("
        <div style='margin-top: 5px;'>
            <h4 id='Derivation_of_disease_rates' style='color: black'>Derivation of Disease Rates</h4>
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

            <p style='text-align: left'>&emsp;&emsp;For any given healthcare visit, up to 10 ICD diagnoses could be assigned 
            in addition to a primary diagnosis. Because the ordering of diagnoses can sometimes reflect administrative rather 
            than clinical priorities, we counted each diagnosis separately rather than relying solely on the primary diagnosis. 
            This approach makes the models more resistant to arbitrary coding decisions and captures disease prevalence more 
            completely. However, we acknowledge this may inflate the apparent frequency of common, chronic, or multi-system 
            diseases (such as diabetes) that generate multiple related diagnostic codes per visit. Multiple visits by the same 
            patient were treated as separate events, as this may reflect greater symptom severity.</p>

            <p style='text-align: left'>&emsp;&emsp;For non-spatial analysis, billing visits were separated into categories
            of 'Pre-K' (ages 0–5), 'pediatric' (6–17 years), 'adult'
            (18–54 years), 'retirement age' (55–74 years), and 'geriatric'
            (75 years and older). Each age cohort was modeled independently.
            Because of the increased computational power required for spatial
            analysis, subjects were grouped as either adult (18 and over) or pediatric
            (under 18 years of age).</p>
            <br>
        </div>
        
        <img style='width: 90%; margin-bottom: -20px; height: 45px' src='Paddle divider.png'>
        "),
        
        # Modeling
        HTML("
        <div style='margin-top: 20px;'>
            <h4 id='Identification_of_pollution_exposures_and_modeling' style='color: black'>Identification of Pollution Exposures and Modeling</h4>
            <p style='text-align: left'>&emsp;&emsp;Air pollution exposure was derived from the EPA databases <a href='https://www.epa.gov/rsei' target='_blank'>
            Risk-Screening Environmental Indicators (RSEI)</a> and <a href='https://www.epa.gov/toxics-release-inventory-tri-program' target='_blank'>
            Toxics Release Inventory (TRI)</a>. Outdoor concentrations of O<sub>3</sub>, CO, SO<sub>2</sub>, NO
            <sub>2</sub>, PM<sub>10</sub>, and PM<sub>2.5</sub> were derived from the <a href='https://www.caces.us/' target='_blank'>
            Center for Air, Climate, &amp; Energy Solutions (CACES)</a> using their Land Use Regression (LUR) model, 
            with census tract-level data averaged for overlapping zip codes. Water pollution was separately evaluated
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
            </sup>. In brief, for each zip code in the AHRQ database, a 30-mile catchment area was defined around the 
            zip code centroid. The total amount of each pollutant released by facilities within that catchment area was 
            summed. summed. A Gaussian distance-weighting function was applied to account for the fact that pollutants 
disperse beyond the boundaries of the zip code in which an emitting facility is located, and nearby facilities 
should contribute more to a given zip code's estimated exposure than distant ones. Water pollution was 
            attributed only to the zip code of the EPA measurement device, as cross-zip dissemination patterns are not captured in the UCMR data.</p>

            <p style='text-align: left'>&emsp;&emsp;Two complementary modeling approaches were used to assess 
            associations between environmental toxicant exposures and disease diagnosis rates: a non-spatial 
            penalized regression and a spatial penalized regression. Both approaches were applied separately to 
            each of 5,984 disease diagnoses. The predictor matrix for air pollution models included 592 variables: 
            571 environmental exposures and 21 sociodemographic covariates (census age distributions, deprivation 
            index, population density, and for non-spatial models, latitude and longitude). The water pollution 
            model contained 42 environmental exposures alongside the same sociodemographic covariates. All predictors 
            were standardized prior to modeling to allow comparison across variables with different measurement scales.</p>

            <p style='text-align: left'>&emsp;&emsp;Non-spatial analysis was performed as previously described<sup>
            <a href='https://pubmed.ncbi.nlm.nih.gov/36608129/' target='_blank'>2</a>,
            <a href='https://pubmed.ncbi.nlm.nih.gov/37692200/' target='_blank'>3</a></sup>,
            using the glmnet package in R. For each disease-age combination, an elastic net regression model was 
            fit (alpha = 0.5) with the regularization parameter tuned via 10-fold cross-validation. Because 
            elastic net regression does not produce p-values, we filtered associations to those with beta-coefficients 
            more than two standard deviations (2SD) from the mean. Correlations more than 5SD from the mean are 
            displayed on the website to improve readability, but all correlations are available in the underlying data.</p>

            <p style='text-align: left'>&emsp;&emsp;For spatial modeling, a negative binomial generalized linear 
            mixed effects model was fit with nested spatial random effects, applied only to air pollution data 
            and the two collapsed age strata (pediatric and adult) due to computational demands. A four-level 
            nested spatial hierarchy was constructed using hierarchical clustering on distances between zip code 
            centroids, generating clusters of approximately 81, 27, 9, and 3 zip codes at each successive level. 
            These clusters were included as random effects to capture spatial autocorrelation at multiple geographic scales.</p>
            <br>
        </div>
        
        <img style='width: 90%; margin-bottom: -20px; height: 45px' src='Paddle divider.png'>
        "),
        
        # Additional Comparisons
        HTML("
        <div style='margin-top: 20px;'>
            <h4 id='Additional_comparisons' style='color: black'>Additional Comparisons</h4>
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

            <p style='text-align: left'>&emsp;&emsp;Protein-toxicant interactions were accessed from the 
            <a href='http://www.t3db.ca/' target='_blank'>Toxin-Target Database (T3DB)</a>. Protein-level 
            enrichment analysis was performed using Fisher's exact test to identify proteins disproportionately 
            targeted by disease-associated toxicants. Pathway enrichment analysis was performed using the 
            enrichR package, referencing GO Biological Process, GO Molecular Function, KEGG, and WikiPathways
            databases. Enriched proteins are indicated on the website where FDR-corrected p-values are less than 0.05.</p>
            <br>
        </div>
        
        <img style='width: 90%; margin-bottom: -20px; height: 45px' src='Paddle divider.png'>
        "),
        
        # Limitations
        HTML("
        <div style='margin-top: 20px;'>
            <h4 id='Limitations' style='color: black'>Limitations</h4>
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

            <p style='text-align: left'>&emsp;&emsp;The use of zip code-aggregated data introduces important caveats, including the
ecological fallacy (population-level associations may not reflect individual risk) and the possibility that a patient's
home zip code differs from where they work or are otherwise exposed during the day. Despite these limitations, aggregated data enables 
            nationwide analysis while protecting individual privacy, and our penalized regression 
            approach and spatial smoothing terms help mitigate some of these effects. This approach 
            is consistent with our goal of designing a hypothesis-generating tool to identify 
            associations warranting further investigation with individual-level data.</p>

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
            Notably, negative associations could theoretically represent a protective effect, but because 
            our clinical data derives from healthcare visits rather than individual diagnoses, it is more 
            likely that a given chemical generates other diseases that displace visits for certain 
            ailments, similar to how areas with the highest rates of COVID-19 saw a drop in outpatient 
            visits for non-COVID conditions.<sup><a href='https://pubmed.ncbi.nlm.nih.gov/36893413/'
            target='_blank'>4</a></sup>
            Both negative and positive associations should spur mechanistic
            follow-up studies rather than be assumed to reflect causal relationships.</p>
            </div>
            
            </div>
          "
       ),
                                    
                                    
       # Downloads Section --------------------------------------------------                         
        HTML("
          <div>
            <h3 id='download_data' class='syncopate-bold' style='font-size: 50px; color: white; margin-top: 20px'>Download Data</h3>
          </div>
          
          <div style='background-color: #00425A; width: 100%; height: 20px;'></div>
          
          <div style='background-color: white; color: black; padding: 10px; margin-bottom: 0px'>
            <div class='col-sm-6' style='padding: 15px;'>
              <h4 style='color: #026285;'>Adult Non-Spatial Odds Ratios</h4>
              <p style='font-size: 14px;'>Download the full table of adult non-spatial elastic net regression coefficients across all diagnoses and pollutants.</p>

        "),
                                    
        downloadButton("downloadNonspatialad", "Download", class = "download-btn"),
                                    
        HTML("
            </div>
            <div class='col-sm-6' style='padding: 15px;'>
              <h4 style='color: #026285;'>Adult Spatial Odds Ratios</h4>
              <p style='font-size: 14px;'>Download the full table of adult spatial mixed-effects model coefficients across all diagnoses and pollutants.</p>
        
        "),
                                    
        downloadButton("downloadSpatialad", "Download", class = "download-btn"),
                                    
        HTML("
            </div>
            </div>
            <div style='background-color: white; color: black; padding: 10px; margin-bottom: 0px'>
            <div class='col-sm-6' style='padding: 15px;'>
              <h4 style='color: #026285;'>Pediatric Non-Spatial Odds Ratios</h4>
              <p style='font-size: 14px;'>Download the full table of pediatric non-spatial elastic net regression coefficients across all diagnoses and pollutants.</p>
        
        "),
                                    
        downloadButton("downloadNonspatial", "Download", class = "download-btn"),
                                    
        HTML("
            </div>
            <div class='col-sm-6' style='padding: 15px;'>
              <h4 style='color: #026285;'>Pediatric Spatial Odds Ratios</h4>
              <p style='font-size: 14px;'>Download the full table of pediatric spatial mixed-effects model coefficients across all diagnoses and pollutants.</p>
        
        "),
                                    
        downloadButton("downloadSpatial", "Download", class = "download-btn"),
       
       HTML("</div></div>"), 
       
       
       
      # Additional Links Section --------------------------------------------------             
      HTML("
        <div>
        <h3 id='additional_links' class='syncopate-bold' style='font-size: 50px; color: white; margin-top: 60px'>Additional Links</h3>
        </div>
        
        <div style='background-color: #00425A; width: 100%;  height: 20px; margin-top: 0px;'></div>
        <div style='background-color: white; color: black; padding: 10px 40px 10px 40px; margin-top: 0px; margin-bottom: 40px'>
        
        <div style='margin-top: 5px;'>
         <h4 id='epa_where_you_live', style='color: black'>EPA Where You Live</h4>
         <p><a href = 'https://www.epa.gov/trinationalanalysis/where-you-live'>https://www.epa.gov/trinationalanalysis/where-you-live</a></p>
         <p style='text-align: left'>This site reports the specific chemicals released by factories, 
                   and the amounts released.  Search for your state or zip code to 
                   see the sources of industrial pollution in your area.  Note, 
                   this site does not collect data on road/automobile exhaust.  
                   Using this site would allow one to assess the pollutants in 
                   their area, which can then be searched in PADDLE for disease 
                   associations of concern.</p>
        </div>
        
        <div style='margin-top: 20px;'>
         <h4 id='toxics_tracker' style='color: black'>EPA TRI Toxics Tracker</h4>
         <p><a href = 'https://edap.epa.gov/public/extensions/TRIToxicsTracker/TRIToxicsTracker.html'>https://edap.epa.gov/public/extensions/TRIToxicsTracker/TRIToxicsTracker.html</a></p>
         <p style='text-align: left'>This site reports the exact locations for factories releasing 
                   toxic substances.  Search by address, state, or zip code to see 
                   the facilities in your area.  Note, this site does not collect
                   data on road/automobile exhaust.</p>
        </div>
        
        </div>
       "),
      
      # Citation --------------------------------------------------------
      HTML("
        <h3 id='citation' class='syncopate-bold' style='font-size: 50px; color: white; margin-top: 20px'>Cite PADDLE</h3>
        <div style='background-color: #00425A; width: 100%; margin-top: 0px; height: 20px'>
        </div>
        <div style='background-color: white; color: black; width: 100%; padding: 10px 200px 10px 200px; margin-top: 0px;'>
         <h4 style='color: black'></h4>
         <p>Grace Ratley, Aditi Vijendra, Jalin Jordan, Pranav Thota, Jordan Zeldin, 
         Prem Prashant Chaudhary, Ian A Myles. <a href='https://doi.org/10.1038/s41598-026-39836-2' target='_blank'><i>P.A.D.D.L.E.: a hypothesis generation tool for 
         assessing pollution’s potential role in disease.</i></a> Scientific Reports. 16, 8808 (2026). </p>
        </div>
        "),
       
      # About Us Section -----------------------------------------------------
      
      HTML("
      <div>
        <h3 id='about_us' class='syncopate-bold' style='font-size: 50px; color: white; margin-top: 40px'>About Us</h3>
        </div>
        
        <div style='background-color: #00425A; width: 100%;  height: 20px; margin-top: 0px;'></div>
        <div style='background-color: white; color: black; padding: 10px 40px 10px 40px; margin-top: 0px; margin-bottom: 40px; width: 100%;'>
          "),
      

      
      # Top row
      HTML("
          <div style='margin-top: 20px;'>
            <div style='margin-bottom: 20px; display: flex; justify-content: center; align-items: flex-start;'>
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;grace-about&apos;)'>
                  <img class='icon-filter' src='Headshots/Grace_Ratley.jpg'>
                </div>
                <p class='about-name'>Grace Ratley<span style='font-size: 12px; color: #15aee5'>  BSPH</span></p>
              </div>
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;aditi-about&apos;)'>
                  <img class='icon-filter' src='Headshots/Aditi_Vijendra.jpg'>
                </div>
                <p class='about-name'>Aditi Vijendra<span style='font-size: 12px; color: #15aee5'>  BA</span></p>
              </div>
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;jalin-about&apos;)'>
                  <img class='icon-filter' src='Headshots/Jalin_Jordan.jpg'>
                </div>
                <p class='about-name'>Jalin Jordan<span style='font-size: 12px; color: #15aee5'> MD</span></p>
              </div>
              
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;pranav-about&apos;)'>
                <img class='icon-filter' src='Headshots/Pranav_Thota.jpg'>
                </div>
                <p class='about-name'>Pranav Thota<span style='font-size: 12px; color: #15aee5'>  BS</span></p>
              </div>
              
            </div>
            
            <div id='grace-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Grace is a PhD candidate 
                originally from Niceville, Florida. She earned a Bachelor of Science in Public Health 
                in nutrition from the University of North Carolina at Chapel Hill. 
                During her undergraduate studies, she served as Operations Manager for The Bioinformatics 
                CRO, a remote computational biology company, and following graduation, spent a year working and 
                traveling across 17 countries. In 2022, Grace joined the Epithelial Therapeutics Unit (ETU) at 
                the National Institute of Allergy and Infectious Diseases as a postbaccalaureate fellow, 
                where she developed an interest in environmental medicine. Through a graduate partnership program, 
                she now splits her time between the Karolinska Institutet in Stockholm, Sweden, and the 
                ETU Lab in Bethesda, Maryland. Her doctoral research focuses on the 
                environmental drivers of allergic diseases, employing a range of bioinformatics and 
                epidemiological methods.</p>
            </div>
            <div id='aditi-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Aditi grew up in Toledo, Ohio and received her Bachelor 
                of Arts in Public Policy at the University of Michigan. After graduating, she completed a postbaccalaureate
                fellowship in Dr. Ian Myles’s lab at the National Institutes of Allergy and Infectious Disease. She is now 
                an MD/MPH student in the Miller School of Medicine at the University of Miami with a strong interest in 
                addressing disparities in the burden of chronic inflammatory disease.</p>
            </div>
            <div id='jalin-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Dr. Jordan, is originally from Michigan and was raised 
                in the Metro Detroit area. He completed medical school at the Perelman School of Medicine at the University 
                of Pennsylvania. He will begin residency training as a preliminary internal medicine resident at Howard 
                University Hospital before continuing as a dermatology resident at Howard University. As a medical student, 
                he trained in the laboratory of Dr. Ian Myles through the NIH Medical Research Scholars Program. His work 
                has included projects involving hidradenitis suppurativa, atopy, and pollution-related disease associations. 
                He will ultimately continue training as a clinical-scientist with a research focus on inflammatory skin diseases.</p>
            </div>
            <div id='pranav-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>
                Pranav grew up in Edison, New Jersey and earned his Bachelor of Science in Biology at George Washington University. 
                Following his undergraduate studies, he began his medical education at the George Washington University School of 
                Medicine and Health Sciences, where he is currently an MD candidate. He is a member of the Clinical and Translational 
                Research (CTR) track at GW, with a strong interest in utilizing large-scale healthcare databases to investigate 
                surgical outcomes and patient recovery within orthopaedic trauma.</p>
            </div>"),
       

      
      # Bottom Row
      HTML("
            <div style='margin-top: 20px; display: flex; justify-content: center; align-items: flex-start; margin-bottom: 10px'>
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;jordan-about&apos;)'>
                <img class='icon-filter' src='Headshots/Jordan_Zeldin.jpg'>
                </div>
                <p class='about-name'>Jordan Zeldin<span style='font-size: 12px; color: #15aee5'> MD</span></p>
              </div>
      
              <div style='width: 25%; text-align: center;'>
                <div class='image-cropper' onclick='toggleAbout(this, &apos;prem-about&apos;)'>
                  <img class='icon-filter' src='Headshots/Prem_Prashant_Chaudhary.jpg'>
                </div>
                <p class='about-name'>Prem Prashant Chaudhary<span style='font-size: 12px; color: #15aee5'> PhD</span></p>
              </div>
              
              <div style='width: 25%; text-align: center;'>
              <div class='image-cropper' onclick='toggleAbout(this, &apos;ian-about&apos;)'>
                <img class='icon-filter' src='Headshots/Ian_Myles.png'>
              </div>
              <p class='about-name'>Ian A. Myles<span style='font-size: 12px; color: #15aee5'>  MD MPH</span></p>
              </div>
            </div>
            
            
            <div id='jordan-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Dr. Zeldin is an internal medicine resident 
                at NYU Brooklyn with a background in epidemiology and environmental health. He studied interdisciplinary 
                medicine and mathematics at University of Florida, completed medical school at University of Florida, earned 
                a masters in medical anthropology via scholarship at University of Oxford, and was part of the Medical Research
                Scholars Program at the National Institutes of Health. He is interested in leveraging large-scale environmental
                and health datasets with mechanistic studies to understand the etiologies of diseases classically considered
                idiopathic.</p>
            </div>
            <div id='prem-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Dr. Chaudhary is a Staff Scientist in the Epithelial 
                Therapeutics Unit at the National Institute of Allergy and Infectious Diseases (NIAID). He earned 
                his PhD in Biotechnology from India and has over a decade of international research experience 
                spanning the United States, Europe, and Asia. 
                Dr. Chaudhary’s research focuses on understanding the complex interactions between the microbiome, 
                metabolome, and environmental factors in human health and disease, with a particular emphasis on 
                skin biology and atopic dermatitis. He applies systems biology and multi-omics integration approaches 
                to uncover disease mechanisms, identify biomarkers, and develop translational therapeutic strategies. 
                He has extensive expertise in microbiome and metabolomics data analysis, including next-generation 
                sequencing, machine learning, network biology, and high-performance computing. His work integrates 
                diverse datasets such as 16S, shotgun metagenomics, metatranscriptomics, and LC-MS/GC-MS metabolomics 
                to study host–microbe–environment interactions. Dr. Chaudhary has authored more than 60 peer-reviewed 
                publications and contributed to multiple interdisciplinary projects across institutions including 
                The Ohio State University, Nanyang Technological University, and Université d’Auvergne. His work has 
                advanced understanding of microbiome-driven disease processes and supports precision medicine approaches. 
                In addition to his research, he develops open-source computational tools for multi-omics integration and
                actively mentors trainees in computational biology and microbiome research. His work continues to bridge 
                fundamental biology with clinical applications, contributing to innovative strategies for managing 
                inflammatory and microbiome-associated diseases. </p>
            </div>
            <div id='ian-about' class='about-hidden'>
                <p style='padding: 20px 40px 20px 40px; margin-bottom: 0px'>Dr. Myles grew up in Colorado.  After completing medical school at the Univ of Colorado, he trained in 
                internal medicine at The Ohio State Univ. Medical Center.  He then began training in allergy and clinical 
                immunology at the National Institutes of Allergy and Infectious Diseases, part of the National Institutes 
                of Health.  He has worked as a researcher for over 15 years, investigating how environmental factors impact 
                allergic disease.  He has authored more than 80 peer-reviewed publications on eczema, allergies, and 
                topical steroid withdrawal.  Dr. Myles has also authored a book, GATTACA Has Fallen, which is about the 
                harms of researchers looking for the “gene for” common diseases like eczema at the expense of researching 
                environmental causes.  His research lab has partnered with numerous patient advocacy groups over the years. 
                Overall, his work has educated the public on the environmental causes of allergic disease and produced the 
                first topical probiotic targeted for eczema treatment.  He continues to serve as the chief of The Epithelial 
                Therapeutics Unit and as a medical officer in the United States Public Health Service.</p>
            </div>

          </div>
        </div>
        "),
      
      # Retired Logo -----------------------------------------------------------------
        HTML("
        <div style='padding: 10px 40px 10px 40px; margin-top: 20px; margin-bottom: 40px'>
        <div class='col-sm-8'>
          <p class='syncopate-bold' style='font-size: 30px; margin-bottom: 0px'>Retired logo</p>
        </div>
        <div class='col-sm-2'>
        <img height='50px' src='White no background paddle logo.png'></img>
        
        </div>
        </div>
        
        "),
      
      ),
    ),
  ),
  
  # Search chemicals ----
  tabPanel(
    "Search Chemicals",
    sidebarLayout(
      
      # Sidebar
      sidebarPanel(
        id    = "searchByChem",
        class = "paddle-sidebar",
        div(class = "sidebar-header",
            h3("Search Chemicals", class = "sidebar-title")
        ),
        
        hr(class = "sidebar-divider"),
        
        # ── Pollution Source ────────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Pollution Source",
                   class = "input-label",
                   # Inline tooltip trigger
                   tags$span(
                     class = "tooltip-trigger",
                     `data-tooltip` = "Air: 571 TRI/RSEI chemicals.\nWater: 42 UCMR drinking-water contaminants (non-spatial only).",
                     HTML("&#9432;")   # ⓘ
                   )
            ),
            div(class = "toggle-pill-group",
                tags$input(type = "radio", name = "pollutionSource_chem", id = "ps_air",
                           value = "Air", checked = NA,
                           class = "toggle-pill-input", `data-target` = "pollutionSource_chem"),
                tags$label(`for` = "ps_air",   class = "toggle-pill", "Air"),
                tags$input(type = "radio", name = "pollutionSource_chem", id = "ps_water",
                           value = "Water",
                           class = "toggle-pill-input", `data-target` = "pollutionSource_chem"),
                tags$label(`for` = "ps_water", class = "toggle-pill", "Water")
            ),
            # Hidden Shiny binding — keep the actual selectInput for server reactivity
            # but visually replace it with the pill toggle above via JS
            div(style = "display:none;",
                selectInput("pollutionSource_chem", NULL,
                            choices  = c("Air", "Water - non-spatial only" = "Water"),
                            selected = "Air"
                )
            )
        ),
        
        # ── Chemical of Interest ────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Chemical of Interest", class = "input-label"),
            selectizeInput("searchChemical_chem", NULL, choices = NULL)
        ),
        
        # ── Model Type ──────────────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Model Type",
                       class = "input-label",
                       tags$span(
                         class = "tooltip-trigger",
                         `data-tooltip` = "Spatial: negative-binomial GLMM with nested geographic random effects (air only).\nNon-spatial: elastic net regression with lat/lon as covariates.",
                         HTML("&#9432;")
                       )
            ),
            selectInput("dataSource_chem", NULL,
                        choices  = c("Spatial" = "spatial", "Non-Spatial" = "non_spatial"),
                        selected = "spatial"
            )
        ),
        
        # ── Age Group ───────────────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Age Group", class = "input-label"),
            selectInput("ageGroup_chem", NULL, choices = NULL)
        ),
        
        hr(class = "sidebar-divider"),
        
        # ── Chemical Properties card ────────────────────────────────────────
        div(class = "chem-props-card",
            div(class = "chem-props-title", "Chemical Profile"),
            div(class = "chem-props-grid",
                
                div(class = "prop-item",
                    div(class = "prop-label",
                        "Class",
                        tags$span(
                          class = "tooltip-trigger",
                          `data-tooltip` = "Broad chemical family (e.g. Heavy Metal, PAH, Phthalate).",
                          HTML("&#9432;")
                        )
                    ),
                    div(class = "prop-value", textOutput("chem_class_chem", inline = TRUE))
                ),
                
                div(class = "prop-item",
                    div(class = "prop-label",
                        "Carcinogen",
                        tags$span(
                          class = "tooltip-trigger",
                          `data-tooltip` = "Whether the chemical is classified as a known or probable carcinogen.",
                          HTML("&#9432;")
                        )
                    ),
                    uiOutput("carcinogen_badge_chem")   # rendered as a coloured badge (see server note)
                ),
                
                div(class = "prop-item prop-item--wide",
                    div(class = "prop-label",
                        "Organ Toxicity",
                        tags$span(
                          class = "tooltip-trigger",
                          `data-tooltip` = "Organ systems with documented toxicity for this chemical.",
                          HTML("&#9432;")
                        )
                    ),
                    div(class = "prop-value", textOutput("organ_tox_chem", inline = TRUE))
                ),
                
                div(class = "prop-item prop-item--wide",
                    div(class = "prop-label",
                        "Toxicity Timing",
                        tags$span(
                          class = "tooltip-trigger",
                          `data-tooltip` = "Whether toxicity is primarily acute, chronic, or both.",
                          HTML("&#9432;")
                        )
                    ),
                    div(class = "prop-value", textOutput("tox_timing_chem", inline = TRUE))
                )
            )
        )
        
      ), # /sidebarPanel
      
      
      # ── MAIN PANEL ──────────────────────────────────────────────────────────
      mainPanel(
        align = "center",
        class = "paddle-main",
        
        # ── Currently viewing banner ─────────────────────────────────────────
        div(class = "viewing-banner",
            htmlOutput("currentlyViewing_chem"),
        ),
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 1: Forest Plot
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header",
                div(class = "section-num", "01"),
                div(class = "section-title-block",
                    h3("Top 15 Disease Associations", class = "section-title"),
                    p("Ranked by log-odds from the selected model.", class = "section-subtitle")
                ),
                # Collapsible legend toggle
                tags$button(
                  id    = "legend_toggle",
                  class = "legend-toggle-btn",
                  onclick = "toggleLegend('chart-legend')",
                  HTML("&#9432; How to read this chart")
                )
            ),
            
            # Collapsible legend
            div(id = "chart-legend", class = "chart-legend collapsed",
                div(class = "legend-note",
                    HTML("<p>The graph below displays the 15 diagnoses with the strongest positive 
                         associations with your selected chemical, based on the absolute 
                         value of the beta coefficient from a logistic elastic net 
                         model. The odds ratios were calculated by exponentiating the
                         beta coefficients, representing the change in odds of having 
                         the diagnosis for each unit increase in the chemical of 
                         interest.</p>")
                ),
                
                br(),
                
                div(class = "legend-grid",
                    p("Legend"),
                    div(class = "legend-item",
                        div(class = "legend-swatch swatch-red-dot"), "Chemical's odds ratio"
                    ),
                    div(class = "legend-item",
                        div(class = "legend-swatch swatch-black-line"), "Range of odds across all chemicals within disease"
                    ),
                    div(class = "legend-item",
                        div(class = "legend-swatch swatch-x"), "Mean odds ratio for disease"
                    ),
                    div(class = "legend-item",
                        div(class = "legend-swatch swatch-grey-band"), "± 1 SD"
                    ),
                    div(class = "legend-item",
                        div(class = "legend-swatch swatch-dashed"), "No association (OR = 1)"
                    )
                ),
                
                div(class = "legend-note",
                    HTML("If the red dot sits far right of the grey band, this chemical is one of 
                    the strongest contributors to that disease. Odds ratios below 1 most likely reflect 
                    <em>visit displacement</em> (this chemical drives other diagnoses that crowd out this one) 
                    rather than a protective effect.")
                )
            ), # / End of How to read this chart
            div(class = "collapsible-body",
              div(class = "plot-container",
                  plotlyOutput("viewPlots_chem") %>% withSpinner(color = "transparent", type = 6),
                  
                  div(class = "legend-note",
                      HTML("<p style='text-align: left;'><b>Note: </b>If a diagnosis is presented, then the chemical interactions with that diagnosis are potentially important. Any association presented should be evaluated for molecular or epidemiologic connections beyond this analysis alone.</p>")
                  )
              ),
            ),

        ), # /section 01
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 2: Full data table  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('table-section')",
                div(class = "section-num", "02"),
                div(class = "section-title-block",
                    h3("All Disease Associations", class = "section-title"),
                    p("Table of all diseases associated with selected chemical.", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "table-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "table-section", class = "collapsible-body",
                div(class = "legend-note",
                    HTML("The <b>Total Predictors</b> column shows how many chemicals were associated with a change 
                       in risk in each diagnosis. The <b>Mean, Standard Deviation, Max,</b> and <b>Min</b> columns show 
                       summary statistics for the chemicals associated with each diagnosis, so you can guage 
                       the relative importance of the chemical in moderating diagnosis risk.")
                ),
                br(),
                DT::dataTableOutput("viewTable_chem"),
                br(),
                downloadButton("download_diseases_chem", "Download CSV",
                               class = "download-btn", style="margin-top: 20px;")
            )
            
        ), # /section 02
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 3: US Map  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('map-section')",
                div(class = "section-num", "03"),
                div(class = "section-title-block",
                    h3("US Exposure Distribution", class = "section-title"),
                    p("County-level chemical release.", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "map-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "map-section", class = "collapsible-body",
                
                div(class = "legend-note",
                    HTML("Results are min-max scaled (The county with the highest release is 1). Click <b>Generate Map</b> to load (may take ~1 min).")
                ),
                
                div(class = "map-warning",
                    HTML("&#9888; Generating this map reads large CSV files and may temporarily 
            slow other sections of the app.")
                ),
                div(style="margin-bottom: 10px;",
                    actionBttn("generate_map_chem",
                               label = HTML("Generate Map<br/><span style='font-size:12px; font-weight:400;'>Re-click after changing inputs to refresh</span>"),
                               style = "fill", color = "primary", size = "sm"),
                ),
                plotlyOutput("US_map_chem")  %>% withSpinner(color = "transparent", type = 6),
                
            )
            
        ), # /section 03
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 4: At-Risk Groups  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('risk-section')",
                div(class = "section-num", "04"),
                div(class = "section-title-block",
                    h3("At-Risk Groups", class = "section-title"),
                    p("Associations with deprivation index, ethnicity & historic redlining (where present).", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "risk-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "risk-section", class = "collapsible-body",
                div(class = "legend-note",
                    HTML("Log odds > 0 (Red) indicate the chemical is more prevalent in zip codes
                    with a higher proportion of that group or greater deprivation/redlining.
                    Log odds < 0 (blue) suggest the inverse.
                    Only categories with a meaningful association are shown.")
                ),
                br(),
                
                uiOutput("at_risk_combined_chem")
            )
            
        ), # /section 04
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 5: Products  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('products-section')",
                div(class = "section-num", "05"),
                div(class = "section-title-block",
                    h3("Products Containing This Chemical", class = "section-title"),
                    p("Source: EPA ChemExpo database. Absence of results does not rule out presence in other products.", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "products-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "products-section", class = "collapsible-body",
                div(class = "center-container",
                    column(10,
                           DT::dataTableOutput("product_table_chem")
                    )
                )
            )
            
        ) # /section 05
        
      ) # /mainPanel
    ) # /sidebarLayout
  ), # /tabPanel
  
  # Search by diagnosis ----
  tabPanel(
    "Search Diagnoses",
    # Make a page layout that contains a side panel for inputs  and a main panel for outputs
    
    sidebarLayout(
      
      sidebarPanel(
        id    = "searchByDisease",
        class = "paddle-sidebar",
        
        div(class = "sidebar-header",
            h3("Search Diagnoses", class = "sidebar-title")
        ),
        
        hr(class = "sidebar-divider"),
        
        # ── Pollution Source ─────────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Pollution Source",
                       class = "input-label",
                       tags$span(
                         class = "tooltip-trigger",
                         `data-tooltip` = "Air: 571 TRI/RSEI chemicals.\nWater: 42 UCMR drinking-water contaminants (non-spatial only).",
                         HTML("&#9432;")
                       )
            ),
            div(class = "toggle-pill-group",
                tags$input(type = "radio", name = "pollutionSource_disease", id = "ps_disease_air",
                           value = "Air", checked = NA,
                           class = "toggle-pill-input", `data-target` = "pollutionSource_disease"),
                tags$label(`for` = "ps_disease_air",   class = "toggle-pill", "Air"),
                tags$input(type = "radio", name = "pollutionSource_disease", id = "ps_disease_water",
                           value = "Water",
                           class = "toggle-pill-input", `data-target` = "pollutionSource_disease"),
                tags$label(`for` = "ps_disease_water", class = "toggle-pill", "Water")
            ),
            div(style = "display:none;",
                selectInput("pollutionSource_disease", NULL,
                            choices  = c("Air", "Water - non-spatial only" = "Water"),
                            selected = "Air"
                )
            )
        ),
        
        # ── Disease of Interest ──────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("Disease of Interest", class = "input-label"),
            selectizeInput(
              "searchDisease_disease", NULL,
              choices = NULL,
              selected = NULL,
              options = list(
                placeholder = 'Start typing or select disease from dropdown',
                onInitialize = I('function() { this.clear(); }')
              )
            )
        ),
        
        # ── Combined / Stratified ────────────────────────────────────────────
        div(class = "input-group-paddle",
            tags$label("View Data",
                       class = "input-label",
                       tags$span(
                         class = "tooltip-trigger",
                         `data-tooltip` = "Combined: single model pooling all ages.\nStratified: separate models by age group and model type.",
                         HTML("&#9432;")
                       )
            ),
            div(class = "toggle-pill-group",
                tags$input(type = "radio", name = "comb_or_strat_disease", id = "cs_combined",
                           value = "Combined", checked = NA,
                           class = "toggle-pill-input", `data-target` = "comb_or_strat_disease"),
                tags$label(`for` = "cs_combined",  class = "toggle-pill", "Combined"),
                tags$input(type = "radio", name = "comb_or_strat_disease", id = "cs_stratified",
                           value = "Stratified",
                           class = "toggle-pill-input", `data-target` = "comb_or_strat_disease"),
                tags$label(`for` = "cs_stratified", class = "toggle-pill", "Stratified")
            ),
            div(style = "display:none;",
                selectInput("comb_or_strat_disease", NULL,
                            choices  = c("Combined", "Stratified"),
                            selected = "Combined"
                )
            )
        ),
        
        uiOutput("strat_select_input_disease"),
        
        
      ),
      
      mainPanel(
        align = "center",
        class = "paddle-main",
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 1: Chemical Associations (combined or stratified)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header",
                div(class = "section-num", "01"),
                div(class = "section-title-block",
                    h3("Chemical Associations", class = "section-title"),
                    p("Chemicals associated with the selected diagnosis.", class = "section-subtitle")
                ),
                tags$button(
                  id    = "disease_legend_toggle",
                  class = "legend-toggle-btn",
                  onclick = "toggleLegend('disease-chart-legend')",
                  HTML("&#9432; How to read this chart")
                )
            ),
            
            # Collapsible legend
            div(id = "disease-chart-legend", class = "chart-legend collapsed",
                div(class = "legend-note",
                    HTML("<p>If a chemical is presented, then the interactions with the disease you 
                         selected is potentially important. If the red dot sits far right of the grey 
                         band, this chemical is one of the strongest contributors to that disease.
                         Odds ratios below 1 most likely reflect visit displacement (this chemical 
                         drives other diagnoses that crowd out this one) rather than a protective effect.
                         </p><p>For example, a chemical which triggered asthma would drive more people with asthma to 
                         see their health care provider; if enough people were being seen for 
                         asthma, it may leave less clinic appointments for people with other lung 
                         diseases. Thus, any association presented should be evaluated for 
                         molecular or epidemiologic connections beyond this analysis alone.</p>")
                )
            ),
            
            div(class = "viewing-banner",
                htmlOutput("currentlyViewing_disease"),
            ),
            
            div(class = "plot-container",
                uiOutput("comb_or_strat_disease_out")
            )
            
        ), # /section 01
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 2: Distribution Across the US  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('disease-map-section')",
                div(class = "section-num", "02"),
                div(class = "section-title-block",
                    h3("Distribution Across the US", class = "section-title"),
                    p("County-level disease visit rates.", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "disease-map-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "disease-map-section", class = "collapsible-body",
                
                div(class = "map-warning",
                    HTML("&#9888; Generating this map reads large files and may temporarily slow other sections of the app.")
                ),
                div(class = "legend-note",
                    HTML("Note: Rates are derived from the patient's home zip code.")
                ),
                div(style="margin-bottom: 10px;",
                    actionBttn("generate_map_disease",
                               label = HTML("Generate Map<br/><span style='font-size:12px; font-weight:400;'>Re-click after changing inputs to refresh</span>"),
                               style = "fill", color = "primary", size = "sm")
                ),
                plotlyOutput("US_map_disease") %>% withSpinner(color = "transparent", type = 6),
                br(),
                
                div(class = "legend-note", 
                    h3(class='section-title', "Top 10 Counties in Each Age Group"),
                    HTML("The following counties had the highest clinical visit rates for the selected diesease for each age group. If an age group does not appear in the table, that means no visits were recorded for the selected diagnosis in that age group.")
                ),
                DT::dataTableOutput("top_10_disease"),
                br()
            )
            
        ), # /section 02
        
        # ════════════════════════════════════════════════════════════════════
        # SECTION 3: Chemical Class Summary  (collapsible)
        # ════════════════════════════════════════════════════════════════════
        div(class = "content-section",
            
            div(class = "section-header collapsible-header",
                onclick = "toggleSection('disease-class-section')",
                div(class = "section-num", "03"),
                div(class = "section-title-block",
                    h3("Chemical Class Summary", class = "section-title"),
                    p("Breakdown of associated chemical classes.", class = "section-subtitle")
                ),
                tags$span(class = "collapse-arrow", id = "disease-class-section-arrow", HTML("&#9660;"))
            ),
            
            div(id = "disease-class-section", class = "collapsible-body",
                div(class = "legend-note",
                    HTML("The donut chart shows the chemical classes associated with this diagnosis. The table lists classes from most to least common with the chemicals within each class.
                         <br><b>Note:</b> Classes overlap: Phthalates are also Endocrine Disruptors, and BTEX are also Volatile Organic Compounds. Hover over chart or table for details.")
                ),
                br(),
                fluidRow(
                  column(5, plotlyOutput("chem_class_pie_disease")),
                  column(7,
                         DT::dataTableOutput("chem_class_count_disease"),
                         br(),
                  ),
                ),
                downloadButton("download_chem_class_count", "Download CSV",
                               class = "download-btn")
            )
            
        ) # /section 03
        
      ) # /mainPanel
    ), # /sidebarLayout
    
    # ════════════════════════════════════════════════════════════════════
    # SECTION 4: Pathway Enrichment  (collapsible) - FULL WIDTH
    # ════════════════════════════════════════════════════════════════════
    fluidRow(
      align = "center",
      column(12,
             div(class = "content-section",
                 
                 div(class = "section-header collapsible-header",
                     onclick = "toggleSection('disease-pathway-section')",
                     div(class = "section-num", "04"),
                     div(class = "section-title-block",
                         h3("Pathway Enrichment", class = "section-title"),
                         p("Biological pathways implicated by associated chemicals.", class = "section-subtitle")
                     ),
                     tags$span(class = "collapse-arrow", id = "disease-pathway-section-arrow", HTML("&#9660;"))
                 ),
                 
                 div(id = "disease-pathway-section", class = "collapsible-body",
                     uiOutput("pathway_enrichment_disease")
                 )
                 
             ) # /section 04
      )
    )
  ), 
  
  # Social determinants of health ----
  tabPanel("Determinants of Health",
           
           sidebarLayout(
             
             sidebarPanel(
               id    = "determinantsPanel",
               class = "paddle-sidebar",
               
               div(class = "sidebar-header",
                   h3("Search Determinants", class = "sidebar-title")
               ),
               
               hr(class = "sidebar-divider"),
               
               # ── Pollution Source ──────────────────────────────────────────
               div(class = "input-group-paddle",
                   tags$label("Pollution Source", class = "input-label"),
                   div(class = "toggle-pill-group",
                       tags$input(type = "radio", name = "pollutionSource_determinant",
                                  id = "pd_air", value = "Air", checked = NA,
                                  class = "toggle-pill-input", `data-target` = "pollutionSource_determinant"),
                       tags$label(`for` = "pd_air",   class = "toggle-pill", "Air"),
                       tags$input(type = "radio", name = "pollutionSource_determinant",
                                  id = "pd_water", value = "Water",
                                  class = "toggle-pill-input", `data-target` = "pollutionSource_determinant"),
                       tags$label(`for` = "pd_water", class = "toggle-pill", "Water")
                   ),
                   div(style = "display:none;",
                       selectInput("pollutionSource_determinant", NULL,
                                   choices  = c("Air", "Water - non-spatial only" = "Water"),
                                   selected = "Air")
                   )
               ),
               
               # ── Model Type ────────────────────────────────────────────────
               div(class = "input-group-paddle",
                   tags$label("Model Type", class = "input-label"),
                   div(class = "toggle-pill-group",
                       tags$input(type = "radio", name = "dataSource_determinant",
                                  id = "dt_nonspatial", value = "non_spatial", checked = NA,
                                  class = "toggle-pill-input", `data-target` = "dataSource_determinant"),
                       tags$label(`for` = "dt_nonspatial", class = "toggle-pill", "Non-spatial"),
                       tags$input(type = "radio", name = "dataSource_determinant",
                                  id = "dt_spatial",    value = "spatial",
                                  class = "toggle-pill-input", `data-target` = "dataSource_determinant"),
                       tags$label(`for` = "dt_spatial",    class = "toggle-pill", "Spatial")
                   ),
                   div(style = "display:none;",
                       selectInput("dataSource_determinant", NULL,
                                   choices  = c("Non-spatial" = "non_spatial", "Spatial" = "spatial"),
                                   selected = "non_spatial")
                   )
               ),
               
               # ── Determinant ───────────────────────────────────────────────
               div(class = "input-group-paddle",
                   tags$label("Determinant", class = "input-label"),
                   div(class = "toggle-pill-group",
                       tags$input(type = "radio", name = "determinant_tab",
                                  id = "dt_ethnicity",  value = "Ethnicity", checked = NA,
                                  class = "toggle-pill-input", `data-target` = "determinant_tab"),
                       tags$label(`for` = "dt_ethnicity",  class = "toggle-pill", "Ethnicity"),
                       tags$input(type = "radio", name = "determinant_tab",
                                  id = "dt_deprivation", value = "Deprivation",
                                  class = "toggle-pill-input", `data-target` = "determinant_tab"),
                       tags$label(`for` = "dt_deprivation", class = "toggle-pill", "Deprivation"),
                       tags$input(type = "radio", name = "determinant_tab",
                                  id = "dt_hrs",        value = "HRS",
                                  class = "toggle-pill-input", `data-target` = "determinant_tab"),
                       tags$label(`for` = "dt_hrs",        class = "toggle-pill", "Red Lining")
                   ),
                   div(style = "display:none;",
                       selectInput("determinant_tab", NULL,
                                   choices  = c("Ethnicity", "Deprivation", "HRS"),
                                   selected = "Ethnicity")
                   )
               ),
               
               # ── Ethnicity (shown only when Ethnicity selected) ────────────
               uiOutput("ethnicity_select_ui"),
               
             ),
             
             
             # Determinants Main Panel -------------------------------
             mainPanel(
               class = "paddle-main",
               
               div(class = "content-section",
                   
                   div(class = "section-header",
                       div(class = "section-num", "01"),
                       div(class = "section-title-block",
                           h3("Chemical Associations", class = "section-title"),
                           p("Chemicals whose exposures correlate with this determinant.", class = "section-subtitle")
                       )
                   ),
                   
                   
                   div(id = "det-about-section", class = "collapsible-body",
                       
                     div(class = "legend-note",
                         uiOutput("determinant_about_text")
                     ),
                   
                     div(class = "plot-container",
                         plotlyOutput("viewPlots_determinant_main")
                     ),
                     
                     br(),
                     
                     div(class = "center-container",
                       column(10,
                          DT::dataTableOutput("viewTable_determinant_main"),
                          br(),
                          downloadButton("downloadSDOH", "Download", class = "download-btn")
                       )
                     )
                   ),
                   
               ) # / Graph and table
             ) # /mainPanel
           ) # /sidebarLayout
  ),
  
)
