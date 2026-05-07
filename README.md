# PADDLE

**Pollution Associated Disease Diagnosis Likelihood Estimator**

An interactive, hypothesis-generation tool for exploring associations between environmental toxicant releases and disease diagnoses across the United States.

[**Launch the App**](https://ratleyg-paddle.share.connect.posit.cloud/) | [**Project Website**](https://ratleyge.github.io/PADDLE/) | [**Paper (Scientific Reports)**](https://doi.org/10.1038/s41598-026-39836-2)

---

## Repository Contents

| File | Description |
|---|---|
| `global.R` | Loads packages, application data (`Data/appData.RData`), and helper functions |
| `ui.R` | Shiny UI: navbar, methods, chemical/diagnosis search, determinants of health, summary data |
| `server.R` | Shiny server logic: reactive plots, tables, maps |
| `Data/` | Input matrices, pollutant exposures, and disease-mapping percentile data |

## Running PADDLE Locally

### Requirements

- R ≥ 4.3
- The following CRAN packages:

```r
install.packages(c(
  "shiny", "shinythemes", "shinyWidgets", "shinycssloaders",
  "tidyverse", "DT", "plotly", "enrichR"
))
```

### Launch

```r
shiny::runApp()
```

## Data Availability

All code and files required to run PADDLE including the analysis data presented in the manuscript are in this repository. Spreadsheets of toxicant exposures by disease/zip code are included.

Due to privacy protections, **absolute** visitation rates per disease per zip code are not publicly available; they can be obtained by direct application to AHRQ. **Relative** visit rates (percentiles) are available under `Data/Disease_mapping_data/`.

## Citation

If you use PADDLE in your work, please cite:

> Ratley G, Vijendra A, Jordan J, Thota P, Zeldin J, Chaudhary PP, Myles IA. **P.A.D.D.L.E.: a hypothesis generation tool for assessing pollution's potential role in disease.** *Scientific Reports* 16, 8808 (2026). https://doi.org/10.1038/s41598-026-39836-2
