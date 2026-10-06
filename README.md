# Illinois-Soybean-Stem-Disease-Map
Interactive R Shiny application mapping the prevalence of fungal stem pathogens in soybean across Illinois counties, and relating it to climate.
**Live app:** https://dcoelho.shinyapps.io/Illinois_Soybean_STEM_Disease_MAP/

## What it does

- **County-level choropleth** of fungal pathogen prevalence across 45 Illinois
  counties, built with Leaflet
- **Reactive filters** by pathogen and by survey year
- **Summary panel**: counties with data, total samples, mean prevalence
- **Climate correlation module**: relates county mean temperature or
  precipitation to disease prevalence, with a fitted regression, confidence
  band, Pearson correlation, and point size scaled by sample number

## Built with

R · Shiny · leaflet · sf · dplyr · readr · ggplot2 · ggrepel

## Running locally

```r
install.packages(c("shiny", "leaflet", "sf", "dplyr", "readr", "ggplot2", "ggrepel"))
shiny::runApp("app.R")
```

## Data

Soybean stem disease survey data collected in Illinois as part of ongoing
Ph.D. research at Southern Illinois University Carbondale, paired with
county-level climate summaries.

## Author

**Danillo Leite** — Ph.D. Candidate, Plant Pathology
Southern Illinois University Carbondale
[LinkedIn](https://linkedin.com/in/danillo-leite)

## License

MIT
