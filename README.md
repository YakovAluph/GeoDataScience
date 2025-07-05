# Variogram & Kriging Shiny App

## 📌 Project Description

This interactive Shiny web application allows users to upload geospatial datasets, explore and visualize **variograms**, perform **ordinary kriging**, and generate **interactive spatial maps** using `leaflet`. It is designed to assist researchers, geoscientists, and students in understanding spatial interpolation and geostatistics through an easy-to-use visual platform.

The app supports:
- Multiple variogram models (Spherical, Exponential, Gaussian, Matern)
- Real-time interactive mapping with leaflet
- Cross-validation with downloadable results (In file version 0.5 only until further notice)
- Automatic jittering of duplicate coordinates
- Geospatial coordinate support (Latitude/Longitude or Easting/Northing)
- Kriging raster visualizations
- Downloadable outputs (CSV, PNG)
- Deployable via [ShinyApps.io](https://www.shinyapps.io/) or locally on R Studio (recommended option for optimal RAM usage).

## 🔗 Author Information

- **[Yakov Aluph](https://github.com/YakovAluph)** – *Senior Developer*  
- **[Professor Ashok Krishnamurthy](https://github.com/ashokkrish)** – *Supervisor*

## 💻 How to Use

1. Upload a `.csv`, `.txt`, `xls`, or a `xlsx` file with at least three columns: X, Y, and a value column (e.g., groundwater level).
2. Choose the coordinate type (Lat/Lon or Easting/Northing).
3. Select the variogram model.
4. Click "Run Analysis" to view plots and maps.
5. Use "Run Cross-Validation" for accuracy check. (In file version 0.5 only until further notice)
6. Download results via buttons on the main panel.

## 🗃️ Files
- `README.md` – Project description and usage guide.

## 🗃️ Current App version **[0.8.6](https://github.com/YakovAluph/GeoDataScience/tree/main/App%20files/Version%200.8.6%207-4-2025%20(NEW))**
- `global.R` – Global variables and setup.
- `ui.R` – UI layout using `shiny`.
- `server.R` – Server logic: data wrangling, modeling, visualization.

## 🚀 Technologies Used

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [gstat](https://cran.r-project.org/web/packages/gstat/)
- [leaflet](https://rstudio.github.io/leaflet/)
- [ggplot2](https://ggplot2.tidyverse.org/)
