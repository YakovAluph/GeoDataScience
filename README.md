# Variogram & Kriging Shiny App

## Project Description

This interactive Shiny web application allows users to upload geospatial datasets, explore and visualize **variograms**, perform **ordinary kriging**, and generate **interactive spatial maps** using `leaflet`. It is designed to assist researchers, geoscientists, and students in understanding spatial interpolation and geostatistics through an easy-to-use visual platform.

The app supports:
- Multiple variogram models (Spherical, Exponential, Gaussian, Matern)
- Real-time interactive mapping with leaflet
- Cross-validation with downloadable results
- Automatic jittering of duplicate coordinates
- Geospatial coordinate support (Latitude/Longitude or Easting/Northing)
- Kriging raster visualizations
- Downloadable outputs (CSV, PNG)

## Author Information

- **[Yakov Aluph](https://github.com/YakovAluph)** – *Senior Developer*  
- **[Professor Ashok Krishnamurthy](https://github.com/ashokkrish)** – *Supervisor*

## How to Use

1. Upload a `.csv` file with at least three columns: X, Y, and a value column (e.g., groundwater level).
2. Choose the coordinate type (Lat/Lon or Easting/Northing).
3. Select the variogram model.
4. Click "Run Analysis" to view plots and maps.
5. Use "Run Cross-Validation" for accuracy check.
6. Download results via buttons on the main panel.

## Technologies Used

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [gstat](https://cran.r-project.org/web/packages/gstat/)
- [leaflet](https://rstudio.github.io/leaflet/)
- [ggplot2](https://ggplot2.tidyverse.org/)
