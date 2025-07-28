# Chicago Crime Analysis, 2016–2020

This project presents a rigorous, data-driven analysis of crime in Chicago between 2016 and 2020. Open data is sourced from the City of Chicago Data Portal ([data.cityofchicago.org](https://data.cityofchicago.org/)), enabling transparent and reproducible research into both temporal and spatial patterns of criminal activity.

## Overview

Key elements of the analysis:

- **Time series decomposition and forecasting** for major crime categories
- **Continuous regression modeling** (ARIMA, SARIMA) for predicting incident counts
- **Logistic regression** to estimate probabilities of specific crime types
- **Spatial analysis** visualized via geo-maps of police districts
- **Thorough data cleaning**: missing value handling, ensuring stationarity, feature engineering
- **Robust model evaluation**: addressing class imbalance and low AUC
- **Automated, reproducible pipeline** for transparent results

## Project Structure
```
project-root/
│
├── report/                   # Analysis summaries, visualizations, documentation
├── src/                      # R scripts for modeling and data processing
│     ├── data_cleaning.R
│     ├── time_series_analysis.R
│     ├── arima_model.R
│     ├── sarima_model.R
│     ├── logistic_regression.R
│     └── geo_map_visualization.R
├── data/                     # (Not included) Reference for data sources
│     ├── 2016-2020-chicago-crime
│     └── PoliceDistrictDec2012_20250508
├── requirements.txt          # Required R packages
└── README.md
```

## Data Sources

All data originates from the [City of Chicago Data Portal](https://data.cityofchicago.org/):

- `2016-2020-chicago-crime`: Crime records (incidents, categories, results)
- `PoliceDistrictDec2012_20250508`: Police district boundaries for geo-mapping

## Technologies

- **R** — with tidyverse, forecast, caret, and spatial visualization libraries

## Purpose

This project demonstrates transparent, reproducible analysis for real-world public safety data. The repository is structured for clarity and ease of use, meeting standards for a data science portfolio.

> Data: [City of Chicago Data Portal](https://data.cityofchicago.org/)
