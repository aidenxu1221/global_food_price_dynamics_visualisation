# Global Food Price Dynamics: Economic & Health Implications

An interactive narrative visualisation exploring global food price fluctuations and their impacts on macroeconomic indicators and human well-being across 180+ countries.

## Background
Rising food prices have become a global economic challenge, affecting household purchasing power, national economic stability, and population health. Understanding how food price inflation evolves across countries and how it interacts with economic and health indicators is critical for policy makers, economists, and industry stakeholders.

## Objectives
- Identify global patterns of food price inflation over time.
- Evaluate the relationship between food price fluctuations and macroeconomic indicators.
- Assess the potential impacts of food price changes on nutritional status and life satisfaction.

## Data Sources
- FAO Consumer Price Indices (2001–2023)  
- FAO Producer Prices & Production Indices  
- FAO Macro Statistics (GDP, PPI, GPI)  
- WHO World Health Statistics (nutrition & obesity)  
- World Happiness Report (life satisfaction)  
- Natural Earth spatial shape files  

(All datasets are publicly available and open-access.)

## Methods
- Extensive data wrangling and integration across 6 heterogeneous datasets.
- Preprocessing and feature engineering (growth rates, imputation, merging).
- Spatial data processing using country-level shapefiles.
- Interactive narrative visualisation built with **R Shiny**.
- Linked visual analytics with coordinated views across multiple charts.

## Key Features
- Interactive global choropleth map with temporal animation.
- Linked line charts for macroeconomic trend analysis.
- Radar charts for multi-dimensional health comparisons.
- Dynamic tooltips, zooming, filtering, and cross-chart interaction.
- One-click country selection updating all visual components.

## Tools and Technologies
- R (shiny, dplyr, plotly, leaflet)
- Spatial data processing (Natural Earth)
- Advanced reactive programming
- Interactive visual analytics design (Five Design Sheet methodology)

