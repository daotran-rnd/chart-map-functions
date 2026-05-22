# 📊 RVizToolbox: R Functions for Visualisation 📈

## About this project 💡
**RVizToolbox** is a curated collection of R functions designed to streamline **data visualisation** and **spatial mapping**. This toolbox helps researchers and analysts transform raw data into polished, publication-ready charts and maps with minimal code.

## Quick start 🚀 
You can call functions from this repository directly into your R environment (no download required). This ensures you are always using the latest, most optimized version of the visualisation tools.

### Step 1: Define the loader function
Copy and paste this loader function into your R script. It acts as a bridge to this repository.

```R
CallRVizToolbox <- function(file) {
  source(paste0(
    "https://raw.githubusercontent.com/daotran-rnd/chart-map-functions/main/r_functions/",
    file)) }
```

### Step 2: Load a specific visualisation tool
Call the specific tool you need. For example, to load a chart function `PlotLineChart`:
```R
CallRVizToolbox("PlotLineChart.R")
```

### Step 3: Use the function for its designated purpose 
You can read description in each R function to know parameters to input. 

Example: We have a dataset `transactions_data` and there are columns called `OrderDate`, `QuantitySold`. 
Here is how to input parameters and use the function `PlotLineChart()` for this example data. 
```R
PlotLineChart(data = transactions_data,
              date_col = OrderDate,
              value_col = QuantitySold ) 
```

**Enjoy and Stay Strong!⚡**
