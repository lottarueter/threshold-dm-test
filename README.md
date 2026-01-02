# **Code for the project _Comparing Forecast Performance on Large Panel Data with Unknown Clustering Structure_**

This repository contains the R code used for the empirical analysis in **Chapter 3 (“Comparing Forecast Performance on Large Panel Data with Unknown Clustering Structure”)** of my PhD dissertation.

The project studies the comparison of forecast performance in large panel datasets with unknown and potentially complex cross-sectional dependence structures. In particular, it implements a Diebold–Mariano–type test based on a thresholded covariance matrix estimator, designed to remain valid under heterogeneity, serial correlation and unknown clustering.

---

## 📂 Contents

The repository currently includes:

- Data preparation scripts (`CDS_data_prep.R`, `expl_variables_prep.R`)
- Descriptive statistics of the data (`summary_statistics.R`)
- Model estimation script (`model_fits.R`)
- Implementation of the Diebold–Mariano test with thresholded covariance matrix (`hac_hard_threshold_sigma.R`, `test.R`)
- Visualisation scripts of the empirical results (`visualisations.R`)

Each script is designed to be run in sequence or used as part of a custom analysis pipeline.

--- ## 📊 Data Availability

The code and data-processing pipelines are fully documented and publicly available. However, the empirical application relies on **proprietary sovereign CDS data**, which cannot be shared. As a result, the scripts can be inspected and adapted freely, but full replication of the empirical results requires access to the underlying data.

---

## 📦 Software & Dependencies

The empirical analysis is implemented in **R**. The following packages are required to run the scripts in this repository.

### Data manipulation and utilities
- dplyr – data manipulation and pipelines  
- tidyr – reshaping data  
- purrr – functional programming tools  
- lubridate – handling date and time formats  
- zoo – time-series utilities (e.g. forward/backward filling)  
- rlist – list manipulation helpers  

### Time series modelling and imputation
- rugarch – GARCH-type volatility models  
- imputeTS – time-series imputation (e.g. Kalman filtering)  

### Forecasting models
- systemfit – systems of simultaneous equations  
- ranger – random forests  
- xgboost – gradient boosting  

### Visualisation
- ggplot2 – plotting  
- patchwork – combining multiple plots  

### Installation

All required packages can be installed via:

```r
install.packages(c(
  "tidyr", "purrr", "lubridate", "zoo", "rlist",
  "rugarch", "imputeTS",
  "systemfit", "ranger", "xgboost",
  "ggplot2", "patchwork",
  "dplyr"
))
