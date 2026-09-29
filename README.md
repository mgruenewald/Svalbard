
# Svalvard Snow Cover Analysis (2001-2025)

This repository contains the data processing scripts, analysis code, and derived data used to investigate temporal changes in snow cover over Svalbard for the period 2001–2025.


## Project Description

The study investigates temporal patterns and changes in snow cover across Svalbard using daily MODIS snow cover data and surface skin temperature from the MERRA-2 reanalysis.

The analysis focuses on:

* temporal changes in snow cover between 2001 and 2025
* seasonal variations in snow cover
* the relationship between snow cover and surface temperature
* longer-term trends based on five-year averages.
## Repository Structure

```text
svalbard-snow-cover/
│
├── README.md
├── data/
│   ├── processed/
│   └── README.md
├── scripts/
│   ├── 01_modis_preprocessing.R
│   ├── 02_data_analysis.R
│   └── 03_figures.R
└── results/
    ├── figures/
    └── tables/
```
## Data Sources & Methodology

### MODIS MOD10A1 - Daily Snow Cover
* **Source**: NASA AppEEARS (2001–2025)
* **Variable**: Normalized Difference Snow Index (NDSI) at 500 m spatial resolution in the native sinusoidal projection.
* **Processing**: The data were spatially subsetted using a GADM land mask for Svalbard, excluding Jan Mayen. Pixels with a snow cover fraction of ≥ 50% were classified as snow-covered. Cloud-covered and otherwise unusable pixels were treated as NA.
### MERRA-2 Surface Temperature
* **Source**: NASA MERRA-2 Reanalysis (2001–2025)
* **Variable**: Hourly Surface Skin Temperature (TS), aggregated to daily mean values.

**Note**: The original MODIS and MERRA-2 datasets are not included in this repository and must be obtained from the respective data providers.
## Software Requirements

The analyses were conducted using R (version 4.6.0).

The following R packages are required:

* **Spatial & data processing:** terra, geodata, luna, dplyr, stringr, lubridate
* **Modeling & visualization:** lme4, ggplot2, viridis
## Reproducibility

The analysis can be reproduced using the R scripts provided in the `scripts/` directory.

The original **MODIS MOD10A1** and **MERRA-2** data can be obtained from the respective data providers. Preprocessed data are provided in the `data/` directory.

For the original datasets, run all scripts in the following order. When using the preprocessed data, start with `02_data_analysis.R`.

1. `01_modis_preprocessing.R`
2. `02_data_analysis.R`
3. `03_figures.R`
