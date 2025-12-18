# ULEZ Impact on NO₂ Concentrations (Difference-in-Differences Analysis)

This repository contains an R script that evaluates the causal impact of London’s **Ultra Low Emission Zone (ULEZ)** on nitrogen dioxide (NO₂) concentrations using a **difference-in-differences (DiD)** research design.

The analysis exploits variation across **monitoring stations** (treated vs. control) and **time** (pre- vs. post-ULEZ implementation), with high-dimensional fixed effects and clustered standard errors.

---

## 📁 Repository Contents

- `ulezimpact_ss154.R`  
  Main analysis script. Runs the full pipeline from data preparation to estimation, placebo tests, and output tables/figures.

---

## 🔍 Methodological Overview

The script implements the following steps:

1. **Data preparation**
   - Import and clean station-level daily NO₂ data
   - Construct treatment and post-policy indicators
   - Generate time controls (weekday, month, year)

2. **Summary statistics**
   - Panel structure (station–day observations)
   - Number of treated vs. control stations
   - Temporal coverage and NO₂ distribution

3. **Difference-in-Differences estimation**
   - Two-way fixed effects (station and date)
   - Clustered standard errors at the station level
   - Estimation using `fixest::feols`

4. **Parallel trends diagnostics**
   - Visual inspection of pre-policy trends
   - Event-study style specifications (where applicable)

5. **Placebo-in-space tests**
   - Random reassignment of treatment among control stations
   - Monte Carlo distribution of placebo treatment effects
   - Comparison of true estimate against placebo distribution

6. **Output generation**
   - Tables formatted with `flextable`
   - Figures created using `ggplot2`

---

## 📦 Required R Packages

The script automatically installs and loads the following packages if they are not already available:

- `dplyr`
- `ggplot2`
- `lubridate`
- `zoo`
- `fixest`
- `tidyr`
- `flextable`
- `officer`

No manual package installation is required.

---

## ▶️ How to Run the Analysis

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/ulez-no2-did.git
   cd ulez-no2-did

2.	Open R or RStudio and run:
   source("ulezimpact_ss154.R")
