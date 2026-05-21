# Post-Discharge Care Coordination and Hospital Readmissions

A simulated analysis evaluating whether post-discharge care coordination 
reduces 30-day hospital readmission rates in a value-based care setting.

📄 **[View the full report](https://davechilders.github.io/post-discharge-intervention-analysis/report.html)**

---

## Overview

This project simulates a population health intervention in which primary care 
practices adopt a post-discharge care management program. While this project uses simulated data, a real program might assigning care managers to follow up with patients within 48 hours of hospital discharge to 
coordinate medications, schedule follow-up visits, and flag early warning signs.

The analysis evaluates whether practices that adopted the program saw a 
reduction in 30-day readmission rates relative to non-adopting practices. We simulate a staggered adoption design to reflect real-world rollouts of such programs. 

---

## Methods

- **Simulated Data:** Synthetic patient- and practice-level data generated in R 
  to reflect realistic variation in readmission risk across a Medicare ACO population
- **Study Design:** Difference-in-differences with staggered treatment adoption 
  across practices
- **Model:** Generalized linear mixed model (GLMM) with a logit link, accounting 
  for repeated patient observations and practice-level random effects
- **Covariates:** Patient age, sex, SSDI status, and time since study start

---

## Tools

- R, Quarto
- parquet (cross-language file format for optimized storage and retrieval of columnar data)
- `lme4` (GLMM estimation)
- `tidyverse` (data simulation and wrangling)
- `gt` (tables)
- `ggplot2` (visualization)
