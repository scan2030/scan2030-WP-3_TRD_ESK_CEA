# Cost-effectiveness of Esketamine for Treatment-Resistant Depression (TRD)

This repository contains the analytical code used for the cost-effectiveness analysis presented in the manuscript:

"Cost-effectiveness of Esketamine versus Alternative Treatment Strategies for Treatment-resistant Depression in Hong Kong: A Multi-armed Modelling Study" by Yifan Li et al.


---

## 📦 Repository Structure
```
code/                                                                                                                                                                   
├── Basecase/              # Base-case analysis                                                                                                                         
├── Scenario Analysis/     # Scenario analyses                                                                                                                          
├── Sensitivity Analysis/  # Deterministic and probabilistic sensitivity analyses
```
---

## 🚀 How to Run the Analysis

All analyses are implemented in R.

### 1. Base-case analysis
Run scripts in:
code/Basecase/

Main file:
ESK_Basecase.R

---

### 2. Scenario analyses
Run scripts in:
code/Scenario Analysis/

Each file corresponds to a specific scenario.

---

### 3. Sensitivity analyses
Run scripts in:
code/Sensitivity Analysis/

Includes:
- Deterministic sensitivity analysis (DSA)
- Probabilistic sensitivity analysis (PSA): Cost-effectiveness acceptability curves (CEAC) and 10000 Monte Carlo Simulation Cost Effectiveness Plane 

---

## 🔧 Software Requirements

- Required R packages:

heemod
dplyr
tidyr
ggplot2

---

## 📊 Outputs

The repository generates:
- Cost-effectiveness results (ICERs, QALYs, costs)
- Cost-effectiveness planes
- Sensitivity analysis plots (DSA, PSA)
- Scenario analysis results

---

## 🔁 Reproducibility

This repository provides all scripts necessary to reproduce the analyses reported in the manuscript. Due to data governance restrictions, individual-level data from the Hong Kong Hospital Authority are not publicly available.

---

## 📄 Version

This version corresponds to the manuscript submitted to PLOS Medicine.


