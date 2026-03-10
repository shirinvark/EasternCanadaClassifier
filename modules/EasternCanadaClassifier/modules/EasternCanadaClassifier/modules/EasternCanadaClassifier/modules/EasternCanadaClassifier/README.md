# EasternCanadaClassifier

The **EasternCanadaClassifier** module classifies forest stands into **analysis units (AUs)** based on forest composition, biomass, and stand age.

These analysis units represent groups of stands with similar **growth trajectories** and are used to link the forest landscape to **yield tables** for harvest planning and **AAC calculations**.

The module is part of a larger **SpaDES simulation pipeline** used to model forest dynamics and harvest planning across Eastern Canada.

---

## Module Role in the Simulation Pipeline

The classifier operates after spatial preparation and landbase definition modules, and before yield table application and harvest scheduling.

```
EasternCanadaDataPrep
    ├─ planningGrid
    ├─ landcover
    ├─ standAge
    └─ FMU
          │
          ▼
RiparianBuffers
    ├─ rivers
    ├─ lakes
    └─ riparianFraction
          │
          ▼
EasternCanadaLandbase
    ├─ protectedMask
    ├─ harvestableFraction
    ├─ merchantableForest
    └─ landbase
          │
          ▼
LandR modules
    ├─ cohortData
    ├─ pixelGroupMap
    └─ species cohorts
          │
          ▼
EasternCanadaClassifier
    ├─ cohortData
    ├─ pixelGroupMap
    ├─ harvestableFraction
    └─ analysisUnitMap
          │
          ▼
Yield Tables
    ├─ growth curves
    └─ volume(age)
          │
          ▼
AAC / Harvest Module
    ├─ harvest schedule
    ├─ harvest volume
    └─ future forest
```

---

# Module Inputs

The module expects the following objects from upstream modules.

| Object | Description |
|------|-------------|
| `cohortData` | LandR cohort table containing species composition, biomass, age and pixel group information |
| `pixelGroupMap` | Raster map defining spatial pixel groups associated with cohort data |
| `harvestableFraction` | Raster indicating the fraction of each pixel that is legally harvestable |

---

# Module Outputs

| Object | Description |
|------|-------------|
| `analysisUnitMap` | Raster assigning each pixel to an analysis unit |
| `yieldTables` | Matrix containing yield curves for different analysis units |
| `yieldAges` | Vector of age classes corresponding to the yield table columns |

---

# Classification Logic

The classifier follows several steps to assign analysis units:

1. **Aggregate cohort data**
   - biomass is aggregated by pixel group and species group

2. **Determine stand composition**
   - species are grouped into conifer and broadleaf categories

3. **Estimate stand volume**
   - biomass is used as a proxy for stand volume

4. **Match yield trajectories**
   - stand age and volume are compared to yield tables
   - the closest growth trajectory is selected

5. **Create spatial map**
   - the resulting analysis unit classification is mapped to a raster

---

# Yield Tables

Yield tables represent growth trajectories for different forest types.

Each table provides:

```
Stand Age → Merchantable Volume (m³/ha)
```

The classifier links each stand to the yield trajectory that best matches its current structure.

---

# Example Simulation

Example initialization of a simulation including this module:

```r
library(SpaDES.core)

sim <- simInit(
  times = list(start = 0, end = 1),
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers",
    "EasternCanadaLandbase",
    "EasternCanadaClassifier"
  )
)

sim <- spades(sim)
```

The resulting analysis unit map can be visualized:

```r
plot(sim$analysisUnitMap)
```

---

# Future Extensions

This module provides the structural link required for downstream modules such as:

- yield-based forest growth simulation
- harvest scheduling
- Allowable Annual Cut (AAC) calculation
- forest landscape scenario analysis

---

