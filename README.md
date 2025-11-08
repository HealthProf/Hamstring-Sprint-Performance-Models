# Hamstring Imbalance and Sprint Performance Analysis

## Overview

This repository contains R code for analyzing the relationship between hamstring strength imbalances and 10-yard sprint performance in collegiate women's soccer athletes. The analysis uses hamstring strength data from Nordbord testing equipment and sprint times from SmartSpeed timing systems to identify which types of imbalances most significantly impact acceleration performance.

**Key Finding**: Force imbalance between legs is the primary driver of slower sprint times, followed by torque imbalance when force is balanced, with impulse playing a conditional role only when force imbalances are high.

---

## Background

### Research Question
How do left-right hamstring imbalances (in force, torque, and impulse) influence short-distance sprint speed in soccer athletes?

### Why This Matters
In soccer, quick acceleration over 10 yards can be decisive for performance. If one hamstring is substantially stronger or more "explosive" than the other, it may:
- Slow an athlete's sprint time
- Increase injury risk
- Reduce overall athletic performance

### Measurement Protocol
**Sprint Testing**: 10-yard sprint times recorded in seconds using SmartSpeed timing gates

**Hamstring Testing**: Nordbord hamstring strength assessment measuring:
- **Max Force (N)**: Peak force generated during hamstring contraction
- **Max Torque (Nm)**: Rotational force around the knee joint
- **Max Impulse (Ns)**: Force applied over time (reflects how athletes "load" and lengthen the hamstring)

**Imbalance Calculation**: Left leg measure minus Right leg measure

---

## Repository Contents

### R Scripts

#### `Athlete random effect models.R`
**Purpose**: Mixed-effects regression analysis accounting for repeated measures

**What it does in plain language**:
This script examines how much a hamstring imbalance affects sprint time while accounting for the fact that the same athletes were tested multiple times. It's like asking "if an athlete's left-right force difference increases by 1 Newton, by how much does their sprint time change?" while remembering that each athlete has their own baseline performance.

**Key features**:
- Imports and merges data from multiple CSV files
- Handles testing on the same day by averaging results
- Builds statistical models that account for repeated testing of the same athletes
- Uses 10-fold cross-validation to ensure results are reliable
- Performs bootstrap analysis to estimate confidence intervals
- Generates predictions and visualizations

**Statistical approach**: Linear mixed-effects models with random intercepts and slopes for each athlete

#### `Decision tree models with effect size.R`
**Purpose**: Decision tree analysis with effect size calculations

**What it does in plain language**:
This script creates a flowchart of "if-then" rules to predict sprint times based on hamstring imbalances. It's easier to follow than regression equations and reveals which imbalances matter most. For example: "If force difference is less than 3.1 N, then look at torque difference; if that's below 24Nm, expect faster sprint times."

**Key features**:
- Imports and processes the same data as the mixed-effects script
- Builds a decision tree model using 10-fold cross-validation
- Identifies specific thresholds for force, torque, and impulse differences
- Calculates Cohen's d effect sizes to quantify the practical significance of each split
- Generates tree visualizations showing the decision rules

**Statistical approach**: Recursive partitioning (CART) with cross-validation and effect size analysis

### Data Templates

#### `Nordboard_Test_Export_Template.csv`
Template showing the required format for hamstring testing data export from Nordbord equipment.

**Required columns**:
- `Athlete`: Athlete identifier
- `Date`: Test date
- `L Reps`, `R Reps`: Number of repetitions per leg
- `L Max Force (N)`, `R Max Force (N)`: Maximum force for left and right legs
- `L Max Torque (Nm)`, `R Max Torque (Nm)`: Maximum torque for left and right legs
- `L Max Impulse (Ns)`, `R Max Impulse (Ns)`: Maximum impulse for left and right legs
- Various imbalance percentages

#### `SmartSpeed_Export_Template.csv`
Template showing the required format for sprint timing data export from SmartSpeed systems.

**Required columns**:
- `Date`: Test date
- `Athlete`: Athlete identifier
- `Total`: 10-yard sprint time in seconds

---

## Key Findings

### Practical Thresholds Identified

1. **Force Difference**: >3.1 N between legs significantly predicts slower sprint times
2. **Torque Difference**: >24 Nm (when force is balanced) still impairs sprint performance
3. **Impulse Difference**: Only matters when force AND torque imbalances exists

### Decision Priority for Coaches

The analysis reveals a clear prioritization for addressing hamstring imbalances:

**Priority 1 - Address Force Imbalance First**
- Target: Reduce force difference to <3 N
- Why: This is the strongest predictor of slower sprint times
- Action: Strengthen the weaker leg to match the stronger leg

**Priority 2 - Focus on Torque Imbalance (Once Force is Balanced)**
- Target: Reduce torque difference to <24 Nm
- Why: Even with balanced force, large torque differences slow sprint performance
- Action: Add targeted drills for rotational strength (e.g., slow eccentric exercises)

**Priority 3 - Monitor Impulse (Only if Force OR Torque Remains Imbalanced)**
- Relevant when: Force difference >3 N cannot be immediately corrected
- Why: Impulse can partially compensate for force imbalances
- Action: Plyometric/eccentric drills if loading patterns are asymmetric

---

## Requirements

### Software
- R version 4.0 or higher
- RStudio (recommended but not required)

### R Packages
```r
# Data manipulation and import
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Statistical modeling
library(lme4)        # Mixed-effects models
library(lmerTest)    # P-values for mixed models
library(rpart)       # Decision trees
library(caret)       # Model training and cross-validation
library(effsize)     # Effect size calculations

# Visualization
library(ggplot2)
library(GGally)
library(rpart.plot)
```

---

## Usage

### Data Preparation

1. **Export your data** in the format specified by the CSV templates
   - Nordbord data: Use `Nordboard_Test_Export_Template.csv` as reference
   - Sprint timing data: Use `SmartSpeed_Export_Template.csv` as reference

2. **Place CSV files** in the same directory as the R scripts

3. **Ensure date formats** are consistent (scripts support multiple formats: YYYY-MM-DD, MM/DD/YYYY, DD/MM/YYYY)

### Running the Analysis

#### Mixed-Effects Model Analysis
```r
# Set working directory to folder containing scripts and CSV files
setwd("path/to/your/data")

# Run the mixed-effects analysis
source("Date_and_athlete_random_effect.R")
```

**Output includes**:
- Cross-validation performance metrics (RMSE, R-squared, MAE)
- Model summary with fixed and random effects
- Bootstrap confidence intervals
- F-tests and p-values for predictors
- Visualization of predicted vs. observed sprint times

#### Decision Tree Analysis
```r
# Set working directory to folder containing scripts and CSV files
setwd("path/to/your/data")

# Run the decision tree analysis
source("Decision_tree_with_effect_size.R")
```

**Output includes**:
- Cross-validated decision tree model
- Tree visualization showing split thresholds
- Fold-wise RMSE values
- Cohen's d effect sizes for each split
- Confidence intervals for effect sizes

---

## Understanding the Results

### Reading Mixed-Effects Model Output

The model output shows:
- **Fixed effects**: How much sprint time changes per unit increase in imbalance
  - Example: Force difference coefficient = 0.0003 means each 1 N increase in force imbalance adds 0.0003 seconds to sprint time
- **Random effects**: Individual athlete variations from the average pattern
- **F-values and p-values**: Statistical significance of each predictor

### Interpreting the Decision Tree

The tree visualization shows a flowchart:
- **Root node**: All athletes start here
- **Split conditions**: "If force_diff < 3.125 N, go left; otherwise go right"
- **Leaf nodes**: Final predicted sprint times for athletes meeting that path's conditions
- **Effect sizes**: Cohen's d values indicate practical significance (small <0.5, medium 0.5-0.8, large >0.8)

---

## Limitations and Considerations

### Data Privacy
This repository contains **code only** - no athlete data is included. Users must provide their own data following the template formats.

### Sample Considerations
- Results are based on collegiate women's soccer players
- Thresholds may differ for other populations (men, different sports, different competition levels)
- Sufficient repeated measures per athlete improve model reliability

### Statistical Notes
- Mixed-effects models account for repeated measures but require adequate sample size
- Decision trees can overfit if not properly cross-validated (addressed through 10-fold CV in this code)
- Effect sizes provide practical significance beyond statistical significance

---

## Citation

If you use this code or methodology in your research, please cite:

[Your citation information will go here - include DOI if available]

**Related Publication**: [Conference poster/paper title and venue]

---

## Contributing

This code is provided as-is for research and educational purposes. If you identify issues or have suggestions for improvements, please open an issue in this repository.

---

## License

[Specify your license here - e.g., MIT, GPL-3, CC-BY-4.0]

---

## Publications and Presentations



---

## Contact

**Researcher**: Tim Curry  
**Institution**: Northern Arizona University, Department of Health Sciences  
**Program**: NAU Sims-Treharne Sports Performance Research Lab

For questions about methodology or implementation, please [open an issue](link-to-issues) or contact the corresponding author at timothy.curry@nau.edu.

---

## Acknowledgments

- Study conducted in collaboration with NAU Women's Soccer Team.
- Analysis framework developed at NAU Sports Performance Lab

**AI Use Acknowledgement**: The research report associated with this code was outlined using OpenAI ChatGPT-4 mini. All modeling, analysis, and code were created by Tim Curry, and all information was reviewed and edited for accuracy.

---

## Version History

- **v1.0** (Current): Initial public release with mixed-effects and decision tree analysis code
