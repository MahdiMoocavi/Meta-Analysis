# Meta-Analysis of Animacy, Interaction Ratings, and Memory

An R research-analysis script combining data from 65 replication groups collected in English- and Dutch-language versions of a memory experiment. The analyses examine animacy-related recall, interaction ratings, and the association between item-level interaction ratings and recall.

## Analysis questions

1. **Animacy effect:** Do participants recall different numbers of animate and inanimate words?
2. **Interaction ratings:** Do animate and inanimate items differ in their average interaction ratings?
3. **Interaction–recall association:** Are item-level interaction ratings associated with recall proportions?
4. **Language comparison:** Do the paired recall patterns differ across the two language groups?

## Workflow

- Combine group-level CSV files and harmonize the variables used in the analyses.
- Apply the script's predefined outlier-screening rules.
- Inspect descriptive statistics and distributional assumptions.
- Conduct paired comparisons for recall and interaction ratings.
- Calculate study-level statistics and fit meta-analytic models.
- Examine the interaction-rating/recall association across replication groups.
- Produce forest plots, funnel plots, and descriptive visualizations.

## Input data

The script expects 65 CSV files in the working directory, named:

```text
group1.csv
group2.csv
...
group65.csv
```

The files must contain the variable names used in the script, including participant demographics, animate/inanimate interaction ratings, recall measures, and item-level responses. The original participant-level data are not included.

## Requirements

R and the packages below:

```r
install.packages(c(
  "ggpubr", "ggplot2", "gplots", "psych", "tidyverse",
  "rstatix", "apaTables", "ez", "gtsummary", "car",
  "metafor", "plyr", "effsize", "gridExtra"
))
```

## Run

Set the working directory to the folder containing the 65 group files, install the dependencies, and execute `MetaAnalysis_2023_04.R`.

## Outputs

The script writes intermediate study-level tables, including `T_table_A.csv`, `T_table_I.csv`, and `C_table.csv`, and generates forest/funnel plots and descriptive visualizations in the active graphics device.

## Methodological validation note

This is a legacy research script and its effect-size definitions, sampling-variance calculations, and model-estimator choices should be reviewed before treating pooled estimates as final. The current script also contains an object-initialization issue in the transition between the first and second meta-analyses. The source data and a clean end-to-end execution environment are required for full reproduction.
