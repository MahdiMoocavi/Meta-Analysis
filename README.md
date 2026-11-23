# Meta-Analysis of Animacy, Interaction Ratings, and Memory

An R research-analysis script combining 65 replication groups from English- and Dutch-language versions of a memory experiment. The analyses examine animacy-related recall, interaction ratings, and the association between item-level interaction ratings and recall.

## Research questions

1. **Animacy effect:** Do participants recall different numbers of animate and inanimate words?
2. **Interaction ratings:** Do animate and inanimate items differ in average interaction ratings?
3. **Interaction–recall association:** Are item-level interaction ratings associated with recall proportions?
4. **Language comparison:** Do paired recall patterns differ across the two language groups?

## Workflow

- Combine group-level CSV files and harmonize variables used in the analyses.
- Apply the study's predefined outlier-screening rules.
- Inspect descriptive statistics and distributional assumptions.
- Conduct paired comparisons for recall and interaction ratings.
- Calculate study-level paired standardized mean-change effects and sampling variances using metafor's SMCC measure.
- Fit an equal-effects model for the animacy-recall comparison and random-effects models for interaction ratings and item-level correlations.
- Transform item-level correlations using Fisher's r-to-z (ZCOR) before pooling.
- Produce forest plots, funnel plots, and descriptive visualizations.

## Input data

The script expects 65 CSV files in the working directory, named group1.csv through group65.csv. Files must follow the variable schema used in the original study, including participant demographics, animate/inanimate interaction ratings, recall measures, and item-level responses. The original participant-level data are not included.

## Requirements

R and the following packages:

```r
install.packages(c(
  "ggpubr", "ggplot2", "gplots", "psych", "tidyverse",
  "rstatix", "apaTables", "ez", "gtsummary", "car",
  "metafor", "plyr", "effsize", "gridExtra"
))
```

## Run

Set the working directory to the folder containing the 65 group files, install the dependencies, and execute MetaAnalysis_2023_04.R.

## Outputs

The script writes study-level tables (T_table_A.csv, T_table_I.csv, and C_table.csv) and generates forest/funnel plots and descriptive visualizations in the active graphics device.

## Interpretation and limitations

The paired comparisons use complete pairs within each group. Standardized paired effects are calculated with the SMCC measure and large-sample sampling-variance approximation; correlation effects are Fisher-z transformed before random-effects pooling. The item-level correlation analysis is based on the 24 stimulus items, not the participant count. The original data are required to reproduce numerical results. Review the study's outlier rules and the dependence structure of item-level observations before treating pooled estimates as definitive.
