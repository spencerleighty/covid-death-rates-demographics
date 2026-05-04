# COVID-19 Death Rates by Demographics

## Overview
This project analyzes monthly COVID-19 death rates by demographic group to identify differences in death rates across age, race and ethnicity, sex, and region. The analysis uses statistical testing and visualization in R to explore public health disparities within the dataset.

## Tools Used
- R
- Base R
- ANOVA
- Kruskal-Wallis Test
- Tukey HSD
- Data visualization

## Dataset
The dataset contains monthly COVID-19 death rates per 100,000 people, grouped by demographic characteristics such as age group, race and ethnicity, sex, and region.

Key variables included:
- Deaths
- Crude death rate
- Age group
- Race and ethnicity
- Sex
- Region
- Confidence interval bounds

## Research Questions
- Do COVID-19 death rates differ significantly across age groups?
- Do COVID-19 death rates differ significantly across racial and ethnic groups?
- Are there measurable differences in death rates by sex or region?
- Do non-parametric tests support the results of traditional ANOVA?

## Process

### Data Cleaning
- Standardized column names for easier analysis
- Handled missing values carefully due to subgroup structure
- Filtered observations based on reporting adequacy where appropriate
- Converted demographic variables into categorical factors
- Reviewed death counts and crude death rates for zero values and outliers

### Exploratory Data Analysis
- Examined distributions of crude death rates
- Compared crude death rates across demographic categories
- Created visualizations to identify group-level differences and outliers
- Assessed whether the data met assumptions for ANOVA

### Statistical Testing
- Used ANOVA to test for differences in mean crude death rates across groups
- Conducted Tukey HSD post-hoc comparisons to identify which groups differed
- Used Kruskal-Wallis tests as a non-parametric alternative when assumptions were not fully met

## Key Findings
- COVID-19 death rates differed substantially across age groups
- Older age groups showed the highest crude death rates
- Race and ethnicity groups showed statistically significant differences in crude death rates
- Non-parametric testing supported the presence of group-level differences
- Assumption checking was important because crude death rates were skewed and contained outliers

## Visualizations

![Death Rates by Sex](images/sex_boxplot.png)
![Death Rates by Age Group](images/age_group_boxplot.png)
![Death Rates by Race and Ethnicity](images/race_ethnicity_boxplot.png)
