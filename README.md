# Unveiling Disparities in Correctional Offender Management Profiling for Alternative Sanctions (COMPAS) Algorithm

**Prepared by:** Shubham Vats

## Introduction
Across the nation, judges, probation and parole officers are increasingly using algorithms to assess a criminal defendant’s likelihood of becoming a recidivist – a term used to describe criminals who re-offend. The dataset used more than 10,000 criminal defendants in Broward County, Florida, and compared their predicted recidivism rates with the rate that occurred over a two-year period. In our analysis, we used two statistical models: Logistic Regression and Cox Proportional Hazards Model.

## Key Objectives:
- **Exposing Disparities:** Investigating the distribution of COMPAS scores among demographic groups.
- **Demographic Predictors:** Identifying key predictors, such as age and race, influencing higher COMPAS scores.
- **Predictive Accuracy:** Evaluating the correlation between high COMPAS scores and the likelihood of recidivism.
- **Gender Disparities:** Examining variations in recidivism rates between high-risk men and women.

## Major Findings:
### Score Distribution Disparities:
- **General Recidivism:** Black defendants show similar score distributions, while white defendants' scores favor lower-risk groups.
- **Violent Recidivism:** Discrepancies persist in the distribution of scores between white and black defendants.

### Predictive Models:
- **Logistic Regression (General Recidivism):**
  - Age is a robust predictor, with defendants under 25 having a 2.5-fold increased chance of higher scores.
  - Black defendants are 45% more likely than white defendants to receive higher scores.
- **Logistic Regression (Violent Recidivism):**
  - Age is a significant predictor, with youthful defendants having a 6.4-fold higher chance of higher scores.
  - Black defendants have a 77.3% higher chance of receiving higher scores.
- **Cox Proportional Hazards Model:**
  - High COMPAS scores correlate with a 3.5 times higher likelihood of general recidivism.
  - Concordance score of 63.6% falls below industry standards, raising concerns about reliability.

### Gender-Based Analysis:
- **High-Risk Men vs. Women:**
  - High-risk men recidivate more (61.2%) compared to high-risk women (47.5%).

### Race-Based Analysis:
- **Predictive Accuracy:**
  - COMPAS predictive accuracy remains consistent between white (62.5%) and black defendants (62.3%).
  - Slight variation in concordance scores by race (69% for white, 67% for black).
- **Race-by-Score Interaction:**
  - Racial differences in hazard ratios: 2.99 for high-risk black defendants, 3.61 for high-risk white defendants.

## Limitations:
- Despite predictive value, COMPAS scores fall below industry standards, casting doubt on the system's overall reliability.
- Significant disparities exist in COMPAS predictions across racial and gender subgroups, raising concerns about potential biases and challenging law enforcement interpretation.
