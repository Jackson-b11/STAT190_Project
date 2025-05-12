# STAT 190 final project


## Table of Contents

- Project Overview
- Data Privacy Notice
- Repository Structure
  - #1. Data Cleaning
  - #2. Model Creation
  - #3. Visualizations


## Project Overview


This final project for STAT 190 at Drake University examines how the demographics of DMARC’s customer base, a food pantry, shifted throughout 2024. As a team, we cleansed sensitive customer data (not included in this repo), then grouped records by family and labeled a family as “dependent” if they visited all 12 months of 2024. Our two main objectives were:

1. **Characterize the new customer profile** – identify key demographic traits of families joining 
2. **Predict family dependency status** – build classification models to forecast whether a family will be classified as dependent. A "dependent" family is defined as one that visits a DMARC food pantry all 12 months out of the year.

We implemented multiple predictive models in both Python and R, and created visualizations to illustrate demographic trends, data distributions, and model performance.


## Data Privacy Notice

The raw data used in this project is private and cannot be shared publicly. No datasets are included in this repository. To reproduce the analysis, you must supply your own data in the required format.

## Repository Structure

 - **Cleaning**: Contains one python file that cleans the raw data by grouping by families
 - **Model_Creation**: Contains multiple files in python and R that implement preditive models that aim to predict a dependent family
     - `AR1timeSeries.R` creates a simple AR1 time series model. After further analysis, this model was not the best fit for the data and was not used to create any final conclusions
     - `Bernoulli GLM.R` creates a Bernoulli GLM to predict whether a family will be dependent
     - `Modeling_2024.ipynb` creates a logistic regression model predicting whether a faimly will be depenfent
     - `Random Forest.R` creates a Random Forest to predict whether a family will be dependent
     - `Random forest Tuning.R` tunes that random forest to create the most accurate model.
     - `boosting model.R` creates a Gradient Descent Boosting to predict whether a family will be dependent
     - `boosting tuning.R` tunes that boosting model.
 - **Visualizations**: Contains multiple files in pythona nd R that create visualization using both the cleaned data and raw data
     - `FamilyVisits.R` creates a visualization that shows the total amount of visits by month and the unique households by month. This visualization shouldn't fully be trusted since we learned that the intake system often does not record visits if it is not a family's first time visiting in a month.
     - `IncomeBracket.R` creates a visualization that shows the amount of visits in a given month for families in different income brackets.
     - `NewFamilies.R` creates a Random Forest model that predicts if a family was 'new' in 2023 (meaning they had not visited a DMARC pantry in the past). That model is then used to guide the creation of visualizations that describe the difference between these new families and the traditional families.
     - `Visualization_2024_Project.ipynb` creates visualizations that describe the amount of dependent families by income bin.
     - `Visualizations.R` creates a visualization that describes the amount of dependent families by household size.


