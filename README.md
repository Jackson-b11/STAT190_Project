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
2. **Predict family dependency status** – build classification models to forecast whether a family will be classified as dependent.

We implemented multiple predictive models in both Python and R, and created visualizations to illustrate demographic trends, data distributions, and model performance.


## Data Privacy Notice

The raw data used in this project is private and cannot be shared publicly. No datasets are included in this repository. To reproduce the analysis, you must supply your own data in the required format.

## Repository Structure

**Cleaning**
Contains one python file that cleans the raw data by grouping by families
**Model_Creation**
Contains multiple files in python and R that implement preditive models that aim to predict a dependent family   
**Visualizations**
Contains multiple files in pythona nd R that create visualization using both the cleaned data and raw data


