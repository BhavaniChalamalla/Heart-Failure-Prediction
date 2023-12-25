# Classification model of Heart Failure prediction
## Overview:
This project focuses on predicting heart disease using machine-learning models. The dataset used in this project includes various health-related features, and the goal is to build models that can accurately predict the likelihood of heart disease in individuals.

## Project Structure:
### Data Exploration and Preprocessing:
Explored the dataset for insights and patterns.
Handled missing values and checked for duplicate rows.
Separated categorical and numerical variables.
Created visualizations (bar plots, histograms, boxplots) to understand data distributions.

### Data Transformation:
Performed dummy encoding for categorical variables.
Addressed skewness and outliers in numerical columns using various transformations.
Conducted spatial sign transformation for outlier removal.

### Model Building:
Implemented a variety of machine learning models, including Logistic Regression (GLM), Support Vector Machine (SVM), k-nearest Neighbors (KNN), Neural Networks, and more.
Utilized advanced techniques such as hyperparameter tuning and spatial sign transformation to optimize model performance.
Evaluated models using confusion matrices and key metrics.

### Model Comparison:
Compared the performance of SVM and GLM models, highlighting metrics such as sensitivity, specificity, and AUC.
Chose the GLM model as the preferred choice for heart failure prediction.

## Tools and Libraries:
### R Packages:
**"reader"** for data import.

**"dplyr"** for data manipulation.

**"e1071"** for SVM implementation.

**"corrplot"** for creating correlation plots.
**"caret"** for machine learning model training and evaluation.
**"moments"** for statistical analysis.
**"klaR"** for Naive Bayes implementation.
**"pROC"** for ROC curve creation and AUC calculation.

## Project Outcome:
Successfully built and evaluated machine learning models for heart disease prediction.
Choose the Penalized(GLM) model as the preferred model based on its superior sensitivity, specificity, and AUC.
This project showcases proficiency in data preprocessing, exploratory data analysis, and machine learning model implementation using R.

## How to Use:
Clone the repository.
Ensure you have R and the required packages installed.
Run the R script to reproduce the analysis and model training.
Explore the generated visualizations and model evaluation metrics.
