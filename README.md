# hospital-los-classifier
Ensemble model classifier for long hospital admission Length of Stay (LOS)

The following analysis was done as part of a data science course I took at York University. The research question was to predict long length of stay (LOS) for diabetic patients admitted to one of the cohort hospitals in the dataset. Since patients with LOS greater than 15 days were already removed, this means predicting whether or not a patient stayed 7-15 days in hospital as opposed to 1-6 days.

The dataset was obtained from very popular the UCI Machine Learning Repository https://archive.ics.uci.edu/ml/datasets/diabetes

To replicate my analysis, the order in which to run the files is as follows:

1) Data Exploration.R
2) Data Cleaning Phase 1.R
3) Data Cleaning Phase 2.R
4) GAM Model.R, XG Model.R, LR Model.R (in any order)
5) Ensemble Model.R

I have also provided the fully trained model files to expedite the process of rerunning the analysis.

Finally, a summary report is also attached which outlines further details on the analysis and a discussion of the results.