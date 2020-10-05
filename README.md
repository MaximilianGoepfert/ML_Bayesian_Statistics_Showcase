# Machine Learning & Bayesian Statistics: Project Overview
- abc
- def

## Code and Resources Used
__R Version__: 3.6.2 \
__Packages__: dplyr, ggplot2, ggthemes, multcomp, caret, randomForest, class, e1071, R2jags, ggmcmc

## Part One: Machine Learning
The following steps have been conducted:
- Visualized the data via bivariate scatterplots with colour-coding
- Split the data in training and test set
- Applied the k-nearest-neighbours (KNN) method in order to construct a classifier to predict the location
- Applied the Random Forest (bagging) method in order to construct a classifier to predict the location
- Applied the Support Vector Machines (SVM) algorithm in order to construct a classifier to predict the location
- Visualized the resulting classification rule for each method
- Tested the methods on the test set, finding the test error for each model
<br/>
![alt text](https://github.com/MaximilianGoepfert/ML_Bayesian_Statistics_Showcase/blob/master/knn_classifier.PNG "KNN Figure")

## Part Two: Frequentist Inference
- Created the data and saved it inside a dataframe
- Visualized the data using a boxplot diagram
- Performed a frequentist ANOVA test on the 95%-confidence interval of whether the underlying average crop yield is different when a different fertilizer is applied
- Performed a Follow-up Analysis using Tukey HSD-Test
- Tested whether the underlying crop yield obtained using the fourth fertilizer is more than 0.5 units greater than the average of the underlying crop yield levels obtained using the other three fertilizers
<br/>
![alt text](https://github.com/MaximilianGoepfert/ML_Bayesian_Statistics_Showcase/blob/master/frequentist_plot.PNG "Frequentist Plot")

## Part Three: Bayesian Inference
- Wrote jags/BUGS code to perform inference about a related Bayesian one-way Analysis of Variance model
- Visualized the posterior densities
- Included a graphical representation and the numerical values of the 95% credible intervals for the parameters 
- Modified our jags code to also perform inference about a selection of group differences
- Found the posterior probabability that the underlying crop yield obtained using the fourth fertilizer is more than 0.5 units greater than the average of the underlying crop yield levels obtained using the other three fertilizers
- Also considered a simplified Bayesian model
- Compared performance of 'full' and simplified models using the Deviance Information Criterion (DIC)
<br/>
![alt text](https://github.com/MaximilianGoepfert/ML_Bayesian_Statistics_Showcase/blob/master/density_plot.PNG "Density Plots")

