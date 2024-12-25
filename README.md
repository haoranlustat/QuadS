# Mortgage Prepayment Modeling via a Smoothing Spline State Space Model

## Introduction

Loan behavior modeling is crucial in financial engineering. In particular, predicting loan prepayment 
based on large-scale historical time series data of massive customers is challenging.
Existing approaches, such as logistic regression or nonparametric regression, could only model
the direct relationship between the features and the prepayments. Motivated by extracting the
hidden states of loan behavior, we propose the smoothing spline state space (QuadS) model based
on a hidden Markov model with varying transition and emission matrices modeled by smoothing
splines. In contrast to existing methods, our method benefits from capturing the loans’ unobserved 
state transitions, which not only increases prediction performances but also provides more
interpretability. The overall model is learned by EM algorithm iterations, and within each iteration, 
smoothing splines are fitted with penalized least squares. Simulation studies demonstrate
the effectiveness of the proposed method. Furthermore, a real-world case study using loan data
from the Federal National Mortgage Association illustrates the practical applicability of our
model. The QuadS model not only provides reliable predictions but also uncovers meaningful,
hidden behavior patterns that can offer valuable insights for the financial industry.

<img src="illu_1.png" width="560" height="340" />


## Set environments and install packages

Required R version 4.4.1

Required R packages:

- gss

- earth

- pROC

- bigsplines

- dplyr

## Files

File `QuadS.R` is the R code for generating loan prepayment data and analysis with the QuadS method.

File `functions/model.R` contains the code for the QuadS algorithm and the loan prepayment data generation procedure.

File `functions/forward_backward.R` and `functions/function.R` contain the auxiliary function for the QuadS algorithm.


## Contact us

Website: https://haoranlustat.github.io/
