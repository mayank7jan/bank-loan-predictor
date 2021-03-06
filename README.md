
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Website
shields.io](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)
[![made-with-Rshiny](https://img.shields.io/badge/Made%20with-RShiny-blue?style=plastic&logo=r)](https://shiny.rstudio.com/)
[![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.svg)](http://commonmark.org)
[![made-with-latex](https://img.shields.io/badge/Made%20with-LaTeX-1f425f.svg)](https://www.latex-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Open Source?
Yes\!](https://badgen.net/badge/Open%20Source%20%3F/Yes%21/blue?icon=github)]()

<!-- badges: end -->

# Predicting the success of Bank Telemarketing

This project uses actual data from a Portuguese bank which uses a data
driven approach to predict the success of telemarketing calls for
selling long-term bank deposits.

You can read much more about this data (released to the UCI Machine
Learning community) by clicking
[here](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#)

## Data Set Information

The data is related with direct marketing campaigns of a Portuguese
banking institution. The marketing campaigns were based on phone calls.
Often, more than one contact to the same client was required, in order
to access if the product (bank term deposit) would be (‘yes’) or not
(‘no’) subscribed. You can read more on the dataset by clicking
[here](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#)

### Attribute Information

#### Input variables

**bank client data:**

1.  age (numeric)
2.  job : type of job
    (categorical:‘admin.’,‘blue-collar’,‘entrepreneur’,‘housemaid’,‘management’,‘retired’,‘self-employed’,‘services’,‘student’,‘technician’,‘unemployed’,‘unknown’)
3.  marital : marital status (categorical:
    ‘divorced’,‘married’,‘single’,‘unknown’; note:
    ‘divorced’ means divorced or widowed)
4.  education (categorical:
    ‘basic.4y’,‘basic.6y’,‘basic.9y’,‘high.school’,‘illiterate’,‘professional.course’,‘university.degree’,‘unknown’)
5.  default: has credit in default? (categorical: ‘no’,‘yes’,‘unknown’)
6.  housing: has housing loan? (categorical: ‘no’,‘yes’,‘unknown’)
7.  loan: has personal loan? (categorical: ‘no’,‘yes’,‘unknown’)

**related with the last contact of the current campaign:**

8.  contact: contact communication type (categorical:
    ‘cellular’,‘telephone’)
9.  month: last contact month of year (categorical: ‘jan’, ‘feb’, ‘mar’,
    …, ‘nov’, ‘dec’)
10. day\_of\_week: last contact day of the week (categorical:
    ‘mon’,‘tue’,‘wed’,‘thu’,‘fri’)
11. duration: last contact duration, in seconds (numeric).

<!-- end list -->

  - *Important note:* this attribute highly affects the output target
    (e.g., if duration=0 then y=‘no’). Yet, the duration is not known
    before a call is performed. Also, after the end of the call y is
    obviously known. Thus, this input should only be included for
    benchmark purposes and should be discarded if the intention is to
    have a realistic predictive model.

**other attributes:**

12. campaign: number of contacts performed during this campaign and for
    this client (numeric, includes last contact)
13. pdays: number of days that passed by after the client was last
    contacted from a previous campaign (numeric; 999 means client was
    not previously contacted)
14. previous: number of contacts performed before this campaign and for
    this client (numeric)
15. poutcome: outcome of the previous marketing campaign (categorical:
    ‘failure’,‘nonexistent’,‘success’)

**social and economic context attributes:**

16. emp.var.rate: employment variation rate - quarterly indicator
    (numeric)
17. cons.price.idx: consumer price index - monthly indicator (numeric)
18. cons.conf.idx: consumer confidence index - monthly indicator
    (numeric)
19. euribor3m: euribor 3 month rate - daily indicator (numeric)
20. nr.employed: number of employees - quarterly indicator (numeric)

**Output variable (desired target):**

21. y - has the client subscribed a term deposit? (binary: ‘yes’,‘no’)

## Project Goal

TO create a Data Science Dashboard or a Data Science web application
product to be used in real-time by a salesperson.

The app would allow a salesperson to input the specific details of the
customer they are talking to, and receive a prediction from each of the
underlying models. Based on this prediction, the salesperson could make
targeted offers to the customer in real-time. Thus, resulting in higher
conversion ratios for the sales team.

## Machine Learning Algorithms used

Our R Shiny Data Science dashboard uses following three Machine Learning
algorithms to aid in decision making. The apps build the ML models and
then for each model, it shows visualizations of the model and its
fitness for purpose in the dashbaord.

### 1\. Decision Tree

Create a decision tree which can predict class membership of the “y”
variable. The final model used for the DT was decided after running the
simulations on train and test data through multiple DT variations.

### 2\. Logistic Regression

Logistic Regression helps to estimate of the probability that a given
customer will respond. The goal is to create the best model we can to
predict the probability a customer will take up the offer.

The marketing team will then use this model, and thus your probability
estimate, to decide who to mail information out to. Clearly, the better
the prediction, the less the cost of running the campaign.

### 3\. K-Nearest Neighbours Model

Although Decision Trees and Logistic Regression models are good models
to predict a user’s behaviour pattern. We go the extra mile by finding
nearest similar users using knn classification to determine the
probability that a given customer will take up the offer.

Thus, a sales person can read the probability from all the three models
and change his/her loan offer accordingly in real time.

## Website

The Data Science dashbaord is deployed and live at
[shinyapps.io](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

## [Dashboard Screenshots](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![prediction-page](images/prediction.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![prediction-calc-page](images/prediction_calc.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![decisionTree-summary-page](images/dtSummary.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![decisionTree-accuracy-page](images/dtAM.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![logistic-regression-page](images/lr.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![knn-page](images/knn.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![dataset-page](images/dataset.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![attribute-page](images/attr.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)

[![glossary-page](images/glossary.png)](https://mayankagrawalbond.shinyapps.io/bank-loan-predictor/)
