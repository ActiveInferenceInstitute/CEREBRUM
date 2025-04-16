# CEREBRUM Example: Linear Regression through Linguistic Cases

This document demonstrates how the CEREBRUM framework applies linguistic cases to analyze a fundamental statistical model: Linear Regression. It draws upon the concepts and structure presented in `src/tests/test_linear_regression.py`.

## Introduction to Linear Regression

**Linear Regression** is a supervised learning algorithm used to model the linear relationship between a dependent variable (output) and one or more independent variables (inputs). The goal is typically to find the best-fitting straight line (or hyperplane in higher dimensions) through the data points.

The model is defined by the equation:
\[ y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + ... + \beta_n x_n + \epsilon \]
Where:
- \( y \) is the dependent variable.
- \( x_1, ..., x_n \) are the independent variables.
- \( \beta_0 \) is the intercept (bias).
- \( \beta_1, ..., \beta_n \) are the coefficients (weights) representing the change in \(y\) for a one-unit change in the corresponding \(x\).
- \( \epsilon \) is the error term, representing unexplained variance.

Training involves finding the optimal values for \( \beta_0, ..., \beta_n \) that minimize a loss function, typically the Mean Squared Error (MSE) between the predicted and actual \(y\) values.

## CEREBRUM Cases for Linear Regression

While seemingly simple, Linear Regression involves multiple facets: data input, parameter estimation, prediction, evaluation, and analysis of its properties. CEREBRUM uses linguistic cases to isolate and examine these different roles and processes, as explored in `test_linear_regression.py`:

### 1. NOMINATIVE Case: The Model as Parameter Estimator

- **Linguistic Meaning:** Subject/Doer of action.
- **Linear Regression Context:** Focuses on the model *actively fitting* the data to estimate the optimal parameters (\(\beta\) coefficients). The model *performs the action* of estimation.
- **Test Focus:** Simulating the parameter estimation process (e.g., using Gradient Descent or Ordinary Least Squares). Visualizations might show the path of coefficients converging towards their optimal values in parameter space or the evolution of the regression line fitting the data over iterations.
- **Why it Matters:** Understanding the core optimization or calculation process that determines the model's parameters.

### 2. ACCUSATIVE Case: The Model as Object of Evaluation

- **Linguistic Meaning:** Direct object/Receiver of action.
- **Linear Regression Context:** Focuses on the model *being evaluated* based on its performance on unseen data. The model is the *object* of scrutiny.
- **Test Focus:** Calculating performance metrics like MSE, R-squared, MAE on a test set. Visualizations often include scatter plots of predicted vs. actual values, residual plots (errors vs. predicted values), or histograms of the errors.
- **Why it Matters:** Assessing the model's predictive accuracy and generalization ability.

### 3. DATIVE Case: The Model as Recipient of Data

- **Linguistic Meaning:** Indirect object/Recipient.
- **Linear Regression Context:** Focuses on the model *receiving* the input data (X and y) for training or prediction. How does the model handle the incoming data?
- **Test Focus:** Analyzing the input data itself (distributions, correlations) and how it's fed *to* the model. Visualizations might include scatter plots of the raw data, pair plots for multiple features, or animations showing data points being presented to the model during training.
- **Why it Matters:** Understanding the influence of the input data characteristics on the model.

### 4. GENITIVE Case: The Model's Parameters and Properties

- **Linguistic Meaning:** Possessive/Source.
- **Linear Regression Context:** Focuses on the inherent properties *of* the fitted model, particularly its estimated parameters (\(\beta\) coefficients) and their interpretation (e.g., confidence intervals, significance).
- **Test Focus:** Examining the values, standard errors, and confidence intervals of the estimated \(\beta\) coefficients. Visualizations might include bar charts of coefficient values or plots showing confidence intervals.
- **Why it Matters:** Interpreting what the model has learned about the relationships between variables.

### 5. INSTRUMENTAL Case: The Model as a Tool for Analysis/Prediction

- **Linguistic Meaning:** By means of/Using.
- **Linear Regression Context:** Views the fitted model as an *instrument* or *method* used to make predictions or analyze relationships.
- **Test Focus:** Using the model to predict outcomes for new data points. Analyzing the model equation itself as a tool. Visualizations might demonstrate the model being used to draw the regression line or predict values for specific inputs.
- **Why it Matters:** Highlighting the practical application of the trained model.

### 6. LOCATIVE Case: The Model's Position in Parameter Space

- **Linguistic Meaning:** In/At/Within (Location).
- **Linear Regression Context:** Focuses on the final set of estimated parameters (\(\beta\) values) as a specific point *within* the multi-dimensional parameter space.
- **Test Focus:** Visualizing the location of the optimal parameters within the parameter space, potentially alongside the loss function landscape (e.g., contour plots of MSE for different \(\beta\) combinations).
- **Why it Matters:** Understanding the solution in the context of all possible parameter values.

### 7. ABLATIVE Case: The Model as Source of Predictions/Errors

- **Linguistic Meaning:** From/Out of/Because of.
- **Linear Regression Context:** Focuses on the model as the *source* from which predictions and prediction errors originate.
- **Test Focus:** Analyzing the distribution and characteristics of the residuals (errors) generated *by* the model. Investigating assumptions like normality and homoscedasticity of errors. Visualizations typically include residual plots and Q-Q plots.
- **Why it Matters:** Diagnosing potential problems with the model fit and validating its underlying assumptions.

### 8. VOCATIVE Case: Addressing/Querying the Model

- **Linguistic Meaning:** Direct address/Invocation.
- **Linear Regression Context:** Views the model as an entity that can be directly addressed or queried, perhaps interactively.
- **Test Focus:** Simulating specific queries, like "*Model*, what is the prediction for input X?" or analyzing its sensitivity to specific input changes. Visualizations might involve interactive plots or specific scenario analyses.
- **Why it Matters:** Interacting with the model for specific insights or 'what-if' analyses.

## Demonstration (`test_linear_regression.py`)

The test suite structure reflects these cases:
1.  **Data Generation:** Creates simple synthetic linear data (`DataGenerator.linear_data`).
2.  **Case Functions:** Dedicated functions (e.g., `test_nominative_case`, `test_accusative_case`, often located in sub-modules like `src/tests/linear_regression_cases/`) implement the logic and visualizations for each case.
3.  **Orchestration:** The `run_all_case_tests` function in the main test file calls each case-specific test function.
4.  **Output Generation:** Each case function generates relevant plots and potentially animations, saving them into case-specific subdirectories within `src/tests/output/linear_regression/` (e.g., `nominative/`, `accusative/`). These visualizations serve to "Show, not Tell" the aspect of linear regression highlighted by that specific CEREBRUM case.
    - Parameter convergence (NOMINATIVE)
    - Predicted vs. Actual plots, Residual plots (ACCUSATIVE, ABLATIVE)
    - Data scatter plots (DATIVE)
    - Coefficient plots (GENITIVE)
    - Regression line plots (INSTRUMENTAL)
    - Parameter space/Loss contours (LOCATIVE)

## Conclusion

Applying the CEREBRUM framework to Linear Regression illustrates how even simple models have distinct functional aspects that can be analyzed separately. By adopting the perspective of each linguistic case (model as estimator, object of evaluation, data recipient, tool, etc.), we gain a more comprehensive understanding than by looking at the model monolithically. The tests and visualizations generated provide concrete examples of how each case illuminates a different part of the linear regression process, from data intake to parameter estimation, evaluation, and interpretation. 