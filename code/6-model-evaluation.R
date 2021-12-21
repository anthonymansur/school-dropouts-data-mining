# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
dropout_test = read_tsv("data/clean/dropout-test.tsv") %>%
  # remove the data we won't be regressing on
  select(-num_of_schools, 
         -fips, 
         -county, 
         -state, 
         -mean_experience, 
         -mean_salary)
  
# load ordinary least square fit object
load("results/lm_fit.rda")

# load ridge fit object
load("results/ridge_fit.rda")

# load lasso fit object
load("results/lasso_fit.rda")

# load elastic net regression object
load("results/elnet_fit.rda")

# load random forest fit object
load("results/rf_fit.rda")

# load boosting fit object
load("results/gbm_fit.rda")

# evaluate intercept-only rmse
m = mean(dropout_train$dropout_rate)
intercept_rmse = 
  round(sqrt(mean((m - dropout_test$dropout_rate)^2)),3)

# evaluate ols rmse
linear_predictions = predict(lm_fit, newdata = dropout_test)
linear_rmse = 
  round(sqrt(mean((linear_predictions - dropout_test$dropout_rate)^2)),3)

# evaluate ridge rmse
ridge_predictions = predict(ridge_fit,              # fit object
                            newdata = dropout_test, # use test data
                            s = "lambda.1se",       # value of lamda to use
                            type = "response") %>%  # to output probabilities
  as.numeric()  
ridge_rmse = 
  round(sqrt(mean((ridge_predictions - dropout_test$dropout_rate)^2)),3)

# evaluate lasso rmse 
lasso_predictions = predict(lasso_fit,              # fit object
                            newdata = dropout_test, # use test data
                            s = "lambda.1se",       # value of lamda to use
                            type = "response") %>%  # to output probabilities
  as.numeric()                                        # convert to vector
lasso_rmse = 
  round(sqrt(mean((lasso_predictions - dropout_test$dropout_rate)^2)),3)

# evaluate elastic net regression rmse
elnet_predictions = predict(elnet_fit, 
                            alpha = elnet_fit$alpha,
                            newdata = dropout_test,
                            s = "lambda.1se") %>% as.numeric()
elnet_rmse = 
  round(sqrt(mean((elnet_predictions - dropout_test$dropout_rate)^2)),3)

# evaluate random forest rmse
rf_predictions = predict(rf_fit, newdata = dropout_test)
rf_rmse = 
  round(sqrt(mean((rf_predictions - dropout_test$dropout_rate)^2)),3)

# evaluate boosting rmse
gbm_predictions = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          newdata = dropout_test)
gbm_rmse = 
  round(sqrt(mean((gbm_predictions - dropout_test$dropout_rate)^2)),3)

# save rmse table
tibble(Method = c("Intercept-only",
                  "Ordinary Least Squares",
                  "Ridge Regression", 
                  "Lasso Regression", 
                  "Elastic Net Regression", 
                  "Random Forest",
                  "Boosting"), 
       `Test RMSE` = c(intercept_rmse,
                       linear_rmse,
                       ridge_rmse,
                       lasso_rmse,
                       elnet_rmse,
                       rf_rmse,
                       gbm_rmse)) %>%
  write_tsv("results/model-evaluation.tsv")
