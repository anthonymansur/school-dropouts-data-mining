# load libraries
library(glmnetUtils)                   # to run ridge, lasso, and net elastic
library(broom)                         # to save ols summary table
library(tidyverse)

source("code/functions/plot_glmnet.R")

# read in the training data
dropout_train = read_tsv("data/clean/dropout-train.tsv") %>%
  # remove the data we won't be regressing on
  select(-num_of_schools, 
         -fips, 
         -county, 
         -state, 
         -mean_experience, 
         -mean_salary)

##--------------------------------------------------------------------
##                      Ordinary Least Squares                       -
##--------------------------------------------------------------------

# run ordinary least squares
lm_fit = lm(formula=dropout_rate ~ ., data = dropout_train)

# save the ols object and summary
write.csv(tidy(lm_fit), "results/ols-summary.csv")
save(lm_fit, file = "results/lm_fit.rda")

##-------------------------------------------------------------
##                      Ridge Regression                      -
##-------------------------------------------------------------

# run ridge regression analysis
set.seed(1)
ridge_fit = cv.glmnet(dropout_rate ~ .,       # formula notation
                      alpha = 0,              # alpha = 0 for ridge
                      nfolds = 10,            # number of folds
                      data = dropout_train)   # data to run ridge on

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
p = plot_glmnet(ridge_fit, dropout_train, features_to_plot = 7)
ggsave(filename = "results/ridge-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

##-------------------------------------------------------------
##                      Lasso Regression                      -
##-------------------------------------------------------------

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(dropout_rate ~ .,       # formula notation
                      alpha = 1,              # alpha = 1 for lasso
                      nfolds = 10,            # number of folds
                      data = dropout_train)   # data to run lasso on

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, dropout_train, features_to_plot = 5)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, dropout_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/lasso-features-table.tsv")

##--------------------------------------------------------------------
##                      Elastic Net Regression                       -
##--------------------------------------------------------------------

# run elastic net regression
set.seed(1)
elnet_fit = cva.glmnet(dropout_rate ~ .,       # formula notation, as usual
                       nfolds = 10,            # number of folds
                       data = dropout_train)   # data to run on

# create minimum CV error for each value of alpha
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/elnet-min-cv-plot.png")
plot_cva_glmnet(elnet_fit)
dev.off()

# Extract optimal alpha
elnet_fit_best = extract_best_elnet(elnet_fit)

# save the elnet best fit object
save(elnet_fit_best, file="results/elnet_fit.rda")

# create elnet CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/elnet-cv-plot.png")
plot(elnet_fit_best)
dev.off()

# create elnet trace plot
p = plot_glmnet(elnet_fit_best, dropout_train, features_to_plot = 6)
ggsave(filename = "results/elnet-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)
