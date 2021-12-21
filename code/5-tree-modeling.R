# load libraries
library(randomForest)  # to run random forest
library(gbm)           # to run boosting
library(tidyverse)

# read in the training data
dropout_train = read_tsv("data/clean/dropout-train.tsv") %>%
  # remove the data we won't be regressing on
  select(-fips, 
         -county, 
         -state, 
         -mean_experience, 
         -mean_salary)

##-----------------------------------------------------------
##                      Random Forest                       -
##-----------------------------------------------------------

# run random forest
set.seed(1)
rf_fit = randomForest(dropout_rate ~ ., data = dropout_train)

# calculate oob error
oob_error = tibble(ntree = 1:500, oob_err = rf_fit$mse)

# save oob error plot 
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/rf-ntree-oob-err.png")
oob_error %>% 
  ggplot(aes(x = ntree, y = oob_err)) + geom_line() + theme_bw()
dev.off()

# Find which value of m minimizes error (we see it's the default floor(p/3))
mvalues = seq(1,17, by = 2) 
oob_errors = numeric(length(mvalues)) 
ntree = 200
for(idx in 1:length(mvalues)){
  set.seed(idx)
  m = mvalues[idx]
  rf_fit = randomForest(dropout_rate ~ ., mtry = m, data = dropout_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}

# plot this oob error
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/rf-mfeature-oob-err.png")
tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = mvalues) +
  theme_bw()
dev.off()

# visualize importance 
set.seed(1)
rf_fit = randomForest(dropout_rate ~ ., 
                      importance = TRUE, 
                      mtry = 9,
                      data = dropout_train)

png(width = 8, 
    height = 5,
    res = 300,
    units = "in", 
    filename = "results/rf-variable-importance.png")
varImpPlot(rf_fit)
dev.off()

# save the random forest fit object
save(rf_fit, file = "results/rf_fit.rda")

##------------------------------------------------------
##                      Boosting                       -
##------------------------------------------------------
set.seed(1)

# step 1: Find the optimal number of trees
gbm_fit_slow = gbm(dropout_rate ~ .,
                   distribution = "gaussian",
                   n.trees = 2000,
                   interaction.depth = 1,
                   shrinkage = 0.01,
                   cv.folds = 5,
                   data = dropout_train)
opt_num_trees = gbm.perf(gbm_fit_slow)

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/boosting-opt-tree.png")
opt_num_trees
dev.off()

# Step 2: Find the optimal interaction depth
set.seed(1)
ntrees = 1223
gbm_fit_1 = gbm(dropout_rate ~ .,
                distribution = "gaussian",
                n.trees = ntrees,
                interaction.depth = 1,
                shrinkage = 0.1,
                cv.folds = 5,
                data = dropout_train)
set.seed(2)
gbm_fit_2 = gbm(dropout_rate ~ .,
                distribution = "gaussian",
                n.trees = ntrees,
                interaction.depth = 2,
                shrinkage = 0.1,
                cv.folds = 5,
                data = dropout_train)
set.seed(4)
gbm_fit_3 = gbm(dropout_rate ~ .,
                distribution = "gaussian",
                n.trees = ntrees,
                interaction.depth = 3,
                shrinkage = 0.1,
                cv.folds = 5,
                data = dropout_train)
set.seed(4)
gbm_fit_4 = gbm(dropout_rate ~ .,
                distribution = "gaussian",
                n.trees = ntrees,
                interaction.depth = 4,
                shrinkage = 0.1,
                cv.folds = 5,
                data = dropout_train)

cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_4$cv.error, depth = 4)
)

# Save cv error
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/rf-iteration-cvv-err.png")
cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + theme_bw()
dev.off()

set.seed(1)
gbm_fit_optimal = gbm(dropout_rate ~ .,
                      distribution = "gaussian",
                      n.trees = 1500,
                      interaction.depth = 3,
                      shrinkage = 0.01,
                      cv.folds = 5,
                      data = dropout_train)
optimal_num_trees = gbm.perf(gbm_fit_optimal, plot.it = FALSE) 

# save gbm fit object
save(gbm_fit_optimal, file = "results/gbm_fit.rda")

summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE) %>%
  write_tsv("results/gbm-rel-inf-table.tsv")