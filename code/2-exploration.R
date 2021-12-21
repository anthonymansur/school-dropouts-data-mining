# load libraries
library(tidyverse)
library(cowplot)

# read in the cleaned data 
dropout_data = read_tsv("data/clean/dropout-data.tsv")

# create histogram of dropout rate
boxplot = dropout_data %>%
  ggplot(aes(x=dropout_rate)) +
  geom_boxplot() +
  labs(x = "Dropout rate") + 
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "results/response-boxplot.png",
       plot = boxplot,
       device = "png",
       width = 5,
       height = 3)

# examine top 10 counties indicated by this dataset
top_counties = dropout_data %>% 
  arrange(desc(dropout_rate)) %>% 
  select(county, state, dropout_rate) %>%
  head(10) %>%
  rename(County = county, 
         State = state,
         `Dropout Rate` = dropout_rate) %>%
  write_tsv("results/top-10-counties.tsv")

# Feature variation
histogram = function(variable, name){
  p = dropout_data %>%
    ggplot(aes(x=variable)) +
    geom_histogram() +
    labs(x = name) + 
    theme_bw() + theme(legend.position = "none")
  return(p)
}

p_married = histogram(dropout_data$married, "Married")
p_single_mom = histogram(dropout_data$single_mom, "Single Mother")
p_gini_index = histogram(dropout_data$gini_index, "Gini Index")
p_per_capita_income = histogram(dropout_data$per_capita_income, "Per Capita Income")
p_welfare = histogram(dropout_data$welfare, "Welfare")
p_educated = histogram(dropout_data$educated, "Educated")
p_advanced_educ = histogram(dropout_data$advanced_educ, "Teacher: Adv. Educ.")
p_exp_1_5 = histogram(dropout_data$exp_1_5, "Teacher: 1-5 yrs of exp.")
p_exp_6_10 = histogram(dropout_data$exp_6_10, "Teacher: 6-10 yrs of exp.")
p_exp_11_20 = histogram(dropout_data$exp_11_20, "Teacher: 11-20 yrs of exp.")
p_exp_21_30 = histogram(dropout_data$exp_21_30, "Teacher: 21-30 yrs of exp.")
p_exp_gt_30 = histogram(dropout_data$exp_gt_30, "Teacher: >30 yrs of exp.")
p_mbsal_1_5 = histogram(dropout_data$mbsal_1_5, "Salary: 1-5 yrs of exp.")
p_mbsal_6_10 = histogram(dropout_data$mbsal_6_10, "Salary: 5-10 yrs of exp.")
p_mbsal_11_20 = histogram(dropout_data$mbsal_11_20, "Salary: 11-20 yrs of exp.")
p_mbsal_21_30 = histogram(dropout_data$mbsal_21_30, "Salary: 21-30 yrs of exp.")
p_mbsal_gt_30 = histogram(dropout_data$mbsal_gt_30, "Salary: >30 yrs of exp.")

feature_variation = plot_grid(p_married,
          p_single_mom,
          p_gini_index,
          p_per_capita_income,
          p_welfare,
          p_educated,
          p_advanced_educ,
          p_exp_1_5,
          p_exp_6_10,
          p_exp_11_20,
          p_exp_21_30,
          p_exp_gt_30,
          p_mbsal_1_5,
          p_mbsal_6_10,
          p_mbsal_11_20,
          p_mbsal_21_30,
          p_mbsal_gt_30,
          ncol=3)

ggsave(filename = "results/feature-variation.png",
       plot = feature_variation,
       device = "png",
       width = 8,
       height = 10)


# examine the correlation of explanatory variables 
# Demographics
demographic_variables = dropout_data %>%
  select(married, 
         single_mom,
         advanced_educ,
         exp_1_5,
         exp_6_10,
         exp_11_20,
         exp_21_30,
         exp_gt_30)

demographic_cor = cor(demographic_variables)

# Economic
economic_variables = dropout_data %>%
  select(gini_index,
         per_capita_income,
         welfare,
         mbsal_1_5,
         mbsal_6_10,
         mbsal_11_20,
         mbsal_21_30,
         mbsal_gt_30)

economic_cor = cor(economic_variables)

# total 
total_variables = dropout_data %>%
  select(gini_index,
         per_capita_income,
         welfare,
         mbsal_1_5,
         mbsal_6_10,
         mbsal_11_20,
         mbsal_21_30,
         mbsal_gt_30,
         married, 
         single_mom,
         advanced_educ,
         exp_1_5,
         exp_6_10,
         exp_11_20,
         exp_21_30,
         exp_gt_30)
total_cor = cor(total_variables)

plot1 = ggcorrplot(demographic_cor)
plot2 = ggcorrplot(economic_cor)
plot3 = ggcorrplot(total_cor)

# save the correlation plots
ggsave(filename = "results/category-correlation.png", 
       plot = plot_grid(plot1, plot2), 
       device = "png",
       width = 10,
       height = 5)
ggsave(filename = "results/total-correlation.png", 
       plot = plot3, 
       device = "png")

# Explanatory variables with response
dropout_plot = function(variable, name){
  p = dropout_data %>%
    ggplot(aes(x=variable, y=dropout_rate)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    labs(x=name, y="Dropout Rate") + 
    theme_bw()
  return(p)
}

d_gini = dropout_plot(dropout_data$gini_index, "Gini Index")
d_salary = dropout_plot(dropout_data$mean_salary, "Teacher Mean Salary")
d_exp = dropout_plot(dropout_data$mean_experience, "Teacher Mean Experience")
d_educ = dropout_plot(dropout_data$educated, 
                      "% of pop. with high school degree or higher")
d_welfare = dropout_plot(dropout_data$welfare, "% of pop. under welfare")
d_income = dropout_plot(dropout_data$per_capita_income, "Log of Income per capita")

d_plot = plot_grid(
          d_gini,
          d_salary,
          d_exp,
          d_educ,
          d_welfare,
          d_income,
          ncol=3)
ggsave(filename = "results/explanatory-variables.png", 
       plot = d_plot, 
       device = "png",
       width = 10,
       height = 5)