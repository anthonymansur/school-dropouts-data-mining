# load libraries
library(tidyverse)

##------------------------------------------------------------
##                      County Dataset                       -
##------------------------------------------------------------
# Load raw county data
county_data = left_join(read_csv("./data/raw/county2.csv"),
                        read_csv("./data/raw/county1.csv"),
                        by="GISJOIN")

# Select and modify the variables we want to look into
county_data = county_data %>%
  # extract the county identifier
  mutate(fips = paste(STATEA.x,COUNTYA.x,sep="")) %>%
  # select the variables we will be using
  select(c(fips,     # county id
           STATE.x, 
           COUNTY.x, 
           R3FE001,  # gini index
           RPAE001,  # per capita income
           RKYE001,  # total
           RKYE002, # -male pop.
           RLAE001,  # total
           RLAE002, # -white pop.
           RLAE003, # -black pop.
           RLAE005,  # -asian pop.
           RLIE001,  # total
           RLIE012, # -hispanic pop.
           RL4E001,  # total
           RL4E003, # -married household
           RL4E006, # -single mom
           ROAM001,  # total housing units
           ROAM002,  # -housing units w/ public assistance
           RM8E001,  # total
           RM8E011, # highschool male
           RM8E012,
           RM8E013,
           RM8E014,
           RM8E015,
           RM8E016,
           RM8E017,
           RM8E018, # doctors male
           RM8E028, # highschool female
           RM8E029,
           RM8E030,
           RM8E031,
           RM8E032,
           RM8E033,
           RM8E034,
           RM8E035  # doctors female
  )) %>%
  # Remove invalid data
  filter(ROAM002 < ROAM001) %>%
  # Convert into percentages
  mutate(male = round(RKYE002/RKYE001*100, 1),
         white = round(RLAE002/RLAE001*100, 1),
         black = round(RLAE003/RLAE001*100, 1),
         asian = round(RLAE005/RLAE001*100, 1),
         hispanic = round(RLIE012/RLIE001*100, 1),
         married = round(RL4E003/RL4E001*100, 1),
         single_mom = round(RL4E006/RL4E001*100, 1),
         welfare = round(ROAM002/ROAM001*100, 1),
         educated = round((RM8E011+ 
                             RM8E012+
                             RM8E013+
                             RM8E014+
                             RM8E015+
                             RM8E016+
                             RM8E017+
                             RM8E018+
                             RM8E028+
                             RM8E029+
                             RM8E030+
                             RM8E031+
                             RM8E032+
                             RM8E033+
                             RM8E034)/RM8E001*100, 1)
  ) %>%
  # Rename unmodified variables
  rename(gini_index = R3FE001,
         per_capita_income = RPAE001,
         state = STATE.x,
         county = COUNTY.x) %>%
  mutate(per_capita_income = log(per_capita_income)) %>%
  # Select the modified variables we will use
  select(fips, 
         state,
         county,
         male, 
         white, 
         black, 
         asian, 
         hispanic, 
         married, 
         single_mom, 
         gini_index,
         per_capita_income,
         welfare, 
         educated)

##------------------------------------------------------------
##              Teacher Compensation Dataset                 -
##------------------------------------------------------------
# Load in the teacher compensation data
teacher_data = read_tsv("./data/raw/teacher-comp.txt")

# Rename the variables
teacher_data = teacher_data %>%
  rename(leaid = LEAID,
         bachelors_only = PCT_BACH,
         advanced_educ = PCT_ADVC,
         mean_experience = M_EXP,
         exp_1_5 = P_EX_1_5,
         exp_6_10 = P_EX6_10,
         exp_11_20 = P_EX1120,
         exp_21_30 = P_EX2130,
         exp_gt_30 = P_EXGT30,
         mean_salary = M_BSAL,
         mbsal_1_5 = MBSAL1_5,
         mbsal_6_10 = MBSAL610,
         mbsal_11_20 = MBSA1120,
         mbsal_21_30 = MBSA2130,
         mbsal_gt_30 = MBSAGT30)

# Filter out rows with incomplete data
teacher_data = teacher_data %>% 
  filter(across(everything(), ~ . >= 0))

##-----------------------------------------------
##              Dropout Dataset                 -
##-----------------------------------------------
# Load in dropout dataset
dropout_data = read_tsv("./data/raw/dropout.txt")

# Extract our response variable 
dropout_data = dropout_data %>%
  select(leaid, drp912) %>%
  rename(dropout_rate = drp912) %>% 
  filter(dropout_rate >= 0)

##-------------------------------------------------------------------
##              Local Education Agency County Mapping               -
##-------------------------------------------------------------------
# Load in local education agency dataset
lea_county_data = read_tsv("./data/raw/lea-county.txt")

# Select the mapping we are interested in
lea_county_data = lea_county_data %>%
  rename(fips = COUNTYID07) %>%
  filter(fips != "N") %>%
  select(fips, leaid)


##----------------------------------------------
##              Joining datasets               -
##----------------------------------------------
# Join dropout and teacher dataset by leaid
dropout_data = left_join(teacher_data,dropout_data, by="leaid")

# filter out any rows that didn't have teacher data
dropout_data = dropout_data %>% drop_na(dropout_rate)

# Combine with the lea to county mapping
dropout_data = left_join(dropout_data, lea_county_data, by="leaid")

#filter out any rows with no county mapping
dropout_data = dropout_data %>% drop_na(fips)

# Summarize the data from each lea-level to county-level scope
dropout_data = dropout_data %>%
  group_by(fips) %>%
  summarise(bachelors_only = round(mean(bachelors_only),1),
            advanced_educ = round(mean(advanced_educ),1),
            mean_experience = round(mean(mean_experience),1),
            mean_salary = as.integer(mean(mean_salary),1),
            exp_1_5 = round(mean(exp_1_5),1),
            exp_6_10 = round(mean(exp_6_10),1),
            exp_11_20 = round(mean(exp_11_20),1),
            exp_21_30 = round(mean(exp_21_30),1),
            exp_gt_30 = round(mean(exp_gt_30),1),
            mbsal_1_5 = round(mean(mbsal_1_5),1),
            mbsal_6_10 = round(mean(mbsal_6_10),1),
            mbsal_11_20 = round(mean(mbsal_11_20),1),
            mbsal_21_30 = round(mean(mbsal_21_30),1),
            mbsal_gt_30 = round(mean(mbsal_gt_30),1),
            dropout_rate = round(mean(dropout_rate),1))

# Reorder columns with county data first, then teacher compensation, 
# and end with response variable
dropout_data = dropout_data %>%
  left_join(county_data, by="fips") %>%
  select(fips,
         state,
         county,
         #male, 
         #white, 
         #black, 
         #asian, 
         #hispanic, 
         married, 
         single_mom, 
         gini_index,
         per_capita_income,
         welfare, 
         educated,
         #bachelors_only,
         advanced_educ,
         mean_experience, # not to be added to prediction modeling
         mean_salary, # not to be added to prediction modeling
         exp_1_5,
         exp_6_10,
         exp_11_20,
         exp_21_30,
         exp_gt_30,
         mbsal_1_5,
         mbsal_6_10,
         mbsal_11_20,
         mbsal_21_30,
         mbsal_gt_30,
         dropout_rate
  ) %>%
  # filter extreme outliers
  filter(dropout_rate < 40) %>% 
  # remove nan values
  drop_na()

# write file to directory
write_tsv(dropout_data, file = "./data/clean/dropout-data.tsv")

