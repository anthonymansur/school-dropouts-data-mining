# read in the cleaned data
dropout_data = read_tsv("data/clean/dropout-data.tsv")

set.seed(5) # seed set for reproducibility (DO NOT CHANGE) 
n = nrow(dropout_data)
train_samples = sample(1:n, round(0.8*n))

# split data into training and test sets
dropout_train = dropout_data[train_samples,]
dropout_test = dropout_data[-train_samples,]

# save the train and test data
write_tsv(x = dropout_train, file = "data/clean/dropout-train.tsv")
write_tsv(x = dropout_test, file = "data/clean/dropout-test.tsv")