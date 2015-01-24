library(dplyr) # load dplyr library

# function that convert activity code to descriptive activity label
code_to_name <- function(x) {
  names <- c('walking', 'walking_upstairs','walking_downstairs', 'sitting',
             'standing','laying')
  names[x]
}

# load feature vector
features <- read.table('features.txt',
                       colClasses = c('numeric', 'character'))$V2
# because '()' and '-' may cause problem when using as column name, substitute '()' with
# '', substitute '-' with '_'
features <- gsub('()', '', features, fixed = TRUE)
features <- gsub('-', '_', features, fixed = TRUE)
# load activity vector and convert code to descriptive label
activity <- c(read.table('y_test.txt')$V1, read.table('y_train.txt')$V1) %>%
            sapply(code_to_name)
# load subject vector
subject <- c(read.table('subject_test.txt')$V1, read.table('subject_train.txt')$V1)
# load test data and convert it to data frame tbl for dplyr to use
test_data  <- read.table('X_test.txt', col.names = features)  %>% tbl_df
# load train data and convert it to data frame tbl for dplyr to use
train_data <- read.table('X_train.txt', col.names = features) %>% tbl_df
# start cleanning data
data <- rbind_list(test_data, train_data) %>% # merge test data and train data
        # only select the column that produced by mean() function or std() function
        select(matches('mean|std', ignore.case = FALSE), -contains('meanFreq')) %>%
        # add two column: activity and subject
        mutate(activity, subject) %>%
        # group data by activity and subject
        group_by(activity, subject) %>%
        # summarise each column with mean() function
        summarise_each(funs(mean)) %>%
        # write data to file
        write.table(file = 'tidy_data.txt', row.name=FALSE)
