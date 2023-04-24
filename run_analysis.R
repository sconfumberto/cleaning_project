#load packages
library (readr)
library (dplyr)
library (stringr)

#reads the .txt file "test"
test <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/test/X_test.txt", col_names = "measurements")

#reads the .txt file "train"
train <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/train/X_train.txt", col_names = "measurements")

#reads the feature file and splits it into two columns of number and type of feature
features <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/features.txt", col_names = FALSE)
features_split <- separate (features, col = 1, into = c("index", "feature"), sep = " ")
featuresWanted <- grep("(mean|std)\\(\\)", features_split$feature)

#reads the .txt file for testing labels
test_labels <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/test/y_test.txt", col_names = "type_of_activity")

#reads the .txt file for training labels
train_labels <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/train/y_train.txt", col_names = "type_of_activity")

#reads the .txt file for testing subjects
test_subjects <- read_tsv("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/test/subject_test.txt", col_names = "subject_number")

#reads the .txt file for training subjects
train_subjects <- read_tsv ("/Users/sconfumberto/Documents/Data analysis/cleaning_project/UCI HAR Dataset/train/subject_train.txt", col_names = "subject_number")

#merges the test and train datasets
merged_data <- rbind (test, train)

#merges the test and train labels datasets
merged_labels <- rbind (test_labels, train_labels)

#merges the test and train subjects datasets
merged_subjects <- rbind (test_subjects, train_subjects)

#merges the merged datasets all together
merged_data_labels_subjects <- cbind (merged_subjects, merged_labels, merged_data)

#renames columns of the merged datasets
colnames (merged_data_labels_subjects) <- c("subject_number", "type_of_activity", "feature")

#separates the feature columns into separate columns each with one measurement 
separated_mdls <- separate (merged_data_labels_subjects, feature, into = paste0 ("col", 1:561), sep = " ")

#takes the description of the type of feature from the features_split data frame and creates a character vector
#to be used to match to the colums of the data frame with all the subjects, measurements, and type of activity 
vector_features <- as.character(features_split[["feature"]])

#renames columns starting from the third column
sep_with_correct_features <- c("subject_number", "type_of_activity", vector_features)
names(separated_mdls) <- sep_with_correct_features

#this adds 2 to the vector of indexes of the features wanted since the one I did before cannot be used on a merged dataframe
features_wanted_new_index <- featuresWanted + 2

#this keeps only the column of the data frame which match the vector of indexes with the features wanted,
#plus selects also the first two columns, containing the information on subject_number and type_of_activity
mdls_selected <- separated_mdls [, c(1:2, features_wanted_new_index)]

#creates a vector with the activity names
activity_names <- c("walking", "walking_down", "walking_up", "sitting", "standing", "laying")

#renames the values in the activity column
mdls_selected$type_of_activity <- factor(mdls_selected$type_of_activity, levels = 1:length(activity_names), labels = activity_names)

#creates a tidy data frame
tidy_mdls <- mdls_selected %>%
  mutate (across(c(3:68), as.numeric)) %>%
  group_by(subject_number, type_of_activity) %>%
  summarize_all (mean, na.rm = TRUE)

#creates a file with the tidy dataset required
fwrite(tidy_mdls, file = "tidy_mdls.txt", quote = FALSE)