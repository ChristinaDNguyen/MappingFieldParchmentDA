# ------------ Analysis 4: Conservators v. lab techniques --------------
# ------------ How many techniques are used by conservators only? by labs only? by both labs and conservators together?  --------------

library(tidyverse)
library(reshape2)
library(readxl)

# Read in the data
df <- read_excel("4_raw_data_conservator_v_lab.xlsx")
df # checking that the data read in properly

#  Create a group column based on conservator/lab involvement. 
# One group is for when conservators are the only ones on that paper;
# one group is for when labs are the only ones on that paper;
# one group is for when both conservators and labs are on that paper.

df$Group <- case_when(
  df[[2]] == 1 & df[[3]] == 0 ~ "Conservator Only",
  df[[2]] == 0 & df[[3]] == 1 ~ "Lab Only",
  df[[2]] == 1 & df[[3]] == 1 ~ "Both",
  TRUE ~ "Neither"
)

# Prepare the method usage data (columns 4 onward)
method_data <- df[, 4:ncol(df)] # column 4 onward to the end of the dataframe is method_data
method_names <- colnames(method_data) # the method_names should be the column's names 
method_data #checking that the columns have been properly generated

# Add Group info for comparison
method_data$Group <- df$Group

# Summarize method use by group - conservator only, lab only, both conservator and lab.
summary_table <- method_data %>%
  group_by(Group) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

summary_table #now save it as a csv.
write.csv(summary_table, "summary_table.csv", row.names = FALSE)
