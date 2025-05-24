# ------------ Analysis 2: Correlation between methods (binary grouping) ------------
# ------------ Do certain pairs of methods tend to be used with each other? ------------

library(readxl)
library(tidyverse)
library(corrplot)
library(ggcorrplot)


#Read Excel file in, and skip the first row of technique names
raw_data <- read_excel("3_raw_excel_methods_abbreviated_for_analysis_2.xlsx", skip = 1)
raw_data

#Rename first column to "Paper" and move/pipe it to rownames
#Doing this is important because we exclude from the numeric correlation calculation but we still retain it for reference.
colnames(raw_data)[1] <- "Paper" 
data <- raw_data %>% column_to_rownames("Paper")


#Convert all the data to numeric to make it easy to work with
data_numeric <- data %>% mutate(across(everything(), as.numeric))
data_numeric

#Calculate the correlation before we can make a matrix
cor_matrix <- cor(data_numeric, use = "pairwise.complete.obs", method = "pearson")


cor_matrix


#Now we have calculated the correlation matrix and stored it into the 
#cor_matrix variable, we can create a visualization!
diag(cor_matrix) <- NA

#now plot
plot <- ggcorrplot(cor_matrix,
                   method = "square",
                   type = "upper",
                   lab = FALSE,
                   colors = c("blue", "white", "red"),
                   hc.order = TRUE,  # Cluster similar methods together
                   tl.cex = 0.7,
                   tl.col = "black",
                   tl.srt = 45) +
  theme(axis.text.x = element_text(size = 5.5),
        axis.text.y = element_text(size = 5.5)) +
  labs(title = "Correlation of scientific PDA techniques in the literature")

plot


# Save the plot with a custom size
ggsave("correlation_plot.png", plot = plot, width = 12, height = 12, dpi = 300)