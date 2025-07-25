# ------------ Analysis 3: Correlation between methods, more than a binary grouping ------------
# ------------ Do certain groups (not just pairs) of methods tend to be used with each other? ------------

# 1. Load libraries 
install.packages(c("readxl", "proxy", "dendextend"))
library(readxl)
library(proxy)
library(dendextend)

# 2. Read Excel file in.
file_path <- "3_raw_excel_methods_abbreviated_for_analysis_2.xlsx"
df <- read_excel(file_path)
df

# 3. Prepare binary matrix (rows = papers, cols = techniques) 
df_binary <- df[, -1]  # Remove paper title column
df_binary
rownames(df_binary) <- df[[1]]  #Setting the paper titles as row names

# Transpose so techniques are rows
df_t <- t(df_binary)
df_t

#4. Calculate Jaccard distance
dist_matrix <- dist(df_t, method = "Jaccard")

# 5. Hierarchical clustering
hc <- hclust(dist_matrix, method = "average")

# 6. Plot the dendrogram.
plot(hc, main = "Dendrogram of PDA Techniques", xlab = "", sub = "", cex = 0.8)

# 7. Convert the dendrogram into an object so I can easily view it.
dend <- as.dendrogram(hc)
dend

#8. Rotate the dendrogram for easier reading in the publication.
par(mar = c (5, 2, 2, 20)) 
plot(dend, horiz = TRUE, main = "Dendrogram of PDA techniques", xlab = "Height", sub = "", cex = 0.7)
