# Load necessary libraries 
library(dplyr) 
library(purrr) 
library(knitr) # New library for kable 

# Set the path to the current working directory 
folder_path <- " " 
# List all the files and store 
file_list <- list.files(path = folder_path, full.names = TRUE) 
process_file <- function(file_path, index) { 
 df <- read.delim(file_path, sep = "\t") 

 # New code: Total counts 
 result0 <- df %>% 
 summarise(count = n_distinct(Gene.symbol)) 

# Extract the file name without extension 
 file_name <- basename(file_path) 
 file_new <- tools::file_path_sans_ext(file_name) 

# Data manipulation: Filter and rename columns for significant 
 result1 <- df %>% 
 filter(P.Value <= 0.05, !is.na(Gene.symbol) & Gene.symbol != "") %>% 
 mutate(Gene.symbol = sapply(strsplit(Gene.symbol, "///"), `[`, 1)) %>% 
 group_by(Gene.symbol) %>% 
 filter(P.Value == min(P.Value)) %>% 
 ungroup() 

 # New code: Count of Gene.symbol column for significant 
 count1 <- result1 %>% 
 summarise(count = n_distinct(Gene.symbol))

# Data manipulation: Filter and rename columns for logFC > 0.5 
 result2 <- df %>% 
 filter(P.Value <= 0.05, !is.na(Gene.symbol) & Gene.symbol != "") %>% 
 mutate(Gene.symbol = sapply(strsplit(Gene.symbol, "///"), `[`, 1)) %>% 
 group_by(Gene.symbol) %>% 
 filter(P.Value == min(P.Value)) %>% 
 ungroup() %>% 
 filter(logFC > 0.5) 

 # New code: Count of Gene.symbol column for logFC > 0.5 
 count2 <- result2 %>% 
 summarise(count = n_distinct(Gene.symbol)) 

 # Data manipulation: Filter and rename columns for logFC < -0.5 
 result3 <- df %>% 
 filter(P.Value <= 0.05, !is.na(Gene.symbol) & Gene.symbol != "") %>% 
 mutate(Gene.symbol = sapply(strsplit(Gene.symbol, "///"), `[`, 1)) %>% 
 group_by(Gene.symbol) %>% 
 filter(P.Value == min(P.Value)) %>% 
 ungroup() %>% 
 filter(logFC < -0.5) 

 # New code: Count of Gene.symbol column for logFC < -0.5
count3 <- result3 %>% 
 summarise(count = n_distinct(Gene.symbol)) 

# Data manipulation: Filter and rename columns for logFC > 1 
 result4 <- df %>% 
 filter(P.Value <= 0.05, !is.na(Gene.symbol) & Gene.symbol != "") %>% 
 mutate(Gene.symbol = sapply(strsplit(Gene.symbol, "///"), `[`, 1)) %>% 
 group_by(Gene.symbol) %>% 
 filter(P.Value == min(P.Value)) %>% 
 ungroup() %>% 
 filter(logFC > 1) 

 # New code: Count of Gene.symbol column for logFC > 1 
 count4 <- result4 %>% 
 summarise(count = n_distinct(Gene.symbol)) 

# Data manipulation: Filter and rename columns for logFC < -1 
 result5 <- df %>%
filter(P.Value <= 0.05, !is.na(Gene.symbol) & Gene.symbol != "") %>% 
 mutate(Gene.symbol = sapply(strsplit(Gene.symbol, "///"), `[`,1)) %>% 
 group_by(Gene.symbol) %>% 
 filter(P.Value == min(P.Value)) %>% 
 ungroup() %>% 
 filter(logFC < -1) 

 # New code: Count of Gene.symbol column for logFC < -1 
 count5 <- result5 %>% 
 summarise(count = n_distinct(Gene.symbol))

# Generate a data frame with all counts 
 counts_df <- data.frame( 
 File_Name = file_new, 
 Total_Counts_filtered = result0$count, 
 Significant = count1$count, 
 Greater_than_0.5 = count2$count,
Less_than_0.5 = count3$count, 
 Greater_than_one = count4$count, 
 Less_than_one = count5$count 
 ) 

 # Print the counts data frame using kable 
 print(kable(counts_df)) 
} 
imap(file_list, process_file)