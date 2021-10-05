## Factsheet Indepth Analysis
rm(list = ls())
today <- Sys.Date()

# load required packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(reshape2)
library(srvyr)
library(questionr)

# load clean data
df <- rio::import("output/checking/2021-08_dataset_checked.csv", na=c(""))
names(df) <- gsub("\\/", ".",names(df))

## Analysis of Select Multiple Questions
compute_pct <- function(var_name,df) {
  
  binaries <- df %>% select(starts_with(paste0(var_name,"."))) %>% na.omit(FALSE)
  names(binaries) <- gsub("^[^\\.]*\\.","",names(binaries))
  results <- tibble::rownames_to_column(round(colSums(binaries,na.rm = T) / nrow(binaries),digits = 2) %>% as.data.frame(), "option") 
  colnames(results)[2] <- "pct"
  results
}

# Analysis
hygiene_increase <- compute_pct("predictions_hygiene_increase", df)
food_price_increase <- compute_pct("predictions_food_increase", df)
payment_modalities <- compute_pct("payment_modalities", df)

# Export output
analysis <- list(hygiene_increase, food_price_increase, payment_modalities)
write.xlsx(analysis, paste0("output/analysis/factsheet-indepth-analysis_",today,".xlsx"))
