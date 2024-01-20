# This file is shall be used to prepare the data that can be used in the Shiny App for predicting the next word


library(magrittr)
library(stringr)
library(tidyverse)
library(ngram)


dataset_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
proj_path <- "/Users/harsha/Projects/DataScience_Coursera/NextWordPredictor"

# Save the path for storing RDS files
rds_path <- file.path(proj_path, "data")

# Save the path for storing RData files
rData_path <- file.path(proj_path, "RData")


# This function takes in language setting and saves the combined and sampled data as file in RDS format
getCSampledData <- function() {

  # Check if the data folder exists, else create it
  setwd(proj_path)
  if(!file.exists("data")) {
    dir.create("data")
  }
  
  # Download the data zip file and upzip it into data folder
  if(!file.exists("Coursera-SwiftKey.zip")){
    download.file(dataset_url, "Coursera-SwiftKey.zip")
    unzip(zipfile = "./Coursera-SwiftKey.zip", exdir = "./data")
  }
  datapath <- "/Users/harsha/Projects/DataScience_Coursera/NextWordPredictor/data/final"

  # For language setting is en_us  
  en_dataset <- file.path(datapath, "en_US")
  # List the files in English dataset
  list.files(en_dataset)
  
  # Read the files with files handler objects
  con = file(file.path(en_dataset, "en_US.blogs.txt"))
  blogdata <- readLines(con, encoding = "UTF-8-MAC", skipNul = TRUE)
  close(con)
  
  con = file(file.path(en_dataset, "en_US.news.txt"))
  newsdata <- readLines(con, encoding = "UTF-8-MAC", skipNul = TRUE)
  close(con)
  
  con = file(file.path(en_dataset, "en_US.twitter.txt"))
  xdata <- readLines(con, encoding = "UTF-8-MAC", skipNul = TRUE)
  close(con)
  
  # Lets take 1% of the data and merge them
  sample_size <- 0.01
  
  sampleBdata <- sample(blogdata, length(blogdata) * sample_size, replace = FALSE)
  sampleNdata <- sample(newsdata, length(newsdata) * sample_size, replace = FALSE)
  sampleXdata <- sample(xdata, length(xdata) * sample_size, replace = FALSE)
  
  # Lets remove non-English characters if any
  sampleBdata <- iconv(sampleBdata, "latin1", "ASCII", sub = "")
  sampleNdata <- iconv(sampleNdata, "latin1", "ASCII", sub = "")
  sampleXdata <- iconv(sampleXdata, "latin1", "ASCII", sub = "")
  
  combined.data.sample <- c(sampleBdata, sampleNdata, sampleXdata)
  
  # Save the combined.data.sample in a RDS file
  saveRDS(combined.data.sample, file = file.path(rds_path, "cds_data_en.RDS"))
  
  return(combined.data.sample)
}

# This function cleans the sampled data by removing 
cleanSampledData <- function(combined_data) {
  cleanData <- combined_data %>% 
    unlist() %>% 
    concatenate() %>%
    preprocess(case = "lower", remove.punct = TRUE, remove.numbers = TRUE)
  
  # Save the cds_stats in a RDS file
  saveRDS(cleanData, file = file.path(rds_path, "cleadData_cds_en.RDS"))
  return(cleanData)
}

# The function tokenize would take the input data and make n-gram data where n range from 1 to 5
tokenize <- function(inputdat, n=1){
  outdat<-inputdat %>%
    ngram_asweka( min = n, max=n, sep = " ") %>%
    table() %>% data.frame()
  outdat[,1]<-outdat[,1] %>% as.character()
  outdat<-arrange(outdat, desc(Freq))
  colnames(outdat)<-c("Word", "Freq")
  return(outdat)
}

# Function to compute Unigrams
getUnigrams <- function(cleanData) {
  ngram_df <- tokenize(cleanData, 1)
  ngram_df$Word <- as.character(ngram_df$Word)        # Store tokens as character words
  word_split <- strsplit(ngram_df$Word, split = " ")  # Split the words with white space separator
  unigram_df <- transform(ngram_df, first = sapply(word_split, "[[", 1))  # Append separated words as columns
  # Write to a CSV and read it back, this flattens the file and reduces the size of variable
  write.csv(unigram_df, file = file.path(rds_path, "unigram.csv"), row.names = FALSE)
  unigram <- read.csv(file = file.path(rds_path, "unigram.csv"), stringsAsFactors = FALSE)
  saveRDS(unigram, file = file.path(rData_path, "unigram.RData"))
  return(unigram)
}


# Function to compute Bigrams
getBigrams <- function(cleanData) {
  ngram_df<-tokenize(cleanData, 2)
  ngram_df$Word <- as.character(ngram_df$Word)        # Store tokens as character words
  word_split <- strsplit(ngram_df$Word, split = " ")  # Split the words with white space separator
  bigram_df <- transform(ngram_df,                   # Append separated words as columns
                         first = sapply(word_split, "[[", 1), 
                         second = sapply(word_split, "[[", 2))
  # Remove all single frequency tokens
  bigram_df <- bigram_df[bigram_df$Freq > 1, ]
  # Write to a CSV and read it back, this flattens the file and reduces the size of variable
  write.csv(bigram_df, file = file.path(rds_path, "bigram.csv"), row.names = FALSE)
  bigram <- read.csv(file = file.path(rds_path, "bigram.csv"), stringsAsFactors = FALSE)
  saveRDS(bigram, file = file.path(rData_path, "bigram.RData"))
  return(bigram)
}

# Function to compute Trigrams
getTrigrams <- function(cleanData) {
  ngram_df<-tokenize(cleanData, 3)
  ngram_df$Word <- as.character(ngram_df$Word)        # Store tokens as character words
  word_split <- strsplit(ngram_df$Word, split = " ")  # Split the words with white space separator
  trigram_df <- transform(ngram_df,                   # Append separated words as columns
                          first = sapply(word_split, "[[", 1), 
                          second = sapply(word_split, "[[", 2),
                          third = sapply(word_split, "[[", 3))  
  # Remove all single frequency tokens
  trigram_df <- trigram_df[trigram_df$Freq > 1, ]
  # Write to a CSV and read it back, this flattens the file and reduces the size of variable
  write.csv(trigram_df, file = file.path(rds_path, "trigram.csv"), row.names = FALSE)
  trigram <- read.csv(file = file.path(rds_path, "trigram.csv"), stringsAsFactors = FALSE)
  saveRDS(trigram, file = file.path(rData_path, "trigram.RData"))
  return(trigram)
}

# Function to compute Fourgrams
getFourgrams <- function(cleanData) {
  ngram_df <-tokenize(cleanData, 4)
  ngram_df$Word <- as.character(ngram_df$Word)        # Store tokens as character words
  word_split <- strsplit(ngram_df$Word, split = " ")  # Split the words with white space separator
  fourgram_df <- transform(ngram_df,                   # Append separated words as columns
                           first = sapply(word_split, "[[", 1), 
                           second = sapply(word_split, "[[", 2),
                           third = sapply(word_split, "[[", 3),
                           fourth = sapply(word_split, "[[", 4))  
  #fourgram_df <- fourgram_df[fourgram_df$Freq > 1, ]
  # Write to a CSV and read it back, this flattens the file and reduces the size of variable
  write.csv(fourgram_df, file = file.path(rds_path, "fourgram.csv"), row.names = FALSE)
  fourgram <- read.csv(file = file.path(rds_path, "fourgram.csv"), stringsAsFactors = FALSE)
  saveRDS(fourgram, file = file.path(rData_path, "fourgram.RData"))
  return(fourgram)
}


# Function to compute Fivegrams
getFivegrams <- function(cleanData) {
  ngram_df<-tokenize(cleanData, 5)
  ngram_df$Word <- as.character(ngram_df$Word)        # Store tokens as character words
  word_split <- strsplit(ngram_df$Word, split = " ")  # Split the words with white space separator
  fivegram_df <- transform(ngram_df,                   # Append separated words as columns
                           first = sapply(word_split, "[[", 1), 
                           second = sapply(word_split, "[[", 2),
                           third = sapply(word_split, "[[", 3),
                           fourth = sapply(word_split, "[[", 4),
                           fifth = sapply(word_split, "[[", 5))  
  #fivegram_df <- fivegram_df[fivegram_df$Freq > 1, ]
  # Write to a CSV and read it back, this flattens the file and reduces the size of variable
  write.csv(fivegram_df, file = file.path(rds_path, "fivegram.csv"), row.names = FALSE)
  fivegram <- read.csv(file = file.path(rds_path, "fivegram.csv"), stringsAsFactors = FALSE)
  saveRDS(fivegram, file = file.path(rData_path, "fivegram.RData"))
  return(fivegram)
}


# This is primary function that is called to prep the data required for the Shiny App
computeNGrams <- function () {
  library(NLP)
  
  gc()
  tStart <- proc.time()
  # Get combined and sampled data from English source files
  combined_data <- getCSampledData()
  # Clean the sampled data
  cds_data <- cleanSampledData(combined_data)
  # Compute N-Grams
  unigram <- getUnigrams(cds_data)
  bigram <- getBigrams(cds_data)
  trigram <- getTrigrams(cds_data)
  fourgram <- getFourgrams(cds_data)
  fivegram <- getFivegrams(cds_data)
  proc.time() - tStart
  gc()
}
  