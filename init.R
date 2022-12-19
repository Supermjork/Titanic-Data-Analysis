# Loading R libraries to use
library(statsr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lemon)
library(ggpubr)

# Loading the csv with passenger data into "titanic" data frame
# Main dataset
titanic <- read.csv("dataset/train.csv")
titanic_clean <- na.omit(titanic)

# Sourcing our custom function files
source("generic-functions.R") # Generic reusable functions (Mostly for means)
source("based-matrix.R")      # Matrix function for better output
source("fare-functions.R")    # Functions relating to Fare column
source("age-functions.R")     # Functions for Age, and plots/data analysis
source("all-ze-variances.R")  # Functions regarding the variance
source("estimations.R")       # Functions regarding the estimators (MME, MLE)
source("intervals.R")         # Functions regarding the confidence intervals