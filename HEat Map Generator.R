rm(lr())

# Load the relevant libraries - do this every time
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)

deaths <- read_csv("Desktop/Brian Stuff/SalishRG/Opioid Provider Mapping/OpioidProvders/co_opioid_death_1418.csv")
deaths$d <- ifelse(deaths$d == ".",1,deaths$d)
deaths$d <- as.numeric(deaths$d)

