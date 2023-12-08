# Install and load readr package
install.packages("readr")
library(readr)
#Libraries I Always Load
library(rmarkdown) # Dynamic Documents for R
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(ggthemes) # Extra Themes, Scales and Geoms for 'ggplot2'
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(lemon) # Freshing Up your 'ggplot2' Plots
library(ragg) # Graphic Devices Based on AGG
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
library(skimr) # Compact and Flexible Summaries of Data
library(here) # A Simpler Way to Find Your Files
source("/Users/nickmac/Dropbox/Creative Projects/Themes/My_Theme_NoY.R")
source("/Users/nickmac/Dropbox/Creative Projects/Themes/My_Theme_WithY.R")

options(scipen=999)

# Load
trade_union_density <- read_csv("2023-09-14-OECD-Trade_union_density.csv")
collective_bargaining_coverage <- read_csv("2023-09-14-OECD-Collective_bargaining_coverage.csv")

select_tud <- trade_union_density |> 
  select(Country, Stat, Avg_2011_2020) |> 
  arrange(desc(Avg_2011_2020))

select_cbc <- collective_bargaining_coverage |> 
  select(Country, Stat, Avg_2012_2021) |> 
  arrange(desc( Avg_2012_2021))