##2019_01_23 NOTES:
#Set up project, got initial data set loaded. To do:
#1 Look up data wrangling for TidyR, specially to flip co2_data1 so each oversation is a country

##2019_01_30 NOTES:
#Got over my weird anxiety about doing this.
#Loaded all necessary data sets, reviewed how to manipuate data with dplyr and tidyr cheatsheats (From RStudio Blog: https://www.rstudio.com/resources/cheatsheets/)
#Think I know how to do it. Each observation/case/row will be country*year. 
#Year 4 variables will be emissions per cap, and emission change, total emissions and happiness. 
#Use tidyr (see "Data Import Cheat Sheat" page 2), GATHER, SPREAD, MISSING VALUES, COMBINE

setwd("~/Desktop/Dropbox/CREATIVE projects/climate_change")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)

co2_data1 <- read.csv("raw_data/co2_emissions.csv",stringsAsFactors=F)
unhappy_data1 <- read.csv("raw_data/un_happiness_2018.csv",stringsAsFactors=F)

ggplot(data=housing_data,
       aes(x=Med_RentP1000Sqft_to_IncMon,
           y=Med_HVIP1000Sqft_to_IncMon,
           size=Homelessness_rate_per_10K,
           color=Unsheltered_Pct))+
  scale_fill_gradient2(low="blue",high="red") +
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Unsheltered_Pct)) +
  geom_smooth(method=lm)+
  scale_size_continuous(range = c(.01, 25)) +
  geom_text_repel(aes(label=RegionName), size=5, color="grey") +
  theme_minimal()  +
  labs(x="CO2 Emissions Per Person", y="Change from 2007 to 2017 in CO2EPP", size="Total CO2 Emissions", 
       title="Housing Affordability and Homelessness in America", 
       subtitle="By State and Most Recent Housing Data (Data: Zillow Research, US HUD, US Census)")

ggsave("docs/housing_bubble_graph_labels.pdf", width=15, height=15, units="in", useDingbats=FALSE)

ggplot(data=housing_data,
       aes(x=Med_RentP1000Sqft_to_IncMon,
           y=Med_HVIP1000Sqft_to_IncMon,
           size=Homelessness_rate_per_10K,
           color=Unsheltered_Pct))+
  scale_fill_gradient2(low="blue",high="red") +
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Unsheltered_Pct), show.legend = FALSE) +
  geom_smooth(method=lm, color='black', show.legend = FALSE)+
  scale_size_continuous(range = c(.01, 25)) +
  geom_text_repel(aes(label=RegionName), size=5, color="grey") +
  guides(fill=FALSE, color=FALSE)+
  theme_minimal()

ggsave("docs/housing_bubble_graph_NO_labels.pdf", width=15, height=15, units="in", useDingbats=FALSE)

