setwd("~/Desktop/Dropbox/SEIU/Legislative Guide/housing_graph")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)

housing_data <- read.csv("raw_data/2019_01_16_housing_data.csv",stringsAsFactors=F)

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
  labs(x="Median Rent (per sqft) to Median Income Ratio", y="Median Home Value (per sqft) to Median Income Ratio", size="Homelessness Rate Per 10,000", 
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

