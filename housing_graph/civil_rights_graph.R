#Step 1: Creating folder shortcuts for all new projects (from Andrew Ba Tran's R-Journalism Course)

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")

sapply(folder_names, dir.create)

setwd("~/Desktop/Dropbox/SEIU/Legislative Guide/civil_rights_graph")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)

civil_rights_data <- read.csv("raw_data/civil_rights_data.csv",stringsAsFactors=F)

civil_rights_data_copy <- civil_rights_data %>% select(-Country)

ggplot(data=civil_rights_data,
       aes(x=VAP_turnout,
           y=RVP_turnout,
           size=Incaration_rate))+
  geom_point() +
  geom_text_repel(aes(label=Country), size=5, color="grey") +
  scale_size(range=c(1,20)) +
  scale_x_continuous(limits=c(0.3,1)) +
  scale_y_continuous(limits=c(0.3,1)) +
  theme_minimal()  +
  labs(x="Voting Age Turnout (%)", y="Registered Voter Turnout (%)", size="Incaration Rate", 
       title="Voting and Incaration Rates Among Developed Countries", 
       subtitle="By Country and Most Recent Election With Data (Data: Pew Research, Prison Studies)")

ggsave("docs/civil_rights_bubble_graph_labels.pdf", width=15, height=15, units="in", useDingbats=FALSE)

ggplot(data=civil_rights_data,
       aes(x=VAP_turnout,
           y=RVP_turnout,
           size=Incaration_rate))+
  geom_point() +
  scale_size(range=c(1,20)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Voting Age Turnout (%)", y="Registered Voter Turnout (%)", size="Incaration Rate", 
       title="Civil Rights in the US", 
       subtitle="The % of the public and private sector workers that belong to unions, and total membership (bubble size) by State", 
       caption="Created by Nick Warino")

ggsave("docs/civil_rights_bubble_graph.pdf", width=11, height=8.5, units="in" , useDingbats=FALSE)


