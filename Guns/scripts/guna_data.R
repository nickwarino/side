#Step 1: Creating folder shortcuts for all new projects (from Andrew Ba Tran's R-Journalism Course)

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")

sapply(folder_names, dir.create)

# Step 2: install packages

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)

# Step 3: load data

gun_data <- read.csv("raw_data/gun_data.csv",stringsAsFactors=F)

# Test theme

My_Theme = theme(
  plot.title = element_text(size = 30),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  plot.background = element_rect(fill="white", color="black"))

# Step 4: visualize data by creating 4 variable bubble plot 
# get median for gun deaths

mid_gun_deaths<-mean(gun_data$GunDeathPerc, na.rm=TRUE)

ggplot(gun_data,
       aes(x=FirearmHouseholdPerce,
           y=HandgunHouseholdPerc,
           size=GunsPerc))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=GunDeathPerc)) +
  scale_fill_gradient2(midpoint=mid_gun_deaths, low="blue", mid="white",
                       high="red", space ="Lab", name="Gun Deaths Per 100K People") +
  scale_size(range = c(1, 24)) +
  geom_text_repel(aes(label=Location), size=3, color="black") +
  theme_minimal()+
  labs(title="Lots of guns in America", 
       subtitle="OECD Countries, Latest year available\nnickwarino.com, created 2022-05-24", 
       caption="source: https://en.wikipedia.org/wiki/Percent_of_households_with_guns_by_country, \nhttps://en.wikipedia.org/wiki/Estimated_number_of_civilian_guns_per_capita_by_country, \nhttps://worldpopulationreview.com/country-rankings/gun-deaths-by-country", 
       x="% Households With Gun",
       y="% Households with Hand Gun",
       size="Guns Per 100 People",
       color="Gun Deaths Per 100K People") +
  theme(legend.position=c(.9,.7)) +
  My_Theme
ggsave("docs/gun_deaths.png", width=15, height=15, units="in")
ggsave("docs/gun_deaths.pdf", width=15, height=15, units="in", useDingbats=FALSE)


