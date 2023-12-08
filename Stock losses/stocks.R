remotes::install_github("wilkelab/ggtext")

#Step 1: Creating folder shortcuts for all new projects (from Andrew Ba Tran's R-Journalism Course)

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")

sapply(folder_names, dir.create)

setwd("~/Desktop/Dropbox/CREATIVE projects/Stock losses")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel", "extrafont", "remotes"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(extrafont)
library(remotes)

stocks_data <- read.csv("raw_data/stocks.csv",stringsAsFactors=F)

stocks_top <- stocks_data %>%
  select(Date, ThisYear, Loss) %>%
  arrange(desc(Loss)) %>%
  arrange(Loss) %>%
  mutate(Date = factor(Date, levels = .$Date)) %>%
  mutate(color = ifelse(ThisYear == "T", "red", "black"))

ggplot(stocks_top, 
       aes(x=Date,
           y=Loss,
           color = ThisYear,
           label = paste0(Loss*100,"%"))) + 
  geom_point(size=12) + 
  geom_segment(aes(x=Date, 
                   xend=Date, 
                   y=0, 
                   yend=Loss)) + 
  scale_color_manual(values = c("black", "red")) +
  geom_text(color = "white", size = 3, family = "Avenir Next Condensed") +
  scale_y_continuous(labels = function(Loss) paste0(Loss*100, "%"),limits = c(0, .25)) +
  theme_minimal()  +
  labs(title="Biggest 1-Day Loss for Dow Jones Index", 
       caption="source: Wikipedia") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        text = element_text(family = "Avenir Next Condensed", face = "bold", size = 14),
        title = element_text(family = "Avenir Next Condensed", face = "bold")
        # axis.text.y = ggtext::element_markdown(color = stocks_top$color)
  ) +  
  coord_flip()

# Solution to PDF save problem http://zevross.com/blog/2014/07/30/tired-of-using-helvetica-in-your-r-graphics-heres-how-to-use-the-fonts-you-like-2/
ggsave("docs/stocklosses.pdf", width=15, height=15, units="in", useDingbats=FALSE)