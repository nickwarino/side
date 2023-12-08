#Step 1: Creating folder shortcuts for all new projects (from Andrew Ba Tran's R-Journalism Course)

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")

sapply(folder_names, dir.create)

setwd("~/Desktop/Dropbox/CREATIVE projects/2020_Primary")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)

primary_data <- read.csv("raw_data/2020_PRIMARY_DATA.csv",stringsAsFactors=F)

real_candidates <- filter(primary_data,Delegates>0)

str(primary_data)


mid_NationalPoll_Per<-median(real_candidates$NationalPoll_Per)

ggplot(real_candidates,
       aes(x=PV_Per,
           y=D_Per,
           size=StatesWon,
           color=NationalPoll_Per))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=NationalPoll_Per)) +
  scale_fill_gradient2(midpoint=mid_NationalPoll_Per, low="blue", mid="white",
                       high="red", space ="Lab" , name ="538 National Poll Avg") +
  geom_abline(intercept=0, slope=1, color='grey')+
  geom_abline(intercept=.5, slope=0, color='black')+
  annotate("text", x=0.05, y=.51, label="Majority Delegates") + 
  scale_size(range = c(5, 20), name="States Won (Popular Vote)") +
  scale_x_continuous(limits = c(0, .6), name ="Popular Vote %") +
  scale_y_continuous(limits = c(0, .6), name ="Delegate %") +
  geom_text_repel(aes(label=Candidate), size=4, color="black") +
  theme_minimal()+
  labs(title="BERNIE IS CLOSE AND CAN STILL WIN", 
       subtitle="Showing all candidates who've won pledged delegates", 
       caption="All votes and delegates awarded as of 3/4/20 1PM. Many votes and delegates in CA still outstanding. Source: GreenPapers.com & FiveThirtyEight") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/2020_primary_summary.pdf", width=15, height=15, units="in", useDingbats=FALSE)