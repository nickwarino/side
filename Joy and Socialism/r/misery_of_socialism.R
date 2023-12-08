
setwd("~/Desktop/Dropbox/CREATIVE projects/Joy and Socialism/r")

install.packages(c("learnr","rmarkdown","tidyverse","ggthemes","ggrepel"))
library(learnr)
library(rmarkdown)
library(tidyverse)
library(ggthemes)
library(ggrepel)


misery_of_socialism_data <- read.csv("raw_data/2019_11_15_misery_of_socialism_data.csv",stringsAsFactors=F)

dem_socialism_top <- misery_of_socialism_data %>%
  select(Country, Dem_Socialism_Score) %>%
  arrange(desc(Dem_Socialism_Score)) %>%
  arrange(Dem_Socialism_Score) %>%
  mutate(Country = factor(Country, levels = .$Country))

ggplot(dem_socialism_top, 
       aes(x=Country,
           y=Dem_Socialism_Score,
           label = paste0(round(Dem_Socialism_Score, 1)))) + 
  geom_point(size=10) + 
  geom_segment(aes(x=Country, 
                   xend=Country, 
                   y=0, 
                   yend=Dem_Socialism_Score)) + 
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_minimal()  +
  labs(title="Democratic Socialism Scores", 
       subtitle="OECD Countries Ranked", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +  
  coord_flip()

ggsave("docs/dem_socialism_lollipop_graph.pdf", width=15, height=15, units="in", useDingbats=FALSE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=GDP_Per_Hour_Worked_USD_PPP))+
  geom_point(color='hotpink2') +    # Use hollow circles
  geom_smooth(method=lm, color='grey') +   # Add linear regression line #  (by default includes 95% confidence region)
  geom_text_repel(aes(label=Country), size=3, color="black") +
  scale_x_continuous(limits = c(-2, 2)) +
  theme_minimal()  +
  labs(title="Democratic Socialism Scores vs. GDP Per Hour Worked (USD PPP)", 
       subtitle="OECD Countries Ranked", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org")

ggsave("docs/dem_socialism_x_gdp_per_hour_worked.pdf", width=15, height=15, units="in", useDingbats=FALSE)

mid_gender_wage_gap<-median(misery_of_socialism_data$Gender_Wage_Gap)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Labor_Income_Per_Hour,
           size=P90_P10_gross_earnings_decile_ratio,
           color=Gender_Wage_Gap))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Gender_Wage_Gap)) +
  scale_fill_gradient2(midpoint=mid_gender_wage_gap, low="blue", mid="white",
                       high="red", space ="Lab" ) +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Gross Earnings\nRatio between\n90th & 10th Deciles") +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 60)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="The most socialist countries have well-paid workers with the most equal earnings", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_laborincomeperhour_x_grossearningsdecile_x_genderwagegap.pdf", width=15, height=15, units="in", useDingbats=FALSE)


DemSoc_X_MedDisInc_linMod <- lm(Dem_Socialism_Score ~ Median_Disposable_Income_PPP, data=misery_of_socialism_data)  # build linear regression model on full data

summary(DemSoc_X_MedDisInc_linMod)

mid_Total_Poverty_Gap_Rate_x_Gap<-median(misery_of_socialism_data$Total_Poverty_Gap_Rate_x_Gap)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Median_Disposable_Income_PPP,
           size=P90_P10_disposable_income_decile_ratio,
           color=Total_Poverty_Gap_Rate_x_Gap))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Total_Poverty_Gap_Rate_x_Gap)) +
  scale_fill_gradient2(midpoint=mid_Total_Poverty_Gap_Rate_x_Gap, low="blue", mid="white",
                       high="red", space ="Lab", name="Total Poverty\nGap") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Disposable Income\nRatio between\n90 & 10th Deciles") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="Socialist countries have some of the most well-off, most equal households with the lowest poverty", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_disposableincome_x_gisposableincomedecile_x_povertygap.pdf", width=15, height=15, units="in", useDingbats=FALSE)

mid_Total_Top_1perc_Wealth_Share<-mean(misery_of_socialism_data$Top_1perc_Wealth_Share, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Median_NetWealth_PPP_thousands,
           size=Median_Disposable_Income_PPP,
           color=Top_1perc_Wealth_Share))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Top_1perc_Wealth_Share)) +
  scale_fill_gradient2(midpoint=mid_Total_Top_1perc_Wealth_Share, low="blue", mid="white",
                       high="red", space ="Lab", name="Top 1%\nWealth Share") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Median Household\nIncome") +
  scale_x_continuous(limits = c(-1, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="No obstacles to accumulating wealth for the typical household in socialist countries", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.7))

ggsave("docs/demsocialism_x_mediannetwealth_x_medianincome_x_top1percent.pdf", width=15, height=15, units="in", useDingbats=FALSE)

mid_Total_Top_1perc_Wealth_Share<-mean(misery_of_socialism_data$Top_1perc_Wealth_Share, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Economic_Equality_Score,
           size=Total_Poverty_Gap_Rate_x_Gap,
           color=Top_1perc_Wealth_Share))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Top_1perc_Wealth_Share)) +
  scale_fill_gradient2(midpoint=mid_Total_Top_1perc_Wealth_Share, low="blue", mid="white",
                       high="red", space ="Lab", name="Top 1%\nWealth Share") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Total Poverty\nGap") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="The most socialist countries are the most equal societies", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_economic_equality_x_povertygap_x_top1percent.pdf", width=15, height=15, units="in", useDingbats=FALSE)



mid_Paid_Annual_Leave_plus_Holidays<-mean(misery_of_socialism_data$Paid_Annual_Leave_plus_Holidays, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Leisure_Score,
           size=Paid_Maternity_Paternity_Leave,
           color=Paid_Annual_Leave_plus_Holidays))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Paid_Annual_Leave_plus_Holidays)) +
  scale_fill_gradient2(midpoint=mid_Paid_Annual_Leave_plus_Holidays, low="blue", mid="white",
                       high="red", space ="Lab", name="Paid Annual Leave\nand Holidays") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Paid Maternity and\nPaternity Leave") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="The most socialist countries have the most free time", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_leisurescore_x_maternitypaternityleave_x_paidtimeoff.pdf", width=15, height=15, units="in", useDingbats=FALSE)




mid_Infant_Mortality_Rate_per_1000<-mean(misery_of_socialism_data$Infant_Mortality_Rate_per_1000, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Health_Score,
           size=Maternal_Mortality_rate_per_100000,
           color=Infant_Mortality_Rate_per_1000))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Infant_Mortality_Rate_per_1000)) +
  scale_fill_gradient2(midpoint=mid_Infant_Mortality_Rate_per_1000, low="blue", mid="white",
                       high="red", space ="Lab", name="Infant Mortality\nRate Per 1000") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Maternal Mortality\nRate Per 100000") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="The most socialist countries have healthy people with low rates of preventable deaths", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, WHO, UNICEF, UNFPA, World Bank Group, United Nations Population Division, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_healthscore_x_maternalmortality_x_infantmortality.pdf", width=15, height=15, units="in", useDingbats=FALSE)



mid_Average_of_Health_Leisure_Inequality_Scores<-mean(misery_of_socialism_data$Average_of_Health_Leisure_Inequality_Scores, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Life_Satisifaction,
           size=Median_Disposable_Income_PPP,
           color=Average_of_Health_Leisure_Inequality_Scores))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Average_of_Health_Leisure_Inequality_Scores)) +
  scale_fill_gradient2(midpoint=mid_Average_of_Health_Leisure_Inequality_Scores, low="blue", mid="white",
                       high="red", space ="Lab", name="Average of Health,\nLeisure, & Equality\nScores") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="Median Disposable\nIncome") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="The most socialist countries report the highest life satisifaction", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, WHO, UNICEF, UNFPA, World Bank Group, United Nations Population Division, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.9,.3))

ggsave("docs/demsocialism_x_lifesatisfaction_x_medianincome_x_scoreaverage.pdf", width=15, height=15, units="in", useDingbats=FALSE)


mid_Gini_Post_TaxesandTransfers<-mean(misery_of_socialism_data$Gini_Post_TaxesandTransfers, na.rm=TRUE)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=GII_2019_Innovation_Output_Index,
           size=GDP_Per_Hour_Worked_USD_PPP,
           color=Gini_Post_TaxesandTransfers))+
  geom_point(alpha=0.7, shape=21, color='black', aes(fill=Gini_Post_TaxesandTransfers)) +
  scale_fill_gradient2(midpoint=mid_Gini_Post_TaxesandTransfers, low="blue", mid="white",
                       high="red", space ="Lab", name="Gini Coeffecienet\nPost Taxes & Transfers") +
  geom_smooth(method=lm, color='grey', show.legend = FALSE)+
  scale_size(range = c(5, 20), name="GDP Per Hour\nWorked") +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal()+
  labs(title="No signs that socialist institutions hurt innovation", 
       subtitle="OECD Countries", 
       caption="source: stats.oecd.org, Global Innovation Index, wid.world, freedomhouse.org") + 
  theme(legend.position=c(.15,.75))

ggsave("docs/demsocialism_x_innovation_x_gdpperhour_x_gini.pdf", width=15, height=15, units="in", useDingbats=FALSE)




DemSoc_X_Life_Expectancy_linMod <- lm(Life_Expectancy_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Maternal_Mortality_rate_per_100000_linMod <- lm(Maternal_Mortality_rate_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Neonatal_Mortality_Rate_per_1000_linMod <- lm(Neonatal_Mortality_Rate_per_1000_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Infant_Mortality_Rate_per_1000_linMod <- lm(Infant_Mortality_Rate_per_1000_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Life_Satisifaction_linMod <- lm(Life_Satisifaction_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Leisure_and_Personal_Care_Time_linMod <- lm(Leisure_and_Personal_Care_Time_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Paid_Maternity_Paternity_Leave_linMod <- lm(Paid_Maternity_Paternity_Leave_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Paid_Annual_Leave_plus_Holidays_linMod <- lm(Paid_Annual_Leave_plus_Holidays_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Mean_Annual_Hours_Worked_Per_Worker_linMod <- lm(Mean_Annual_Hours_Worked_Per_Worker_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_GDP_Per_Hour_Worked_USD_PPP_linMod <- lm(GDP_Per_Hour_Worked_USD_PPP_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Labor_Income_Share_linMod <- lm(Labor_Income_Share_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Labor_Income_Per_Hour_linMod <- lm(Labor_Income_Per_Hour_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_P90_P10_gross_earnings_decile_ratio_linMod <- lm(P90_P10_gross_earnings_decile_ratio_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Gender_Wage_Gap_linMod <- lm(Gender_Wage_Gap_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Median_Disposable_Income_PPP_linMod <- lm(Median_Disposable_Income_PPP_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Total_Poverty_Gap_Rate_x_Gap_linMod <- lm(Total_Poverty_Gap_Rate_x_Gap_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_P90_P10_disposable_income_decile_ratio_linMod <- lm(P90_P10_disposable_income_decile_ratio_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Gini_Post_TaxesandTransfers_linMod <- lm(Gini_Post_TaxesandTransfers_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Top_1perc_Wealth_Share_linMod <- lm(Top_1perc_Wealth_Share_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Health_Score_linMod <- lm(Health_Score ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Leisure_Score_linMod <- lm(Leisure_Score ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_Equality_linMod <- lm(Economic_Equality_Score ~ Dem_Socialism_Score, data=misery_of_socialism_data)  # build linear regression model on full data
DemSoc_X_Average_of_Health_Leisure_Inequality_Scores_linMod <- lm(Average_of_Health_Leisure_Inequality_Scores ~ Dem_Socialism_Score, data=misery_of_socialism_data)
DemSoc_X_GII_2019_Innovation_Output_Index_linMod <- lm(GII_2019_Innovation_Output_Index_TScore ~ Dem_Socialism_Score, data=misery_of_socialism_data)


summary(DemSoc_X_Life_Expectancy_linMod)
summary(DemSoc_X_Maternal_Mortality_rate_per_100000_linMod)
summary(DemSoc_X_Neonatal_Mortality_Rate_per_1000_linMod)
summary(DemSoc_X_Infant_Mortality_Rate_per_1000_linMod)
summary(DemSoc_X_Life_Satisifaction_linMod)
summary(DemSoc_X_Leisure_and_Personal_Care_Time_linMod)
summary(DemSoc_X_Paid_Maternity_Paternity_Leave_linMod)
summary(DemSoc_X_Paid_Annual_Leave_plus_Holidays_linMod)
summary(DemSoc_X_Mean_Annual_Hours_Worked_Per_Worker_linMod)
summary(DemSoc_X_GDP_Per_Hour_Worked_USD_PPP_linMod)
summary(DemSoc_X_Labor_Income_Share_linMod)
summary(DemSoc_X_Labor_Income_Per_Hour_linMod)
summary(DemSoc_X_P90_P10_gross_earnings_decile_ratio_linMod)
summary(DemSoc_X_Gender_Wage_Gap_linMod)
summary(DemSoc_X_Median_Disposable_Income_PPP_linMod)
summary(DemSoc_X_Total_Poverty_Gap_Rate_x_Gap_linMod)
summary(DemSoc_X_P90_P10_disposable_income_decile_ratio_linMod)
summary(DemSoc_X_Gini_Post_TaxesandTransfers_linMod)
summary(DemSoc_X_Top_1perc_Wealth_Share_linMod)
summary(DemSoc_X_Health_Score_linMod)
summary(DemSoc_X_Leisure_Score_linMod)
summary(DemSoc_X_Equality_linMod)
summary(DemSoc_X_Average_of_Health_Leisure_Inequality_Scores_linMod)
summary(DemSoc_X_GII_2019_Innovation_Output_Index_linMod)

ggplot(misery_of_socialism_data,
       aes(x=Dem_Socialism_Score,
           y=Mean_Annual_Hours_Worked_Per_Worker_TScore))+
  geom_point(color='hotpink2') +
  geom_smooth(method=lm, color='grey') +
  geom_text_repel(aes(label=Country), size=3, color="black") +
  theme_minimal() 