# installing required packages
install.packages(c("readr", "ggplot2","dplyr"))
# loading installed packages
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(tidyr)
library(treemapify)
library(ggplot2)
# reading dataset and naming as covid19
covid19 <- read_csv("coronavirus1.csv")
head(covid19)
glimpse(covid19)
levels(factor(covid19$type))

##### total covid confirmed cases country wise
confirmed_cases <- covid19 %>%
  filter(type == "confirmed")%>%
  group_by(country)%>%
  summarize(total_cases = sum(cases))%>%
  arrange(desc(total_cases))#%>%

confirmed_cases

#ggplot(confirmed_cases,aes(area = total_cases, fill = country))+
  #geom_treemap()

##### confirmed cases in CHINA till date
china_cases <- covid19 %>%
  filter(type == "confirmed", country == "China" )%>%
  # converting the days date to month and summarizing total cases per month
  group_by(year = year(date), month_date = month(date))%>%
  summarize(cases_month = sum(cases))#%>%
glimpse(china_cases)  

# plotting china confirmed cases
ggplot(china_cases, aes(x= month_date, y= cases_month, fill = month_date))+
  geom_col()+
  # text on each bar
  geom_text(aes(label = cases_month), vjust = -0.5)+
  #plotting month names(month.abb)
  scale_x_discrete(limits = month.abb)+
  ggtitle("Total number of confirmed cases till date in China")+
  xlab("Months") + ylab("Total Confirmed Cases")
 
##### US confirmed,recovered and death cases and its plotting
US_cases <- covid19 %>%
  filter(country == "US" )%>%
  group_by(year = year(date), month_date = month(date), type)%>%
  summarize(total_cases = sum(cases))#%>% # all confirmed,death,recovered
  
US_cases

# plotting US cases
ggplot(US_cases, aes(x= month_date, y= total_cases,color = type))+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  geom_line()+
  scale_x_discrete(limits = month.abb)+
  ggtitle("Total number of confirmed cases,death recovered an till date in US")+
  xlab("Months") + ylab("Total number of Cases")

#### total cases,death,recovered and active cases of all countries till date
world_wide_cases <- covid19 %>%
  group_by(type, date)%>%
  summarize(total_cases = sum(cases))%>%
  pivot_wider(names_from = type, values_from = total_cases)%>%
  arrange(date)%>%
  mutate(active_cases = confirmed - recovered - death)%>%
  mutate(total_recovered = cumsum(recovered), total_active = cumsum(active_cases), 
         total_death = cumsum(death))%>%  #,total_confirmed = cumsum(confirmed

  pivot_longer(
    cols = starts_with("total_"),
    names_to = "total_cumCases",
    #names_prefix = "wk",
    values_to = "total_cumCases_values",
    values_drop_na = TRUE
  )

#world_wide_cases$total_recovered
world_wide_cases
head(world_wide_cases)
ggplot(world_wide_cases, aes(x = date, y= total_cumCases_values, fill = total_cumCases ))+
  geom_area(size=.3, colour="white")+
  
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  labs(fill = "Covid19 cases worldwide")+
  theme(legend.position = "top", 
       legend.title = element_text(colour="black", size=12, face="bold"))+
  xlab("Date") + ylab("Number of Cases")+
  geom_vline(xintercept = as.numeric(as.Date("2015-01-22")), linetype=4)
  



