#Importing data 
library(readxl)
covid_zim <- read_xlsx("ZimCovidlatest.xlsx")
#Viewing the data
str(covid_zim)
#Summary of the data
summary(covid_zim)
#Converting date into a date format
covid_zim$Date <- as.Date(covid_zim$Date)
covid_zim$Date
#A graph of cumulative cases
library(ggplot2)
ggplot(data = covid_zim, aes(x = Date, y = Cum_cases)) +
  geom_bar(stat = "identity", fill = "darkblue")+
  scale_y_continuous(position = "right", breaks = seq(0,40000, by =10000))+
  scale_x_date(date_labels = "%b %Y")+
  labs(y = "Cumulative daily cases", title = "Zimbabwe Daily Reported Cumulative Covid-19 Infections:Nov 2020-Jan 2021")+
  theme(axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())

#A graph of cumulative deaths 
ggplot(data = covid_zim, aes(x = Date, y = Cum_deaths)) +
  geom_bar(stat = "identity", fill = "darkblue")+
  scale_y_continuous(position = "right", breaks = seq(0,1200, by =200))+
  scale_x_date(date_labels = "%b %Y") +
  labs(y= "Cumulative deaths")+
  theme(axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())

#Comparing cumulative daily cases against cumulative deaths
ggplot(data = covid_zim, aes(x = Date, y = Cum_cases , color = "Cumulative cases"))+
  geom_line()+
  geom_line(aes(y = Cum_deaths, color = "cumulative deaths"))+
  scale_y_log10()+
  labs(y= "Cumulative Cases/ Deaths", title = "Cumulative Cases and Deaths")+
  theme(axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())
  

#Moving average
library(zoo)
covid_zim$New_cases_MA <- rollmean(covid_zim$New_Cases, 
                                   k =5,
                                   align = "right",
                                   fill = NA)

#Plotting a graph- daily reported deaths-5 day moving average
covid_zim$New_Deaths_MA <- rollmean(covid_zim$New_Deaths,
                                    k = 5,
                                    align = "right",
                                    fill = NA)
#Plotting a graph-daily covid infections
library(ggplot2)
ggplot(data = covid_zim, aes(x = Date, y = New_Cases)) +
  geom_bar(stat = "identity", fill = "darkblue")+
  geom_line(aes(x = Date, y = New_cases_MA, color = "5 day Moving Average"), size =1)+
  scale_y_continuous(position = "right", breaks = seq(0,2000, by =200))+
  scale_x_date(date_labels = "%b %Y")+
  labs(y = "New Cases", title = "Zimbabwe Daily Reported Covid-19 Infections:Nov 2020-Jan 2021",
       caption = "Data Source:MoHCC, Government of Zimbabwe") +
  theme(axis.line =element_line(size =2,color = "green", linetype = 1),
        axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())

#Plotting a graph -daily reported deaths
ggplot(data = covid_zim, aes(x = Date, y = New_Deaths )) +
  geom_bar(stat = "identity", fill = "darkblue")+
  geom_line(aes(x = Date, y = New_Deaths_MA, color = "5 day Moving Average"), size = 1)+
  scale_y_continuous(position = "right")+
  scale_x_date(date_labels = "%b %Y") +
  labs(y = "New Deaths", title = "Zimbabwe Daily Reported Covid-19 Deaths:Nov 2020 to Jan 2021",
       caption = "Data Source: MoHCC, Government of Zimbabwe") +
  theme(axis.line =element_line(size =2,color = "green", linetype = 1),
        axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())
  
  
  


#Plotting a graph of daily covid infections-5 day moving average
ggplot(data = covid_zim, aes(x = Date, y = New_cases_MA)) +
         geom_bar(stat = "identity", fill = "darkblue")+
  scale_y_continuous(position = "right", breaks = seq(0,2000, by =200))+
  labs(y = "New Cases", title = "Daily Cases-5 Day Moving Average") +
  theme(axis.line =element_line(size =2,color = "blue", linetype = 1),
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())
  
#Plotting a graph -daily reported deaths
ggplot(data = covid_zim, aes(x = Date, y = New_Deaths )) +
  geom_bar(stat = "identity", fill = "darkblue")+
  scale_y_continuous(position = "right")+
  labs(y = "New Deaths", title = "Daily Reported Deaths") +
  theme(axis.line =element_line(size =2,color = "blue", linetype = 1),
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())
#Plotting a graph- daily reported deaths-5 day moving average
covid_zim$New_Deaths_MA <- rollmean(covid_zim$New_Deaths,
                                    k = 5,
                                    align = "right",
                                    fill = NA)
#Plotting a graph of reported deaths -5 day moving average
ggplot(data = covid_zim, aes(x = Date, y = New_Deaths_MA )) +
geom_bar(stat = "identity", fill = "dark green")+
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(position = "right")+
  labs(y = "New Deaths", title = "Daily Reported Deaths-5 Day Moving Average") +
  theme(axis.line =element_line(size =2,color = "blue", linetype = 1),
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())

#Comparing Daily deaths Moving Average vs Moving Average of daily cases 
ggplot(data = covid_zim, aes(x = Date, y = New_cases_MA , color = "New cases"))+
  geom_line()+
  geom_line(aes(y = New_Deaths_MA, color = "cumulative deaths"))+
  scale_y_log10()+
  labs(y= "Cumulative Cases/ Deaths", title = "Cumulative Cases and Deaths")+
  theme(axis.ticks = element_line(size = 2, colour = "blue"), 
        axis.text.x = element_text(size = 10, colour = "blue", face = "bold"),
        axis.text.y = element_text(size = 10, colour = "blue", face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "blue", face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title =  element_text(size = 12, colour = "blue", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "light grey"),
        panel.grid = element_blank())





