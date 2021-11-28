# load packages
library(ggplot2)
library(plotly)
library(astsa)

# load dataset
airquality <- read.csv('SD_airquality_2016-2020.csv')

# data summary
summary(airquality)

# data visualizations (convert Date into date data type first)
airquality$Date <- as.Date(airquality$Date, format="%m/%d/%Y")

# looking at PM2.5 across all five years
firstfigure <- airquality %>%
  ggplot( aes(x=Date, y=PM2.5.AQI.Value)) +
  geom_area(fill='darkblue', alpha=0.5) +
  geom_line(color='darkblue') +
  ylab('PM 2.5 AQI Value') + 
  ggtitle('PM 2.5 AQI Data from 2016-2020') +
  theme(plot.title=element_text(size=14, face="bold"), 
        axis.title.y=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"))

firstfigure <- ggplotly(firstfigure)
firstfigure

# OBSERVATIONS:
# - quick observations show that many peak PM2.5 readings occur in Q4 of the year
# - interestingly, some of the lowest readings occur around MAR/APR
# - sharp decrease in PM2.5 readings in MAR 2020 (possibly related to COVID lockdowns?)
# - max PM2.5 reading from SEP-OCT 2020 (coinciding with fires)

# year comparisons of PM2.5
# add additional columns for year and day of the year
airquality <- transform(airquality,
                        Year = format(Date, '%Y'),
                        DayOfYear = as.numeric(format(Date, '%j')))

secondfigure <- airquality %>%
  ggplot( aes(x=DayOfYear, y=PM2.5.AQI.Value, group=Year, colour=Year)) +
  geom_point() + 
  geom_line() +
  ylab('PM 2.5 AQI Value') + 
  ggtitle('PM 2.5 AQI Data - Year Comparisons') +
  theme(plot.title=element_text(size=14, face="bold"), 
        axis.title.y=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"))

secondfigure <- ggplotly(secondfigure)
secondfigure

# OBSERVATIONS:
# - 2016 has erratic readings during Q1, including both highest and lowest PM2.5 of that year
# - 2017 has peak readings during last two months of year
# - 2018 has peaks around JUL and from DEC-FEB
# - 2019 peaks around OCT, but has noticeably less variation in spikes compared to previous years
# - 2020 has the biggest range, with the lowest of the 5 year readings occurring MAR2020 and the highest occuring SEP2020
# - 2020 has a general trend upwards as year progresses (random walk?)
# majority of PM2.5 readings occur between 50-100

# ACF and PACF plots (lag=365 selected to represent a year)
acf2(airquality$PM2.5.AQI.Value, 365, main='ACF & PACF')
