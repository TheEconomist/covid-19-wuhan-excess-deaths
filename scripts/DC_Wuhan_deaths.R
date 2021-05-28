# Daily Chart - Wuhan deaths

library(readr)
library(ggplot2)

# 1. Load Wuhan data --------------------------------------------------

# Source: https://www.bmj.com/content/372/bmj.n415
# Data from online supplemental materials. I requested full replication data but was not supplied it.
china <- read_csv("source-data/china_bmj_excess_deaths.csv", skip = 1)
china <- china[, 1:5]

# Population associated with each reporting area also from supplemental materials:
china$type <- "Wuhan"

# Reporting population in 2020
china$population <- 2300887 

# Mean reporting population in past years
china$mean_population_2015_2019 <- (2202663 + 2280200 + 2314269 + 2293425 + 2300887)/5  
# Total population of the area it generalizes to
china$total_population <- 11081000 # from http://www.hb.xinhuanet.com/2019-03/26/c_1124281764.htm

# This adjusts for deaths data not being recorded when the paper was written:
china$reporting_adjusted_deaths <- china$`No. of reported deaths in 2020`/(china$`Delay adjustment ratio (%)*`/100)

# Since they do not supply the raw data, we cannot do better than:
china$expected_deaths <- china$`Mean No. of reported Deaths, 2015-19`

# With population-change multiplier:
china$expected_deaths <- china$expected_deaths*(china$population/china$mean_population_2015_2019)
# Note: supplemental figure 2 suggests the mean should work well, no strong over-time-trend in per capita death.

# To inspect our estimates for China excess deaths by area, uncomment the below chunk:
ggplot(china[china$type == "Wuhan", ], aes(x=as.numeric(Week), y=100000*reporting_adjusted_deaths/population, col = "2020"))+
  geom_line()+
  geom_line(aes(y=100000*expected_deaths/population, col = "Expected"))+
  geom_hline(aes(yintercept = 0), col = "black")+ylim(c(-10, 100))+
  theme_minimal()+facet_grid(.~type)+xlab("Week in 2020")+ylab("Deaths per 100k population")

# Specify excess deaths (scaling to population represented)
china$excess_deaths <- china$reporting_adjusted_deaths - china$expected_deaths
china$excess_deaths_per_100k <- 100000*china$excess_deaths/china$population
china$excess_deaths_total_for_area <- china$excess_deaths*(china$total_population/china$population)

# Add proper date
china <- china[china$`Starting date of the week` != "Jan-March", ]
china$date <- as.Date(paste0(china$`Starting date of the week`, "-2020"), 
                            format = "%d-%b-%Y")

# Plot 
ggplot(china[china$type == "Wuhan", ], aes(x=date, y=excess_deaths_total_for_area/7, col = "2020"))+
  geom_step()+
  geom_hline(aes(yintercept = 0), col = "black")+
  theme_minimal()+xlab("")+
  ylab("Excess deaths, daily")+facet_grid(.~type)

# Load China's official Wuhan data:
library(readxl)
wuhan_official <- read_xlsx("source-data/wuhan_cases_deaths.xlsx") 
wuhan_official$city <- "Wuhan"
wuhan_official$country <- "China"
wuhan_official$province <- "Hubei"
wuhan_official$date <- as.Date(wuhan_official$date)

# Merge them together:
library(lubridate)
china$week <- week(china$date)
wuhan_official$week <- week(wuhan_official$date)

wuhan <- merge(china[china$type == "Wuhan" & !is.na(china$date), c("excess_deaths_total_for_area", "week")], wuhan_official, by = "week", all.y = T)

# Transform excess deaths to daily data
wuhan <- wuhan[order(wuhan$date), ]
wuhan$excess_deaths <- wuhan$excess_deaths_total_for_area/7
library(lubridate)
wuhan$excess_deaths <- ave(wuhan$excess_deaths, wuhan$week, FUN = function(x) na.omit(x)[1])

# Transform deaths to daily data
wuhan <- wuhan[order(wuhan$date), ]
wuhan$new_deaths <- wuhan$dead - c(0, wuhan$dead)[1:length(wuhan$dead)]

ggplot(wuhan, aes(x=date, y=excess_deaths, col = "Excess deaths"))+geom_line()+geom_line(aes(y=new_deaths, col = "Official covid-19 deaths"))+theme_minimal()+ggtitle("Wuhan")+xlab("")

# 2. Load London data --------------------------------------------------

# Excess deaths
# Source: https://fingertips.phe.org.uk/static-reports/mortality-surveillance/excess-mortality-in-London-latest.html#download-the-data
london <- read_xlsx("source-data/uk_excess_deaths.xlsx")
london <- london[london$RGN09NM == "London" &
                   london$Chart_Name == "Region", ]
london$start <- as.Date(london$Period_Start)
london$end <- as.Date(london$Period_End)

# Covid deaths
# Source: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
london_official <- read_csv("source-data/london_official.csv")
london_official <- london_official[london_official$name == "London", ]
london_official <- london_official[order(london_official$date), ]

# Merge the two:
london_official$excess_deaths <- NA
for(i in 1:nrow(london)){
  london_official$excess_deaths[london_official$date >= london$start[i] & london_official$date <= london$end[i]] <- london$Excess_Deaths[i] / length(london$start[i]:london$end[i])
}

london <- london_official
ggplot(london, aes(x=date, y=excess_deaths))+
  geom_line()+geom_line(aes(y=new.deaths, col = "offical covid-19 deaths"))+theme_minimal()+ylab("Daily deaths")+xlab("")+ggtitle("London")

# 3. Load New York City data --------------------------------------------------

# Excess deaths
# Source: https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2767980
# Specifically: https://zenodo.org/record/3893882#.YK5eGKhKjb0
# To recreate, run script 'us_pi_excess' up to line 1042

nyc <- read_csv("source-data/NYC_excess_deaths.csv")
nyc <- nyc[!is.na(nyc$obs),]

nyc$excess <- nyc$obs - nyc$pred
nyc <- nyc[!is.na(nyc$pred),]
nyc <- nyc[nyc$week_start>= as.Date('2020-01-01'),]
nyc$week_end <- nyc$week_start + days(6)

# Get official deaths:
# Source: https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv

nyc_official <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv")

nyc_official$date <- as.Date(nyc_official$date_of_interest, format = "%m/%d/%Y")

# Merge the two:
nyc_official$excess_deaths <- NA
for(i in 1:nrow(nyc)){
  nyc_official$excess_deaths[nyc_official$date >= nyc$week_start[i] & nyc_official$date <= nyc$week_end[i]] <- nyc$excess[i] / length(nyc$week_start[i]:nyc$week_end[i])
}
nyc <- nyc_official



# 3. Combine all three --------------------------------------------------
wuhan$city <- "Wuhan"
nyc$city <- "New York City"
london$city <- "London"

wuhan$covid_deaths <- wuhan$new_deaths
nyc$covid_deaths <- nyc$DEATH_COUNT
london$covid_deaths <- london$new.deaths

cities <- rbind(wuhan[, c("date", "city", "excess_deaths", "covid_deaths")],
                nyc[, c("date", "city", "excess_deaths", "covid_deaths")],
                london[, c("date", "city", "excess_deaths", "covid_deaths")])

cities$city <- factor(cities$city, levels = unique(cities$city))


# 4. Export data for plots --------------------------------------------------

# Line chart:
write_csv(cities, "output-data/dc_wuhan_line_chart.csv")
ggplot(cities, aes(x=date, col=city))+
  geom_line(aes(y=excess_deaths, col = 'Excess deaths'))+
  geom_line(aes(y=covid_deaths, col = "Covid-19 deaths"))+
  theme_minimal()+facet_grid(.~city)+
  xlab("")+ylab("")+theme(legend.title = element_blank())+theme(legend.position = "bottom")
ggsave("excess_and_covid_deaths_by_city.png", width = 8, height = 4)

# Bar chart:
# 1. Calculate total excess and covid-19 deaths
cities$total_covid_deaths <- ave(cities$covid_deaths, cities$city, 
                                 FUN = function(x) sum(x, na.rm = T))
cities$total_excess_deaths <- ave(cities$excess_deaths, cities$city, 
                                  FUN = function(x) sum(x, na.rm = T))
# 2. Calculate first and second waves
cities$total_covid_deaths_first_wave <- NA
cities$total_covid_deaths_second_wave <- NA
cities$total_excess_deaths_first_wave <- NA
cities$total_excess_deaths_second_wave <- NA
for(i in unique(cities$city)){
  cities$total_covid_deaths_first_wave[cities$city == i] <- sum(cities$covid_deaths[cities$city == i & cities$date <= as.Date("2020-06-01")], na.rm = T)
  
  cities$total_covid_deaths_second_wave[cities$city == i] <- sum(cities$covid_deaths[cities$city == i & cities$date >= as.Date("2020-06-01")], na.rm = T)
  
  cities$total_excess_deaths_first_wave[cities$city == i] <- sum(cities$excess_deaths[cities$city == i & cities$date <= as.Date("2020-06-01")], na.rm = T)
  
  cities$total_excess_deaths_second_wave[cities$city == i] <- sum(cities$excess_deaths[cities$city == i & cities$date >= as.Date("2020-06-01")], na.rm = T)
  }

# 3. Transform from wide to long
pdat <- cities[!duplicated(cities$city), ]
library(tidyr)
pdat <- pdat %>% gather(key = "type", value, -city)
pdat <- pdat[pdat$type %in% c("total_excess_deaths_first_wave",
                              "total_covid_deaths_first_wave",
                              "total_covid_deaths"), ]
pdat <- rbind(pdat, 
              c("Wuhan", "total_reported_excess_deaths**", 5954))
pdat$value <- as.numeric(pdat$value)

pdat$type[pdat$type == "total_excess_deaths_first_wave" & pdat$city == "Wuhan"] <- "total_excess_deaths_first_wave*"

write_csv(pdat, "output-data/dc_wuhan_bar_chart.csv")
ggplot(pdat, aes(y=type, x=value, yend=type, xend=0, col = city))+geom_segment()+facet_grid(city~.)+theme_minimal()+xlab("")+ylab("")+theme(legend.position = "none")


# 5. Not used: Demography data --------------------------------------------------

# Population over 65:
# China: 11.472 -- source; https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS?locations=CN
# London: 11.6 -- source: https://cambridgeshireinsight.org.uk/population/ons-population-pyramid/
# New York City: 15.54 -- source: https://www.baruch.cuny.edu/nycdata/population-geography/age_distribution.htm 


