print("Weiyang Wang")
print(1505028)
print("wwang65@ucsc.edu")

# Question 1
library(foreign)
df.airports<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv",stringsAsFactors=F)
df.flights<-read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv",stringsAsFactors=F)
df.planes<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv",stringsAsFactors=F)
df.weather<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv",stringsAsFactors=F)

# Question 2
df.flights$date=as.Date(df.flights$date)
df.weather$date=as.Date(df.weather$date)

# Question 3
require(dplyr)
df.flights.2a<-df.flights %>%
  dplyr::filter(
    dest=="SFO" | dest=="OAK")
print(nrow(df.flights.2a))
df.flights.2b<-df.flights %>%
  dplyr::filter(
    dep_delay>1)
print(nrow(df.flights.2b))
df.flights.2c<-df.flights %>%
  dplyr::filter(
    arr_delay>2*dep_delay)
print(nrow(df.flights.2c))

# Question 4
a.4<-select(df.flights, contains("dep_delay"))
b.4<-select(df.flights, starts_with("dep_"))
c.4<-select(df.flights, matches(".p_de."))

# Question 5
df.flights.5a<-arrange(df.flights,desc(dep_delay)) %>% head(5)
df.flights.5a
df.flights.5b<-arrange(df.flights,desc(dep_delay-arr_delay)) %>% head(5)
df.flights.5b

# Question 6
df.flights<-df.flights %>%
  mutate(speed=dist/(time/60))
arrange(df.flights.6a,desc(speed)) %>% head(5)
df.flights<-df.flights %>%
  mutate(delta=dep_delay-arr_delay)
arrange(df.flights.6b,desc(delta)) %>% head(5)
arrange(df.flights.6b,delta) %>% head(1)

# Question 7
df.flights.7a<-df.flights %>%
  group_by(carrier) %>% summarise(
    cancelled = sum(cancelled),
    total=n(),
    cancelled.percentage = cancelled/total,
    min = min(delta, na.rm=T),
    Quant.1st= quantile(delta, 0.25, na.rm=T),
    median = median(delta, na.rm=T),
    mean = mean(delta, na.rm=T),
    Quant.3rd= quantile(delta, 0.75, na.rm=T),
    Quant.4th= quantile(delta, 0.90, na.rm=T),
    max= max(delta, na.rm=T))
arrange(df.flights.7a, desc(cancelled.percentage))
day.delay<-df.flights %>%
  dplyr::filter(
    !is.na(dep_delay)) %>%
    group_by(date) %>% (summarise(
      delay= mean(dep_delay),
      n= n()) %>%
        dplyr::filter(n>10))

# Question 8
day_delay<- day_delay %>%
  mutate(diff = delay-lag(delay))
arrange(day_delay, desc(diff)) %>% head(5)

# Question 9
dest_delay<-df.flights %>% group_by(dest) %>%
  summarise(
    avg.arr_delay= mean(arr_delay, na.rm=T),
    number.flights = n())
df.airports<- df.airports %>%
  select(dest=iata, name=airport,city,state,lat,long)
df.9a<-df.airports %>% left_join(dest_delay, by="dest")
arrange(df.9a, desc(avg.arr_delay)) %>% head(5)
df.9b<-df.airports %>% inner_join(dest_delay, by="dest")
# Do not match
df.9c<-df.airports %>% right_join(dest_delay, by="dest")
# 116 observations, and no NA
df.9d<-df.airports %>% full_join(dest_delay, by="dest")
# 3378 observations, and 3262 NAs

# Question 10
hourly_delay <-df.flights %>% 
  filter(!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(
    delay = mean(dep_delay), 
    n = n())
hourly_delay %>% full_join(weather) %>% group_by(conditions) %>% 
  summarise(
    max_delay = max(delay, na.rm=T)) %>% 
  arrange(desc(max_delay))

# Question 11
library(tidyr)
# part a 
df<-data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>% gather(subject, value, -treatment) %>% 
  mutate(
    subject = subject %>% 
      substr(8,9)) %>% 
      select(subject, treatment, value)

# part b
df<-data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6))
df
df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

# part c
df<-data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6))
df
df %>% separate(demo, into = c('sex','age','state') , sep = '_')

#Part D
df<-data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6))
df

df<-df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df











