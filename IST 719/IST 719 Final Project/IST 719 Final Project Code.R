######################################################################

library(rnaturalearth)
library(lubridate)
library(dplyr) # to use the Distinct function
library(tidyr)
library(sp)
library(jsonlite)
library(tidyverse)
library(maps)
library(mapproj)
library(ggmap)
library(nycflights13)
library(geosphere)
library(knitr)
library(kableExtra)
library(stats)
library(ggplot2)

######################################################################

#getwd()
setwd('/Users/gml/Desktop')

df1 <- read.csv("Origin_DEN1.csv", stringsAsFactors = FALSE)

######################################################################

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

NewLatLon<-function(addresses){
  d <- suppressWarnings(lapply(addresses, function(address) {
    #set the elapsed time counter to 0
    t <- Sys.time()
    #calling the nominatim OSM API
    api_output <- nominatim_osm(address)
    #get the elapsed time
    t <- difftime(Sys.time(), t, 'secs')
    #return data.frame with the input address, output of the nominatim_osm function and elapsed time
    return(data.frame(address = address, api_output, elapsed_time = t))
  }) %>%
    #stack the list output into data.frame
    bind_rows() %>% data.frame())
  #output the data.frame content into console
  return(d)
}

d <- suppressWarnings(lapply(addresses, function(address) {
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- nominatim_osm(address)
  #get the elapsed time
  t <- difftime(Sys.time(), t, 'secs')
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  return(data.frame(address = address, api_output, elapsed_time = t))
}) %>%
  #stack the list output into data.frame
  bind_rows() %>% data.frame())

######################################################################

df2 <-airports[c('faa','lat','lon')]
colnames(df2) <- c('DEST','D.lat','D.lon')
df<-merge(x=df1,y=df2,by="DEST", all.x=TRUE)
df['DEP_DEL15'][is.na(df['DEP_DEL15'])] <- 0
df$FL_DATE <- as.Date(df$FL_DATE, '%m/%d/%y')
df1 <- data.frame(df[df['DEP_DELAY_NEW'] > 0,])
df1 <- data.frame(df1[df1$CANCELLED == '0',])

############################################################################################################################################

### Time Bar Cancel
a <- df %>% group_by(FL_DATE) %>%
  summarize(sum(CANCELLED))
colnames(a) <- c('Day', 'Canceled')
a <- a[order(a$`Day`,decreasing = F), ]

ggplot(a, aes(x=Day)) + 
  geom_line(aes(y=Canceled)) +
  coord_cartesian(ylim =c(-1, 1.05*max(a$Canceled)))+
  labs(title="Daily Canceled Flights")
#subtitle="Returns Percentage from 'Economics' Dataset", 
#caption="Source: Economics", 
#y="Returns %")

############################################################################################################################################


addresses <- "Denver International Airport"
Den_LL <- NewLatLon(addresses)
flydata <- df[c('D.lon','D.lat')]
flydata <- unique(flydata)
flydata1 <- filter(flydata, D.lat < 49)
flydata <- filter(flydata1, D.lon > -130)

map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15",
    ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))

points(Den_LL$lon[1], Den_LL$lat[1], pch=8, cex=1, col="turquoise2")

points(flydata$D.lon,flydata$D.lat, pch=3, cex=0.1, col="chocolate1")

for (i in (1:dim(df)[1])) { 
  inter <- gcIntermediate(c(Den_LL$lon[1], Den_LL$lat[1]), c(flydata$D.lon[i],flydata$D.lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")   
}

############################################################################################################################################

Departure_addresses <- c("Los Angeles International Airport", "Phoenix Sky Harbor International Airport",
                         "Salt Lake City International Airport", "McCarran International Airport",
                         "Seattle-Tacoma International Airport", "O'Hare International Airport",
                         "San Francisco International Airport", "Minneapolis−Saint Paul International Airport",
                         "DFW International Airport", "Hartsfield Jackson Atlanta International Airport")

Departure_abbv <- c("LAX","PHX","SLC","LAS","SEA","ORD","SFO","MSP","DFW","ATL")

Departure_W <- c(3, 2.75, 2.5, 1, 1, 1, .8, .5, .4, .25)

latlon<-NewLatLon(Departure_addresses)
map("state",fill=T, col="White")
points(latlon$lon, latlon$lat,  pch = 1, cex = .8, col = "Red",)
for (i in (1:dim(latlon)[1])) { 
  inter <- gcIntermediate(c(Den_LL$lon[1], Den_LL$lat[1]), c(latlon$lon[i],latlon$lat[i]), n=200)
  lines(inter, lwd=Departure_W[i], col="turquoise2")   
}
points(Den_LL$lon[1], Den_LL$lat[1], pch=8, cex=1, col="black")

map("world", regions=c("usa"), fill=T, col="White",
    ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))

############################################################################################################################################

##Tables

airlines <- c("Southwest Airlines Co.",
              "United Air Lines Inc.",
              "SkyWest Airlines Inc.",
              "Frontier Airlines Inc.",
              "Delta Air Lines Inc.",
              "American Airlines Inc.",
              "Republic Airline",
              "Spirit Air Lines",
              "Alaska Airlines Inc.",
              "JetBlue, Virgin America, 
Allegiant, Endeavor")
Num_Flights <- c(68512, 60303,48474, 22033,11622, 10559,3464, 3412,1827, 1700)

Top_Airlines <- data.frame(c(airlines))
Top_Airlines['Total Flights'] <- Num_Flights
colnames(Top_Airlines) <- c('Airlines', 'Total Flights')

Top_Airlines %>%
  kable() %>%
  kable_styling()

dest <- c("LAX",
          "PHX",
          "SLC",
          "LAS",
          "SEA",
          "ORD",
          "SFO",
          "MSP",
          "DFW",
          "ATL")

Num_Flights <- c(8318,7505,7250,6924,6623,6450,6343,5893,5741,5662)

Top_Destinations <- data.frame(c(dest))
Top_Destinations['Total Flights'] <- Num_Flights
colnames(Top_Destinations) <- c('Top Destinations', 'Total Flights')

Top_Destinations %>%
  kable() %>%
  kable_styling()

############################################################################################################################################

### Delay Codes Cirlce
a <- df %>% group_by() %>%
  summarize(Airline = sum(CARRIER_DELAY), Weather = sum(WEATHER_DELAY),
            NAS = sum(NAS_DELAY), Security = sum(SECURITY_DELAY),
            Late_Aircraft = sum(LATE_AIRCRAFT_DELAY))

a <- data.frame(category = c(colnames(a[1]),colnames(a[2]),'National Airspace System',colnames(a[4]),'Late Aircraft'),
                counts = c(a$Airline,a$Weather,a$NAS,a$Security,a$Late_Aircraft))

# Compute percentages
a$fraction <- a$count / sum(a$count)

# Compute the cumulative percentages (top of each rectangle)
a$ymax <- cumsum(a$fraction)

# Compute the bottom of each rectangle
a$ymin <- c(0, head(a$ymax, n=-1))

# Compute label position
a$labelPosition <- (a$ymax + a$ymin) / 2

# Compute a good label
a$label <- paste0(a$category, "\n value: ", a$count)

# Make the plot
ggplot(a, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=2.5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() 

############################################################################################################################################

### Circle
canceled <- sum(df$CANCELLED)
delayed <- sum(df$DEP_DEL15)
NotC_D <- length(df$DEST) - delayed - canceled

a <- data.frame(category = c('Canceled','Delayed','Not Canceled or Delayed'),
                counts = c(canceled,delayed,NotC_D))

# Compute percentages
a$fraction <- a$count / sum(a$count)

# Compute the cumulative percentages (top of each rectangle)
a$ymax <- cumsum(a$fraction)

# Compute the bottom of each rectangle
a$ymin <- c(0, head(a$ymax, n=-1))

# Compute label position
a$labelPosition <- (a$ymax + a$ymin) / 2

# Compute a good label
a$label <- paste0(a$category, "\n value: ", a$count)

# Make the plot
ggplot(a, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=2.5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()

############################################################################################################################################

a <- df %>% group_by(FL_DATE) %>%
  summarize(sum(DEP_DEL15),sum(Flights))
colnames(a) <- c('Day', 'Number of Delays', 'Number of Total Flights')
a['norm'] <- (a['Number of Delays'])/(a['Number of Total Flights'])
a['Number of Delays'][is.na(a['Number of Delays'])] <- 0
mdelays <- median(a$norm)
a$norm <-100*((a$norm-mdelays)/mdelays)

ggplot(a, aes(x=Day)) + 
  geom_line(aes(y=norm)) +
  coord_cartesian(ylim =1.05*(c(-1*(max(a$norm)),max(a$norm)))) +  
  labs(title="Delay % Above or Delow the Daily Medain of Delay", 
       y="% of Median" )+ 
  geom_hline(yintercept = 0)

##
##b <- a[order(a$norm,decreasing = T), ]
##head(b, 20)

############################################################################################################################################

a <- df1 %>% group_by(Time2) %>%
  summarize(quantile(DEP_DELAY_NEW,.99,na.rm=T))
a <- df1 %>% group_by(Time2) %>%
  summarize(mean(DEP_DELAY_NEW))
a<-a[3,2]


df2<-df1[!is.na(df1),]
df2<-df2[!is.na(df2$DEP_DELAY_NEW),]
a <- df1 %>% group_by(df$OP_UNIQUE_CARRIER) %>%
  summarize(quantile(DEP_DELAY_NEW,.99,na.rm=T))
#a<-a[3,2]
mean_time <- data.frame(Time2 = c('Morning','Afternoon','Night'), DEP_DELAY_NEW = c( 27,33, 39))
mean_time3 <- data.frame(Time2 = c('Morning','Afternoon','Night'), DEP_DELAY_NEW = c( 10,14, 18))
mean_time2 <- data.frame(Time2 = c('Morning','Afternoon','Night'), DEP_DELAY_NEW = c( 68,85, 94))

quantile(df1$DEP_DELAY_NEW,.994,na.rm=T)

a <- data.frame(Distance = df2['DISTANCE'],DEP_DELAY_NEW = df2['DEP_DELAY_NEW'], time2 = df2['Time2'])
a <- unique(a)

ggplot(a, aes(DISTANCE, DEP_DELAY_NEW)) +
  geom_point(alpha = .05) +
  geom_hline(aes(yintercept = DEP_DELAY_NEW),mean_time, col= 'blue') +
  geom_hline(aes(yintercept = DEP_DELAY_NEW),mean_time2, color = 'red') +
  geom_hline(aes(yintercept = DEP_DELAY_NEW),mean_time3, color = 'green') +
  coord_cartesian(ylim =c (-1, 300)) +
  facet_wrap(~ Time2)


############################################################################################################################################

### Scatter by month and Carrier
#df3 <- na.omit(df)
#a <- df3 %>% group_by(Month,OP_UNIQUE_CARRIER) %>%
#  summarize(sum(DEP_DEL15), sum(Flights), median(DEP_DELAY_NEW))#

#a <- df2 %>% group_by(Month,OP_UNIQUE_CARRIER) %>%
#  summarize(mean(DEP_DELAY_NEW))
#a <- data.frame(Month = z,Airline = y, Delays_Norm = x/w, AVG_Delay = v )
#colnames(a) <- c('Month', 'Airline','Normalized_Delays', 'Average_Delay_Time')

#a

a[order(a$Normalized_Delays, decreasing = T),]
a[order(a$`Number of Delays`, decreasing = F), ]



#a <- df3 %>% group_by(OP_UNIQUE_CARRIER) %>%
#  summarize(sum(DEP_DEL15), sum(Flights), median(DEP_DELAY_NEW))

df3 <- read.csv("finalfile.csv", stringsAsFactors = FALSE)
df3$Normalized_Delays<- 100*(df3$Normalized_Delays)

ggplot(df3, aes(x=Month, y=Normalized_Delays)) + 
  geom_point(aes(col=Airline, size=Average_Delay_Time)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Area Vs Population", 
       y="Monthly % of Delayed Flights", 
       x="Months", 
       title="Scatterplot of Normalized Delays") + theme(axis.text.x = element_blank())
