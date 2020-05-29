#Load Libraries
library(dplyr) # to use the Distinct function
library(tidyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tmaptools)
library(usmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(maps)

world <- ne_countries(scale = "medium", returnclass = "sf")
lake_coords <- geocode_OSM('Onondaga Lake, Syracuse, NY')
sites <- data.frame(location = 'Onondaga Lake' ,longitude = lake_coords$coords[1], latitude = lake_coords$coords[2])
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("new york", counties$ID))

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  geom_text_repel(data = sites, aes(x = longitude, y = latitude, label = location), 
                  fontface = "bold", nudge_x = c(1), nudge_y = c(-.5)) +
  coord_sf(xlim = c(-80, -73), ylim = c(40.5, 45.5), expand = FALSE)


#Load Function to make NA into average of the data
mymean <- function(cleaned_us){
  column_total = sum(cleaned_us, na.rm = TRUE) #change
  column_length = sum(!is.na(cleaned_us)) #change
  return(column_total/column_length)
}

#Set Working Directory
getwd()
setwd('\\\\hd.ad.syr.edu/02/fe7f28/Documents/Desktop')

#Load Data
df <- data.frame(read.csv('BuoyData_2_2_2.csv', 
                          header = T, stringsAsFactors = F))

#View Data
#View(df)
str(df)
summary(df)

#Checking if there is NAs in the dataset
sum(is.na(df))

#Checking each column if there is any NAs
sum(is.na(df[1]))
sum(is.na(df[2]))
sum(is.na(df[3]))
sum(is.na(df[4]))
sum(is.na(df[9]))
sum(is.na(df[10]))
sum(is.na(df[11]))
sum(is.na(df[12]))

#If I wanted to remove all NA Rows
#df <- na.omit(df)

#instead I decided to make all NAs the average of the data
sum(is.na(df[5]))
  df[5][is.na(df[5])] <-  mymean(df[5])
sum(is.na(df[5]))

sum(is.na(df[6]))
  df[6][is.na(df[6])] <-  mymean(df[6])
sum(is.na(df[6]))
  
sum(is.na(df[7]))
  df[7][is.na(df[7])] <-  mymean(df[7])
sum(is.na(df[7]))

sum(is.na(df[8]))
  df[8][is.na(df[8])] <-  mymean(df[8])
sum(is.na(df[8]))

a <- c("DATE_TIME", "DEPTH_m","T_DEGC","SC_us_cm","pH","Dox_mg_L",
       "Tn_Ntu","Chl_ug_L","PRECIP_in","AWND_mph","WDF5_deg","WSF5_mph")
colnames(df) <- a
#Data from Syracuse Airport Meterological Station
df1 <- df # just the 

#Data from Syracuse Airport Meterological Station
df1$DAY <- as.Date(df1$DATE_TIME, '%m/%d/%y %H:%M')
DATElookup <- as.POSIXlt(strptime(df1$DATE_TIME, '%m/%d/%y %H:%M'))

a <- DATElookup$year
  a <- a+1900
b <- DATElookup$mon +1
c <- paste(as.character(DATElookup$mon +1), as.character(a))
d <- DATElookup$mday
e <- paste(as.character(DATElookup$mon +1), as.character(DATElookup$mday))

df1$YEARS <- a
df1$MONTHS <- b
df1$MONTHS_YEARS <- c
df1$MDAYS <- d
df1$MDAYS_MONTHS <- e

df1$DEPTH_m_c <-cut(df$DEPTH_m, breaks= seq(0, 19, by = 6))
df1$T_DEGC_C <-cut(df$T_DEGC, breaks= seq(0, 31, by = 6))
summary(df1$T_DEGC)
df1$YEARS <- as.factor(df1$YEARS)
df1$MONTHS <- as.factor(df1$MONTHS)
df1$MONTHS_YEARS <- as.factor(df1$MONTHS_YEARS)
df1$MDAYS_MONTHS <- as.factor(df1$MDAYS_MONTHS)

df_day <- df1 %>% group_by(Day=floor_date(DAY, "day")) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_day$T_DEGC_C <-cut(df_day$T_DEGC, breaks= seq(0, 31, by = 6))

df_day_Depth <- df1 %>% group_by(Day=floor_date(DAY, "day"),
                                 DEPTH_m=cut(DEPTH_m, breaks= seq(0, 19, by = 6))) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_day_Depth$T_DEGC_C <-cut(df_day_Depth$T_DEGC, breaks= seq(0, 31, by = 6))

df_week <- df1 %>% group_by(week=floor_date(DAY, "week")) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_week$T_DEGC_C <-cut(df_week$T_DEGC, breaks= seq(0, 31, by = 6))

df_week_Depth <- df1 %>% group_by(week=floor_date(DAY, "week"),
                                 DEPTH_m=cut(DEPTH_m, breaks= seq(0, 19, by = 6))) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_week_Depth$T_DEGC_C <-cut(df_week_Depth$T_DEGC, breaks= seq(0, 31, by = 6))

df_month <- df1 %>% group_by(month=floor_date(DAY, "month")) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_month$T_DEGC_C <-cut(df_month$T_DEGC, breaks= seq(0, 31, by = 6))


df_month_Depth <- df1 %>% group_by(month=floor_date(DAY, "month"),
                                  DEPTH_m=cut(DEPTH_m, breaks= seq(0, 19, by = 6))) %>%
  summarize(T_DEGC = mean(T_DEGC),SC_us_cm = mean(SC_us_cm),
            pH = mean(pH),Dox_mg_L = mean(Dox_mg_L),
            Tn_Ntu = mean(Tn_Ntu),Chl_ug_L = mean(Chl_ug_L),
            PRECIP_in = mean(PRECIP_in),AWND_mph = mean(AWND_mph), 
            WDF5_deg = mean(WDF5_deg),WSF5_mph=mean(WDF5_deg))
df_month_Depth$T_DEGC_C <-cut(df_month_Depth$T_DEGC, breaks= seq(0, 31, by = 6))

ggplot(df1, aes(MONTHS, Chl_ug_L)) + geom_boxplot()
ggplot(df1, aes(MONTHS, pH )) + geom_boxplot()
ggplot(df1, aes(MONTHS, Dox_mg_L )) + geom_boxplot()
ggplot(df1, aes(MONTHS, Tn_Ntu )) + geom_boxplot() + scale_y_continuous(limits = quantile(df1$Tn_Ntu, c(0, .98)))
ggplot(df1, aes(MONTHS, SC_us_cm )) + geom_boxplot()

ggplot(df1, aes(T_DEGC_C, Chl_ug_L)) + geom_boxplot()
ggplot(df1, aes(T_DEGC_C, pH )) + geom_boxplot()
ggplot(df1, aes(T_DEGC_C, Dox_mg_L )) + geom_boxplot()
ggplot(df1, aes(T_DEGC_C, Tn_Ntu )) + geom_boxplot() + scale_y_continuous(limits = quantile(df1$Tn_Ntu, c(0, .98)))
ggplot(df1, aes(T_DEGC_C, SC_us_cm )) + geom_boxplot()

ggplot(df1, aes(YEARS, Chl_ug_L, fill = T_DEGC_C)) + geom_boxplot()
ggplot(df1, aes(YEARS, pH, fill = T_DEGC_C)) + geom_boxplot()
ggplot(df1, aes(YEARS, Dox_mg_L, fill = T_DEGC_C)) + geom_boxplot()
ggplot(df1, aes(YEARS, Tn_Ntu, fill = T_DEGC_C)) + geom_boxplot() + scale_y_continuous(limits = quantile(df1$Tn_Ntu, c(0, .98)))
ggplot(df1, aes(YEARS, SC_us_cm, fill = T_DEGC_C)) + geom_boxplot()

ggplot(df1, aes(YEARS, Chl_ug_L, fill = DEPTH_m_c)) + geom_boxplot()
ggplot(df1, aes(YEARS, pH, fill = DEPTH_m_c)) + geom_boxplot()
ggplot(df1, aes(YEARS, Dox_mg_L, fill = DEPTH_m_c)) + geom_boxplot()
ggplot(df1, aes(YEARS, Tn_Ntu, fill = DEPTH_m_c)) + geom_boxplot() + scale_y_continuous(limits = quantile(df1$Tn_Ntu, c(0, .98)))
ggplot(df1, aes(YEARS, SC_us_cm, fill = DEPTH_m_c)) + geom_boxplot()

df_day$MDAYS_MONTHS <- format(df_day$Day, format="%m-%d")
df3 <- df_day[c(2,8,13)]
df3 %>% group_by(MDAYS_MONTHS) %>%
  summarize(T_DEGC = mean(T_DEGC), PRECIP_in = max(PRECIP_in))

mid<-mean(df3$PRECIP_in)
ggplot(df3, aes(MDAYS_MONTHS, T_DEGC, size = PRECIP_in, col=PRECIP_in)) + geom_point() + 
  scale_colour_gradient2(midpoint=mid, low="black", mid="black", high="yellow", space ="Lab" ) + 
  theme(axis.title.x=element_blank(),
axis.text.x=element_blank(),
axis.ticks.x=element_blank(),
panel.background = element_rect(
        fill = "lightblue",
        colour = "lightblue",
        size = 0.5, linetype = "solid"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#melted_df3 <- reshape2::melt(df3, id.var='Day')
#ggplot(melted_df3, aes(x=Day, y=value, col=variable)) + geom_line()

##
df_day_Depth$YEARS = floor_date(df_day_Depth$Day, "year")

g = ggplot(data = df_day_Depth) + geom_point(aes(T_DEGC,Dox_mg_L,
                                        fill = DEPTH_m), alpha = .5, shape = 21)
g + theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

g = ggplot(data = df_day_Depth) + geom_point(aes(pH,Dox_mg_L,

                                                                                                  fill = DEPTH_m), alpha = .1, shape = 21)
ggplot(df_day, aes(Dox_mg_L, fill = T_DEGC_C)) +
  geom_density(alpha = 0.4)

ggplot(df_day_Depth, aes(Dox_mg_L, fill = DEPTH_m)) +
  geom_density(alpha = 0.2)



g + theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

ggplot(df_week_Depth, aes(x=week, y=SC_us_cm, col=DEPTH_m)) + geom_line()
ggplot(df_week_Depth, aes(x=week, y=Tn_Ntu, col=DEPTH_m)) + geom_line()
ggplot(df_week_Depth, aes(x=week, y=Chl_ug_L, col=DEPTH_m)) + geom_line()

ggplot() + geom_line(data = df_week_Depth, aes(x=week,y=Dox_mg_L,col=DEPTH_m)) + geom_line(data = df_week,aes(x=week,y=T_DEGC))
ggplot() + geom_line(data = df_week_Depth, aes(x=week, y=pH, col=DEPTH_m)) + geom_line(data = df_week,aes(x=week,y=(T_DEGC)))

#size = Chl_ug_L,
##

############################################################
##Extra
############################################################


p <- ggplot(df4, aes(x=DATE_TIME, y=mean_T_DEGC, group = date)) + 
  geom_boxplot()
p

ggboxplot(df4, DATE_TIME, mean_T_DEGC, orientation = "horizontal")
colnames(df4)


#df4 <- data.frame(df4,df2[2:5])
df5 <- df_week_Depth[c(1,3:6)]
df6 <- df_week_Depth[c(1:2,4:7)]
df7 <- df_week[c(1,2,9)]

melted_df5 <- reshape2::melt(df5, id.var='week')
ggplot(melted_df5, aes(x=week, y=value, col=variable)) + geom_line()

melted_df6 <- reshape2::melt(df6, id.var='week')
ggplot(melted_df6, aes(x=week, y=value, col=variable)) + geom_line()

melted_df7 <- reshape2::melt(df7, id.var='week')
ggplot(melted_df7, aes(x=week, y=value, col=variable)) + geom_line()

#df2 <- df1 %>% distinct() # removes duplicates from df3

#df$TIME <- strftime(dateofaction,format="%H:%M")
#df$TIME_C <- e

#hist(df$DEPTH_m)
#hist(df$T_DEGC)
#hist(df$SC_us_cm)
#hist(df$pH)
#hist(df$Dox_mg_L)
#hist(df$Tn_Ntu)
#hist(df$Chl_ug_L)
#hist(df$PRECIP_in)
#hist(df$AWND_mph)
#hist(df$WDF5_deg)
#hist(df$WSF5_mph)

#Data from Onondaga Lake Water Quality Monitoring Buoy
df3 <- df[c(1:8,13:17)]

df$DATE_TIME <- as.Date(df$DATE_TIME, '%m/%d/%y %H:%M')

#df4 <- df %>%
#  group_by(DATE_TIME) %>%
#  summarise(mean_T_DEGC = mean(T_DEGC),mean_SC_us_cm = mean(SC_us_cm),
#            mean_pH = mean(pH),mean_Dox_mg_L = mean(Dox_mg_L),
#            mean_Tn_Ntu = mean(Tn_Ntu),mean_Chl_ug_L = mean(Chl_ug_L))


df5 <- df %>%
  select(DATE_TIME, SC_us_cm, pH,Dox_mg_L,Tn_Ntu, Chl_ug_L) %>%
  gather(key = "variable", value = "value", -DATE_TIME)
head(df, 3)

ggplot(df5, aes(x = DATE_TIME, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("gray", "red", "blue", 'green','yellow')) +
  theme_minimal()

p = ggplot() + 
  geom_line(data = df4, aes(DATE_TIME, mean_pH), color = "blue") +
  geom_line(data = df4, aes(DATE_TIME, mean_Tn_Ntu), color = "red") +
  geom_line(data = df4, aes(DATE_TIME, mean_Dox_mg_L), color = "green") +
  geom_line(data = df4, aes(DATE_TIME, mean_Chl_ug_L), color = "yellow") +
  xlab('Dates') +
  ylab('VALUE') + theme(legend.position="right")
print(p)

ggplot(as.data.frame(table(df)), aes(x=gender, y = Freq, fill=fraud)) + 
  geom_bar(stat="identity")

#ggplot() + geom_line(data = df_week, aes(x=week,y=Dox_mg_L), col = 'red') + geom_line(data = df_week,aes(x=week,y=T_DEGC), col = "blue")
#ggplot() + geom_line(data = df_week, aes(x=week, y=pH), col = "red") + geom_line(data = df_week,aes(x=week,y=(T_DEGC/2)), col = 'blue')

