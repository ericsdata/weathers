### File to create graphics related to weather comparisons


#### ===== DATA SET

## Read in Data
dat <- read.csv("Data/GlobalData.csv")
daily <- read.csv("Data/DailyData.csv")


### Function to split dates 
Date_Splitter <- function(dat, dateFormat){
  dat$name <- factor(as.character(dat$NAME), labels = c("Boston", "Portland", "District"))
  dat$date <- parse_date_time(dat$DATE, dateFormat)
  dat$date <- as.Date(dat$date)

  dat$month <- as.numeric(month(dat$date))
  dat$year <- as.numeric(year(dat$date))
  
  dat$month_name <- month(dat$date, label = TRUE, abbr =TRUE)
  
  
  
  return (dat)
}



#winter <- c("December", "January","February")
#spring <- c("March", "April", "May")
#summer <- c("June", "July", "August")
#fall <- c("September", "October", "November")

### Simplifying dates and adding categories
dat<- Date_Splitter(dat,"ym")
dat$decade <- factor(floor(dat$year/10)*10, labels = c("1990s","2000s", "2010s", "2020s"))
dat$season <- factor(dat$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Winter","Winter", 
                                                                               "Spring","Spring","Spring",
                                                                               "Summer","Summer","Summer",
                                                                               "Fall", "Fall", "Fall",
                                                                               "Winter"))
dat$season <- factor(dat$season, levels = c("Winter", "Spring","Summer", "Fall"))



daily <- Date_Splitter(daily,"ymd")
daily$decade <- factor(floor(daily$year/10)*10, labels = c("1990s","2000s", "2010s", "2020s"))
daily$season <- factor(daily$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("Winter","Winter", 
                                                                               "Spring","Spring","Spring",
                                                                               "Summer","Summer","Summer",
                                                                               "Fall", "Fall", "Fall",
                                                                               "Winter"))

####==========TEMPERATURE CHART


daily$season <- relevel(daily$season, "Fall")

g1 <- ggplot(daily[(daily$name != "Boston"),], 
             aes(x = name, y = calcTAVG, group = name,color = name))

g1 +  theme_minimal() + 
  ##Scakes
  scale_y_continuous(breaks=c(0,20,40,60,80,100), labels=scales::number_format(suffix = "°"))+
  scale_x_discrete(expand=c(3, 3))+
  scale_shape_manual(values = c(0,4)) +
  scale_alpha_continuous(range = c(.25,1), guide = "none")+
  scale_color_manual(values = wes_palette("Zissou1")[c(1,3)] ) +
   labs(title = "Daily Temperatures",
        subtitle = "Previous 40 years",
        y = "Mean Daily Temp [F]", 
        x = "") +
  ## Facet into months
  facet_grid(. ~ month_name, scales = "free", space = "free_x") +
  
  stat_summary( fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    position = ggplot2::position_nudge(x = 0.10, y = 0),
    ) +
  
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,3)] )  +
  
  theme(axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(face = "italic"),
        panel.spacing = unit(0, "lines")
        ,panel.grid.major.x = element_blank())

ggsave(
  "Temperatures.png",
  plot = last_plot(),
  device = NULL,
  path = "Plots",
  scale = 1,
  width = 10,
  height = 10,
  units = c("in"),
  dpi = 500,
  limitsize = TRUE
)



#### ===== SNOW DEPTHS


## For calendar heatmap
library(tidyquant)




## Adapted from
#https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

#add column for freezing dat
#filter for Portland after 2009
snowdat <- daily%>%mutate(FreezeDay = factor(ifelse(TAVG < 32,1,0))) %>%
  select( name,date,month,year,month_name, season, SNWD, SNOW, FreezeDay) %>% filter(name == "Portland", 
                                                                                           year > 2009) %>%
  fill(SNWD, .direction = c("downup"))


snowdat$weekNo <- as.numeric(format(snowdat$date,"%W"))## Number the week
snowdat$daysInMonth <- days_in_month(snowdat$date) #Count days in each month
snowdat$dayOfWeek <- wday(snowdat$date) #get day of week
snowdat$weekdayf <- factor(snowdat$dayOfWeek,levels=c(2:7,1),
                           labels=c("Mon","Tue","Wed",                                                              "Thu","Fri","Sat","Sun"),                                           ordered=TRUE) #Factor the day of the week

snowdat$month_name<-factor(snowdat$month,levels=as.character(1:12),
                           labels=c("Jan","Feb","Mar","Apr","May","Jun",
                                                                  "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) 

snowdat$SNOW[is.na(snowdat$SNOW)] <- 0 #Set NAS as no snow
snowdat$snowed <- ifelse(snowdat$SNOW > 0.25,"*","")#make text character to symbolize snowed

snowdat$snDepth <- cut(snowdat$SNWD, 
                   breaks=c(-Inf, 0,6,12, Inf),
                   labels=c("0","1-6","7-12","12+")) #categorize snow inches



#For Transformation
snowdat$week <- as.numeric(format(snowdat$date,"%W")) 
snowdat$yearmonth<- factor(as.yearmon(snowdat$date)) 
## calculate numbered weeks in each moth
snowdat<-ddply(snowdat,.(yearmonth),transform,monthweek=1+weekNo-min(weekNo))

p1d <- snowdat
#filter for Nov to April
p1d <- p1d[p1d$month %in% c(11,12,1,2,3,4),]
p1d$month_name <- factor(p1d$month_name, levels = 
                           c ("Nov","Dec", "Jan","Feb","Mar", "Apr")) #refactor


p1d$winterYear <- factor(ifelse(p1d$month %in% c(10,11,12), 
                                p1d$year+1, p1d$year))
p1d <- filter(p1d, !(winterYear %in% c(2010,2021)))
p1d$SNWD[p1d$SNWD > 6] <- 6



g <- ggplot(p1d, aes( y = monthweek, x = weekdayf, fill = snDepth))


g + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill=("grey93"), alpha =.05)+
  geom_tile( aes(color = snowed),width=0.9, height=0.9) + 
  geom_text(aes(label=snowed), color = "darkslategrey", size = 5, 
            nudge_y = -.25)  +
  theme_minimal()+
  scale_fill_manual(values = brewer.pal(4,"Blues"))+
  scale_color_manual(values = c("grey60", "grey60"))+
  labs(x = "Month", y = "Year") +
  scale_y_reverse() +
  facet_grid(winterYear ~ month_name ) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =6),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y = "Calendar Days", x = "Day of the Week", fill = "Ground Snow\nDepth (in.)",
       title = "Portland, Maine Winters", subtitle = "10 years prior",
       color = "Legend",
       caption = "'*' shows days where snow preciptation was greater than a 1/4 inch")  +
  guides(color = FALSE)

ggsave(
  "SnowTiles.png",
  plot = last_plot(),
  device = NULL,
  path = "Plots",
  scale = 1,
  width = 7,
  height = 10,
  units = c("in"),
  dpi = 500,
  limitsize = TRUE
)


### ======= SUNLIGHT

### Format data
sundat_d <- daily %>% select(name, month, month_name,season, year, decade, TSUN)
sundat_d$SunHours <- sundat_d$TSUN/60
missing_sun <- sundat_d[!(complete.cases(sundat_d$TSUN)),]
sundat <- sundat_d %>% filter(!is.na(TSUN) & year < 1995 & name != "Boston") %>%
                group_by(name, season) %>% 
                    mutate(mode = getmode(SunHours), 
                           med = median(SunHours,na.rm = TRUE), avg = mean(SunHours, na.rm = TRUE)) 


## Plot
g5 <-  ggplot(sundat, aes(x = SunHours, fill = name)) + geom_density(alpha = 0.5, binwidth = 1) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,3)])+
  geom_vline(aes(xintercept = med, color = name), size = 1,show.legend = NA, linetype = "dashed") +
  scale_color_manual(values = wes_palette("Zissou1")[c(2,4)])+
  theme() + 
  labs(x = "Hours of Sunlight per Day", 
       y = "Count of Observed Days",
       title = "Observed Hours of Sunlight\n1990 - 1994")

ggsave(
  "Sunlight.png",
  plot = last_plot(),
  device = NULL,
  path = "Plots",
  scale = 1,
  width = 7,
  height = 10,
  units = c("in"),
  dpi = 500,
  limitsize = TRUE
)