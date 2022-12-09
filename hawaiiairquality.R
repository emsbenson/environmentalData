hawaiiData <- read.csv("/cloud/project/hawaiidata.csv")

hawaiiData$Month <- factor(hawaiiData$Month, levels=unique(hawaiiData$Month))

#hawaiiData$Date <- as.Date(hawaiiData$Date, "%Y-%m-%d")
hawaiiData$Date <- my(hawaiiData$Date)
hawaiiData$month1 <- month(hawaiiData$Date)
hawaiiData$year1 <- year(hawaiiData$Date)


# max air quality time series
ggplot(hawaiiData, aes(Date, mmaxPM2.5, color = county))+
   geom_point()+
   geom_line()+
   ggtitle("Maximum Air Quality Levels Per Month")+
  xlab("Date")+
  ylab("Air Quality Level(PM 2.5)")+
  labs(color = "County")+
  scale_color_manual(values = c("#410B13", "#CD5D67"))+
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size=16))

# getting rid of the outlier for fun
hawaiinooutlier <- hawaiiData %>%
   filter(mmaxPM2.5 <= 300)

ggplot(hawaiinooutlier, aes(Date, mmaxPM2.5, color = county))+
   geom_point()+
   geom_line()+
   ggtitle("Maximum Air Quality Levels Per Month")

# mean data time series
ggplot(hawaiiData, aes(Date, mmeanPM2.5, color = county))+
   geom_point()+
  geom_line()+
   ggtitle("Monthly Average Air Quality Levels")+
   xlab("Date")+
   ylab("Air Quality Level (PM 2.5)")+
  labs(color = "County")+
  scale_color_manual(values = c("#410B13", "#CD5D67"))+
   theme(axis.title = element_text(size = 15),
         axis.text.y = element_text(size = 13),
         axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 18, face = "bold",
         margin = margin(10, 0, 10, 0)),
        legend.title = element_text(size=16))

#aggregate the mean and max data
avgmmean <- aggregate(hawaiiData$mmeanPM2.5, by=list(NAME = hawaiiData$county, YEAR =hawaiiData$Year, DATE =hawaiiData$Date), 
                      FUN="mean", na.rm=TRUE)
colnames(avgmmean) <- c("County","Year", "Date", "AQMean")

avgmmax <- aggregate(hawaiiData$mmaxPM2.5, by=list(NAME = hawaiiData$county, YEAR =hawaiiData$Year, DATE =hawaiiData$Date), 
                      FUN="mean", na.rm=TRUE)
colnames(avgmmax) <- c("County","Year", "Date", "AQMax")

avgmmean <- aggregate(avgmmean$AQMean, 
                 by=list(County = avgmmean$County, Year = avgmmean$Year),
                 FUN="mean")
avgmmax <- aggregate(avgmmax$AQMax, 
                      by=list(County = avgmmax$County, Year = avgmmax$Year),
                      FUN="mean")

avgmmean$County <- as.factor(avgmmean$County)
avgmmax$County <- as.factor(avgmmax$County)
countiesmean <- levels(avgmmean$County)
countiesmax <- levels(avgmmax$County)



honolulu <- avgmmean[avgmmean$County == countiesmean[1], ]
pahala <- avgmmean[avgmmean$County == countiesmean[2], ]

honolulu1 <- avgmmax[avgmmax$County == countiesmax[1], ]
pahala1 <- avgmmax[avgmmax$County == countiesmax[2], ]

plot(pahala$Year, pahala$x,
     type = "b",
     pch = 19,
     ylab = "Air Quality Levels (PM2.5)",
     xlab = "Year",
     main = "Summary Statistic Plot")
# add honolulu
points(honolulu$Year, honolulu$x, # x and y data
       type = "b", # points and lines
       pch = 19, # filled in circle points
       col="darkorchid2")

plot(pahala1$Year, pahala1$x,
     type = "b",
     pch = 19,
     ylab = "Air Quality Levels (PM2.5)",
     xlab = "Year",
     main = "Summary Statistic Plot", 
     yaxt="n",
     ylim=c(0, 100))
axis(2, seq(0,100, by=20), las=2 )
# add honolulu
points(honolulu1$Year, honolulu1$x, # x and y data
       type = "b", # points and lines
       pch = 19, # filled in circle points
       col="darkorchid2")

# subsetting data per county

#mean air quality
hmean <- avgmmean[avgmmean$County == countiesmean[1], ]
pmean <- avgmmean[avgmmean$County == countiesmean[2], ]

colnames(hmean) <- c("County","Year", "AQMean")
colnames(pmean) <- c("County","Year", "AQMean")

#max air quality
hmax <- avgmmax[avgmmax$County == countiesmax[1], ]
pmax <- avgmmax[avgmmean$County == countiesmax[2], ]

colnames(hmax) <- c("County","Year", "AQMax")
colnames(pmax) <- c("County","Year", "AQMax")

hmean.lm <- lm(hmean$AQMean ~ hmean$Year)
pmean.lm <- lm(pmean$AQMean ~ pmean$Year)

hmean.res <- rstandard(hmean.lm)
pmean.res <- rstandard(pmean.lm)

shapiro.test(hmean.res)
# p-value = 0.1807, normal distribution
shapiro.test(pmean.res)
# p-value = 0.8305, normal distribution

hmax.lm <- lm(hmax$AQMax ~ hmax$Year)
pmax.lm <- lm(pmax$AQMax ~ pmax$Year)

hmax.res <- rstandard(hmax.lm)
pmax.res <- rstandard(pmax.lm)

shapiro.test(hmax.res)
# p-value = 0.1163, normal distribution

shapiro.test(pmax.res)
# p-value = 0.4148, normal distribution

#make mean residual plot honolulu----
plot(hmean$Year, hmean.res, 
      xlab = "Mean Air Quality (PM2.5)", 
      ylab = "Residual", main="Residual Plot Honolulu Mean")
abline(h=0)

#summary statistic plot
plot(hmean$Year, hmean$AQMean, 
     main="Honolulu Average Air Quality Levels Per Year", 
     xlab="Year", ylab="Average Air Quality (PM 2.5)")
abline(hmean.lm, lwd=2)

#Regression table
summary(hmean.lm)

#correlation coefficient
cor(hmean$Year, hmean$AQMean)
#-0.177941, weak negative correlation

### make mean residual plot pahala----
plot(pmean$Year, pmean.res, 
      xlab = "Date", 
      ylab = "Residual", main="Residual Plot Pahala Mean")
abline(h=0)

#summary statistic plot
plot(pmean$Year, pmean$AQMean, 
     main="Pahala Average Air Quality Levels Per Year", 
     xlab="Year", ylab="Average Air Quality (PM2.5)")
abline(pmean.lm, lwd=2)

#regression table
summary(pmean.lm)

### make max residual plot honolulu----
plot(hmax$Year, hmax.res, 
      xlab = "Year", 
      ylab = "Residual", main="Residual Plot Honolulu Max")
abline(h=0)

#summary statistic plot
plot(hmax$Year, hmax$AQMax, 
     main="Honolulu Maximum Air Quality Levels Per Year", 
     xlab="Year", ylab="Maximum Air Quality ")
abline(hmax.lm, lwd=2)

#regression table
summary(hmax.lm)

#correlation coefficient
cor(hmax$Year, hmax$AQMax)
#0.1512

### make max residual plot pahala----
plot(pmax$Year, pmax.res, 
      xlab = "Date", 
      ylab = "Residual", main="Residual Plot Pahala Max")
abline(h=0)

#summary statistic plot
plot(pmax$Year, pmax$AQMax, 
     main="Pahala Average Maximum Air Quality Levels Per Year", 
     xlab="Year", ylab="Air Quality (PM 2.5)")
abline(pmax.lm, lwd=2)

#regression table
summary(pmax.lm)

#correlation coefficient
cor(pmax$Year, pmax$AQMax)
#0.1294