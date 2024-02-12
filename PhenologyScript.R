##FWS Phenology Data project
##Objective: work with data and generate useful figures to write about
##Data are from Inouye et al (RMBL) 

graphics.off()
rm(list=ls())

setwd()

#Install and load libraries. Don't worry too much about what each library does. They're hear to make our lives easier. 
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("viridis")

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)


#Load Data into RStudio: first load the CSV files using the "Import Dataset", then rename each dataset with the two lines of code below
climate.data <- read.csv("RMBL_climatedf.csv", header=TRUE)
flower.data<-read.csv("RMBL_flowersdf.csv", header=TRUE)


################################# Part 1: prep data #######################################################################
#After you've loaded your datasheets, it's important to take a look and confirm that everything is right. Also, you need to know what format the data is in.

#Flowering Data from Inouye et al long term project near Rocky Mountain Biological Laboratory
View(flower.data) #take a look at the spreadsheet in a new RStudio tab
str(flower.data) #what types of variables are in the spreadsheet?
dim(flower.data) #What are the dimensions (number of rows and columns) of the spreadsheet?
head(flower.data) #Take a look at the first few rows of the data file

#Climate data from Inouye et al long term project near Rocky Mountain Biological Laboratory
View(climate.data)
str(climate.data)
dim(climate.data)
head(climate.data)


################################# Part 2: Explore the Flowering Data ########################################################

##Now that the data are loaded, let's explore it a bit. 
ggplot(flower.data, aes(x = year, y = peakfl.count)) +
  geom_point(alpha = 0.5, position = position_jitter())

quantile(flower.data$peakfl.count)
plot(density(flower.data$peakfl.count))


#Peak flower count over time with (plot 1) and without (plot 2) linear trends. Colored by flower species. 
ggplot(flower.data, aes(x = year, y = peakfl.count, colour = species)) +
  geom_point(alpha = 0.5, position = position_jitter())+
  stat_smooth(method = "lm")

ggplot(flower.data, aes(x = year, y = peakfl.count, colour = species)) +
  geom_point(alpha = 0.5, position = position_jitter())



####
####TASK:  Write one sentence that translates the graphical information above into written language. What is your interpretation of these two graphs?
####


##Take a look at peak flowering count for a single species of flower at a time. In this example, we will plot Erigeron speciosus across the different sites.
flower.data %>%
  filter(species == "Erigeron speciosus") %>%
  ggplot(aes(x = year, y = peakfl.count, colour=plot)) +
  geom_point(alpha = 0.5,  position = position_jitter()) +
  stat_smooth(alpha=0.3, method = "lm")

####
####TASK: Has this graph challenged or supported your interpretation above?
####


#####Summary statistics describing mean and SD for flower.data variables in a tibble (basically a 'table' format).
group_by(flower.data, year) %>%
  summarise(
    count = n(),
    mean = mean(firstfl.doy, na.rm = TRUE),
    sd = sd(firstfl.doy, na.rm = TRUE)
  )

firstflw.aov <- aov(firstfl.doy ~ lastfl.doy, data = flower.data)
summary(firstflw.aov)




################################# Part 3: Explore the Climate data ############################################################

#Take a look at how the timing of snow melt has changed over time
ggplot(climate.data, aes(x = year, y = snowmelt.doy)) + 
  geom_point(alpha = 0.5, position = position_jitter())+
  stat_smooth(alpha=0.3, method = "lm")

ggplot(climate.data, aes(x = year, y = july.temp.max.C)) + 
  geom_point(alpha = 0.5, position = position_jitter())+
  stat_smooth(alpha=0.3, method = "lm")

ggplot(climate.data, aes(x = year, y = july.all.precip.cm)) + 
  geom_point(alpha = 0.5, position = position_jitter())+
  stat_smooth(alpha=0.3, method = "lm") #try replacing 'lm' with 'loess' and see what happens



################################# Part 4: Connect climate data with flowering data ############################################
#To do this, we will subset our data, then combine climate and flowering data into a new data.frame object

#remove two years from the climate data (1978 and 1990) because flowering data weren't collected those years. We need to do this because the size of the dataframes must be the same (42 years)
climate.data<-climate.data[climate.data$year !=1978,] #Remove values from 1978
climate.data<-climate.data[climate.data$year !=1990,] #Remove values from 1990
dim(climate.data) #check the dimensions of your data frame to make sure you removed the two rows, but nothing else.

#subset flowering data by calculating mean DOY for first flowering, last flowering and peak flowering. You could copy/paste these three lines and change the variables to calculate means for other flowering variables, too!
firstflw<-aggregate(firstfl.doy ~ year, flower.data, mean)
lastflw<-aggregate(lastfl.doy ~ year, flower.data, mean)
peakflw<-aggregate(peakfl.doy ~ year, flower.data, mean)


###CREATE our new data frame with mean flowering values and climate data
data.agg <-cbind(climate.data, firstflw$firstfl.doy, lastflw$lastfl.doy, peakflw$peakfl.doy)


data.agg<-data.agg%>%rename(
  first.flw="firstflw$firstfl.doy",
  last.flw="lastflw$lastfl.doy",
  peak.flw="peakflw$peakfl.doy") #don't worry about this step too much. I'm just renaming the columns so they're easier to work with.

View(data.agg)
str(data.agg)


#Plot first flowering DOY by snowmelt DOY. 
#This plot shows that there is a positive correlation between peak flowering and snowmelt

ggplot(data.agg, aes(x = snowmelt.doy, y = peak.flw, color = year)) +
  geom_point(alpha = 0.8,  position = position_jitter())+
  stat_smooth(alpha=0.2, color="grey", size=0.5, method = "lm")+
  scale_color_viridis(option="plasma")+
  labs(title="Average date of first flowering is correlated to snowmelt", y="Date of first flowering (DOY)", x="Snowmelt (DOY)")


#Plot date of snowmelt over the past few decades again. 
#This plot show that snow is melting earlier in the year
ggplot(climate.data, aes(x = year, y = snowmelt.doy)) + 
  geom_point(alpha = 0.5, position = position_jitter())+
  scale_color_viridis(option="plasma")+
  stat_smooth(alpha=0.3, colour="grey", size=0.5, method = "lm")


###PLOT temperature and last flowering
ggplot(data.agg, aes(x = july.temp.max.C, y = last.flw, colour = year)) +
  geom_point(alpha = 0.8,  position = position_jitter())+
  stat_smooth(alpha=0.2, colour="grey", size=0.5, method = "lm")+
  scale_color_viridis(option="plasma")+
  labs(title="Correlation between last day of flowering and maximum July temperature", y="Date of last flowering (DOY)", x="Maximum temperature (C)")


####
####TASK: Save the two graphs above as PDF files. Together, these graphs tell a story of ecological change and can be used for your technical paper. 
####


####NEXT up, combine dataset different with a merge() function. This will retain all data from the flower.data data frame, and add annual climate rows. 
###CREATE another new data frame with all of the data in the original 'flower.data' plus the climate data for each year
data.full <- merge(flower.data, climate.data, by="year")

ggplot(data.full, aes(x = species, y = fl.duration)) +
  geom_boxplot(aes(fill = species), outlier.shape = 21) +
  labs(
    title = "Flowering duration",
    x = "Species",
    y = "Flower duration (days)"
  ) +
  theme_minimal() +
  theme(legend.position = "top") 


#How does flowering duration vary with different levels of snow? Two visualizations 

#i. Why are all of the points lined up in tidy stacks along the x-axis? 
ggplot(data.full, aes(x=snow.total.cm, y=fl.duration)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  theme_minimal()


#ii. When you treat snow.total.cm as a factor, what can you infer from boxplots instead of point plots?
data.full$snow.total.factor <- factor(data.full$snow.total.cm, levels = unique(data.full$snow.total.cm[order(data.full$year)]))

ggplot(data.full, aes(x = snow.total.factor, y = fl.duration)) +
  geom_boxplot(outlier.color = "turquoise", outlier.size = 1) +
  scale_x_discrete(breaks = seq(400, 1700, by = 100)) +
  theme_minimal()


#now turn the snow data into a ranked ordinal factor with bins of 500 cm width
data.full$snow.total.group <- cut(data.full$snow.total.cm, 
                                   breaks = c(0, 500, 1000, 1500, Inf), 
                                   labels = c("Low", "Medium", "High", "Very High"), 
                                   ordered = TRUE)


ggplot(data.full, aes(x = snow.total.group, y = fl.duration)) +
  geom_boxplot(outlier.color = "turquoise", outlier.size = 1) +
  theme_minimal()



### Hypothesis testing in R ####

#Basic ANOVA for mean flowering time across sites
anova1 <- aov(fl.duration ~ snow.total.cm, data=data.full)
cat("One-way ANOVA for Mean Flowering Time across two sites:\n")
print(summary(anova1))

##The ANOVA result suggests that the impact of snowfall on flowering duration 
#is not statisically significant (F = 2.294, p-value=0.13)


#linear regression
lm1 <- lm(fl.duration~snow.total.cm, data = data.full)
cat("Linear regression estimation of relationship between flowering duration and total snow: \n")
print(summary(lm1))

##The LM regression model suggests that there is a significant relationship between snow amount and 
#flowering duration (F-statistic = 4.344, p < 2.2e-16), but this is possibly due to only a few 
#individual coefficients for snow fall levels.



#generalized linear modeling: more flexible and doesn't assume normality or linearity 
library(lme4)
glm1 <- glm(fl.duration ~ snow.total.cm, data = data.full)
cat("Generalized linear regression estimation of relationship between flowering duration and total snow: \n")
print(summary(glm1))

##The GLM  model results suggests that, after accounting for snowfall, the duration of flowering 
#increases by approximately 0.001988 units per unit increase in 
#snowfall (cm), though this relationship is not statistically significant (t = 1.515, p = 0.13)



#logistic regression: a bit more complicated! And not neccessarily useful here, but take a look. 
library(MASS)
ordinal_model <- polr(factor(fl.duration) ~ snow.total.group, data = data.full, Hess = TRUE) #Hessian matrix returned; second order partial derivitives useful for estimating SE
cat("Logistic regression of flower duration and binned snow: \n")
summary(ordinal_model) #check coefficients; model estimates the change in the log odds of the outcome being in a higher category





###########Done!!! You Did it!!!! ##################################################################3





##
##
##
##
##
##

#####Eager to keep going? Below this line I've added some bonus scripts that you can play around with if you want to. 
#Also, here are two online resources that can provide help:
#1.  https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#2.  https://datacarpentry.org/dc_zurich/R-ecology/05-visualisation-ggplot2.html

#####EXTRA CODE TO PLAY WITH IF YOU WANT TO

#subset flowering data for only one species
helianthella<-subset(flower.data, species=="Helianthella quinquenervis")

ggplot(helianthella, aes(x = year, y = firstfl.doy, colour=plot)) +
  geom_point(alpha = 0.5,  position = position_jitter())



#Treat year as discrete variable instead
ggplot(flower.data, aes(year, peakfl.doy))+
  geom_bar(stat="identity")


#Check out the first DOY of flowering alone
ggplot(flower.data, aes(firstfl.doy))+
  geom_density(kernel="gaussian")




