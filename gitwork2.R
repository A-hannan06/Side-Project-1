library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)


# ----------------------------------------------------------
# data: http://users.stat.ufl.edu/~winner/data/interfaith.dat
# ----------------------------------------------------------

# 17) How is the dataset formatted? Aggregated.

colnames(interfaith) = c('ses', 'religion', 'gender', 'interfaith', 'count')
interfaith$ses = factor(interfaith$ses)
interfaith$religion = factor(interfaith$religion)
interfaith$gender = factor(interfaith$gender)
interfaith$interfaith = factor(interfaith$interfaith)
# str(interfaith)

levels(interfaith$ses) = c('low', 'middle', 'high')
levels(interfaith$religion) = c('protestants', 'catholics')
levels(interfaith$gender) = c('male', 'female')
levels(interfaith$interfaith) = c('yes', 'no')
# ----------------------------------------------------------

# 18) How many individuals participated in the survey?
sum(interfaith$count)
interfaith %>% uncount(count)

# 19) How many women participated in the survey?
unc_inter = interfaith %>% uncount(count)
unc_inter %>% filter(gender == 'female') %>% count()
sum(unc_inter$gender == 'female')
summary(unc_inter$gender)

# 20) How many Catholics participated in the survey?

unc_inter %>% filter(religion == 'catholics') %>% count()
sum(unc_inter$religion == 'catholics')

summary(unc_inter$religion)

# 21, 22) Consider Catholics/Protestants only. What % of them are in an 
# interfaith relationship?
round(100*prop.table(table(unc_inter$interfaith, unc_inter$religion),2),2)



# -------------------------Economics dataset---------------------------
data("economics")
?economics

# 23) How many observations are there in the dataset?
nrow(economics)

# 24) What is the median personal savings rate?
summary(economics$psavert)

# 25) What is the minimum number of unemployed people (in thousands)?
summary(economics$unemploy)
min(economics$unemploy)
economics[which.min(economics$unemploy),]$unemploy


# 26) In what year did the historical minimum of the median duration of 
#     unemployment occur?

economics %>% filter(uempmed == min(uempmed))
economics[which.min(economics$uempmed),]$date


# 27) What percentage of observations in the economics dataset have a 
#     corresponding value of population over 300 million people?
economics %>% filter(pop > 300000)
nrow(economics)
100*(102/574)

100*mean(economics$pop > 300000)



Car_sales <- read_csv("C:/Users/hanna/OneDrive/Desktop/All Folders/Data Science/Data Sets/Car_sales.csv")
View(Car_sales)

car_data = Car_sales %>% mutate_at(c("Manufacturer"), as.factor)

# To retrieve data about a certain model later,
# we will change Model from chr to factor

levels(car_data$Manufacturer)

#-------------------------------------------------------------------------
####################
#### Questions #####
##################

library(readr)
Car_sales <- read_csv("C:/Users/hanna/OneDrive/Desktop/All Folders/Data Science/Data Sets/Car_sales.csv")

car_data = Car_sales 
car_data$Manufacturer = factor(car_data$Manufacturer)

# --------------------------Still needs work ---------------------------------
# Q) Average Retention % of Japanese Auto Manufacturers (Acura, Honda, Lexus, 
# Mitsuibishi, Nissan, Subaru, Toyota)
# --------------------------Still needs work ---------------------------------
car_data %>% select(Manufacturer, `Retention %`) %>% group_by(Manufacturer) %>% mutate()
  
avg_ret_by_manufacturer = car_data %>% group_by(Manufacturer) %>% summarise(mean_retention = mean(Retention %)) %>% arrange(desc(Manufacturer)) 
  
round(100*prop.table(table(car_data$`Retention %`, car_data$Manufacturer)), 2)




#2 difference between Average of Year Resale Value and Average of 
# Price or Toyota Car


diff_resale_and_price = car_data %>% 
  filter(Manufacturer == 'Toyota') %>% 
  select(Manufacturer, Model, `Year Resale Value`, Price) 

Year_Resale_Value = diff_resale_and_price$`Year Resale Value`
Price = diff_resale_and_price$Price


difference = c(car_data$`Year Resale Value` - car_data$Price)
# non-numeric argument to binary operator



#3) Show a graph (area chart) that shows the unit sales of Toyota cars

unit_toyota = car_data %>% 
  filter(Manufacturer == 'Toyota') %>% 
  arrange(desc(`Unit Sales`))

x = unit_toyota %>% select(`Unit Sales`)
y = unit_toyota %>% select(Price)

qplot(x, y, data = unit_toyota)
#can't plot a graph


# Q) Show the difference in mean Power Perf Factor of BMW and Audi Cars

bmw = subset(car_data, Manufacturer =='BMW')
Audi = subset(car_data, Manufacturer =='Audi')

power_purf = bmw %>% full_join(Audi) %>% select(Manufacturer, `Power Perf Factor`)
power_purf %>% rename(`Power Perf Factor` = Power-perf-factor)


difference_power_purf = qplot(x = Power-Perf-Factor,
                              y = Manufacturer,
                              data = power_purf) + geom_smooth()
difference_power_purf
# same problem



# 5. Which Dodge Car has the most horsepower 
Ford_max = car_data %>% filter(Manufacturer == 'Ford') %>% arrange(desc(Horsepower))
Ford_max[which.max(Ford_max$Horsepower), ]$Model

# Ford Expedition has 240 Horsepower.




#6) How many passenger type vehicles are there.
car_data$Vehicle_type = factor(car_data$Vehicle_type)

car_data %>% filter(Vehicle_type == 'Passenger') %>% count()

# There are 155 cars that are passenger type.




# 7) Which car has the least fuel capacity
car_data[which.min(car_data$`Fuel Capacity`), ]$'Fuel Capacity'

# Chevrolets Metro has the least fuel capacity with 10.3
                     


# 8) Which year had the most launch of cars.
car_data %>% seperate(car_data, `Latest Launch`, c('m', 'd', 'y'))
car_data %>% group_by(`Latest Launch`)



#9) print the top 5 most curb weight. 

head(car_data %>% arrange(desc(`Curb Weight`)) %>% select(Manufacturer, Model, `Curb Weight`))

# Cadillac Escalade has the most curb weight, followed by Lexus LX470


#10) Which Manufacturer has the least exspensive cars.

car_data %>% group_by(Manufacturer) %>% 

# How do we select Factor and then find a mean of them.
  
  
# 11) Which Manufacturer has the best Resale Price.
  
  
  
# What is the Average Price of any Oldsmobile car.
  
car_data %>% mutate_at(c("Price", 'Year_Resale_Value'), as.numeric())
#car_data %>% filter(Manufacturer == 'Oldsmobile') 

oldsmobile = subset(car_data, Manufacturer == 'Oldsmobile')
oldsmobile %>% summarize(mean_price = mean(Price))
  

# Why does it say car data not found





