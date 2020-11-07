
library(tidyverse)
library(dplyr)

data("gss_cat")
nrow(gss_cat)
ncol(gss_cat)
#view(gss_cat)

# Q) youngest participant -- is 18.
summary(gss_cat$age)

# Q) count divorced individuals --- 3383
gss_cat %>% filter(marital == 'Divorced') %>% count()
summary(gss_cat$marital)
# table(gss_cat$marital)

# Q) How many individuals identify strongly as Republican? --- 2314
table(gss_cat$partyid)
# summary(gss_cat$partyid)

# Q) What % of respondents identify as Protestant? --- 50.49%
round(100*prop.table(table(gss_cat$relig)),2)

# Q) What % of respondents identify as Catholic? --- 23.85%
round(100*prop.table(table(gss_cat$relig)),2)
# summary(gss_cat$relig) ----- to check the answer.

# Q) How many individuals refused to report their income? --- 975
gss_cat %>% filter(rincome == 'Refused') %>% count()
#sum(gss_cat$rincome == 'Refused')
#summary(gss_cat$rincome)

# Q) How many individuals in the sample reported an income of --- 8646
#   $20000 or higher?
gss_cat %>% group_by(rincome) %>% count()
summary(gss_cat$rincome)
7363+1283

# Q) For this question, consider only respondents who self-identify 
#    as white/Black. What % of them are divorced?
# White Divorced - 16.32 %
# Black Divorced - 15.82 %
race_marital = table(gss_cat$marital, gss_cat$race)
round(100*prop.table(race_marital, 2), 2)


# Q) For this question, consider Catholics/protestants 
# only. What % of them are divorced?
relig_divorced = table(gss_cat$marital, gss_cat$relig)
round(100*prop.table(relig_divorced, 2), 2)
# Cath divorced - 13.35 %
# Prot dicorved - 16.52



# Q) Which year in the survey has the highest/lowest percentage 
#     of individuals identifying strongly as Democrats?

year_partyid = table(gss_cat$partyid, gss_cat$year)
round(100*prop.table(year_partyid,2) ,2)

# Q) How many observations in the dataset come 
#     from the 2012 survey?

gss_cat %>% filter(year == 2012) %>% count()
sum(gss_cat$year == 2012)

# Q) How many observations in the dataset come 
#     from the 2006 survey?

gss_cat %>% filter(year == 2006) %>% count()
sum(gss_cat$year == 2006)




# ----------------------Storm Dataset ----------------------
data(storms)
?storms

# Q) How many observations (rows) are there in the dataset?
nrow(storms)

# Q) How many tropical storms are tracked in the dataset?
storms$status = factor(storms$status)
storms$name = factor(storms$name)

str(storms$name)

# Q) What percentage of observations (rows) correspond to tropical 
#     storms whose status is "hurricane"?

100*prop.table(table(storms$status))
100*mean(storms$status=='hurricane')

storms %>% filter(status == 'hurricane')
nrow(storms)
100*(3091/10010)



# Q) What is the overall maximum recorded wind speed (in knots)?
storms[which.max(storms$wind),]$wind

max_wind = storms %>% filter(wind == max(wind))
max_wind

summary(storms$wind)

# Q) Which tropical storm has the highest average wind speeds? 
# (paste in the name)
# stat_wind = storms %>% group_by(status)

storms %>% 
  group_by(name) %>%
  summarize(avg = mean(wind)) %>% 
  arrange(desc(avg))

#------------------------------------------------------------
# Q) How many observations (rows) correspond to tropical 
#     storms of category 5?
storms %>% filter(category == 5) %>% count()
sum(storms$category == 5)

summary(storms$category == 5)

#------------------------------------------------------------

# Q) Which month contributes the highest number of observations
#     to the dataset? (input the number, not the name of the month;
# e.g., if you think it is January, your answer should be 1).

max_month = storms %>% group_by(year, month) %>% count() 
max_month[which.max(max_month$n),]$month



# Q) How many missing values are there for the variable 
#     ts_diameter.
length(storms$ts_diameter[is.na(storms$ts_diameter)])

# length(storms$ts_diameter[!is.na(storms$ts_diameter)])
# sum of all values that aren't N/A


summary(storms$ts_diameter)
