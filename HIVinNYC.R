HIV<- read.csv("HIV_AIDS_Diagnoses_by_Neighborhood__Age_Group__and_Race_Ethnicity.csv")

HIV <- subset(HIV, HIV$TOTAL.NUMBER.OF.HIV.DIAGNOSES != "*")
HIV$TOTAL.NUMBER.OF.HIV.DIAGNOSES <- as.numeric(as.character(HIV$TOTAL.NUMBER.OF.HIV.DIAGNOSES))

# Get total number by race/ethnicity and year; disregard ages and neighborhoods
HIV.allages<- subset(HIV, HIV$AGE=="All")
HIV.race <- HIV.allages[,c("RACE.ETHNICITY", "YEAR")]
HIV.race$TOTAL.NUMBER.OF.HIV.DIAGNOSES <- ave(HIV.allages$TOTAL.NUMBER.OF.HIV.DIAGNOSES,HIV.race,FUN=sum)
HIV.race <- HIV.race[!duplicated(HIV.race),]

# Remove rows with totals
HIV.race <- subset(HIV.race, HIV.race$RACE.ETHNICITY != "All")


# Reduced ethnicities 
# From report: There is missing Data for Native Americans for five different neighborhoods. Considering that Native Americans only make up 0.4% of New York City’s population, this amount of missing data may be significant. Thus, I will remove this ethnicity from my analysis due to lack of sufficient data.

HIV.reduced<- subset(HIV.race, HIV.race$RACE.ETHNICITY != "Native American" & 
                       HIV.race$RACE.ETHNICITY != "Unknown")
HIV.reduced2010 <- subset(HIV.reduced, HIV.reduced$YEAR==2010)

# HIV totals by year
HIV.totals <- subset(HIV, HIV$NEIGHBORHOOD == "All")


library(ggplot2)

# Totals by Year
ggplot(data=HIV.totals, aes(YEAR, TOTAL.NUMBER.OF.HIV.DIAGNOSES)) + geom_point() + geom_line() +
  ggtitle("Number of HIV Diagnoses in NYC Over Time") +
  xlab("Year") + ylab("Total Number of HIV Diagnoses") + ylim(2000, 3500)

# Grouped by race/ethnicity
ggplot(data=HIV.reduced, aes(YEAR, TOTAL.NUMBER.OF.HIV.DIAGNOSES, 
                             colour=RACE.ETHNICITY,
                             group= RACE.ETHNICITY)) + geom_point() + geom_line() +
                             ggtitle("Number of HIV Diagnoses in NYC by Race/Ethnicity") +
                             xlab("Year") + ylab("Total Number of HIV Diagnoses") +
                             guides(color=guide_legend(title="Race/Ethnicity"))

# Get totals and proportions in 2010 for each race/ethnicity
HIV.prop10 <- aggregate(HIV.reduced2010$TOTAL.NUMBER.OF.HIV.DIAGNOSES, 
                        by=list(Category=HIV.reduced2010$RACE.ETHNICITY),
                        FUN=sum)
HIV.prop10$"People living with HIV in NYC" <- HIV.prop10$x /3353

# Total population of NYC in 2010
pop2010 <- c(1030914, 1861295, 2336076, 223944, 2722904)
HIV.prop10$"New York City"<- pop2010/ sum(pop2010)

library(reshape2)
HIV.prop10.m <- melt(HIV.prop10, id.vars= 'Category')
HIV.prop10.m <- subset(HIV.prop10.m, HIV.prop10.m$variable != "x")
colnames(HIV.prop10.m)[colnames(HIV.prop10.m)=="variable"] <- "Population"

# Comparing New HIV diagnoses with NYC population by race/ethnicity in 2010
ggplot(data=HIV.prop10.m, aes(Category, value)) + 
  geom_bar(aes(fill = Population), width = .7, position = position_dodge(width=.7), stat="identity") +
  ggtitle("Proportion of Each Race/Ethnicity") +
  xlab("Race/Ethnicity") + ylab("Proportion") +
  guides(color=guide_legend(title="Population")) +
  scale_fill_brewer(palette="Set2")
  

sum(HIV.prop$prop.HIV)
sum(HIV.prop$prop.POP)



############################################################################################

HIV.missing <- subset(HIV, HIV$TOTAL.NUMBER.OF.HIV.DIAGNOSES == "*")

# Exploring 2010, checking totals
all.2010 <- subset(HIV, HIV$RACE.ETHNICITY == 'All'& HIV$AGE=="All" & HIV$YEAR==2010)
ethnicity.2010 <- subset(HIV, HIV$RACE.ETHNICITY != 'All' & HIV$AGE=="All" & HIV$YEAR==2010)
hispanic.2010 <- subset(HIV, HIV$RACE.ETHNICITY == 'Hispanic' & HIV$AGE=="All" & HIV$YEAR==2010)
total.hispanic.2010 <- sum(hispanic.2010$TOTAL.NUMBER.OF.HIV.DIAGNOSES)

aggregate(ethnicity.2010$TOTAL.NUMBER.OF.HIV.DIAGNOSES, 
          by=list(Category=ethnicity.2010$RACE.ETHNICITY),
          FUN=sum)

ggplot(data=HIV.reduced, aes(YEAR, TOTAL.NUMBER.OF.HIV.DIAGNOSES, 
                             colour=RACE.ETHNICITY,
                             group= RACE.ETHNICITY)) + geom_point() + geom_line() + 
  geom_line(data=HIV.totals, aes(YEAR, TOTAL.NUMBER.OF.HIV.DIAGNOSES) + geom_point() + geom_line())

library(dplyr)
HIV %>% 
  group_by(RACE.ETHNICITY, YEAR) %>% 
  summarise(Frequency=sum(TOTAL.NUMBER.OF.HIV.DIAGNOSES))

#https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group   
#https://catalog.data.gov/dataset/hiv-aids-diagnoses-by-neighborhood-age-group-and-race-ethnicity
#https://www.census.gov/quickfacts/fact/table/newyorkcitynewyork/PST045217
#https://www1.nyc.gov/assets/planning/download/pdf/data-maps/nyc-population/census2010/pgrhc.pdf
#https://a816-healthpsi.nyc.gov/epiquery/sasresults.jsp
