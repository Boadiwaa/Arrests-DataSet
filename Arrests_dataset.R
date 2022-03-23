
#Objectives:
  #- Black vrs White Releases
  #- Yearly trend of arrests
  #- Employment relation with release
  #- Age categories for arrests
  #- Female vrs Male Releases
  #- Citizen vrs Non-citizen Releases

library(tidyverse)
test_data <- carData::Arrests
summary(test_data)
black_or_white <- test_data %>% group_by(released,colour) %>% summarize(Count=n())
b <- (black_or_white[4,3]/(black_or_white[4,3] + black_or_white[2,3]))*100
a <- (black_or_white[3,3]/(black_or_white[3,3] + black_or_white[1,3]))*100
percent_black_releases <- (black_or_white[3,3]/(black_or_white[3,3] + black_or_white[1,3]))*100
percent_white_releases <- (black_or_white[4,3]/(black_or_white[4,3] + black_or_white[2,3]))*100
c<- 100- percent_black_releases
d <- 100- percent_white_releases
black_or_white$percentages <- c(c,d,percent_black_releases, percent_white_releases)
ggplot(black_or_white, aes(x=released, y= percentages, fill = released)) + geom_col(colour="black") + 
  scale_fill_manual(values = c("burlywood","brown4")) + facet_wrap(~colour) + labs(title = "Black vrs White Release stats") + 
  theme(plot.title = element_text(hjust = 0.5))

arrests_trend <- test_data %>% group_by(year)%>% summarize(Count=n())

ggplot(arrests_trend, aes(x=year,y=Count)) + geom_line() +geom_point(color="brown ", size =3) + labs(title = "Yearly Trends of Arrests") +
  theme(plot.title = element_text(hjust = 0.5))

#Release stats based on employment

employment_factor <- test_data %>% group_by(employed,released) %>% summarize(Count=n())

ggplot(employment_factor, aes(x = employed, y=released)) + geom_point(aes(size=Count), shape=21, colour="black", fill="ivory") + 
  scale_size_area(max_size = 40, guide = "none") + geom_text(aes(label = Count), size=4)

teenagers <- test_data %>% filter(age >= 12, age < 18) %>% summarize(Count=n())
young_adults <- test_data %>% filter(age >= 18, age < 30) %>% summarize(Count=n())
prime_time <- test_data %>% filter(age >= 30, age < 46) %>% summarize(Count=n())
mid_age_to_elderly <- test_data %>% filter(age >= 46) %>% summarize(Count=n())

Number_of_Arrests <- c(teenagers[1,1],young_adults[1,1],prime_time[1,1],mid_age_to_elderly[1,1])
Age_group <- c("12 to 17", "18 to 29", "30 to 45", "46 and above")
Age_stats <- data.frame(Age_group, Number_of_Arrests)

ggplot(Age_stats, aes(x=Age_group, y= Number_of_Arrests, fill=Age_group)) + geom_col(colour="black") + 
  scale_fill_manual(values = c("coral4","burlywood","deeppink4","brown4")) + labs(title = "Age categories and Number of Arrests") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")

male_vrs_female <-test_data %>% group_by(sex) %>% summarize(Count=n())

citizen_vrs_non_citizen <- test_data %>% group_by(citizen,released) %>% summarize(Count=n())
ggplot(citizen_vrs_non_citizen, aes(x=released, y= Count, fill = citizen)) + geom_col(colour="black") + 
  scale_fill_manual(values = c("burlywood","brown4")) + facet_wrap(~citizen) + labs(title = "Citizen vrs Non-citizen Release stats") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label= Count), vjust=-0.2) + theme(strip.text = element_blank())
