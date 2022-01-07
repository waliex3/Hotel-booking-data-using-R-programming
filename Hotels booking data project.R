install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("readr")

library(tidyverse)
library(skimr)
library(janitor)
library(ggplot2)
library(readr)


hotel_bookings <- read_csv("GL0bk8O2Sja9G5PDtko2uQ_31e445d7ca64417eb45aeaa08ec90bf1_hotel_bookings (1).csv")
View(hotel_bookings)

# what is the lowest lead time
min(hotel_bookings$lead_time)

# the lowest lead time is 0

#what is the max lead time?
max(hotel_bookings$lead_time)

# it is 737

# what is the average lead time
mean(hotel_bookings$lead_time)
# it is 104.0114



# now I want to know how there are different from resort hotel. this time I don't want to run code line by line
# so I will do pipes
hotel_summary <- hotel_bookings %>% group_by(hotel) %>% summarise(average_lead_time=mean(lead_time), min_lead_time=min(lead_time), max_lead_time=max(lead_time))

head(hotel_summary)

# the average lead time of the city hotel is the 110 while the resort hotel is the 92.7.
# the minimum lead time for both the city hotel and resort hotel is 0
# the maximum lead time of the city hotel is 629 but the resort hotel is 737

# now this is great 


# A stakeholder tells you, "I want to target people who book early, and I have a hypothesis that people with children have to book in advance.
# so you create this visualtion to him to show him whether he right or wrong 
ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))

#as I can see in the visualizion, the scatterplot doesn't show any relationship
# I sould report back to your stakeholder that many of the advanced bookings are being made by people with 0 children. 


# let's try to see if there is a relationship between staying on the weekend and children.
ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x =stays_in_weekend_nights , y =children ))

# it also doesn't show any relationship 

# I want to see other things such as 
# hat distribution type has the most number of bookings?
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel))

# apears that the answer is TA/TO


# now I want to see what is the most deposit type in the distribution_channel that people are attracted to
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel, fill=deposit_type))

# as you can see in the visualization, the no deposit type is fav for the people in TA/TO


#your stakeholder asks you to create separate charts for each deposit type and market segment to help them understand the differences more clearly

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type )

# now this is great but honestly it is harder to read on the label x-axis 
# so I want it to make it clear
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

# now the distribution channel is easier to read.


# let say the stakeholders ask us to see the market segment based on the distribution channel
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

#I want to see if there is a segment that is empty
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

# yes there is, you can see it in this pictures that if you run the code above, you will see it.

# Your stakeholder tells you that they would like you to share they visualization breaking down payment type by city because it will help inform how the company targets promotions in the future. They ask you to create a cleaned and labeled version and save it as a .png file for them to include in a presentation. 
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel)
# done

# add title and label
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  labs(title="different hotels have different result")


# the x axis is not clear + I want to add time under the title 
mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", mindate, " to ", maxdate))


