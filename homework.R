# Uploading the data ----
library(ggplot2)
library(data.table)
hotels <- readRDS(url('http://bit.ly/CEU-R-hotels-2018-merged'))
hotels

# Data exploration ----
str(hotels)
summary(hotels)
colSums(is.na(hotels))
colnames(hotels)

#1. How many hotels are from Austria? ----
nrow(hotels[country == 'Austria'])
# Answer: 855

#2. What is the rating of the most expensive hotel (based on the price per night)? ----
hotels[order(avg_price_per_night, decreasing = TRUE)][1,rating]
# Answer: 1.5

#3. How many bookings are in 4-star hotels? ----
sum(hotels$bookings[hotels$stars == 4.0], na.rm = TRUE) #na.rm is to handle NAs
# Answer: 38784

#4. Which country has the highest number of 5-star hotels? ----
sort(table(hotels$country[hotels$stars == 5]), decreasing = TRUE)
names(sort(table(hotels$country[hotels$stars == 5]), decreasing = TRUE))[1]
# Answer: United Kingdom

#5. Plot the number of bookings per country! ----
ggplot(data = hotels, aes(x = country, y = bookings)) + geom_bar(stat = "summary", fun = "sum")

#6. Flip the coordinates and use the "classic dark-on-light theme"! ----
ggplot(data = hotels, aes(x = country, y = bookings)) +
  geom_bar(stat = "summary", fun = "sum") +
  coord_flip() + theme_classic()

#7. Drop the Y axis title, and rename the X axis to "Number of hotels"! ----
# Notice it's number of hotels now, instead of bookings
ggplot(data = hotels, aes(x = country)) + geom_bar() +
  coord_flip() + theme_classic() + labs(x = "", y = "Number of hotels")

#8. Count the number of hotels per country! ----
hotels_per_country <- aggregate(hotels$hotel_id, by=list(Country=hotels$country), FUN=length)
colnames(hotels_per_country)[2] <- "Hotels"
hotels_per_country

#9. Order by alphabet! ----
hotels_per_country[order(hotels_per_country$Country),]

#10. Count the number of bookings per country, order by the number of bookings! ----
bookings_per_country <- aggregate(hotels$bookings,
                                  by=list(Country=hotels$country),
                                  FUN=sum)
colnames(bookings_per_country)[2] <- "Bookings"
bookings_per_country[order(bookings_per_country$Bookings),]

#11. Compute the average rating per number of stars! Use the weighted.mean function to account for the number of ratings of the hotels, and experiment with the na.rm argument. Eliminate NAs. Order by stars. ----
comp_average <- aggregate(rating ~ stars, data = hotels, FUN = weighted.mean, na.rm = TRUE)


#12. Plot this computed average rating per stars! ----
ggplot(comp_average, aes(x = stars, y = rating)) + geom_bar(stat = "identity")

#13. Make sure that each star category is printed on the X axis! ----
ggplot(comp_average, aes(x = factor(stars), y = rating)) +
  geom_bar(stat = "identity") + xlab("") + ggtitle("Rating per number of stars")

#14. Create a boxplot on ratings per stars! ----
ggplot(hotels, aes(x = factor(stars), y = rating)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Ratings per Stars",
    x = "",
    y = "Rating"
  )

#15. Create histograms on the nightly prices for each star category! Check out the arguments and disable forcing the same Y axis range for the subplots. ----
ggplot(hotels, aes(x = avg_price_per_night)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~ stars, scales = "free") +
  labs(
    title = "Histograms of Nightly Prices per Stars",
    x = "Average Price per Night",
    y = "Frequency"
  )


