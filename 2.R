source('http://bit.ly/CEU-R-heights-2018')

#TODO compute average height
mean(heights, na.rm = TRUE)
?mean
#TODO visualize the dataset
hist(heights)

library(ggplot2)
library(data.table)
ggplot(data.table(h = heights), aes(x = h)) + geom_histogram()
ggplot(data.table(h = heights), aes(x = h)) + geom_boxplot()
ggplot(data.table(h = heights), aes(x = h)) + geom_density()

rm(list = ls())

rm(list = ls(all = TRUE))

library(data.table)
bookings <- fread("http://bit.ly/CEU-R-hotels-2018-prices")
features <- fread("http://bit.ly/CEU-R-hotels-2018-features")

## TODO count the number of bookings in Austria
merged_df <- merge(bookings, features)
merged_df[country == 'Austria']
colnames(merged_df)
nrow(merged_df[country == 'Austria'])
# another way
merge(bookings, features)[country == 'Austria', .N]


## TODO which hotel is the cheapest in Vienna within 1km
merged_df[city_actual== "Vienna" & distance <= 1][order(price)][1,1]
merged_df[city_actual== "Vienna" & distance <= 1][,
                                                  list(price =mean(price/nnights)),
                                                  by = hotel_id][order(price)][1,1]

merged_df[city_actual== "Vienna" & distance <= 1][,
                                                  list(price =mean(price/nnights)),
                                                  by = hotel_id][which.min(price),hotel_id]
merged_df[city_actual== "Vienna" & distance <= 1][,
                                                  list(price =mean(price/nnights)),
                                                  by = hotel_id][price == min(price),hotel_id]

## TODO create a hotels based on features + number of bookings + average price per night

hotels <- merge(features, bookings[,list(bookings = .N, price = mean(price/nnights)), by = list(hotel_id)])
hotels

## TODO compute the avg price per number of stars (NA)
price_per_stars <- hotels[!is.na(stars),list(avg_price= mean(price)), by = list(stars)][order(stars)]
price_per_stars
# TODO let's do with weighted average
price_per_stars<- hotels[!is.na(stars),list(avg_price= weighted.mean(price, bookings)), by = list(stars)][order(stars)]
?weighted.mean

## TODO visualize the avg price per number of stars.
library(ggplot2)
ggplot(price_per_stars, aes(x = factor(stars), y = avg_price)) +
  geom_bar(stat ='identity')

# TODO do the same for each country
price_per_country<- hotels[!is.na(stars),list(avg_price= weighted.mean(price, bookings)), by = list(stars, country)]
price_per_country

ggplot(price_per_country, aes(x = factor(stars), y = avg_price)) +
  geom_bar(stat ='identity') + facet_wrap(~country, scales = "free")
?facet_wrap

## TODO price discrete
hotels[, pricecat:= cut(price, 3)]
hotels[, .N, by = pricecat]

hotels[, pricecat:= cut(price, c(0,100,250, Inf), labels = c('cheap','average','expensive'))]
hotels[, .N, by = pricecat]

## TODO use stats to define breakpoints
hotels[, pricecat:= cut(price, c(0,100,250, Inf), labels = c('cheap','average','expensive'))]

price_mean <- mean(hotels$price)
price_sd <- sd(hotels$price)

hotels[, pricecat:= cut(price, c(0,price_mean - price_sd,price_mean + price_sd, Inf), labels = c('cheap','average','expensive'))]
hotels[, .N, by = pricecat]

# use per country

hotels[, price_mean := mean(price), by = country]
hotels[, price_sd := sd(price), by = country]
str(hotels)

hotels[, .N, by = .(country,price_mean)]

hotels[!is.na(price_sd), pricecat:= cut(price, c(
  0,
  price_mean[1] - price_sd[1],
  price_mean[1] + price_sd[1],
  Inf
  ), labels = c('cheap','average','expensive')), by = country]

hotels[price_sd == 0]
hotels[is.na(price_sd)]

agg <- hotels[, .N, by = .(country, pricecat)]
agg

## TODO visualize the number of hotels in each category
ggplot(agg, aes(x = pricecat, y = N)) + 
  geom_col() + 
  facet_wrap(~country)

ggplot(hotels, aes(x = country, fill = pricecat)) + geom_bar() + coord_flip()
ggplot(hotels, aes(x = country, fill = pricecat)) + geom_bar(position = 'fill') + coord_flip()
ggplot(agg, aes(x = country,y = N, fill = pricecat)) + geom_col(position = 'fill') + coord_flip()+
  scale_fill_manual(values = c("purple","red","yellow"))

?scale_fill

ggplot(agg, aes(x = country,y = N, fill = pricecat)) + geom_col(position = 'fill') + coord_flip()+
  scale_fill_brewer(palette = "YlGn")


#TODO japan flag (100x100)
# DT: x, y, color (100x100)

points <- data.table(x = rep(1:100,100) , y= rep(1:100,each = 100))
points
?rep
rep(1:10, each = 2)
rep(1:10,2)
# make all white
points[, col :="white"]

# experimenting
points[x>40 & x<60, col :="red"]
points[abs(50-x)<10 & abs(50-y)<10, col :="red"]

# solution
points[((50-x)^2 + (50-y)^2)<=50, col :="red"]

points[,.N,by = col]

ggplot(points, aes(x,y, color = col)) + geom_point() + theme_void()+
  theme(legend.position = "none") +
  scale_color_manual(values = c("red", "white"))
  

ggplot(points, aes(x, y, fill = col)) + geom_tile() +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))


str(points)

?lm

#linear
lm(col~ x+ y, data = points)

# logistic

glm(factor(col)~ x+ y, data = points, family = binomial(link = logit))
points$col <- factor(points$col)
fit <- glm(col~ x+ y, data = points, family = binomial(link = logit))
summary(fit)

predict(fit, newdata = points, type = "response")
hist(predict(fit, newdata = points, type = "response"))

points$pred <- predict(fit, newdata = points, type = "response")

ggplot(points, aes(x,y, fill = pred)) + geom_tile()+
  theme_void() + theme(legend.position = "none")+
  scale_fill_manual(values = c("red","white"))


library(rpart)
fit <- rpart(col ~ x+y, data = points)
plot(fit)
text(fit)

install.packages("partykit")
library(partykit)
plot(as.party(fit))

fit <- rpart(col ~ x + y, data = points, control = rpart.control(cp = 0, minsplit = 1))
fit
plot(as.party(fit))
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + geom_tile() +
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))


# continuing the model
fit <- rpart(col ~ x + y, data = points)
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + 
  geom_tile(aes(fill = col), alpha = 0.5) + # actual color
  geom_tile(aes(fill = pred), alpha = 0.5) + # predicted
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

# reshape long to wide
dcast(points[,.N, by = list(pred,col)], pred ~ col )

install.packages("randomForest")
library(randomForest)

fit <- randomForest(col ~ x + y, data = points)
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + 
  geom_tile(aes(fill = col), alpha = 0.5) + # actual color
  geom_tile(aes(fill = pred), alpha = 0.5) + # predicted
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

fit

# feature engineering
str(points)
points[, x2:=(50-x)^2]
points[,y2:=(50-y)^2]
fit <- glm(col ~ x2 + y2, data = points, family = binomial(link = logit))
summary(fit)
points$pred <- predict(fit, newdata = points, type = "class")
ggplot(points, aes(x, y, fill = pred)) + 
  geom_tile(aes(fill = col), alpha = 0.5) + # actual color
  geom_tile(aes(fill = pred), alpha = 0.5) + # predicted
  theme_void() + theme(legend.position = 'none') +
  scale_fill_manual(values = c("red", "white"))

# last model

str(points)
points[, x2 := (50 - x) ^ 2]
points[, y2 := (50 - y) ^ 2]
fit <- glm(col ~ x2 + y2, data = points, family = binomial(link = logit))
points$pred <- factor(predict(fit, newdata = points, type = "response"))
ggplot(points, aes(x, y)) + geom_tile(aes(fill = pred), alpha = .5)
