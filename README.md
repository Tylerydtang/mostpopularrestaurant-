---
title: "Analysis of the Most Popular U.S. Restaurant Chains in 2020"
author: "Chen Wen, Yuandi Tang, Feifan Wang"
date: "2021/12/10"
output:
  html_document:
    fig_width: 8
    code_folding: hide
    highlight: tango
    toc: true
    toc_float:
      collapsed: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=8, fig.height=5,
                      echo=FALSE, warning=FALSE, message=FALSE)
```



# Dataset Details
This data is obtained from the Restaurant Business magazine website.This dataset carries the link between different chain restaurants with sales in 2020. This dataset supplies with the Top 100 chain restaurants in sales total,YOY changes in sales,unit numbers, YOY changes in units, segments and categories.

## Dataset
```{r echo=TRUE}
library(plotly)
library(stringr)
library(UsingR)
library(tidyverse)
library(dplyr)
data.detail <- read.csv(
  "/Users/wenchen/Desktop/544 project/Top100Chains.csv", header = TRUE)
as_tibble(data.detail)
```

# Objective 
The following data frame captures the Top 100 chain restaurants
To answer the following questions:

1.    Which restaurants are biggest in total sales?

2.    Which is biggest category in Top 100 restaurants?

3.    Which are the biggest in YOY sales changes in Top 100 restaurants?

4.    Which is biggest segments in Top 100 restaurants?

5.    Which are the biggest in YOY units changes in Top 100 restaurants?

6.    Which is biggest units in Top 100 restaurants?

7.    Which are the outcomes for the cross features analysis?

8.    What are the investment suggestions for chain restaurants investors?

9.    What are the correlation between these elements?

```{r echo=TRUE}
top100chains <- read.csv(
  "/Users/wenchen/Desktop/544 project/Top100Chains.csv", header = TRUE)

# data cleaning
# remove $
str_sub(top100chains$X2020.U.S..Sales...000.000., 1, 1) <- ''
top100chains[73, 3] <- '511'

#remove punctuation
str_sub(top100chains$X2020.U.S..Sales...000.000.[1:46], -4, -4) <- ''
a <- as.numeric(top100chains$X2020.U.S..Sales...000.000.)
top100chains$X2020.U.S..Sales...000.000. <- a

#remove %
str_sub(top100chains$YOY.SALES.CHANGE, -1, -1) <- ''
b <- as.numeric(top100chains$YOY.SALES.CHANGE)
top100chains$YOY.SALES.CHANGE <- b

#remove ,
comma <- '([,])'
top100chains$X2020.U.S..UNITS <-  str_replace_all(top100chains$X2020.U.S..UNITS, comma, '')
c <- as.numeric(top100chains$X2020.U.S..UNITS)
top100chains$X2020.U.S..UNITS <- c

#remove %
str_sub(top100chains$YOY.UNIT.CHANGE, -1, -1) <- ''
d <- as.numeric(top100chains$YOY.UNIT.CHANGE)
top100chains$YOY.UNIT.CHANGE <- d

#remove \t
e <- str_trim(top100chains$SEGMENT)
top100chains$SEGMENT <- e
f <- str_trim(top100chains$MENU.CATEGORY)
top100chains$MENU.CATEGORY <- f

# convert to tibble
top100chains_tibble <- as_tibble(top100chains)
```

# Distribution and Sampling
## Total Sales Distribution
Total Sales Distributions are shown below. As expected with sales, the distribution is skewed to the right.

```{r echo=TRUE}
samples <- 5000
sample.size <- 5
xbar <- numeric(samples)

for (i in 1: samples) {
  xbar[i] <- mean(sample(top100chains$X2020.U.S..Sales...000.000., 
                         sample.size, replace = TRUE))
}

plot_ly(x = xbar, type = 'histogram', histnorm = 'probability') %>%
  layout(title="Total Sales Distribution", xaxis = list(
  title = "Total Sales"), yaxis = list(
  title = "Density"))
```


## Central Limit Theorem
Using the total sales attribute in this data set the applicability of the central limit theorem can be shown. As displayed in the histogram above, the distribution is normal. Below are histograms showing the sample means of 5000 random samples of sample sizes 10, 20, 30, and 40 following a normal distribution.

```{r echo=TRUE}
par(mfrow = c(2,2))
cat("Population Mean = 2634.06, SD = 4926,714\n")
options(scipen = 200)
xbar <- numeric(samples)

for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(top100chains$X2020.U.S..Sales...000.000.,
                           size = size, replace = TRUE))
    
  }
  
  hist(xbar, prob = TRUE, col = rainbow(15), xlab = 'Total sales',
       breaks = 15, main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))
```

## Sampling of Total Sales via Simple Random Sample Without Replacement, Systematic Sampling

### Simple Random Sampling
```{r echo=TRUE}
library(sampling)
samples <- 5000
sample.size <- 5
xbar1 <- numeric(samples)

for (i in 1: samples) {
  s <- srswor(20, nrow(top100chains))
  sample.1 <- top100chains[s != 0, ]
  sample_num <- as.numeric(sample.1$X2020.U.S..Sales...000.000.)
  xbar1[i] <- mean(sample(sample_num, size = sample.size, 
                          replace = TRUE))
}

plot_ly(x = xbar1, type = 'histogram', histnorm = 'probability') %>%
  layout(title='Simple Random Sampling', xaxis = list(title = "Total Sales"), yaxis = list(title = "Density"))
```

### Systematic Sampling

```{r echo=TRUE}
set.seed(5966)
samples <- 5000
sample.size <- 5
xbar2 <- numeric(samples)

N <- nrow(top100chains)
n <- 5
k <- ceiling(N / n)

for (i in 1: samples) {
  r <- sample(k, 1)
  s <- seq(r, by = k, length = n)
  sample.2 <- top100chains[s, ]
  sample2_num <- as.numeric(sample.2$X2020.U.S..Sales...000.000.)
  xbar2[i] <- mean(sample(sample2_num, size = sample.size, 
                          replace = TRUE))
}

plot_ly(x = xbar1, type = 'histogram', histnorm = 'probability') %>%
  layout(title='Systematic Sampling', xaxis = list(title = "Total Sales"), yaxis = list(title = "Density"))

par(mfrow = c(1,1))
```

The numbers are highly condensed with among 0 to 5,000, the Simple Random Sample Without Replacement and Systematic Sampling shows the same outcome.

# Single Element Analysis
## Total Sales for All Chain
```{r echo=TRUE}
plot_ly(x = top100chains$X2020.U.S..Sales...000.000., type = 'histogram',
        histnorm = 'probability') %>%
  layout(title='Total Sales for All Chain', xaxis = list(title = "Total Sales"), yaxis = list(title = "Density"))

plot_ly(top100chains_tibble, y = ~X2020.U.S..Sales...000.000.,
        name = 'Chains', type = "box") %>%
  layout(title='Total Sales for All Chain', yaxis = list(title = 'Total Sales'))
```

By categorize total sales of top restaurants from 0 to 40000 with a step of 5000, the histogram and boxplot are shown above.

### Findings

1.Total Sales are highly biased and uneven among all chains.

2.Above 80% of chain restaurants have sales in 0-5000 thousand.

3.There is no top chain restaurants which sales is in the range from 20,000 to 40,000 thousand.

4.The biggest outlier(McDonald’s) is so distracted from the average total sales  by 10,00% higher than the mean.

5.The 8 outliers in total sales are too high and pull the average far away from the mean and lower quartiles.

## YOY sales change for All Chain

```{r echo=TRUE}
plot_ly(x = top100chains$YOY.SALES.CHANGE, type = 'histogram') %>%
  layout(title='YOY sales change for All Chain', xaxis = list(title = "YOY sales change"), yaxis = list(title = "Frequency"))

plot_ly(top100chains_tibble, y = ~YOY.SALES.CHANGE,
        name = 'Chains', type = "box") %>%
  layout(title='YOY Sales Change for All Chains', yaxis = list(title = 'Percentage'))
```

By categorize YOY sales changes of top restaurants from -60% to 40% with a step of 10%, the histogram and boxplot are shown above.

### Findings

1.YOY sales changes are varied from a restaurant to another on the individually basis.

2.Most restaurants witnessed a slightly increase below 10% in 2020.

3.The average increase rate for Top 100 restaurants is negative in 2020.

4.The worst restaurant worst witnessed a sharply decrease in YOY sales changes of over 50%.

5.Less than 10 restaurants have a increase over 20% in YOY sales changes.

## Total Units for All Chain
```{r echo=TRUE}
plot_ly(top100chains_tibble, y = ~X2020.U.S..UNITS,
        name = 'Chains', type = "box") %>%
  layout(title='Total Units for All Chains', yaxis = list(title = 'Total Units'))
```

By categorize total units of top restaurants, the boxplot is shown above.

### Findings

1.Most restaurants have units range from 388 to 1800.

2.The biggest restaurant owns over 20000 units,much bigger than any other restaurants.

3.The average restaurants units among the Top 100 is 706.

4.There are 11 outliers in the units, but the highly concentrated in the  part near 2000.

5.The smallest restaurants only processed 53 units to be in the top 100.

## YOY unit change for All Chains

```{r echo=TRUE}
plot_ly(x = top100chains$YOY.UNIT.CHANGE, type = 'histogram') %>%
  layout(title='YOY Units Change for All Chains', xaxis = list(title = "YOY sales change"), yaxis = list(title = "Frequency"))

plot_ly(top100chains_tibble, y = ~YOY.UNIT.CHANGE,
        name = 'Chains', type = "box") %>%
  layout(title='YOY Units Change for All Chains', yaxis = list(title = 'Percentage'))
```
By categorize YOY unit changes, the histogram and boxplot are shown above.

### Findings

1.Most restaurants have units decrease below 5% in 2020.

2.The biggest restaurant in YOY unit changes has 11% units increase in 2020.

3.The average restaurants units changes is 0%,which means altogether Top 100 restaurant chains are remain the same unit level in 2020 than 2019.

4.The worse restaurant lost 13 units in 2020 compared to 2019.

5.There are four outliers among those units which increased in 2020.

## Proportion of Top 100 chain restaurants in segments

```{r echo=TRUE}
#remove /t
data_segments <- table(str_trim(top100chains_tibble$SEGMENT))
df <- data.frame(data_segments)
colnames(df) <- c('Segments', 'Frequency')
plot_ly(df, labels = ~Segments, values = ~Frequency, type = 'pie') %>%
  layout(title = 'Proportion of Top 100 Chain Restaurants in Segments',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

By calculating the porpotion in segments, the piechart is shown above.

### Findings

1. Almost half restaurants in Top 100 chain is in quick service segments.

2. Only one restaurant in Top chain is from fine dining in 2020.

3. The fast casual restaurants share nearly the same portion with casual dining restaurants.

4. Mid scale restaurants occupied around 5% of the Top 100 chain restaurants.

5. The segments are highly biased in Top 100 chain restaurants.

## Proportion of Top 100 chain restaurants in categories
```{r echo=TRUE}
#remove /t
data_category <- sort(table(str_trim(top100chains_tibble$MENU.CATEGORY)), decreasing = TRUE)
df1 <- data.frame(data_category)
colnames(df1) <- c('Category', 'Frequency')

plot_ly(df1, x = ~Category, y = ~Frequency, type = 'bar', marker = list(color = 'rgb(158, 202, 300)', line = list(color = 'rgb(8, 48, 107)', width = 1.5))) %>% layout(title = 'Proportion of Top 100 chain restaurants in categories')
```

By calculating the proportion in categories, the histogram is shown above.

### Findings

1.The top 5 categories are Burger,Chicken, Sandwich ,Pizza and Varied Menu. All of them are related to fast food.

2.The bottom 5 categories are Cafe, Asian, Sports Bar, Seafood and Noodles All of them are related to specific location,race or ingredient.

3.There are altogether 18 categories in Top 100 chain restaurants, which showed a good level of variation.

4.The Chicken restaurants are four times bigger in number than seafood restaurants in the Top 100 chain.

5.Burger takes up over 15% in number in the Top 100 chain restaurants.

# Top 5 Analysis
## Top 5 chain restaurants in total sales
```{r echo=TRUE}
top100chains_tibble %>% arrange(desc(top100chains_tibble$X2020.U.S..Sales...000.000.)) %>% 
  select(CHAIN, X2020.U.S..Sales...000.000.) %>% head(5) -> top5chains_inTotalSales
colnames(top5chains_inTotalSales)[2] <- 'Total Sales'
df2 <- data.frame(top5chains_inTotalSales, stringsAsFactors = FALSE)
df2$CHAIN <- factor(df2$CHAIN, levels = unique(df2$CHAIN)[order(df2$Total.Sales, decreasing = TRUE)])

plot_ly(df2, x = ~CHAIN, y = ~Total.Sales, type = 'bar', color = df2$CHAIN, colors = c('blue', 'blueviolet', 'darkorange', 'orchid', 'palevioletred'), opacity = 0.5) %>% layout(title = 'Top5 Chains in Total Sales')
```

By calculating top 5 chain restaurants in total sales, the barplot is shown above.

### Findings
1.The top 5 restaurants in total sales is McDonald's, Starbucks, Chick-fil-A,Taco Bell and Wendy’s.

2.McDonald’s as the biggest in total sales is twice bigger than Starbucks,the second biggest in total sales.

3.Wendy’s as the fifth biggest restaurants in total sales is slightly over 10,000 thousand dollars.

## Top 5 categories of restaurants in total sales.
```{r echo=TRUE}
top100chains_tibble %>% group_by(MENU.CATEGORY) %>%
  summarise(total.sale = sum(X2020.U.S..Sales...000.000.)) %>%
  arrange(desc(total.sale)) %>% head(5) -> top5_category_total_sales

colnames(top5_category_total_sales)[1:2] <- c('Category', 'Total Sales')
df3 <- data.frame(top5_category_total_sales, stringsAsFactors = FALSE)
df3$Category <- factor(df3$Category, levels = unique(df3$Category)[order(df3$Total.Sales, decreasing = TRUE)])

plot_ly(df3, x = ~Category, y = ~Total.Sales, type = 'bar', color = df3$Category, colors = c('blue', 'blueviolet', 'darkorange', 'orchid', 'palevioletred'), opacity = 0.5) %>% layout(title = 'Top 5 Categories of Restaurants in Total Sales')
```

By calculating top 5 categories of restaurants in total sales. the barplot is shown above.

### Findings

1.The top 5 categories of restaurants in total sales are burger, chicken, sandwich ,pizza and Mexican. 

2.Burger as the biggest category in total sales is over 80000, more than twice bigger than sandwich,the second biggest category in total sales.

3.Mexican as the fifth biggest category in total sales is just around 20,000 thousand dollars.


## Top 5 segments of restaurants in total sales.
```{r echo=TRUE}

plot_ly(data = top100chains_tibble, x = ~X2020.U.S..Sales...000.000., y = ~X2020.U.S..UNITS, color = ~SEGMENT) %>% layout(title='Top 5 segments of restaurants in total sales', yaxis = list(title = 'Total Units'), xaxis = list(title = 'Total sales'))

```

By calculating top 5 segments of restaurants in total sales. the scatterplot is shown above.

### Findings

1.The top 5 segments of restaurants in total sales are quick service, fast casual, casual dining, midscale and fine dining.

2.Quick service as the biggest segments in total sales can be above 20,000, more than four times bigger than fast casual,the second biggest segment in total sales.

3.Most of top segments restaurants are concentrate in 0-10,000 with a unit number. 

## Top 5 categories of restaurants in YOY sales changes
```{r echo=TRUE}
top100chains_tibble %>% group_by(MENU.CATEGORY) %>%
  summarise(count = n(), avg_YOYsaleschanges = mean(YOY.SALES.CHANGE)) %>%
  arrange(desc(avg_YOYsaleschanges)) %>% head(n = 5) -> top5_category_YOYsaleschanges

top100chains_tibble %>%
  filter(MENU.CATEGORY == as.character(top5_category_YOYsaleschanges[1,1])) %>%
  select(Pizza = YOY.SALES.CHANGE) -> Pizza_YOYsaleschanges

top100chains_tibble %>%
  filter(MENU.CATEGORY == as.character(top5_category_YOYsaleschanges[2,1])) %>%
  select(Chicken = YOY.SALES.CHANGE) -> Chicken_YOYsaleschanges

top100chains_tibble %>%
  filter(MENU.CATEGORY == as.character(top5_category_YOYsaleschanges[3,1])) %>%
  select(Burger = YOY.SALES.CHANGE) -> Burger_YOYsaleschanges

top100chains_tibble %>%
  filter(MENU.CATEGORY == as.character(top5_category_YOYsaleschanges[4,1])) %>%
  select(Frozen_Desserts = YOY.SALES.CHANGE) -> Frozen_Desserts_YOYsaleschanges

top100chains_tibble %>%
  filter(MENU.CATEGORY == as.character(top5_category_YOYsaleschanges[5,1])) %>%
  select(Mexican = YOY.SALES.CHANGE) -> Mexican_YOYsaleschanges

mynames<-c('Pizza', 'Chicken', 'Burger', 'Frozen_Desserts', 'Mexican') 

plot_ly(Pizza_YOYsaleschanges, y = ~Pizza, type="box", name = 'Pizza') %>%
  add_trace(y = ~Chicken_YOYsaleschanges$Chicken, name = 'Chicken') %>%
  add_trace(y = ~Burger_YOYsaleschanges$Burger, name = 'Burger') %>%
  add_trace(y = ~Frozen_Desserts_YOYsaleschanges$Frozen_Desserts, name = 'Frozen Desserts') %>% 
  add_trace(y = ~Mexican_YOYsaleschanges$Mexican, name = 'Mexican') %>%
  layout(title = 'Top 5 Categories of Restaurants in YOY Sales Changes', yaxis = list(title = 'YOY Sales Changes'))
```

By calculating top 5 categories of restaurants in YOY sales changes, the boxplot is shown above.

### Findings

1.The top 5 chain restaurants in YOY sales changes are Pizza, Chicken, Burger, Frozen Desserts and Mexican.

2.Pizza as the biggest category of restaurants in YOY sales changes has an average YOY sales increase slightly above 0 percent, but most increases are concentrated between -5% to 20%.

3.Mexican as the fifth biggest category of restaurants in YOY sales changes has an average YOY sales increase around 0 percent, but increases inside this category varied hugely among each other.

## Top 5 chain restaurants in units
```{r echo=TRUE}
top100chains_tibble %>% arrange(desc(X2020.U.S..UNITS)) %>%
  select(CHAIN, Total.Units = X2020.U.S..UNITS) %>% head(5) -> top5chains_inTotalUnits
df6 <- data.frame(top5chains_inTotalUnits, stringsAsFactors = FALSE)
df6$CHAIN <- factor(df6$CHAIN, levels = unique(df6$CHAIN)[order(df6$Total.Units, decreasing = TRUE)])

plot_ly(df6, x = ~CHAIN, y = ~Total.Units, type = 'bar', color = df6$CHAIN, colors = c('blue', 'blueviolet', 'darkorange', 'orchid', 'palevioletred'), opacity = 0.5) %>% layout(title = 'Top 5 Chain Restaurants in Total Units')
```

By calculating the top 5 chain restaurants in units. the barplot is shown above.

### Findings

1.The top 5 chain restaurants in units are Subway, Starbucks,McDonald’s,Duckin and Burger King.

2.Subway, as the biggest chain restaurant in units has over 20000 chains, 5000 more than Starbucks, the second biggest chain restaurant in units.

3.Burger King as the fifth chain restaurant in units is around 5,000 units, 4 times smaller than the biggest.

# Cross Elements Analysis
## Unit-Sale-Sales Change  Correlations
```{r echo=TRUE}
plot_ly(top100chains_tibble, x = ~X2020.U.S..Sales...000.000., y = ~X2020.U.S..UNITS, text = ~CHAIN, type = 'scatter', mode = 'markers', marker = list(size = ~YOY.SALES.CHANGE, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>% layout(title = 'Unit-Sale-Sales Change  Correlations',
         xaxis = list(title = 'Total Sales', showgrid = FALSE),
         yaxis = list(title = 'Total Units', showgrid = FALSE))
```
By analyzing the differences between unit, sales and YOY sales changes.

### Findings

1.There are no postive or negative, strong or small correlation between these three elements.

2.Bigger sales isn't based on the unit scale,so does YOY Sales change.

3.Small unit restaurants still can have a great sales a significant increase in sales.

## Top 5 chain restaurants in YOY unit changes
```{r echo=TRUE}
top100chains_tibble %>% select(CHAIN, YOY.UNIT.CHANGE) %>%
  arrange(desc(YOY.UNIT.CHANGE)) %>% head(5) -> top5chains_YOYUnitsChange
df8 <- data.frame(top5chains_YOYUnitsChange, stringsAsFactors = FALSE)
df8$CHAIN <- factor(df8$CHAIN, levels = unique(df8$CHAIN)[order(df8$YOY.UNIT.CHANGE, decreasing = TRUE)])

plot_ly(df8, x = ~CHAIN, y = ~YOY.UNIT.CHANGE, type = 'bar', color = df$Segments, colors = c('blue', 'blueviolet', 'darkorange', 'orchid', 'palevioletred'), opacity = 0.5) %>% layout(title = 'Top 5 Chain Restaurants in YOY Unit Changes')
```

By calculating the top 5 chain restaurants in YOY unit changes. the barplot is shown above.

### Findings

1.The top 5 chain restaurants in YOY unit changes are Raising Cane,Jersey Mike’s,First Watch,Ducth Bros and Shake Shack.

2.Raising Cane, as the biggest chain restaurant in YOY unit changes has over 10% chains increase,nearly the same with Jersey Mike’s and First Watch.

3.Shake Shack as the fifth chain restaurant in YOY unit changes is just a little bit lower than the biggest, in other words, the top 5 chain restaurant in YOY unit changes is very average.

## Top 5 segments of restaurants in YOY unit changes
```{r echo=TRUE}

plot_ly(data = top100chains_tibble, x = ~YOY.UNIT.CHANGE, y = ~YOY.SALES.CHANGE, color = ~SEGMENT) %>% layout(title='Top 5 segments of restaurants in YOY unit changes', yaxis = list(title = 'YOY Sales Change'), xaxis = list(title = 'YOY Unit Change'))

```

By calculating top 5 segments of restaurants in YOY unit changes, the scatterplot is shown above.

### Findings
1.The top 5 segments of restaurants in YOY unit changes are Fast Casual, Midscale, Quick Service,Casual Dining and Fine Dining.

2.Elements are scattered in the plot so the even in the same segments, different restaurants witnessed a significantly different in growth rate, which means catering is more based on indivual business. 

# Detail Analysis
## Burger brands and total sales(boxplot)
```{r echo=TRUE}
top100chains_tibble %>% filter(MENU.CATEGORY == 'Burger') %>%
  select(Total.Sales = X2020.U.S..Sales...000.000.) -> Burger_total_sales

plot_ly(Burger_total_sales, y = ~Total.Sales,
        name = 'Burger', type = "box") %>%
  layout(title='Burger Brands and Total Sales', yaxis = list(title = 'Total Sales'))
```

By zoom in to the biggest category and calculating burger brands and total sales, the boxplot is shown above.

### Findings

1.Most burger restaurants have total sales range from 377 to 2,890.

2.The biggest burger restaurant has over 40000 in sales,much bigger than any other burger restaurants.

3.The average burger restaurants sales among the Top 100 is around 1,000.

4.There are 3 outliers in the sales, not concentrated with each other.

## Quick brands and total sales(boxplot)
```{r echo=TRUE}
top100chains_tibble %>% filter(SEGMENT == 'Quick Service') %>%
  select(Total.Sales = X2020.U.S..Sales...000.000.) -> Quick_Service_total_sales

plot_ly(Quick_Service_total_sales, y = ~Total.Sales, name = 'Quick Service',
        type = "box") %>%
  layout(title='Quick Brands and Total Sales', yaxis = list(title = 'Total Sales'))
```

By zoom in to the biggest segment and calculating quick brands and total sales, the boxplot is shown above.

### Findings

1.Most quick brands restaurants have total sales range from 377 to 2,980.

2.The biggest quick brands burger restaurant has over 40000 in sales,much bigger than any other quick brands restaurants.

3.The average quick brands restaurants sales among the Top 100 is around 500.

4.There are 4 outliers in the sales, not concentrated with each other.

# Investment Suggestions
1.If you want to have the best grow in units burger restaurants  are the best choice.

2.If you want to have the best grow in sales quick brands are the best choices.

3.If you want to invest in a brand with a balance of not fierce competition, high increase rate and well known to others, you should choose Shake Shack.