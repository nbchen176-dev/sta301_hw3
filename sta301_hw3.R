library(tidyverse)
library(mosaic)
options(scipen = 999)
theme_set(theme_minimal())
library(scales)

#1 Gas prices and presidential approval
lm(approval ~ gas, data = approval)

ggplot(approval, aes(x = gas, y = approval)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "Gas Prices are inversely related with presidential approval",
       x = "Average price for a gallon of gas (U.S. dollars)",
       y = "Presidential approval rate (Gallup)") +
  scale_x_continuous(labels = dollar)

lm(approval ~ gas, data = approval) %>%
  confint() * 0.1

lm_gas = lm(approval ~ gas, data = approval)
rsquared(lm_gas)
sd(resid(lm_gas))

confint(lm_gas)
favstats(~approval, data = approval)
ggplot(approval) +
  geom_histogram(aes(x=approval))

#2 The Bechdel Test

ggplot(films) +
  geom_histogram(aes(x = votes)) +
  facet_wrap(~test, nrow=2) +
  labs(title = "Right Skewed Distributions of Votes for Films that Passed or Failed Bechdel Test",
       x = "Number of IMDB.com votes",
       y = "Frequency of films")

mean(votes ~ test, data = films)
diffmean(votes ~ test, data = films)
t.test(votes ~ test, data = films)

ggplot(films) +
  geom_bar(aes(x = test)) +
  facet_wrap(~R_rated) + 
  labs(title = "Higher Proportion of R-rated Movies Fail Bechdel Test",
       x = "Bechdel Test Condition",
       y = "Number of Films")

prop(~test, data = films)
prop(test ~ R_rated, data = films)

prop.test(test ~ R_rated, data = films)

#3 - Manufacturing flaws in circuit boards

xtabs(~Solder, data = ATT)
xtabs(~Size, data = ATT)
confint(lm(Skips ~ Solder + Size, data = ATT))


skips_reg = lm(Skips ~ Solder + Size, data = ATT)
confint(skips_reg)

ggplot(ATT) +
  geom_boxplot(aes(x = Size, y = Skips)) +
  labs(title = "Small Openings Have Greatest Variations in Skips",
       x = "Size of solder gun opening",
       y = "Number of solder skips")

ggplot(ATT) +
  geom_histogram(aes(x = Skips, y = ..density..), binwidth = 1) + 
  facet_wrap(~Solder) +
  labs(title = "Higher Proportion of Greater Skips on Thin Solders",
       x = "Number of Skips",
       y = "Proportion of Solders")

                 