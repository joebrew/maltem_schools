library(dplyr)
x <-
  data_frame(number = 1:29,
             description = c('title', 'context', 'maltem', 'timeline', 'objectives',
                             'literature', 'contribution', 'identification', 'map', 'census table',
                             'models', 'data collection', 'data collection 2', 'descriptive data', 'absenteeism',
                             'density1', 'density2', 'proportion passing all', 'proportion passing math', 'mean grade all',
                             'mean grade math', 'regression passed', 'regression passed math', 'regression grade', 'regression grade math', 
                             'event study table', 'event study chart', 'discussion', 'thanks'),
             who = c('joe', 'laia', 'laia', 'laia', 'laia',
                     'laia', 'joe', 'joe', 'laia', 'laia',
                     'laia', 'laia', 'laia', 'joe', 'joe',
                     'joe', 'joe', 'laia', 'laia', 'laia',
                     'laia', 'joe', 'joe', 'joe', 'joe',
                     'laia', 'laia', 'laia', 'laia'),
             time = c(30, 30, 30, 30, 30,
                      60, 30, 30, 20, 20,
                      30, 15, 10, 30, 20,
                      30, 15, 20, 20, 20,
                      20, 30, 20, 20, 20,
                      40, 20, 60, 15))
sum(x$time) / 60
y <- x %>%
  group_by(who) %>%
  summarise(minutes = sum(time))
y
View(x)