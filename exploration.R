source('prepare_student_data.R')

#####################################
# Data quality checks

# Absenteeism: How many turma-years in absenteeism data?
x <- ab %>%
  group_by(year, school) %>%
  summarise(turmas = length(unique(turma))) %>%
  arrange(turmas)
write_csv(x, '~/Desktop/absenteeism_number_of_turmas_by_school_year.csv') %>%
  filter(!is.na(year))

# Absenteeism: How many student-years
x <- ab %>%
  group_by(year, nome_E1_C1) %>%
  tally

# Performance: How much variation is there between schools
x <- performance %>%
  group_by(school, year) %>%
  summarise(value = mean(value, 
                         na.rm = TRUE))

# Performance, how much variation between subjects
x <- performance %>%
  group_by(subject) %>%
  summarise(value = mean(value, 
                         na.rm = TRUE))

# Performance, how many turma-years
x <- performance %>%
  group_by(year, school, number, letter) %>%
  summarise(length(unique(`Study Subject ID`)))

# See how performance changed over time
x <- performance %>% 
  group_by(district,year, trimester) %>% 
  summarise(average=mean(value, na.rm=TRUE),
            students = length(unique(`Study Subject ID`))) %>%
  mutate(year_trimester = paste0(year, '-', trimester)) %>%
  filter(!is.na(year))
#
ggplot(data = x,
       aes(x = year_trimester,
           y = average,
           group = district,
           color = district)) +
  geom_point(aes(size = students)) +
  geom_line()

# See how performance changed over year
x <- performance %>% 
  group_by(district,year) %>% 
  summarise(average=mean(value, na.rm=TRUE),
            students = length(unique(`Study Subject ID`))) %>%
  filter(!is.na(year))
#
ggplot(data = x,
       aes(x = year,
           y = average,
           group = district,
           color = district)) +
  geom_point(aes(size = students)) +
  geom_line()

# See how performance changed over time by school
x <- performance %>% 
  group_by(district, school,year, trimester) %>% 
  summarise(average=mean(value, na.rm=TRUE),
            students = length(unique(`Study Subject ID`))) %>%
  mutate(year_trimester = paste0(year, '-', trimester)) %>%
  filter(!is.na(year))
#
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(x$school)))
ggplot(data = x,
       aes(x = year_trimester,
           y = average,
           group = school,
           color = school)) +
  geom_point(aes(size = students,
                 pch = district),
             alpha = 0.8) +
  geom_line() +
  scale_color_manual(name = 'School',
                     values = cols) +
  theme_bw()

# Distribution of performance by year and district
ggplot(data = performance,
       aes(x = value)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~year, ncol = 1)

ggplot(data = performance,
       aes(x = value)) +
  geom_histogram(alpha = 0.5) +
  facet_grid(year~district)

ggplot(data = performance %>%
         mutate(year = factor(year)),
       aes(x = value,
           group = year,
           fill = year)) +
  geom_density(alpha = 0.5,
               color = NA) +
  facet_wrap(~school)

ggplot(data = performance %>%
         mutate(year = factor(year)),
       aes(x = year,
           y = value)) +
  # geom_point(alpha = 0.6) +
  geom_violin(alpha = 0.3, 
              fill = 'darkorange') +
  facet_wrap(~school)

#####################################
# Visualize, etc.



# Students per school
x <- ab %>%
  group_by(school,
           year) %>%
  summarise(n_students = length(unique(`Study Subject ID`))) %>%
  arrange(desc(n_students))

ggplot(data = x,
       aes(x = school,
           y = n_students)) +
  geom_bar(stat = 'identity',
           alpha = 0.6,
           fill = 'darkblue') +
  facet_wrap(~year) +
  theme_cism() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label(aes(label = n_students), size = 3)

prop.table(table(unique(ab$nome_E1_C1[ab$year == 2015]) %in% unique(ab$nome_E1_C1[ab$year == 2016]))) * 100

# Absenteeism by date by intervention
x <- ab %>%
  group_by(district, date) %>%
  summarise(absences = length(which(absent)),
            eligibles = n()) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)

library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(x$district)))
ggplot(data = x,
       aes(x = date,
           y = absenteeism_rate,
           group = district,
           color = district)) +
  geom_point(alpha = 0.3,
             aes(size = eligibles)) +
  geom_smooth() +
  theme_cism() +
  scale_color_manual(name = 'District',
                     values = cols) +
  ylim(0, 20)


# Number of eligible days by day and district
x <- ab %>%
  group_by(district, date) %>%
  tally %>%
  filter(!is.na(date))
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(x$district)))
ggplot(data = x,
       aes(x = date,
           y = n,
           group = district,
           color = district)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  theme_cism() +
  scale_color_manual(name = 'District',
                     values = cols)



# Monthly absenteeism in both districts
x <- ab %>%
  group_by(district,
           date = month) %>%
  summarise(absences = length(which(absent)),
            eligibles = n()) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)

ggplot(data = x,
       aes(x = date,
           y = absenteeism_rate,
           group = district,
           fill = district)) +
  geom_bar(stat = 'identity',
           pos = 'dodge',
           alpha = 0.6) +
  theme_cism() +
  labs(title = 'Absenteeism rate') +
  scale_fill_manual(name = 'District',
                    values = cols)


# Monthly absenteeism
x <- ab %>%
  group_by(district, date = month) %>%
  summarise(absences = length(which(absent)),
            eligibles = n(),
            `Sample size` = length(unique(`Study Subject ID`))) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)

ggplot(data = x,
       aes(x = date,
           y = absenteeism_rate)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_cism() +
  labs(title = 'Absenteeism by month') +
  facet_wrap(~district, ncol = 2) +
  geom_hline(yintercept = mean(x$absenteeism_rate), lty = 2, alpha = 0.5) +
  labs(x = 'Date',
       y = 'Absenteeism rate (%)')

# Observations instead of absences
ggplot(data = x,
       aes(x = date,
           y = eligibles)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_cism() +
  labs(title = 'Absenteeism by month') +
  facet_wrap(~district, ncol = 2) +
  geom_hline(yintercept = mean(x$eligibles), lty = 2, alpha = 0.5)


# Lines instead of bars
g <- ggplot(data = x,
            aes(x = date,
                y = absenteeism_rate,
                color = district)) +
  geom_line(alpha = 0.5) +
  stat_smooth(aes(weight = `Sample size`), fill = NA) +
  # geom_point()
  geom_point(aes(size = `Sample size`),
             alpha = 0.3) +
  theme_cism() +
  labs(title = 'Absenteeism by month') +
  geom_hline(yintercept = mean(x$absenteeism_rate), lty = 2, alpha = 0.5) +
  scale_color_manual(name = 'District',
                     values = cols) +
  xlab('Date') +
  ylab('Monthly absenteeism rate') +
  geom_vline(xintercept = '2016-10-01')
g

# By school
x <- ab %>%
  group_by(district, date = month, school) %>%
  summarise(absences = length(which(absent)),
            eligibles = n(),
            `Sample size` = length(unique(`Study Subject ID`))) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)

ggplot(data = x,
       aes(x = date,
           y = absenteeism_rate,
           color = district)) +
  geom_point(aes(size = `Sample size`),
             alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(~school) +
  scale_color_manual(name = 'District',
                     values = c('darkred', 
                                'green')) +
  theme_cism() +
  labs(x = 'Date',
       y = 'Absenteeism rate',
       title = 'Absenteeism rate by school')



# Examine by term
x <- ab %>%
  group_by(district, year, term) %>%
  summarise(absences = length(which(absent)),
            eligibles = n(),
            `Sample size` = length(unique(`Study Subject ID`))) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)  %>%
  filter(!is.na(term)) %>%
  mutate(year_term = paste0(year, '-', term))

ggplot(data = x,
       aes(x = year_term,
           y = absenteeism_rate,
           fill = district,
           group = district)) +
  geom_bar(stat= 'identity', pos = 'dodge')

ggplot(data = x,
       aes(x = year_term,
           y = absenteeism_rate,
           color = district,
           group = district)) +
  geom_point() +
  geom_line(alpha = 0.3)  +
  geom_vline(xintercept = 2.5, lty = 2, alpha = 0.6) +
  theme_cism() +
  scale_color_manual(name = 'District',
                     values = c('darkred', 'green')) +
  labs(x = 'Year-Trimester',
       y = 'Absenteeism rate',
       title = 'Absenteeism rate by district',
       subtitle = 'Magude (intervention) vs. Manhiça (control)')

# Aggregate by year
x <- ab %>%
  group_by(date = year,
           district) %>%
  summarise(absences = length(which(absent)),
            eligibles = n()) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)


ggplot(data = x,
       aes(x = factor(date),
           y = absenteeism_rate,
           color = district,
           group = district)) +
  geom_line() +
  geom_point() +
  theme_cism() +
  scale_color_manual(name = 'District',
                     values = cols) +
  ylim(0, max(x$absenteeism_rate)) +
  xlab('Date') +
  ylab('Absenteeism rate')

# Overlapped years
x <- ab %>%
  mutate(day_number = as.numeric(format(date, '%j'))) %>%
  mutate(date = as.Date('2000-12-31') + day_number) %>%
  mutate(month = as.Date(paste0('2000-',
                                format(date, '%m'),
                                '-01'))) %>%
  group_by(year,
           month,
           district) %>%
  summarise(absences = length(which(absent)),
            eligibles = n(),
            sample = length(unique(`Study Subject ID`))) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100)

ggplot(data = x,
       aes(x = month,
           y = absenteeism_rate,
           color = factor(year))) +
  geom_line(alpha = 0.6) +
  geom_smooth() +
  facet_wrap(~district) +
  geom_point(aes(size = sample),
             alpha = 0.5) +
  scale_x_date(date_labels = "%B") +
  scale_color_manual(name = 'Year',
                     values = c('darkblue', 'darkred')) +
  theme_cism() +
  xlab('Month')

model_data <- ab
model_data$intervention <- ab$year == 2016

model_data$district <- factor(model_data$district,
                              levels = c('Manhiça',
                                         'Magude'))

# Make linear regression
linear_data <-
  ab %>%
  group_by(year, month, month_number = factor(month_number), district, school) 


fit <- lm(log_ar ~ district*intervention +
            school +
            month_number,
          data = linear_data)
summary(fit)
exp(coef(fit))
exp(confint(fit))

# Do person-level monthly rate
monthly_person <-
  ab %>%
  group_by(year, month, `Study Subject ID`) %>%
  summarise(absences = length(which(absent)),
            eligibles = n(),
            `Sample size` = length(unique(`Study Subject ID`))) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100) %>%
  mutate(intervention = year >= 2016) %>%
  # mutate(district = factor(district,
  #                          levels = c('Manhiça',
  #                                     'Magude'))) %>%
  mutate(log_ar = log(absenteeism_rate)) %>%
  filter(is.finite(log_ar))

ggplot(linear_data,
       aes(x = absenteeism_rate)) +
  geom_density(fill = 'darkorange', alpha = 0.6) +
  theme_cism()

cism_plot(linear_data$absenteeism_rate)
cism_plot(log(linear_data$absenteeism_rate))
# 
# Make logistic regression
fit <- glm(absent ~ district*intervention,
           family = binomial('logit'),
           data = model_data)
summary(fit)

exp(coef(fit))
exp(confint(fit))

fake_data <- expand.grid(district = c('Manhiça', 'Magude'),
                         intervention = c(TRUE, FALSE))
fake_data$predicted <- predict(fit, fake_data, type = 'response')

ggplot(data = fake_data,
       aes(x = intervention,
           y = predicted,
           group = district,
           color = district)) +
  geom_line()

dd <- fake_data %>%
  group_by(district) %>%
  summarise(difference = first(predicted) - last(predicted))
(dd$difference[1] - dd$difference[2]) * 100

# Joe starts working paper outline (National Bureau of Economic Research)
# Joe needs to get census data merged
# Joe needs to use fuzzy matching to try to identify 2015-2016 pairs
# Joe will combine all charts into one document
# Elisa needs to setup a meeting with Judit to discuss (a) in our final regression
# whether we should use daily, individual level data (0, 1, 0, 1) or monthly
# # absenteeism rate (0.25, 0.33, 0.45, etc.)
# # Elisa: Ask Judit about the issue of entries and exits to cohort
# # Everyone looks at AEJ (Applied Economics) to see format, etc.; backup = Journal
# # of Health Economics

# Map of intervention area
library(cism)
library(ggthemes)
library(ggrepel)
moz2_fortified <- moz2_fortified
map <- moz2_fortified %>%
  filter(id %in% c('Manhiça',
                   'Magude'))

ggplot() +
  geom_polygon(data = map,
               aes(x = long,
                   y = lat,
                   group = id,
                   fill = id),
               # color = 'grey',
               alpha = 0.9) +
  theme_map() +
  scale_fill_manual(name = '',
                    values = c('red',
                               'green')) +
  geom_point(data = geo,
             aes(x = lng,
                 y = lat),
             color = 'black',
             alpha = 0.7,
             size = 3) #+
# geom_point(data = people,
#            alpha = 0.2,
#            aes(x = longitude,
#                y = latitude))
# theme(panel.background = element_rect(fill='black', colour='black'))
# geom_label_repel(data = geo,
#            aes(x = lng,
#                y = lat, 
#                label = geo$school),
#            size = 2,
#            alpha = 0.7) +

