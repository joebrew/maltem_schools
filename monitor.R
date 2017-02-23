library(cism)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)

# Goto https://172.16.236.245:4443/OpenClinica/
# user:jbrew
# pass: normal

# df <- read_excel('data/EXCEL_Absentismo_escolar_2017-01-18-101804043.xls',
#                  skip = 12)
# df <- read_csv('data/EXCEL_Absentismo_escolar_2017-01-18-101804043.csv', skip = 12)
# 
# df <- read_excel('data/EXCEL_Mapa_de_Faltas_Joe_B_2017-01-11-140424529.xls',
#                  skip = 12)

df <- read_tsv('data/TAB_Joe_Brew_all_data_2017-02-23-081532631.tsv',
               skip = 13)

# Remove those with no name
df <- df[,!is.na(names(df))]

# Clean up
df$school <- 
  unlist(lapply(strsplit(df$`Study Subject ID`, '_'),
                function(x){
                  x[1]
                }))

df$year <-
  as.numeric(unlist(lapply(strsplit(df$`Study Subject ID`, '_'),
                function(x){
                  x[2]
                })))

df$number <- 
  as.numeric(unlist(lapply(strsplit(df$`Study Subject ID`, '_'),
                           function(x){
                             x[3]
                           })))

df$letter <- 
  unlist(lapply(strsplit(df$`Study Subject ID`, '_'),
                           function(x){
                             x[4]
                           }))

df$roster_number <- 
  as.numeric(unlist(lapply(strsplit(df$`Study Subject ID`, '_'),
                           function(x){
                             x[5]
                           })))

df$turma <- paste0(df$number, '-', df$letter)

# Make long
df <- gather(df, key, value, dplyr::contains('dia'))

# Remove all those days with no observation
df <- df %>% filter(!is.na(value))

# Create an absence variable
df$absent <- ifelse(df$value %in% c('FALSE', 'F'), TRUE,
                    ifelse(df$value == 'P', FALSE, 
                           NA))

# Define dataframe for matching months, etc.
month_matcher <- data_frame(month_number = 1:12,
                            month_name = c('jan',
                                           'fev',
                                           'mar',
                                           'abr',
                                           'mai',
                                           'jun',
                                           'jul',
                                           'ago',
                                           'set',
                                           'out',
                                           'nov',
                                           'dec'))

# Get month number based on date
df$month_name <- substr(df$key, 1, 3)
df <- left_join(x = df,
                y = month_matcher,
                by = 'month_name')
rm(month_matcher)

# Get day number
df$day_number <- 
  unlist(lapply(strsplit(df$key, '_'), function(x){
    z <- x[1]
    as.numeric(substr(z, 7, nchar(z)))
  }))

# Get a year matcher
df$year2 <- ifelse(grepl('C2', df$key), 2016,
                  ifelse(grepl('C1', df$key), 2015,
                         NA))

# What's going on with this?



# Create a date
df$date <- as.Date(paste0(df$year, 
                   '-',
                   df$month_number,
                   '-',
                   df$day_number))

# Get day of week
df$dow <- weekdays(df$date)

# Standardize school names
# # cat(paste0('"', sort(unique(df$school)), '"', collapse = ',\n'))
school_standardizer <-
  data_frame(school = c("1 E 2 GRAU XINAVANE",
                        "1E 2 GRAU XINAVANE",
                        "3 DE FEVEREIRO",
                        "3 DE FEVEREIRO ",
                        "3 DE FEVREIRO",
                        "DUCA",
                        "DUCO",
                        "EPC GRACA MACHEL",
                        "GRACA MACHEL",
                        "ILHA",
                        "ILHA JOSINA",
                        "JOSINA",
                        "JOSINA MACHEL",
                        "MAGUIGUANA",
                        "MARAGRA",
                        "MOINE",
                        "SIMBE",
                        "XINAVANE",
                        "XINAVENE",
                        "XIVANANE"),
             new_school = c('Xinavane', 
                            'Xinavane',
                            '3 de Fev',
                            '3 de Fev',
                            '3 de Fev',
                            'Duco',
                            'Duco',
                            'Graca Machel',
                            'Graca Machel',
                            'Ilha Josina',
                            'Ilha Josina',
                            'Ilha Josina',
                            'Ilha Josina',
                            'Maguiguana',
                            'Maragra',
                            'Moine',
                            'Simbe',
                            'Xinavane',
                            'Xinavane',
                            'Xinavane'))
df <- left_join(x = df,
                y = school_standardizer,
                by = 'school') %>%
  dplyr::select(-school) %>%
  rename(school = new_school) %>%
  filter(!is.na(school))
rm(school_standardizer)

# Define geography
# # cat(paste0('"', sort(unique(df$school)), '"', collapse = ',\n'))
geography <- data_frame(school = c("3 de Fev",
                                   "Duco",
                                   "Graca Machel",
                                   "Maguiguana",
                                   "Maragra",
                                   "Moine",
                                   "Simbe",
                                   "Xinavane",
                                   "Ilha Josina"),
                        district = c('Manhiça',
                                     'Magude',
                                     'Manhiça',
                                     'Magude',
                                     'Manhiça',
                                     'Magude',
                                     'Magude',
                                     'Magude',
                                     'Manhiça'))
df <- left_join(x = df,
                y= geography,
                by = 'school')
rm(geography)

# Get a truncated month
df$month <- as.Date(paste0(df$year, '-', 
                           df$month_number, '-01'))

# Remove those before 2015
df <- df %>%
  filter(year >= 2015)

# Create a year_term variable
df$term <-
  ifelse(as.numeric(format(df$month, '%m')) %in% 2:4, '1',
         ifelse(as.numeric(format(df$month, '%m')) %in% 5:7, '2',
                ifelse(as.numeric(format(df$month, '%m')) %in% 8:10, '3',
                       NA)))
df$year_term <- paste0(df$year,
                       '-',
                       df$term)

#####################################
# Visualize, etc.

# Students per school
x <- df %>%
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
  geom_label(aes(label = n_students))

prop.table(table(unique(df$nome_E1_C1[df$year == 2015]) %in% unique(df$nome_E1_C1[df$year == 2016]))) * 100



# Absenteeism by date by intervention
x <- df %>%
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
                     values = cols)


# Monthly absenteeism in both districts
x <- df %>%
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
x <- df %>%
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
  geom_hline(yintercept = mean(x$absenteeism_rate), lty = 2, alpha = 0.5)

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

# Examine by term
x <- df %>%
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
  geom_line()  +
  geom_vline(xintercept = 3.1, lty = 2, alpha = 0.6) +
  theme_cism()

# Variation
months <- sort(unique(x$date))
for (i in 1:length(months)){
  out <- g +
    xlim(min(x$date), months[i])
  print(out)
  Sys.sleep(1)
}
  


# Aggregate by year
x <- df %>%
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
x <- df %>%
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
  facet_wrap(~district) +
  geom_point(aes(size = sample),
             alpha = 0.5) +
  scale_x_date(date_labels = "%B") +
  scale_color_manual(name = 'Year',
                     values = c('darkblue', 'darkred')) +
  theme_cism() +
  xlab('Month')

model_data <- df
model_data$intervention <- df$year == 2016

model_data$district <- factor(model_data$district,
                              levels = c('Manhiça',
                                         'Magude'))
  
# Make linear regression
linear_data <- 
  df %>%
  group_by(year, month, month_number = factor(month_number), district, school) %>%
 

fit <- lm(log_ar ~ district*intervention +
            school +
            month_number, 
          data = linear_data)
summary(fit)
exp(coef(fit))
exp(confint(fit))

# Do person-level monthly rate
monthly_person <-
  df %>%
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
# absenteeism rate (0.25, 0.33, 0.45, etc.)
# Elisa: Ask Judit about the issue of entries and exits to cohort
# Everyone looks at AEJ (Applied Economics) to see format, etc.; backup = Journal
# of Health Economics