library(cism)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)
library(readxl)

# Goto https://172.16.236.245:4443/OpenClinica/
# user:jbrew
# pass: normal

#######
# PERFORMANCE
#######
performance <- 
  read_csv("data/EXCEL_Pauta_frecuencia_2017-04-13-090813462_modified_by_joe.csv")

# Clean up
performance$school <- performance$`Study Subject ID`
  # unlist(lapply(strsplit(performance$`Study Subject ID`, '_'),
  #               function(x){
  #                 x[1]
  #               }))

# Standardize school names
performance <-
  performance %>%
  mutate(school = ifelse(grepl('MACHEL|GRACA|GRAÇA|JOSI', school), 'J MACHEL',
                         ifelse(grepl('FAV|FEV|REIR', school), '3 DE FEV',
                                ifelse(grepl('XINA|SIV', school), 'XINAVANE',
                                       ifelse(grepl('MAGUI', school), 'MAGUIGUANA',
                                              ifelse(grepl('MARAG', school), 'MARAGRA',
                                                     ifelse(grepl('SIMB', school), 'SIMBE',
                                                            ifelse(grepl('MOIN', school), 'MOINE', 
                                                                   ifelse(grepl('MAGU', school), 'MAGUDE', 
                                                                          ifelse(grepl('DUCO', school), 'DUCO', NA))))))))))

# Remove those with no school
performance <- 
  performance %>%
  filter(!is.na(school))

# Get year, grade, turma and number
get_info <- function(x,
                     info = 'year'){
  y <- strsplit(x, '20')
  z <- unlist(lapply(y, function(a){
    if(length(a) > 1){
      a[[2]]
    } else {
      NA
    }
  }))
  z_split <- strsplit(z, '_')
  if(info == 'year'){
    out <- unlist(lapply(z_split, function(a){
      a[[1]]
    }))
    out <- as.numeric(paste0('20', out))
  } else if(info == 'grade'){
    out <- unlist(lapply(z_split, function(a){
      if(length(a) > 1){
        a[[2]]
      } else {
        NA
      }
    }))
    out <- as.numeric(out)
  } else if(info == 'turma'){
    out <- unlist(lapply(z_split, function(a){
      if(length(a) > 2){
        a[[3]]
      } else {
        NA
      }
    }))
  } else if(info == 'number'){
    out <- unlist(lapply(z_split, function(a){
      if(length(a) > 3){
        a[[4]]
      } else {
        NA
      }
    }))
    out <- as.numeric(as.character(out))
  }
  return(out)
}

# Get year, grade, turma and number
performance <-
  performance %>%
  mutate(year = get_info(x = `Study Subject ID`,
                         info = 'year'),
         number = get_info(x = `Study Subject ID`,
                         info = 'grade'),
         letter = get_info(x = `Study Subject ID`,
                         info = 'turma'),
         roster_number = get_info(x = `Study Subject ID`,
                         info = 'number'))

# Make long
performance <- gather(performance, key, value, 
                      portest1_E2_C4:xichamedia_E2_C4)

# Get district
district_dictionary <- data_frame(school = c('3 DE FEV',
                                             'J MACHEL',
                                             'MAGUDE',
                                             'MAGUIGUANA',
                                             'MARAGRA',
                                             'MOINE',
                                             'SIMBE',
                                             'XINAVANE',
                                             'DUCO'),
                                  district = c('Manhiça',
                                               'Manhiça',
                                               'Magude',
                                               'Magude',
                                               'Manhiça',
                                               'Magude',
                                               'Magude',
                                               'Manhiça',
                                               'Magude'))

# Join districts to performance data
performance <-
  performance %>%
  left_join(district_dictionary,
            by = 'school')

# Get trimester numbers
performance$trimester <- unlist(
  lapply(strsplit(performance$key, '_'), function(x){
    y <- x[1]
    ncy <- nchar(y)
    substr(y, ncy, ncy)
  }))
# Remove the media observations
performance <- 
  performance %>%
  filter(trimester != 'a')

# Get the name of the subject
performance$subject <- unlist(
  lapply(strsplit(performance$key, '_'), function(x){
    y <- x[1]
    ncy <- nchar(y)
    substr(y, 1, (ncy - 1))
  }))
# Remove the "test" part of the subject
performance$subject <- gsub('test', '', performance$subject)

# Create a dictionary of school subjects
performance_dictionary <-
  data_frame(subject = c('cna', 
                         'csoc',
                         'efis',
                         'evis',
                         'ingles',
                         'ma',
                         'music',
                         'ofic',
                         'por',
                         'xicha'),
             subject_name = c('Natural science',
                              'Social science',
                              'Physical education',
                              'Visual education',
                              'English',
                              'Math',
                              'Music',
                              'Home economics',
                              'Portuguese',
                              NA))

# Join the dictionary to the data
performance <-
  performance %>%
  left_join(performance_dictionary,
            by = 'subject') %>%
  dplyr::select(-subject) %>%
  rename(subject = subject_name) 

# Make value numeric
performance$value <- as.numeric(as.character(performance$value))

# Remove those without a turma letter
performance <- performance %>%
  filter(is.na(as.numeric(letter)))

########
# ABSENTEEISM
########

# df <- read_tsv('data/TAB_Joe_Brew_all_data_2017-02-23-081532631.tsv',
#                skip = 13)
df <- read_csv('data/EXCEL_Mapa_de_Faltas_2017-04-12_modified_by_joe.csv',
               skip = 0)

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
# 

# Get locations
geo <-
  data_frame(school = c('3 DE FEV',
                        'J MACHEL',
                        'MAGUDE',
                        'MAGUIGUANA',
                        'MARAGRA',
                        'MOINE',
                        'SIMBE',
                        'XINAVANE',
                        'DUCO'),
             lng = c(32.796798928,
                     32.9282333538,
                     32.6450501,
                     32.6698304014,
                     32.7780356379,
                     32.5305054949,
                     32.5159635997,
                     32.7900299315,
                     32.5416627188),
             lat = c(-25.157889943,
                     -25.0965166999,
                     -25.0259158,
                     -25.0381675415,
                     -25.4528053754,
                     -24.8901998727,
                     -24.8185362371,
                     -25.0441123614,
                     -24.9252875459))

#####################################
# Data quality checks

# Absenteeism: How many turma-years in absenteeism data?
x <- df %>%
  group_by(year, school) %>%
  summarise(turmas = length(unique(turma))) %>%
  arrange(turmas)
write_csv(x, '~/Desktop/absenteeism_number_of_turmas_by_school_year.csv') %>%
  filter(!is.na(year))

# Absenteeism: How many student-years
x <- df %>%
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
             size = 3) 
  # theme(panel.background = element_rect(fill='black', colour='black'))
  # geom_label_repel(data = geo,
  #            aes(x = lng,
  #                y = lat, 
  #                label = geo$school),
  #            size = 2,
  #            alpha = 0.7) +
    
