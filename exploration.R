# Get school data
source('prepare_student_data.R')

# Attach some packages
library(cism)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(waffle)
library(gganimate)

# # Compare old and new datasets
# old <- read_csv('~/Desktop/performance_elisa.csv')
# old <- old[,names(old) %in% names(performance)]
# performance <- performance[,names(performance) %in% names(old)]
# performance$trimester <- as.numeric(as.character(performance$trimester))
# x = dplyr::setdiff(performance, old)
# y = old %>% 
#   group_by(name, school, year, trimester, turma, subject) %>%
#   summarise(n = length(unique(original_name)),
#             original_names = paste0(sort(unique(original_name)), collapse = ', '),
#             district = first(district)) %>%
#   ungroup %>%
#   arrange(desc(n))
# # z = performance %>% 
# #   group_by(name, school, year, trimester, turma, subject) %>%
# #   summarise(n = n(), #length(unique(original_name)),
# #             original_names = paste0(sort(unique(original_name)), collapse = ', ')) %>%
# #   ungroup %>%
# #   arrange(desc(n))
# y <- y %>% filter(n > 1)
# View(y)
# write_csv(y, '~/Desktop/table_with_duplicates_clarification.csv')

# Regressions
#####################################

# Passed trimester exam
model_data <- performance %>%
  mutate(pass = ifelse(value >= 10, 1, 0),
         interv = ifelse(district == 'Magude', 'intervention', 'control'),
         after = ifelse(year == 2016, 'after', 'before'),
         interv_after = year == 2016 & district == 'Magude') %>%
  mutate(intervention = interv)

#### BINOMIAL ANALYSIS -------------------------------------

# performance <- read_csv('~/Desktop/performance_elisa.csv')
# Regression number 1
# reg pass intervention after interv_after i.school_n, cluster(intervention)
fit <- lm(pass ~ after + intervention + interv_after + school, data = model_data) # missing the cluster intervention stuff
summary(fit)

# Regression number 2
# reg pass intervention after interv_after i.school_n i.subject_n if subject_n!=., cluster(intervention)
fit <- lm(pass ~ after + intervention + interv_after + school + subject, data = model_data %>% filter(!is.na(subject)))
summary(fit)

# Regression number 3
# reg pass intervention after interv_after i.school_n i.subject_n i.trimester if subject_n!=., cluster(intervention)
fit <- lm(pass ~ after + interv + interv_after + school + subject + factor(trimester), data = model_data)
summary(fit)

# Regression number 4
# identical to regression number 3 but only for math
x <- model_data %>% filter(subject == 'Math')
fit <- lm(pass ~ after + intervention + interv_after + school + factor(trimester), data = x)
summary(fit)

### CONTINUOUS VARIABLE ANALYSIS --------------------

# Regression number 1
# reg value intervention after interv_after i.school_n, cluster(intervention)
fit <- lm(value ~ after + intervention + interv_after + school, data = model_data) # missing the cluster intervention stuff
summary(fit)

# Regression number 2
# reg value intervention after interv_after i.school_n i.subject_n if subject_n!=., cluster(intervention)
fit <- lm(value ~ after + intervention + interv_after + school + subject, data = model_data %>% filter(!is.na(subject)))
summary(fit)

# Regression number 3
# reg value intervention after interv_after i.school_n i.subject_n i.trimester if subject_n!=., cluster(intervention)
fit <- lm(value ~ after + interv + interv_after + school + subject + factor(trimester), data = model_data)
summary(fit)

# Regression number 4
# identical to regression number 3 but only for math
x <- model_data %>% filter(subject == 'Math')
fit <- lm(value ~ after + intervention + interv_after + school + factor(trimester), data = x)
summary(fit)

# Final piece = pass examination
time_df <- data_frame(year = rep(c(2015, 2016), each = 3),
                      trimester = as.character(rep(1:3, 2)),
                      time = -3:2)
model_data <- 
  model_data %>%
  left_join(time_df) %>%
  mutate(intervention = ifelse(intervention == 'intervention', 1, 0)) %>%
  mutate(timeinterv = time * intervention) %>%
  mutate(timetreat = factor(timeinterv)) %>%
  mutate(period = factor(time))
fit <- lm(pass ~ intervention + period  + timetreat + school + subject, data = model_data)
summary(fit)

# Just math
fit <- lm(pass ~ intervention + period  + timetreat + school , data = model_data %>% filter(subject == 'Math'))
summary(fit)

#####################################
# Data quality checks

# Laia: for Ilha Josina and Graça Machel schools, give me, next to the table you sent me before, the first and second name of the students
x <- ab %>%
  filter(school %in% c('GRACA MACHEL',
                       'ILHA JOSINA')) %>% 
  dplyr::select(name, school) %>%
  arrange(school, name) %>%
  filter(!duplicated(paste0(name, school))) %>%
  mutate(source = 'absenteeism')
y <- performance %>%
  filter(school %in% c('GRACA MACHEL',
                       'ILHA JOSINA')) %>% 
  dplyr::select(name, school) %>%
  arrange(school, name) %>%
  filter(!duplicated(paste0(name, school))) %>%
  mutate(source = 'performance')
z <- bind_rows(x, y)
write_csv(z, '~/Desktop/names_of_ij_and_gm_students.csv')
# Laia: Report, per each district and school, the name of “turma-class”
# for which we have info, both for absenteeism and performance (so
# separately).
x <- ab %>%
  group_by(year, school, number, letter) %>% 
  summarise(observations_ab = n(),
            name_1_ab = first(name),
            name_2_ab = last(name)) %>%
  ungroup %>%
  full_join(
    performance %>%
      group_by(year, school, number, letter) %>%
      summarise(observations_performance = n(),
                name_1_performance = first(name),
                name_2_performance = last(name)) %>%
      ungroup
  )
write_csv(x, '~/Desktop/laia_info.csv')

# Absenteeism: How many turma-years in absenteeism data?
x <- ab %>%
  group_by(year, school) %>%
  summarise(turmas = length(unique(turma))) %>%
  arrange(turmas)
write_csv(x, '~/Desktop/absenteeism_number_of_turmas_by_school_year.csv') 

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

# For each year, school, how many turmas

x <- ab %>%
  group_by(year, school) %>%
  summarise(turmas = length(unique(turma)))
write_csv(x, '~/Desktop/laia_turmas_per_year_school_absenteeism.csv')

x <- performance %>%
  mutate(turma = paste0(number, '-', letter)) %>%
  group_by(year, school) %>%
  summarise(turmas = length(unique(turma)))
write_csv(x, '~/Desktop/laia_turmas_per_year_school_performance.csv')


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
# 
# # Make table comparing manhica and magude
# x <- students %>%
#   left_join(census %>%
#               filter(!duplicated(perm_id)))
# 
# # Get info on sex, age, etc.
# y <- x %>%
#   mutate(district = factor(district, levels = c('Manhiça', 'Magude'))) %>%
#   group_by(district) %>%
#   mutate(age = (as.numeric(as.Date('2016-01-01') - dob)) / 365.25) %>%
#   summarise(males = length(which(sex == 'M')),
#             females = length(which(sex == 'F')),
#             distance_to_school_mean = mean(km_to_school, na.rm = TRUE),
#             distance_to_school_sd = sd(km_to_school, na.rm = TRUE),
#             siblings_mean = mean(n_children_agregado, na.rm = TRUE),
#             siblings_sd = sd(n_children_agregado, na.rm = TRUE),
#             education_mean = mean(ses_education_household, na.rm = TRUE),
#             education_sd = sd(ses_education_household, na.rm = TRUE),
#             age_mean = mean(age, na.rm = TRUE),
#             age_sd = sd(age, na.rm = TRUE),
#             conditions_score_mean = mean(ses_conditions_score, na.rm = TRUE),
#             conditions_score_sd = sd(ses_conditions_score, na.rm = TRUE),
#             ses_asset_score_mean = mean(ses_asset_score, na.rm = TRUE),
#             ses_asset_score_sd = sd(ses_asset_score, na.rm = TRUE),
#             identified = length(which(!is.na(district_census))),
#             n = n())  %>%
#   mutate(p_identified = identified / n * 100)
# 
# # 
# # Plots by these risk factors
# x <- ab %>%
#   filter(!is.na(year_term)) %>%
#   filter(!grepl('NA', year_term)) %>%
#   left_join(census %>%
#               mutate(census_name = name) %>%
#               dplyr::select(census_name, sex)) %>%
#   group_by(sex, year_term) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   filter(!is.na(sex))
# 
# ggplot(data = x,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            group = sex,
#            color = sex)) +
#   geom_line() +
#   scale_color_manual(name = 'Sex',
#                      values = cols) +
#   theme_cism() +
#   labs(x = 'Year term',
#        y = 'Absetneeism rate',
#        title = 'Absenteeism rate by sex')
# 
# 
# # Number of siblings and absetneeism rate
# x <- ab %>%
#   filter(!is.na(year_term)) %>%
#   filter(!grepl('NA', year_term)) %>%
#   left_join(census %>%
#               mutate(census_name = name) %>%
#               dplyr::select(census_name, n_children_agregado)) %>%
#   group_by(n_children_agregado) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   filter(!is.na(n_children_agregado))
# 
# 
# x <- ab %>%
#   group_by(census_name) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   left_join(census %>%
#               mutate(census_name = name) %>%
#               dplyr::select(census_name, n_children_agregado)) %>%
#   mutate(siblings = ifelse(n_children_agregado == 1, '0',
#                            ifelse(n_children_agregado ==2, '1',
#                                   ifelse(n_children_agregado <5, '2-4',
#                                          ifelse(n_children_agregado >=5, '5+', NA))))) %>%
#   filter(!is.na(siblings))
# 
# avg <- x %>%
#   group_by(siblings) %>%
#   summarise(absenteeism_rate = mean(absenteeism_rate))
# 
# ggplot(data = x,
#        aes(x = siblings,
#            y = absenteeism_rate)) +
#   geom_jitter(alpha = 0.6,
#               color = 'darkgreen') +
#   geom_violin(alpha = 0.3) +
#   geom_point(data = avg,
#              aes(x = siblings, 
#                  y = absenteeism_rate),
#              color = 'red',
#              size = 40,
#              pch = '-') +
#   scale_y_log10() +
#   theme_cism() +
#   labs(x = 'Siblings',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism rate by siblings',
#        subtitle = 'No clear relationship')
# 
# 
# ggplot(data = x,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            group = sex,
#            color = sex)) +
#   geom_line() +
#   scale_color_manual(name = 'Sex',
#                      values = cols) +
#   theme_cism() +
#   labs(x = 'Year term',
#        y = 'Absetneeism rate',
#        title = 'Absenteeism rate by sex')
# 
# # Plot of age and absenteeism
# x <- ab %>%
#   mutate(grade = substr(turma, 1, 1)) %>%
#   group_by(grade) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   filter(grade != 'N')
# 
# ggplot(data = x,
#        aes(x = grade,
#            y = absenteeism_rate)) +
#   geom_bar(stat = 'identity',
#            fill = 'darkgreen',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(x = 'Grade',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism rate by grade')
# 
# 
# # Plot of age and absenteeism
# x <- ab %>%
#   mutate(grade = substr(turma, 1, 1)) %>%
#   group_by(census_name, grade) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   filter(grade != 'N') %>%
#   left_join(census %>%
#               mutate(age = as.numeric((as.Date('2016-01-01') - dob)) / 365.25) %>%
#               mutate(age = round(age)) %>%
#               rename(census_name = name) %>%
#               dplyr::select(age, census_name))
# 
# ggplot(data = x,
#        aes(x = age,
#            y = absenteeism_rate)) +
#   geom_bar(stat = 'identity',
#            fill = 'darkgreen',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(x = 'Age',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism rate by age and grade') +
#   facet_wrap(~grade)
# 
# # Define some colors
# cols <- colorRampPalette(brewer.pal(n = 9,
#                                     name = 'BrBG'))(2)
# 
# # Explore performance by district over time
# x <- performance %>%
#   mutate(year_trimester = paste0(year, '-', trimester)) %>%
#   group_by(year_trimester, district) %>%
#   summarise(performance = mean(value, na.rm = TRUE))
# 
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = performance,
#            group = district,
#            color = district)) +
#   geom_line() +
#   geom_point() +
#   theme_cism() +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   labs(x = 'Year trimester',
#        y = 'Average grade',
#        title = 'School performance',
#        subtitle = 'Over time, by district')
# 
# # Explore household conditions
# ggplot(data = census,
#        aes(x = ses_conditions_score,
#            group = district,
#            fill = district)) +
#   geom_density(n = 20,
#                alpha = 0.4) +
#   scale_fill_manual(name = 'District',
#                     values = cols) +
#   theme_cism() +
#   labs(x = 'SES conditions score',
#        y = 'Density',
#        title = 'Household conditions',
#        subtitle = 'Magude vs. Manhiça')
# 
# # Visualize assets
# ggplot(data = census,
#        aes(x = ses_asset_score,
#            group = district,
#            fill = district)) +
#   geom_density(n = 40,
#                alpha = 0.4) +
#   scale_fill_manual(name = 'District',
#                     values = cols) +
#   theme_cism() +
#   labs(x = 'SES assets score',
#        y = 'Density',
#        title = 'Household assets',
#        subtitle = 'Magude vs. Manhiça') +
#   scale_x_sqrt() +
#   xlim(0, 15000)
# 
# # Explore EDUCATION
# ggplot(data = census,
#        aes(x = ses_education_household,
#            group = district,
#            fill = district)) +
#   geom_density(n = 6,
#                alpha = 0.4) +
#   scale_fill_manual(name = 'District',
#                     values = cols) +
#   theme_cism() +
#   labs(x = 'SES education score',
#        y = 'Density',
#        title = 'Household education (highest in household)',
#        subtitle = 'Magude vs. Manhiça')
# 
# ggplot(data = census,
#        aes(x = district,
#            y = ses_education_household,
#            fill = district)) +
#   geom_violin(adjust = 4,
#               alpha = 0.4) +
#   # geom_jitter(aes(color = district),
#   #             alpha = 0.2) +
#   scale_fill_manual(name = 'District',
#                     values = cols) +
#   theme_cism() +
#   labs(x = 'District',
#        y = 'SES education score',
#        title = 'Household education (highest in household)',
#        subtitle = 'Magude vs. Manhiça')
# 
# # Occupation
# ggplot(data = census,
#        aes(x = district,
#            y = ses_occupation_household,
#            fill = district)) +
#   geom_violin(adjust = 10,
#               alpha = 0.4) +
#   # geom_jitter(aes(color = district),
#   #             alpha = 0.2) +
#   scale_fill_manual(name = 'District',
#                     values = cols) +
#   theme_cism() +
#   labs(x = 'District',
#        y = 'SES occupation score',
#        title = 'Household occupation (highest in household)',
#        subtitle = 'Magude vs. Manhiça')
# 
# # Relationship between ses and absetneeism
# x <- ab %>%
#   group_by(census_name,
#            year_term) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   left_join(census %>%
#               rename(census_name = name) %>%
#               dplyr::select(census_name,
#                             ses_conditions_score)) %>%
#   mutate(ses_conditions_bi = ifelse(ses_conditions_score >
#                                       median(ses_conditions_score, na.rm = TRUE), 'high', 'low')) %>%
#   group_by(ses_conditions_bi, year_term) %>%
#   mutate(absenteeism_rate = sum(absences) / sum(eligibles) * 100) %>% filter(!is.na(ses_conditions_bi),
#                                                                              !grepl('NA', year_term))   %>%
#   ungroup %>%
#   mutate(ses_conditions_bi = factor(ses_conditions_bi,
#                                     levels = c('low', 'high')))
# 
# 
# ggplot(data = x,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            color = ses_conditions_bi,
#            group = ses_conditions_bi)) +
#   geom_line(alpha = 0.6, size = 1.5) +
#   scale_color_manual(name = 'SES',
#                      values = cols) +
#   theme_cism() +
#   labs(x = 'Year-term',
#        y = 'Absenteeism rate',
#        title = 'SES (based on assets) and absenteeism',
#        subtitle = 'Manhiça and Magude, all census-identified students')
# 
# 
# 
# # Make a year-trimester variable
# performance$year_trimester <-
#   paste0(performance$year, '-', performance$trimester)
# 
# # How many students do we have data for both years
# x <- performance %>%
#   group_by(name, year) %>%
#   tally %>% 
#   group_by(name) %>%
#   summarise(both = length(unique(year)) > 1)
# 
# # Performance by year_trimester by school
# x <- performance %>%
#   group_by(year_trimester, district, school) %>%
#   summarise(value = mean(value, na.rm = TRUE)) %>%
#   ungroup
# cols <- colorRampPalette(brewer.pal(n = 9,
#                                     name = 'Spectral'))(length(unique(x$school)))
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = value,
#            color = school,
#            group = school,
#            lty = district)) +
#   # geom_point() +
#   geom_line(size = 1,
#             alpha = 0.8) +
#   theme_cism() +
#   labs(x = 'Year-trimester',
#        y = 'Average grade',
#        title = 'School performance',
#        subtitle = 'Over time, by school') +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(name = 'School',
#                      values = cols)
# 
# 
# # Performance = year_term on x, grade on y,
# # facet by sujbect, color by district
# x <- performance %>%
#   group_by(year_trimester, district, subject) %>%
#   summarise(value = mean(value, na.rm = TRUE),
#             obs = n()) %>%
#   ungroup %>%
#   filter(!is.na(subject),
#          obs > 10)
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = value,
#            color = district,
#            group = district)) +
#   facet_wrap(~subject) +
#   geom_line(alpha = 0.6) +
#   theme_cism() +
#   labs(x = 'Year-trimester',
#        y = 'Average grade',
#        title = 'School performance',
#        subtitle = 'Over time, by school/subject') +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_color_manual(name = 'District',
#                      values = c('darkred', 'darkgreen'))
# 
# 
# 
# 
# 
# # Performance
# x <- performance %>%
#   group_by(name, year_trimester) %>%
#   summarise(value = mean(value, na.rm = TRUE))
# 
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = value,
#            group = name)) +
#   geom_line(alpha = 0.1) +
#   ylim(0, 20)
# 
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = value)) +
#   geom_jitter(alpha = 0.3) +
#   geom_violin(alpha = 0.5)
# 
# # Performance by district by trimester
# x <- performance %>%
#   group_by(year_trimester, school, district) %>%
#   summarise(value = mean(value, na.rm = TRUE)) %>%
#   ungroup
# ggplot(data = x,
#        aes(x = year_trimester,
#            y = value,
#            group = school,
#            lty = district, 
#            color = school)) +
#   geom_line() +
#   theme_cism()
# 
# library(gganimate)
# x <- performance %>%
#   mutate(trimester = factor(year_trimester))
# g <- ggplot(data = x,
#             aes(x = value,
#                 # frame = trimester,
#                 group = district,
#                 fill = district)) +
#   geom_density(alpha = 0.3, n = 20) +
#   facet_wrap(~year_trimester) +
#   scale_fill_manual(name = 'District',
#                     values = c('darkred', 'darkgreen')) +
#   theme_cism()
# g
# gganimate(g)
# 
# ggplot(data = x %>% mutate(year = factor(year)),
#        aes(x = value,
#            # frame = trimester,
#            group = year,
#            fill = year)) +
#   geom_density(alpha = 0.7, n = 20, color = 'black') +
#   facet_wrap(~district) +
#   theme_cism() +
#   labs(x = 'Grade (1-20)',
#        y = 'Density',
#        title = 'School performance') +
#   scale_fill_manual(name = 'Year',
#                     values = c('black', 'white'))
# 
# # DISTANCE TO SCHOOL AND ABSENTEEISM
# x <- 
#   ab %>%
#   group_by(census_name) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   left_join(students %>%
#               dplyr::select(census_name,
#                             km_to_health_facility,
#                             km_to_school,
#                             km_to_other_district_student)) %>%
#   filter(!is.na(km_to_school)) %>%
#   mutate(distance_to_school = 
#            ifelse(km_to_school < 1, '0-1',
#                   ifelse(km_to_school < 2, '1-2',
#                          ifelse(km_to_school < 7, '2-7', 
#                                 '7+'))))
# 
# ggplot(data = x,
#        aes(x = distance_to_school,
#            y = absenteeism_rate)) +
#   geom_jitter(alpha = 0.2, color = 'darkorange') +
#   geom_boxplot(alpha = 0.2,
#                fill = 'darkgreen') +
#   # scale_x_log10() +
#   scale_y_log10() +
#   labs(x = 'Kilometers to school',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism and distance to school',
#        subtitle = 'Closer = better') +
#   theme_cism()
# 
# # DISTANCE TO INTERVENTION AREA AND ABSENTEEISM
# x <- ab %>%
#   filter(district == 'Manhiça') %>%
#   group_by(year, census_name) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   left_join(students %>%
#               dplyr::select(census_name,
#                             km_to_other_district_student) %>%
#               filter(!duplicated(census_name))) %>%
#   filter(!is.na(km_to_other_district_student)) %>%
#   mutate(distance_to_other_district = 
#            ifelse(km_to_other_district_student < 10,
#                   '0-10',
#                   ifelse(km_to_other_district_student < 20,
#                          '10-20',
#                          '20+'))) %>%
#   group_by(distance_to_other_district,
#            year) %>%
#   summarise(absenteeism_rate = sum(absences) / sum(eligibles) * 100) %>%
#   ungroup %>%
#   filter(!grepl('NA', year)) %>%
#   arrange(year) %>%
#   group_by(distance_to_other_district) %>%
#   mutate(relative_absenteeism_rate = absenteeism_rate /
#            first(absenteeism_rate) * 100) %>%
#   ungroup 
# 
# ggplot(data = x,
#        aes(x = factor(year),
#            y = relative_absenteeism_rate,
#            group = distance_to_other_district,
#            color = distance_to_other_district)) +
#   geom_line() +
#   theme_cism()
# 
# 
# # Waffle of number of student days
# x <- rep(1, 1)
# waffle(x, colors = c('darkgreen'),
#        legend_pos = 'none',
#        rows = 1,
#        size = 1) +
#   theme_cism_map() +
#   theme(legend.position = 'none')
# 
# the_colors <- sample(c('darkgreen', 'darkorange'), 
#                      length(x), 
#                      replace = TRUE, prob = c(0.8, 0.2))
# x <- rep(1, 1000)
# waffle(x, colors = the_colors,
#        legend_pos = 'none',
#        rows = 10,
#        size = 0.1) +
#   theme_cism_map() +
#   theme(legend.position = 'none')
# 
# x <- expand.grid(x = 1:100,
#                  y = 1:100)
# x$status <- base::sample(c('Present',
#                            'Absent'),
#                          replace = TRUE,
#                          size = nrow(x),
#                          prob = c(0.9, 0.1))
# ggplot(data = x,
#        aes(x = y,
#            y = x,
#            color = status)) +
#   geom_point(size = 0.4) +
#   scale_color_manual(name = '',
#                      values = c('darkorange', 'darkgreen')) +
#   theme_cism()
# 
# 
# # Graph on monthly average absenteeism rate, by district and across time. Moreover, write down the average absenteeism rate by district, for 4 or 5 different points (months) at time in the graph, to see the differences in numbers
# monthly_by_student <-
#   ab %>%
#   group_by(name, district, school, month) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup %>%
#   arrange(name, month) %>%
#   mutate(x = 1) %>%
#   mutate(step = cumsum(x)) %>%
#   ungroup
# # monthly_by_student <- monthly_by_student[1:100,]
# ggplot(data = monthly_by_student,
#        aes(x = month,
#            y = absenteeism_rate,
#            group = name,
#            frame = step)) +
#   geom_line(alpha = 0.1) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One student = one line')
# 
# # Monthly by turma
# monthly_by_turma <-
#   ab %>%
#   group_by(district, turma, month) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup
# 
# ggplot(data = monthly_by_turma,
#        aes(x = month,
#            y = absenteeism_rate,
#            group = turma)) +
#   geom_line(alpha = 0.5) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One class = one line') +
#   ylim(0, 40)
# 
# ggplot(data = monthly_by_turma,
#        aes(x = month,
#            y = absenteeism_rate,
#            group = turma,
#            color = district)) +
#   geom_line(alpha = 0.5) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One class = one line') +
#   ylim(0, 40) +
#   scale_color_manual(name = 'District',
#                      values = cols)
# 
# # Monthly by school
# monthly_by_school <-
#   ab %>%
#   group_by(district, school, month) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup
# 
# ggplot(data = monthly_by_school,
#        aes(x = month,
#            y = absenteeism_rate,
#            group = school,
#            color = district)) +
#   geom_line(alpha = 0.8,
#             size = 1.5) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One school = one line') +
#   scale_color_manual(name = 'District',
#                      values = cols)
# 
# # Monthly by district
# monthly_by_district <-
#   ab %>%
#   group_by(district, month) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup
# 
# ggplot(data = monthly_by_district,
#        aes(x = month,
#            y = absenteeism_rate,
#            group = district,
#            color = district)) +
#   geom_line(alpha = 0.8,
#             size = 2) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One district = one line') +
#   scale_color_manual(name = 'District',
#                      values = cols)
# 
# # year term by district
# year_term_by_district <-
#   ab %>%
#   group_by(year_term, district) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup %>%
#   filter(!is.na(year_term)) %>%
#   filter(!grepl('NA', year_term))
# 
# ggplot(data = year_term_by_district,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            group = district,
#            color = district)) +
#   geom_line(alpha = 0.8,
#             size = 2) +
#   theme_cism() +
#   labs(x = 'Year - term',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One district = one line') +
#   scale_color_manual(name = 'District',
#                      values = cols)
# 
# 
# year_by_district <-
#   ab %>%
#   group_by(year, district) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n()) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup 
# 
# library(ggrepel)
# ggplot(data = year_by_district,
#        aes(x = factor(year),
#            y = absenteeism_rate,
#            group = district,
#            color = district)) +
#   geom_line(alpha = 0.8,
#             size = 2) +
#   geom_point(size = 5) +
#   theme_cism() +
#   labs(x = 'Year - term',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One district = one line') +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   geom_hline(yintercept = year_by_district %>% 
#                filter(year == min(year)) %>% 
#                .$absenteeism_rate,
#              lty = 2,
#              alpha = 0.6) +
#   geom_text_repel(aes(label = paste0(round(absenteeism_rate, digits = 2), '%')),
#                   point.padding = unit(1, "lines")) +
#   ylim(0, 8)
# 
# 
# # Standardized
# year_by_district <-
#   year_by_district %>%
#   arrange(year) %>%
#   group_by(district) %>%
#   mutate(p = absenteeism_rate / first(absenteeism_rate) * 100)
# 
# ggplot(data = year_by_district,
#        aes(x = factor(year),
#            y = p,
#            group = district,
#            color = district)) +
#   geom_line(alpha = 0.8,
#             size = 2) +
#   geom_point(size = 5) +
#   theme_cism() +
#   labs(x = 'Year - term',
#        y = 'Absenteeism rate',
#        title = 'Montly absenteeism rate',
#        subtitle = 'One district = one line') +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   geom_hline(yintercept = 100,
#              lty = 2,
#              alpha = 0.6) +
#   geom_text_repel(aes(label = paste0(round(p, digits = 2), '%')),
#                   point.padding = unit(1, "lines")) 
# 
# 
# # Down to 1
# final <- year_by_district %>%
#   ungroup %>%
#   filter(year == 2016) %>%
#   summarise(diff = abs(first(p) - last(p))) %>%
#   .$diff
# 
# # gganimate(g,
# #           filename = '~/Desktop/lines.gif',
# #           title_frame = FALSE, 
# #           ani.width = 800,
# #           ani.height = 500,
# #           interval = 0.05,
# #           cumulative = TRUE)
# 
# 
# 
# Map of intervention area

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
               color = 'white',
               # color = 'grey',
               alpha = 0.8) +
  theme_map() +
  scale_fill_manual(name = '',
                    values = cols) +
  # geom_point(data = geo,
  #            aes(x = lng,
  #                y = lat),
  #            color = 'black',
  #            alpha = 0.7,
  #            size = 3) +
  # theme_black(base_size = 0) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = 'none') +
  theme_cism() %+replace%
  theme(#axis.line = element_blank(), axis.text = element_blank(),
    #axis.ticks = element_blank(), axis.title = element_blank(),
    # panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0,
                         "lines"),
    legend.justification = c(0, 0),
    legend.position = c(0,
                        0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

x <- moz1_fortified
x$color <- x$id %in% c('Maputo', 'Gaza')

cols <- c('black', 'darkred')
g1 <- ggplot() +
  geom_polygon(data = x,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = color),
               color = 'white',
               # color = 'grey',
               alpha = 0.8,
               size = 0.2) +
  theme_map() +
  scale_fill_manual(name = '',
                    values = cols) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = 'none') +
  coord_map()
g1

y <- 
  moz2[moz2@data$NAME_1 %in% c('Maputo', 'Gaza'),]
y <- broom::tidy(y, region = 'NAME_2')
a <- moz2@data$NAME_2[moz2@data$NAME_1 %in% c('Maputo', 'Gaza')]
y <- y %>% filter(id %in% a)
y$color <- ifelse(y$id == 'Magude', 'darkorange', 'darkred')
cols <- c('darkorange', 'darkred')
g2 <- ggplot() +
  geom_polygon(data = y,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = color),
               color = 'white',
               # color = 'grey',
               alpha = 0.8,
               size = 0.2) +
  theme_map() +
  scale_fill_manual(name = '',
                    values = cols) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = 'none') +
  coord_map()
g2
Rmisc::multiplot(g1 +
                   labs(title = 'Mozambique',
                        subtitle = 'Gaza and Maputo provinces in red'), 
                 g2 +
                   labs(title = 'Gaza and Maputo provinces',
                        subtitle = 'Magude district in orange'), cols = 2)
# 
# 
# # Animated map with schools located, and the dots of each school location that will be blue (if school average absenteeism rate for that month is less than 6%) and red (if more than 6% for that month), to compare, in a map, school by school across time…. (we will see more dots in red in manhiça across time and less in magude…)
# x <- ab %>%
#   group_by(school, date) %>%
#   summarise(absences = length(which(absent)),
#             presences = length(which(!absent)),
#             eligible_days = n(),
#             Students = length(unique(name))) %>%
#   mutate(absenteeism_rate = absences / eligible_days * 100) %>%
#   ungroup %>%
#   mutate(high_absenteeism = ifelse(absenteeism_rate >= 6, 'High', 'Low')) %>%
#   left_join(geo %>%
#               mutate(lng = jitter(lng),
#                      lat = jitter(lat))) #%>%
# # filter(date >= '2015-03-01',
# #        date <= '2015-06-01')
# 
# g <- ggplot() +
#   geom_polygon(data = map,
#                aes(x = long,
#                    y = lat,
#                    group = id,
#                    fill = id),
#                color = 'white',
#                alpha = 0.8) +
#   theme_map() +
#   scale_fill_manual(name = '',
#                     values = cols) +
#   geom_point(data = x,
#              aes(x = lng,
#                  y = lat,
#                  size = Students,
#                  color = high_absenteeism,
#                  frame = date),
#              alpha = 0.7) +
#   scale_color_manual(name = 'Absenteeism',
#                      values = c('darkred', 'darkgreen')) +
#   # theme_black(base_size = 0) +
#   theme(axis.line = element_blank(), axis.text = element_blank(), 
#         axis.ticks = element_blank(), axis.title = element_blank(),
#         legend.position = 'none') +
#   theme_cism_map()
# 
# gganimate(g,
#           filename = '~/Desktop/map.gif',
#           ani.width = 800,
#           ani.height = 500,
#           interval = 0.2,
#           cumulative = FALSE,
#           title_frame = TRUE)
# 
# # Define function which takes a base point
# # And another point and moves towards the base point
# go_to_school <- function(x = 32.65,
#                          y = -25.025,
#                          base_point = c(32.67,
#                                         -25.035),
#                          n = 25,
#                          add_jitter = 0,
#                          absent = FALSE,
#                          add_school_time = TRUE,
#                          add_home_time = TRUE,
#                          home_time = 1,
#                          school_time = 1){
#   
#   this_point <- c(x,y)
#   if(absent){
#     x_line <- rep(this_point[1],
#                   length = n)
#     y_line <- rep(this_point[2],
#                   length = n)
#   } else {
#     x_line <- seq(this_point[1],
#                   base_point[1],
#                   length = n)
#     y_line <- seq(this_point[2],
#                   base_point[2],
#                   length = n)
#   }
#   
#   if(add_school_time){
#     adder <- round(school_time * n)
#     x_line <- c(x_line,
#                 rep(x_line[length(x_line)], adder))
#     y_line <- c(y_line,
#                 rep(y_line[length(y_line)], adder))
#   }
#   if(add_home_time){
#     adder <- round(home_time * n)
#     x_line <- c(rep(x_line[1], adder),
#                 x_line)
#     y_line <- c(rep(y_line[1], adder),
#                 y_line)
#   }
#   
#   if(!absent){
#     # Jitter
#     almost <- 1:(length(x_line)- 1)
#     x_line[almost] <- jitter(x_line[almost], factor = add_jitter)
#     y_line[almost] <- jitter(y_line[almost], factor = add_jitter)
#     
#   }
#   
#   
#   # create a dataframe
#   out <- data_frame(#id = id[i],
#     x = x_line,
#     y = y_line,
#     step = 1:length(x_line))
#   return(out)
# }
# 
# # Define function for going to school animation
# go_to_school_animation <- function(absence_df,
#                                    max_km_to_school = 10,
#                                    j = 0,
#                                    home_time = 1,
#                                    school_time = 1,
#                                    point_size = 1,
#                                    school_size = 15){
#   
#   # Get days and schools
#   schools <- sort(unique(absence_df$school))
#   dates <- sort(unique(absence_df$date))
#   
#   # for(s in 1:length(schools)){
#   results_list <- list()
#   counter <- 0
#   for(d in 1:length(dates)){
#     # Combine the absence_df with some info
#     # from the census
#     z <- absence_df %>%
#       # filter(school == schools[s]) %>%
#       filter(date == dates[d]) %>%
#       left_join(students %>%
#                   dplyr::select(name,
#                                 in_census,
#                                 km_to_school)) %>%
#       filter(!is.na(km_to_school)) %>%
#       filter(km_to_school <= max_km_to_school) %>%
#       left_join(geo) 
#     
#     if(nrow(z) > 0){
#       # Apply the go_to_school function
#       for (i in 1:nrow(z)){
#         counter <- counter + 1
#         to_school <- go_to_school(base_point = c(z$lng[i], z$lat[i]),
#                                   x = z$longitude[i],
#                                   y = z$latitude[i],
#                                   absent = z$absent[i],
#                                   add_jitter = j,
#                                   add_home_time = TRUE,
#                                   add_school_time = TRUE,
#                                   school_time = school_time,
#                                   home_time = home_time) %>%
#           mutate(absent = z$absent[i],
#                  student = i,
#                  date = dates[i]) %>%
#           mutate(number = 1:n())
#         # Also add the way back home
#         back_home <- to_school %>% arrange(desc(number))
#         done <- bind_rows(to_school, back_home)
#         done <- done %>%
#           mutate(step = 1:n())
#         results_list[[counter]] <- done
#       }
#     }
#   }
#   
#   results <- bind_rows(results_list)
#   
#   # Get schools
#   the_schools <- z %>%
#     filter(!duplicated(school))
#   
#   library(gganimate)
#   g <- ggplot() +
#     geom_point(data = the_schools,
#                aes(x = lng,
#                    y = lat),
#                size = school_size, 
#                alpha = 0.3,
#                color = 'blue') +
#     geom_point(data = results,
#                aes(group = student,
#                    x = x,
#                    y = y,
#                    frame = step,
#                    cumulative = FALSE),
#                color = ifelse(results$absent,
#                               'red',
#                               'black'),
#                alpha = 0.6,
#                size = point_size) +
#     geom_line(data = results,
#               alpha = 0.15,
#               aes(group = student,
#                   x = x,
#                   y = y,
#                   frame = step,
#                   cumulative = TRUE),
#               color = ifelse(results$absent,
#                              'red',
#                              'black')) +
#     theme_cism_map() 
#   gganimate(g,
#             filename = '~/Desktop/students.gif',
#             title_frame = FALSE, 
#             ani.width = 800,
#             ani.height = 500,
#             interval = 0.05)
# } 
# 
# 
# 
# a <- ab %>%
#   filter(!is.na(absent)) %>%
#   # filter(!absent) %>%
#   filter(date == '2016-03-09' &
#            date <= '2016-03-12',
#          # school == 'MAGUIGUANA',
#          !is.na(longitude)) 
# go_to_school_animation(absence_df = a,
#                        max_km_to_school = 10,
#                        j = 10,
#                        home_time = 0.5,
#                        school_time = 0.25,
#                        point_size = 0.2,
#                        school_size = 4)
# 
# 
# b <- ab %>%
#   filter(!is.na(absent)) %>%
#   # filter(absent) %>%
#   filter(date == '2016-03-10', #&
#          # date <= '2016-03-12',
#          school == 'SIMBE',
#          !is.na(longitude)) %>%
#   # sample_n(100) %>%
#   mutate(dummy = 1) %>%
#   group_by(absent) %>%
#   mutate(x = cumsum(dummy)) %>%
#   filter(x == 1)
# go_to_school_animation(absence_df = b,
#                        max_km_to_school = 100,
#                        j = 2,
#                        home_time = 0.5,
#                        school_time = 0.5)

# # Students per school
# x <- ab %>%
#   group_by(school,
#            year) %>%
#   summarise(n_students = length(unique(`Study Subject ID`))) %>%
#   arrange(desc(n_students))
# 
# ggplot(data = x,
#        aes(x = school,
#            y = n_students)) +
#   geom_bar(stat = 'identity',
#            alpha = 0.6,
#            fill = 'darkblue') +
#   facet_wrap(~year) +
#   theme_cism() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   geom_label(aes(label = n_students), size = 3)
# 
# prop.table(table(unique(ab$nome_E1_C1[ab$year == 2015]) %in% unique(ab$nome_E1_C1[ab$year == 2016]))) * 100
# 
# # Absenteeism by date by intervention
# x <- ab %>%
#   group_by(district, date) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# library(RColorBrewer)
# cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(x$district)))
# ggplot(data = x,
#        aes(x = date,
#            y = absenteeism_rate,
#            group = district,
#            color = district)) +
#   geom_point(alpha = 0.3,
#              aes(size = eligibles)) +
#   geom_smooth() +
#   theme_cism() +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   ylim(0, 20)
# 
# 
# # Number of eligible days by day and district
# x <- ab %>%
#   group_by(district, date) %>%
#   tally %>%
#   filter(!is.na(date))
# cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(length(unique(x$district)))
# ggplot(data = x,
#        aes(x = date,
#            y = n,
#            group = district,
#            color = district)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth() +
#   theme_cism() +
#   scale_color_manual(name = 'District',
#                      values = cols)
# 
# 
# 
# # Monthly absenteeism in both districts
# x <- ab %>%
#   group_by(district,
#            date = month) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# ggplot(data = x,
#        aes(x = date,
#            y = absenteeism_rate,
#            group = district,
#            fill = district)) +
#   geom_bar(stat = 'identity',
#            pos = 'dodge',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(title = 'Absenteeism rate') +
#   scale_fill_manual(name = 'District',
#                     values = cols)
# 
# 
# # Monthly absenteeism
# x <- ab %>%
#   group_by(district, date = month) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n(),
#             `Sample size` = length(unique(`Study Subject ID`))) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# ggplot(data = x,
#        aes(x = date,
#            y = absenteeism_rate)) +
#   geom_bar(stat = 'identity',
#            fill = 'darkorange',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(title = 'Absenteeism by month') +
#   facet_wrap(~district, ncol = 2) +
#   geom_hline(yintercept = mean(x$absenteeism_rate), lty = 2, alpha = 0.5) +
#   labs(x = 'Date',
#        y = 'Absenteeism rate (%)')
# 
# # Observations instead of absences
# ggplot(data = x,
#        aes(x = date,
#            y = eligibles)) +
#   geom_bar(stat = 'identity',
#            fill = 'darkorange',
#            alpha = 0.6) +
#   theme_cism() +
#   labs(title = 'Absenteeism by month') +
#   facet_wrap(~district, ncol = 2) +
#   geom_hline(yintercept = mean(x$eligibles), lty = 2, alpha = 0.5)
# 
# 
# # Lines instead of bars
# g <- ggplot(data = x,
#             aes(x = date,
#                 y = absenteeism_rate,
#                 color = district)) +
#   geom_line(alpha = 0.5) +
#   stat_smooth(aes(weight = `Sample size`), fill = NA) +
#   # geom_point()
#   geom_point(aes(size = `Sample size`),
#              alpha = 0.3) +
#   theme_cism() +
#   labs(title = 'Absenteeism by month') +
#   geom_hline(yintercept = mean(x$absenteeism_rate), lty = 2, alpha = 0.5) +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   xlab('Date') +
#   ylab('Monthly absenteeism rate') +
#   geom_vline(xintercept = '2016-10-01')
# g
# 
# # By school
# x <- ab %>%
#   group_by(district, date = month, school) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n(),
#             `Sample size` = length(unique(`Study Subject ID`))) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# ggplot(data = x,
#        aes(x = date,
#            y = absenteeism_rate,
#            color = district)) +
#   geom_point(aes(size = `Sample size`),
#              alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   facet_wrap(~school) +
#   scale_color_manual(name = 'District',
#                      values = c('darkred', 
#                                 'green')) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism rate by school')
# 
# 
# 
# # Examine by term
# x <- ab %>%
#   group_by(district, year, term) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n(),
#             `Sample size` = length(unique(`Study Subject ID`))) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)  %>%
#   filter(!is.na(term)) %>%
#   mutate(year_term = paste0(year, '-', term))
# 
# ggplot(data = x,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            fill = district,
#            group = district)) +
#   geom_bar(stat= 'identity', pos = 'dodge')
# 
# ggplot(data = x,
#        aes(x = year_term,
#            y = absenteeism_rate,
#            color = district,
#            group = district)) +
#   geom_point() +
#   geom_line(alpha = 0.3)  +
#   geom_vline(xintercept = 2.5, lty = 2, alpha = 0.6) +
#   theme_cism() +
#   scale_color_manual(name = 'District',
#                      values = c('darkred', 'green')) +
#   labs(x = 'Year-Trimester',
#        y = 'Absenteeism rate',
#        title = 'Absenteeism rate by district',
#        subtitle = 'Magude (intervention) vs. Manhiça (control)')
# 
# # Aggregate by year
# x <- ab %>%
#   group_by(date = year,
#            district) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n()) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# 
# ggplot(data = x,
#        aes(x = factor(date),
#            y = absenteeism_rate,
#            color = district,
#            group = district)) +
#   geom_line() +
#   geom_point() +
#   theme_cism() +
#   scale_color_manual(name = 'District',
#                      values = cols) +
#   ylim(0, max(x$absenteeism_rate)) +
#   xlab('Date') +
#   ylab('Absenteeism rate')
# 
# # Overlapped years
# x <- ab %>%
#   mutate(day_number = as.numeric(format(date, '%j'))) %>%
#   mutate(date = as.Date('2000-12-31') + day_number) %>%
#   mutate(month = as.Date(paste0('2000-',
#                                 format(date, '%m'),
#                                 '-01'))) %>%
#   group_by(year,
#            month,
#            district) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n(),
#             sample = length(unique(`Study Subject ID`))) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100)
# 
# ggplot(data = x,
#        aes(x = month,
#            y = absenteeism_rate,
#            color = factor(year))) +
#   geom_line(alpha = 0.6) +
#   geom_smooth() +
#   facet_wrap(~district) +
#   geom_point(aes(size = sample),
#              alpha = 0.5) +
#   scale_x_date(date_labels = "%B") +
#   scale_color_manual(name = 'Year',
#                      values = c('darkblue', 'darkred')) +
#   theme_cism() +
#   xlab('Month')
# 
# model_data <- ab
# model_data$intervention <- ab$year == 2016
# 
# model_data$district <- factor(model_data$district,
#                               levels = c('Manhiça',
#                                          'Magude'))
# 
# # Make linear regression
# linear_data <-
#   ab %>%
#   group_by(year, month, month_number = factor(month_number), district, school) 
# 
# 
# fit <- lm(log_ar ~ district*intervention +
#             school +
#             month_number,
#           data = linear_data)
# summary(fit)
# exp(coef(fit))
# exp(confint(fit))
# 
# # Do person-level monthly rate
# monthly_person <-
#   ab %>%
#   group_by(year, month, `Study Subject ID`) %>%
#   summarise(absences = length(which(absent)),
#             eligibles = n(),
#             `Sample size` = length(unique(`Study Subject ID`))) %>%
#   ungroup %>%
#   mutate(absenteeism_rate = absences / eligibles * 100) %>%
#   mutate(intervention = year >= 2016) %>%
#   # mutate(district = factor(district,
#   #                          levels = c('Manhiça',
#   #                                     'Magude'))) %>%
#   mutate(log_ar = log(absenteeism_rate)) %>%
#   filter(is.finite(log_ar))
# 
# ggplot(linear_data,
#        aes(x = absenteeism_rate)) +
#   geom_density(fill = 'darkorange', alpha = 0.6) +
#   theme_cism()
# 
# cism_plot(linear_data$absenteeism_rate)
# cism_plot(log(linear_data$absenteeism_rate))
# # 
# # Make logistic regression
# fit <- glm(absent ~ district*intervention,
#            family = binomial('logit'),
#            data = model_data)
# summary(fit)
# 
# exp(coef(fit))
# exp(confint(fit))
# 
# fake_data <- expand.grid(district = c('Manhiça', 'Magude'),
#                          intervention = c(TRUE, FALSE))
# fake_data$predicted <- predict(fit, fake_data, type = 'response')
# 
# ggplot(data = fake_data,
#        aes(x = intervention,
#            y = predicted,
#            group = district,
#            color = district)) +
#   geom_line()
# 
# dd <- fake_data %>%
#   group_by(district) %>%
#   summarise(difference = first(predicted) - last(predicted))
# (dd$difference[1] - dd$difference[2]) * 100
# 
# # Joe starts working paper outline (National Bureau of Economic Research)
# # Joe needs to get census data merged
# # Joe needs to use fuzzy matching to try to identify 2015-2016 pairs
# # Joe will combine all charts into one document
# # Elisa needs to setup a meeting with Judit to discuss (a) in our final regression
# # whether we should use daily, individual level data (0, 1, 0, 1) or monthly
# # # absenteeism rate (0.25, 0.33, 0.45, etc.)
# # # Elisa: Ask Judit about the issue of entries and exits to cohort
# # # Everyone looks at AEJ (Applied Economics) to see format, etc.; backup = Journal
# # # of Health Economics
# 
# # Map of intervention area
# library(cism)
# library(ggthemes)
# library(ggrepel)
# moz2_fortified <- moz2_fortified
# map <- moz2_fortified %>%
#   filter(id %in% c('Manhiça',
#                    'Magude'))
# 
# ggplot() +
#   geom_polygon(data = map,
#                aes(x = long,
#                    y = lat,
#                    group = id,
#                    fill = id),
#                # color = 'grey',
#                alpha = 0.9) +
#   theme_map() +
#   scale_fill_manual(name = '',
#                     values = c('red',
#                                'green')) +
#   geom_point(data = geo,
#              aes(x = lng,
#                  y = lat),
#              color = 'black',
#              alpha = 0.7,
#              size = 3) +
#   # theme_black(base_size = 0) +
#   theme(axis.line = element_blank(), axis.text = element_blank(), 
#         axis.ticks = element_blank(), axis.title = element_blank(),
#         legend.position = 'none') +
#   theme_cism() %+replace% 
#   theme(#axis.line = element_blank(), axis.text = element_blank(), 
#     #axis.ticks = element_blank(), axis.title = element_blank(), 
#     # panel.background = element_blank(), 
#     panel.border = element_blank(),
#     panel.grid = element_blank(), 
#     panel.spacing = unit(0,
#                          "lines"), 
#     legend.justification = c(0, 0), 
#     legend.position = c(0, 
#                         0),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank())
# # geom_point(data = people,
# #            alpha = 0.2,
# #            aes(x = longitude,
# #                y = latitude))
# # theme(panel.background = element_rect(fill='black', colour='black'))
# # geom_label_repel(data = geo,
# #            aes(x = lng,
# #                y = lat, 
# #                label = geo$school),
# #            size = 2,
# #            alpha = 0.7) +



