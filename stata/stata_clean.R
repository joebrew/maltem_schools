# This script is an intermediary script in Laia's cleaning process (in stata).
# It has nothing to do with the R flow in prepare_student_data.R.

library(tidyverse)

# Read the files written in the stata cleaning process
ab <- read_csv('stata/ab_01.csv')
# perf <- read_csv('stata/perf_01.csv')

# Reformat date
ab$date <- as.Date(ab$date, format = '%d%b%Y')

# Overwrite dow
ab$dow <- weekdays(ab$date)

# Remove weekends
ab <- ab %>%
  filter(!dow %in% c('Saturday', 'Sunday'))

# Recode absent to TRUE/FALSE
ab$absent <- as.logical(ab$absent)

# Get the number of students (max) for each turma
# and drop those dates with very few observations
ab <- ab %>%
  group_by(school, turma, month) %>%
  mutate(max_students = length(unique(name))) %>%
  ungroup %>%
  group_by(school, turma, year) %>%
  mutate(max_students_year = max(max_students, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(flag_less_than_8_students_in_turma_this_year = max_students_year < 8) %>%
  group_by(school, turma, date) %>%
  mutate(n_eligibles = length(unique(name))) %>%
  mutate(flag_fewer_than_half_of_eligible_students_were_recorded_today = n_eligibles < (0.5 * max_students_year) & flag_less_than_8_students_in_turma_this_year) %>%
  ungroup 

# Identify those unlikely chains of presences
x <- ab %>%
  arrange(school, turma, date) %>%
  group_by(school, turma, date) %>%
  summarise(presences = length(which(!absent)),
            absences = length(which(absent))) %>%
  ungroup %>%
  mutate(any_absence = absences > 0,
         any_presence = presences > 0) %>%
  mutate(change = any_absence != dplyr::lag(any_absence, n = 1, default = FALSE)) %>%
  group_by(school, turma) %>%
  mutate(chain = cumsum(change) + 1) %>%
  ungroup %>%
  group_by(school, turma, chain) %>%
  mutate(cs_presences = cumsum(presences)) %>%
  mutate(cs_absences = cumsum(absences)) %>%
  ungroup %>%
  mutate(chain_type = ifelse(any_absence & any_presence, 
                             'mix',
                             ifelse(any_absence & !any_presence,
                                    'absence',
                                    ifelse(!any_absence & any_presence,
                                           'presence', NA)))) %>%
  dplyr::select(-change) %>%
  mutate(chain_length = ifelse(chain_type == 'mix', 0,
                               ifelse(chain_type == 'absence',
                                      cs_absences,
                                      ifelse(chain_type == 'presence',
                                             cs_presences, NA))))

# Define thresholds
thresholds <-
  data_frame(chain_type = c('mix', 'absence', 'presence'),
             threshold = c(Inf, 20, 100))

# Join thresholds to turma info
x <- 
  left_join(x,
            thresholds,
            by = 'chain_type')

# Get maximum chain length for each chain
x <- x %>%
  group_by(school, turma, chain) %>%
  mutate(maximum_chain_length = max(chain_length)) %>%
  ungroup

# Flag invalid chains
x <- x %>%
  mutate(flag_too_many_consecutive_absences_or_presences = maximum_chain_length > threshold)

# Join back to absenteeism database
ab <- 
  ab %>%
  left_join(x %>%
              dplyr::select(school, turma, date, flag_too_many_consecutive_absences_or_presences))

# Define a global flag variable
ab$flag <-
  ab$flag_fewer_than_half_of_eligible_students_were_recorded_today |
  ab$flag_less_than_8_students_in_turma_this_year |
  ab$flag_too_many_consecutive_absences_or_presences

# write
write_csv(ab, 'stata/ab_01_flagged.csv')
