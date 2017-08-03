# Define function for entry into status
entry <- function(x,
                  force_first = FALSE){
  # Ensure that x is boolean
  if(!is.logical(x)){
    stop('x must be a logical/boolean vector.')
  }
  # Define whether there is a change in status
  change_vec <- abs(as.numeric(x) - as.numeric(lag(x)))
  # Flag days which are both a change
  # and are TRUE (ie, entry into lateness or entry into default)
  entry_days <- which(x & change_vec == 1)
  # Return a boolean of entry into lateness
  return_vec <- rep(FALSE, length(x))
  return_vec[entry_days] <- TRUE
  # Force the first value to be copied if applicable
  if(force_first){
    return_vec[1] <- x[1]
  }
  return(return_vec)
}

# Get the number of students (max) for each turma
# and drop those dates with very few observations
ab <- ab %>%
  group_by(school, turma, month) %>%
  mutate(max_students = length(unique(id))) %>%
  ungroup %>%
  group_by(school, turma, year) %>%
  mutate(max_students_year = max(max_students, na.rm = TRUE)) %>%
  ungroup %>%
  filter(max_students_year >= 8) %>%
  group_by(school, turma, date) %>%
  mutate(n_eligibles = length(unique(id))) %>%
  mutate(drop = n_eligibles < (0.5 * max_students_year)) %>%
  ungroup %>%
  filter(!drop) %>%
  dplyr::select(-drop)

# Drop weekends
ab <- ab %>%
  filter(! dow %in% c('Saturday', 'Sunday'))

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
             threshold = c(Inf, 15, 70))

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
  mutate(flag = maximum_chain_length > threshold)

# Join back to absenteeism database
ab <- 
  ab %>%
  left_join(x %>%
              dplyr::select(school, turma, date, flag))
table(ab$flag)
prop.table(table(ab$flag))
prop.table(table(ab$flag)) * 100

# Remove the invalid chains
ab <- ab %>%
  filter(!flag)

# Absenteeism by month by district
x <- 
  ab %>%
  group_by(district, year_term) %>%
  summarise(presences = length(which(!absent)),
            absences = length(which(absent)),
            eligibles = n()) %>%
  ungroup %>%
  mutate(absenteeism_rate = absences / eligibles * 100) %>%
  filter(!grepl('NA', year_term))
  
ggplot(data = x,
       aes(x = year_term,
           y = absenteeism_rate,
           color = district,
           group = district)) +
  geom_line() +
  geom_point(aes(size = eligibles),
             alpha = 0.6) +
  geom_vline(xintercept = 3.5, color = 'black')

# Drop those with NA turmas, or numbers
out <- expand.grid(consec = 1:150,
                   iter = 1:1000)
out$val <- NA
out$inv <- NA
for (i in 1:150){
  print(i)
  for (j in 1:1000){
    this <- sample(c(TRUE, FALSE), size = i, replace = TRUE, prob = c(0.08, 0.92))
    all_present <- all(!this)
    all_absent <- all(this)
    out$val[out$consec == i &
              out$iter == j] <- all_present
    out$inv[out$consec == i &
              out$iter == j] <- all_absent
  }
}
done <- out %>%
  group_by(consec) %>%
  summarise(p = length(which(val)) / length(val) * 100,
            inv = length(which(inv)) / length(inv) * 100)


plot(done$consec, done$p, type = 'l')
lines(done$consec, done$inv,  col = 'darkorange')
abline(h = seq(0, 5, 1), lty = 1, col = adjustcolor('red', alpha.f = 0.2))
