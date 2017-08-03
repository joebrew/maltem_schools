library(dplyr)
if('simulations.RData' %in% dir('data')){
  load('data/simulations.RData')
} else {
  
  # Drop those with NA turmas, or numbers
  out <- expand.grid(consec = seq(0, 100, 5),
                     iter = 1:4000)
  out$val <- NA
  out$inv <- NA
  for (i in unique(out$consec)){
    print(i)
    for (j in unique(out$iter)){
      this <- sample(c(TRUE, FALSE), size = i, replace = TRUE, prob = c(0.08, 0.92))
      all_present <- all(!this)
      all_absent <- all(this)
      out$val[out$consec == i &
                out$iter == j] <- all_present
      out$inv[out$consec == i &
                out$iter == j] <- all_absent
    }
  }
  sims <- out %>%
    group_by(consec) %>%
    summarise(p = length(which(val)) / length(val) * 100,
              inv = length(which(inv)) / length(inv) * 100)
  
  save(sims,
       file = 'data/simulations.RData')
  
}
