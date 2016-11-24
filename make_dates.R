dates <- seq(as.Date('2015-01-01'),
             as.Date('2016-11-30'),
             by = 1)
df <- data.frame(rep(NA, 100))
for (i in 1:length(dates)){
  df[,i] <- NA
  names(df)[i] <- as.character(dates[i])
  df[1,i] <- format(as.Date(dates[i]), '%B')
  df[2,i] <- format(as.Date(dates[i]), '%d')
}

left <- data.frame(permid = rep(NA, 100))
left$name <- NA
left$dob <- NA 
left$sex <- NA
left$grade <- NA
left$turma <- NA
left$school <- NA

df <- cbind(left, df)
for (i in 1:nrow(df)){
  for(j in 1:ncol(df)){
    df[i,j] <- ifelse(is.na(df[i,j]), '', df[i,j])
  }
}

write.csv(df, '~/Desktop/plantilla.csv')
