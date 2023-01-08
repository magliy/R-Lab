setwd("C:\\Users\\Дмитрий\\PycharmProjects\\studyR")



create_df <- function(data) {
  # выбираем только федеральные округа
  pattern = '*федеральный округ'
  df <- data.frame(data)
  df <- df[grep(pattern, df$Регион), ]
  
  # экспорт и импорт
  df$ЭкспСумм = 0
  df$ИмпСумм = 0
  for (i in 1:nrow(df)) {
    for (j in grep('*Экспорт', names(df))) {
      df$ЭкспСумм[i] <- as.numeric(df$ЭкспСумм[i]) + as.numeric(df[i, j])
    }
    for (k in grep('*Импорт', names(df))) {
      df$ИмпСумм[i] <- as.numeric(df$ИмпСумм[i]) + as.numeric(df[i, k])
    }
  }
  
  return (df)
}


find_func <- function(df) {
  result <- vector()
  for (i in 1:nrow(df)) {
    if (as.numeric(df$ЭкспСумм[i]) > as.numeric(df$ИмпСумм[i])) {
      result <- c(result, df$Регион[i])
    }
  }
  return(result)
}


load('data/ExpImp.RData')
df <- create_df(ExpImp)
df
k <- find_func(df)
print(k)


