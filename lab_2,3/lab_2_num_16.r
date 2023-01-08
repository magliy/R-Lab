df <- read.csv('data/outcome-of-care-measures.csv')

find_cities <- function(df, city, min_deaths, max_deaths) {
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  df <- df[df$City == city, ]
  result = data.frame(outcomes = outcomes, hospitals = "")
  
  for (outcome in outcomes) {
    
    switch(outcome, 'heart attack' = {
      col_min = 13
      col_max = 14
    }, 'heart failure' = {
      col_min = 19
      col_max = 20
    }, 'pneumonia' = {
      col_min = 25
      col_max = 26
    })
    
    for (i in 1:nrow(df)) {
      if (df[i, col_min] >= min_deaths & df[i, col_max] <= max_deaths) {
        result[result$outcomes == outcome, 'hospitals'] <- paste(
          as.character(df$Hospital.Name[i]),
          result[result$outcomes == outcome, 'hospitals'],
          sep = ", "
          )
      }
    }
  }
  
  result$hospitals <- gsub(", $", "", result$hospitals)
  
  return(result)
}

a <- find_cities(df, 'MONTGOMERY', 13, 22)

