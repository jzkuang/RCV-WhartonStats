library(tidyverse)
  
getwd()

path = "Ballots/Alameda (Oakland, San Leandro, Berkeley)/"
years <- list.files(path)
for (file in years) {
  print(list.files(path = paste0(path, file, collapse ="/")))
}
