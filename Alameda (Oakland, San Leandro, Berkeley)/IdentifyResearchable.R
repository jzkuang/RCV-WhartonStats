library(readxl)
library(tidyverse)
library("xlsx")


path <- "Ranking_Frequency"
years <- list.files(path)
write.xlsx(data.frame(Empty = "", row.names = NULL), file = "SpecialElections.xlsx", 
           sheetName= "blank")
## This Pulls Each Election and Finds the Special Ones
for (year in years) {
  excel_path <- paste(path, year, sep = "\\")
  elections <- excel_sheets(excel_path)
  for (election in elections){
    if (election != "blank"){
      print(election)
      result <- read_excel(path = excel_path, sheet = election) %>% 
        select(c(2, 3)) %>%
        rename("Ranking" = "Var1") %>% 
        arrange(desc(Freq))
      ratio <- result[[2, 2]]/result[[3, 2]]
      if (ratio > 2) {
        write.xlsx(data.frame(result), file = "SpecialElections.xlsx", 
                   sheetName = gsub(" ", "", paste0(substr(year, 1, 4), election)), append = TRUE, row.names = FALSE)
      }
    }
  }
}
