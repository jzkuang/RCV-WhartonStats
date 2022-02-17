library("tidyverse")
library("readxl")
library("xlsx")

file = "Maine General CD2 CVR Updated(1).xlsx"

nov2018_cvr <- read_xlsx(path = paste0("Maine/November 2018/", file))

ballot_rankings <- june2018_gov %>% 
  unite(col = ballot, 4:8 , sep = ">") %>% 
  filter(grepl("undervote", ballot, fixed=TRUE)) %>% 
  select(c(`Cast Vote Record`, ballot)) %>% 
  group_by(`Cast Vote Record`) %>%
  filter (0 == anyDuplicated(unlist(strsplit(ballot, ">"))))

freq_legitimate_ballot <- data.frame(table(ballot_rankings$ballot))

write.xlsx(freq_legitimate_ballot, file = "Maine/Ranking_Frequency/Nov2018CD2_Frequency.xlsx", 
           sheetName="Ballot Frequency", row.names = FALSE)
