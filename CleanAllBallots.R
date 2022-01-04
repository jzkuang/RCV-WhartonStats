library(tidyverse)
library(rcv)
library("xlsx")

#### Post 2016 Ballots #####

### The Function to Clean
cleanAndPrint_post2016 <- function(input_path, output_path, election) {
  #### Import the ballots ####
  Path_ballot = paste0(input_path, "/ballot_image.txt")
  Path_lookup = paste0(input_path, "/master_lookup.txt")
  Ballots <- read_tsv(Path_ballot, col_names = FALSE)
  Lookup <- read_tsv(Path_lookup, col_names = FALSE)
  # This gets the ballots into a readable format
  cleaned_ballot <- clean_ballot(ballot = Ballots, b_header = F, 
                                 lookup = Lookup, l_header = F, 
                                 format = "WinEDS")
  
  #### This gets the rankings for each ballot type  ####
  unique(cleaned_ballot$candidate)
  number_of_candidates <- max(cleaned_ballot$vote_rank)
  ballot_rankings <- cleaned_ballot %>% 
    mutate(candidate = replace_na(candidate, "Blank")) %>% 
    pivot_wider(id_cols = pref_voter_id,
                names_from = vote_rank, values_from = candidate) %>%
    unite(col = ballot, 2:(number_of_candidates+1), sep = ">") %>% 
    group_by(pref_voter_id) %>%
    filter (0 == anyDuplicated(unlist(strsplit(ballot, ">"))))
  # This one only returns legitimate rankings
  legitimate_ballot_rankings <- ballot_rankings %>%
    filter(!grepl("Blank", ballot))
  freq_legitimate_ballot <- data.frame(table(legitimate_ballot_rankings$ballot))
  
  #### Output these frequency tables #####
  if (nrow(freq_legitimate_ballot) != 0){
    write.xlsx(freq_legitimate_ballot, file = output_path, 
               sheetName = election, append = TRUE)
  }
}

### The Actual Cleaning
path = "Ballots/Alameda (Oakland, San Leandro, Berkeley)/Alameda (Oakland, San Leandro, Berkeley) 2016"
elections <- list.files(path)
output_path = "Ranking_Frequency/2016_Alameda_Ballots.xlsx"
write.xlsx(data.frame(Empty = "", row.names = NULL), file = output_path, 
           sheetName= "blank")
election <- elections[[2]]

for (election in elections) {
  input_path = paste0(path, "/", election)
  cleanAndPrint_post2016(input_path, output_path, election)
}

#### Pre 2016 Ballots ####

### The Function to Clean Pre 2016
cleanAndPrint_pre2016 <- function(ballot_path, lookup_path, output_path, election) {
  #### Import the ballots ####
  Ballots <- read_tsv(ballot_path, col_names = FALSE)
  Lookup <- read_tsv(lookup_path, col_names = FALSE)
  # This gets the ballots into a readable format
  cleaned_ballot <- clean_ballot(ballot = Ballots, b_header = T, 
                                 lookup = Lookup, l_header = T, 
                                 format = "WinEDS")
  
  #### This gets the rankings for each ballot type  ####
  number_of_candidates <- max(cleaned_ballot$vote_rank)
  ballot_rankings <- cleaned_ballot %>% 
    mutate(candidate = replace_na(candidate, "Blank")) %>% 
    pivot_wider(id_cols = pref_voter_id,
                names_from = vote_rank, values_from = candidate) %>%
    unite(col = ballot, 2:(number_of_candidates+1), sep = ">") %>% 
    group_by(pref_voter_id) %>%
    filter (0 == anyDuplicated(unlist(strsplit(ballot, ">"))))
  # This one only returns legitimate rankings
  legitimate_ballot_rankings <- ballot_rankings %>% 
    filter(!grepl("Blank", ballot))
  freq_legitimate_ballot <- data.frame(table(legitimate_ballot_rankings$ballot))
  
  #### Output these frequency tables #####
  if (nrow(freq_legitimate_ballot) != 0){
    write.xlsx(freq_legitimate_ballot, file = output_path, 
               sheetName = election, append = TRUE)
  }
}

path = "Ballots/Alameda (Oakland, San Leandro, Berkeley)/Alameda (Oakland, San Leandro, Berkeley) 2010"
elections <- list.files(path) # For this to work the naming schema has to be the same
elections <- sub("^master_lookup_*[\\ ]?", "", sub("^ballot_image_*[\\ ]?", "", elections))
elections <- elections[duplicated(elections)]

output_path = "Ranking_Frequency/2010_Alameda_Ballots.xlsx"
write.xlsx(data.frame(Empty = "", row.names = NULL), file = output_path, 
           sheetName= "blank")
election <- elections[[3]]
for (election in elections) {
  ballot_path = paste0(path, "/ballot_image_", election)
  lookup_path = paste0(path, "/master_lookup_", election)
  election <- gsub("Member, ", "", election)
  cleanAndPrint_pre2016(ballot_path, lookup_path, output_path, election)
}

