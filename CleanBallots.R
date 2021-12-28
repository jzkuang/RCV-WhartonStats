library(tidyverse)
library(rcv)
library("xlsx")

#The relevant files
# Where it takes it in from
Path = "Ballots/Alameda (Oakland, San Leandro, Berkeley)/Alameda (Oakland, San Leandro, Berkeley) 2016/BerkeleyCouncilD2"
# Where it puts it out
output_path = "\\2016_Ballots.xlsx"

#### Import the ballots ####
Path_ballot = paste0(Path, "/ballot_image.txt")
Path_lookup = paste0(Path, "/master_lookup.txt")
Ballots <- read_tsv(Path_ballot, col_names = FALSE)
Lookup <- read_tsv(Path_lookup, col_names = FALSE)
# This gets the ballots into a readable format
cleaned_ballot <- clean_ballot(ballot = Ballots, b_header = T, 
                        lookup = Lookup, l_header = T, 
                        format = "WinEDS")

## Writing the (default) cleaned ballots
# cleaned_output_path = paste0("Cleaned_Ballots", output_path)
# write_csv(cleaned_ballot, file = cleaned_output_path)
# table(cleaned_ballot$vote_rank)

#### This gets the rankings for each ballot type  ####
ballot_rankings <- cleaned_ballot %>% 
  mutate(candidate = replace_na(candidate, "Blank")) %>% 
  pivot_wider(id_cols = pref_voter_id,
              names_from = vote_rank, values_from = candidate) %>% 
  group_by(pref_voter_id) %>% 
  mutate(ballot = paste(`1`, `2`,`3`, sep = ">"),
         unique_votes = any(duplicated(c(`1`, `2`, `3`))))
freq_ballot <- data.frame(table(ballot_rankings$ballot))
# This one only returns legitimate rankings
legitimate_ballot_rankings <- ballot_rankings %>% 
  filter(!grepl("Blank", ballot)) %>% 
  filter(unique_votes == FALSE)
freq_legitimate_ballot <- data.frame(table(legitimate_ballot_rankings$ballot))

#### Output these frequency tables #####
frequency_output_path = paste0("Ranking_Frequency", output_path) ## Puts it into this folder
write.xlsx(freq_legitimate_ballot, file = frequency_output_path, 
           sheetName="Legitimate Ballots")
# write.xlsx(freq_ballot, file = frequency_output_path,
#           sheetName="Full Ballots", append=TRUE)
