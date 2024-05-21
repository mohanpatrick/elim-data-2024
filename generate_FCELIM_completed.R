library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)



# Run local vars here




# Placeholder for grabbing old completed once we have established

prior_completed_leagues <- read_csv("https://github.com/mohanpatrick/elim-data-2024/releases/download/data-mfl/completed_leagues.csv")
prior_completed_drafts <- read_csv("https://github.com/mohanpatrick/elim-data-2024/releases/download/data-mfl/completed_drafts.csv")

# Get current drafts

current_drafts  <- read_csv("https://github.com/mohanpatrick/elim-data-2024/releases/download/data-mfl/draft_picks_mfl.csv")

newly_completed_leagues <- drafts |>
  filter(!is.na(player_name)) |>
  group_by(league_id, league_name)|>
  summarise(farthest_pick = max(overall))|>
  filter(farthest_pick == 288)



newly_completed_drafts <- newly_completed_leagues |>
  left_join(drafts, by=c("league_id"="league_id", "league_name" = "league_name"))

count_new_leagues <- nrow(newly_completed_leagues)
count_prior_leagues <- nrow(prior_completed_leagues)

if (count_new_leagues == count_prior_leagues) {

  cli::cli_alert_success("Nothing to do here. No net new leagues")

}else {

all_completed_leagues <- union(newly_completed_leagues, prior_completed_leagues)
all_completed_drafts <- union(newly_completed_drafts, prior_completed_drafts)




# This is a throwaway line until we've done a run
write_csv(all_completed_leagues, "completed_leagues.csv")
write_csv(all_completed_drafts, "completed_drafts.csv")

pb_upload("completed_leagues.csv",
          repo = "mohanpatrick/elim-data-2024",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded completed leagues to Git")

pb_upload("completed_drafts.csv",
          repo = "mohanpatrick/elim-data-2024",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded completed drafts to Git")
}



