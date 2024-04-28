library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)
library(httr2)


options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")
#Sys.setenv(MFL_CLIENT = "")
#Sys.setenv(GITHUB_PAT="")

GITHUB_PAT <- Sys.getenv(c("GITHUB_PAT"))
mfl_client <- Sys.getenv(c("MFL_CLIENT"))
mfl_user_id <- Sys.getenv(c("MFL_USER_ID"))
mfl_pass <- Sys.getenv(c("MFL_PWD"))
cli::cli_alert("Client ID: {mfl_client}")

search_draft_year = "2024"
find_leagues = "TRUE"
polite = "FALSE"
mfl_user_id = "android16"
mfl_pass = "R1BRX70x"
search_string="zzz #FCEliminator"
total_picks_in_draft = 288
# Exclude blind bid leagues
leagues_to_exclude = c(19123,33163,39863,52021,64792,58866)
# Hmmm, can we use the number of picks to filter out weird ones? Before we filter?





get_mfl_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(3)
  conn <- mfl_connect(search_draft_year, league_id, user_agent = "MFLRCLIENT", rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60,user_name=mfl_user_id, password = mfl_pass)
  ff_draft(conn)
}

# This is what we'd use if we don't have to use userLeagues
mfl_leagues <- mfl_getendpoint(mfl_connect(search_draft_year),"leagueSearch", user_agent="MFLRCLIENT", SEARCH=search_string, user_name=mfl_user_id, password = mfl_pass) |>
  purrr::pluck("content","leagues","league") |>
  tibble::tibble() |>
  tidyr::unnest_wider(1) |>
  select( league_name = name, league_id = id,league_home = homeURL) |>
  # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
  filter(!(league_id %in% leagues_to_exclude))



# Why are we doing this here?
#mfl_leagues  <- get_elim_leagues(mfl_conn,search_draft_year, search_string) |>
#  select(league_id, league_name, league_url)

# Check to see we are in polite mode and if so

if ( polite == "TRUE") {
cli::cli_alert_success("Getting prior completed league ids")
prior_completed_leagues <- read_csv("https://github.com/mohanpatrick/elim-data-2024/releases/download/data-mfl/completed_leagues.csv", col_names = c("league_id"))

completed_count <- nrow(prior_completed_leagues)
cli::cli_alert("Found {completed_count} leagues to exclude")

# Get rid of leagues that are completed and that we already have picks for. This is to keep the MFL calls to a minimum
mfl_leagues <- mfl_leagues |>
  anti_join(prior_completed_leagues)

run_league_count <- nrow(mfl_leagues)
cli::cli_alert("Running with {run_league_count} leagues")

}



# For testing subset leagues
#mfl_leagues <- mfl_leagues |>
#  slice_head(n=25)



fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)




cli::cli_alert("Starting draft pull")
cli::cli_alert(now())
mfl_drafts <- mfl_leagues |>
  mutate(drafts = map(league_id, possibly(get_mfl_draft, otherwise = tibble()))) |>
  unnest(drafts)
cli::cli_alert("Ending draft pull")
cli::cli_alert(now())

warnings <- dplyr::last_dplyr_warnings(n=20)



# Add interval between picks, note this dies without picks so adding the if

if(nrow(mfl_drafts) > 0) {
mfl_drafts <- mfl_drafts |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  group_by(league_id) |> # Note removed division here
  mutate(
    timestamp = as.POSIXct(timestamp, origin= "1970-01-01"),
    time_to_pick_int = interval(lag(timestamp), timestamp),
    time_to_pick = seconds(time_to_pick_int)
  )
}




write_csv(mfl_drafts,"draft_picks_mfl.csv")
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")

pb_upload("draft_picks_mfl.csv",
          repo = "mohanpatrick/elim-data-2024",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("mfl_league_ids.csv",
          repo = "mohanpatrick/elim-data-2024",
          tag = "data-mfl")
cli::cli_alert_success("Successfully uploaded to Git")


