## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(votesmart)

## -----------------------------------------------------------------------------
# If our key is not registered in this environment variable, 
# the result of `Sys.getenv("VOTESMART_API_KEY")` will be `""` (i.e. a string of `nchar` 0)
key <- Sys.getenv("VOTESMART_API_KEY")

key_exists <- (nchar(key) > 0)

if (!key_exists) knitr::knit_exit()

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
conflicted::conflict_prefer("filter", "dplyr")

## -----------------------------------------------------------------------------
(franks <- 
   candidates_get_by_lastname(
    last_names = "frank", 
    election_years = c(2000, 2004)
  )
)

## -----------------------------------------------------------------------------
(barneys <- 
  franks %>% 
  filter(first_name == "Barney") %>% 
   select(
     candidate_id, first_name, last_name, 
     election_year, election_state_id, election_office
   )
)

## -----------------------------------------------------------------------------
(barney_id <- 
  barneys %>% 
  pull(candidate_id) %>% 
  unique()
)

## -----------------------------------------------------------------------------
(barney_ratings <- 
  rating_get_candidate_ratings(
        candidate_ids = barney_id,
        sig_ids = "" # All SIGs
      )
)

## -----------------------------------------------------------------------------
main_cols <- c("rating", "category_name_1", "sig_id", "timespan")

## -----------------------------------------------------------------------------
(barney_on_env <- 
  barney_ratings %>% 
  filter(category_name_1 == "Environment") %>% 
  select(main_cols)
)

## -----------------------------------------------------------------------------
barney_ratings %>% 
  filter(
    stringr::str_detect(rating, "[A-Z]")
  ) %>% 
  select(rating, category_name_1)

## -----------------------------------------------------------------------------
barney_on_env %>% 
  group_by(timespan) %>% 
  summarise(
    avg_rating = mean(as.numeric(rating), na.rm = TRUE)
  ) %>% 
  arrange(desc(timespan))

## -----------------------------------------------------------------------------
barney_ratings %>% 
  filter(category_name_1 == "Abortion") %>% 
  select(
    rating, sig_id, category_name_1
  )

## -----------------------------------------------------------------------------
(some_sigs <- 
  barney_ratings %>%
    pull(sig_id) %>%
    unique() %>%
    sample(3)
)

## -----------------------------------------------------------------------------
rating_get_sig(
  sig_ids = some_sigs
)

## -----------------------------------------------------------------------------
(category_df <-
  rating_get_categories(
    state_ids = NA # NA for national
  ) %>% 
  distinct() %>% 
  sample_n(nrow(.)) # Sampling so we can see multiple categories in the 10 rows shown here
)

## -----------------------------------------------------------------------------
(some_categories <- category_df$category_id %>% sample(3))

## -----------------------------------------------------------------------------
(sigs <- 
  rating_get_sig_list(
    category_ids = some_categories,
    state_ids = NA
  ) %>% 
    select(sig_id, name, category_id, state_id) %>% 
   sample_n(nrow(.))
)

## -----------------------------------------------------------------------------
sigs %>% 
  rename(
    sig_name = name
  ) %>% 
  left_join(
    category_df,
    by = c("state_id", "category_id")
  ) %>% 
  rename(
    category_name_1 = name
  ) %>% 
  sample_n(nrow(.))

