library(dplyr)
library(styler)
library(readxl)
library(stringr)




combined_data_v2 <- read_csv("data-raw/combined_data_v2.csv", locale=locale(encoding="latin1"))

view(combined_data_v2)
names(combined_data_v2)

# Remove columns that are not related to analyse
combined_data_v2 <- subset(combined_data_v2, select = c(-96,-97, -98, -102, -124))

names(combined_data_v2)

# Replace white spaces with _
names(combined_data_v2) <- gsub("\\s", "_", names(combined_data_v2))
names(combined_data_v2) <- tolower(names(combined_data_v2))


# Change column names - corrected
combined_data_v2 <- combined_data_v2 %>%
  rename(
    "trick_or_treat" = "are_you_going_actually_going_trick_or_treating_yourself?",
    "hershey's_milk_chocolate"=  "hersheyâ\u0080\u0099s_milk_chocolate",
    "any_full_sized_candy_bar" = "any_full-sized_candy_bar",
    "reese's_peanut_butter_cups" = "reeseâ\u0080\u0099s_peanut_butter_cups",
    "peanut_m&m's" = "peanut_m&mâ\u0080\u0099s",
    "chick-o-sticks" = "chick-o-sticks_(we_donâ\u0080\u0099t_know_what_that_is)",
    "country" = "which_country_do_you_live_in?"
  )

# Standarise column - how old are you?
combined_data_v2 <- combined_data_v2 %>%
  mutate(`how_old_are_you?` = as.integer(`how_old_are_you?`)) %>%
  filter(`how_old_are_you?` <= 100)

combined_data_v2 <- combined_data_v2 %>%
  mutate(trick_or_treat = tolower(
    trick_or_treat)) %>%
  mutate(trick_or_treat = ifelse(
    trick_or_treat != 'yes' & trick_or_treat != 'no', NA , trick_or_treat
  ))

# Convert country to lower
combined_data_v2$country <- str_to_lower(combined_data_v2$country)


# Rename country column
clean_country <- function(data, wrong_countries, correct_country) {
  for (wrong_country in wrong_countries) {
    data["country"][data["country"] == wrong_country] <- correct_country
  }

  return(data)
}

usa_country <- list("'merica",  "Alaska", "America", "California", "EUA",
                    "I pretend to be from Canada, but I am really from the United States.",
                    "Merica", "Murica", "N. America", "New Jersey", "New York", "North Carolina",
                    "Pittsburgh", "the best one - usa", "The United States", "The United States of America",
                    "The Yoo Ess of Aaayyyyyy", "Trumpistan", "U S", "u s a", "U.S.",
                    "U.S.A.", "UD", "unhinged states", "Unied States", "unite states",
                    "United  States of America", "United Sates", "United staes", "United State",
                    "United Statea", "United Stated", "united states", "United States of America",
                    "United Statss", "United Stetes","United ststes",  "Unites States",
                    "Units States", "US", "US of A", "usa", "USA (I think but it's an election year so who can really tell)",
                    "USA USA USA", "USA USA USA USA", "USA USA USA!!!!", "USA!", "USA! USA!", "USA! USA! USA!",
                    "USA!!!!!!", "USA? Hard to tell anymore..", "USAA", "usas", "USAUSAUSA", "USSA", "Us", "United States",
                    "us", "United states", "united states of america", "USA", "uSA", "america", "u.s.", "u.s.a.", "united states",
                    "united ststes"
                    )

usa_country <- lapply(usa_country, tolower)

na_country <- list("1", "30.0", "32", "35", "44.0","45", "45.0", "46", "47.0", "51.0", "54.0","A",
                   "A tropical island south of the equator", "Ahem....Amerca", "Atlantis", "Cascadia",
                   "Denial", "Earth", "Europe", "I don't know anymore","Fear and Loathing", "god's country",
                   "insanity lately", "Narnia", "Neverland", "Not the USA or Canada", "one of the best ones", "See above",
                   "Somewhere", "soviet canuckistan", "Sub-Canadian North America... 'Merica", "The republic of Cascadia",
                   "there isn't one for old men", "this one", "subscribe to dm4uz3 on youtube", "murrika")
na_country <- lapply(na_country, tolower)

uk_country <- list("U.K.", "United Kindom", "United Kingdom", "endland", "england", "Scotland", "uk", "united kingdom")
uk_country <- lapply(uk_country, tolower)


nl_country <- list("Netherlands")
nl_country <- lapply(nl_country, tolower)

ca_country <- list("Can", "Canada`", "Canae")
ca_country <- lapply(ca_country, tolower)

es_country <- list("espa a", "spain", "espaÃ±a")
es_country <- lapply(es_country, tolower)

br_country <- list("brasil", "Brasil")
br_country <- lapply(br_country, tolower)

combined_data_v2 = clean_country(combined_data_v2, usa_country, "USA")
combined_data_v2 = clean_country(combined_data_v2, na_country, NA)
combined_data_v2 = clean_country(combined_data_v2, uk_country, "UK")
combined_data_v2 = clean_country(combined_data_v2, nl_country, "The Netherlands")
combined_data_v2 = clean_country(combined_data_v2, ca_country, "Canada")
combined_data_v2 = clean_country(combined_data_v2, es_country, "Spain")
combined_data_v2 = clean_country(combined_data_v2, br_country, "Brazil")


distinct(combined_data_v2, country)

# Capitalise country names
combined_data_v2$country <- str_to_upper(combined_data_v2$country)

view(combined_data_v2)
names(combined_data_v2)


write.csv(combined_data_v2, file = "data-clean/clean_data.csv", row.names = FALSE)


