library(readxl)
library(dplyr)
library(compare)
library(styler)
library(tidyverse)
library(openxlsx)


candy_data_2015 <- read_excel("data-raw/boing-boing-candy-2015.xlsx")
candy_data_2016 <- read_excel("data-raw/boing-boing-candy-2016.xlsx")
candy_data_2017 <- read_excel("data-raw/boing-boing-candy-2017.xlsx")


View(candy_data_2015)
View(candy_data_2016)
View(candy_data_2017)

# Remove [ ] from column names / headers
names(candy_data_2015) <- gsub("\\[|\\]", "", names(candy_data_2015))
names(candy_data_2016) <- gsub("\\[|\\]", "", names(candy_data_2016))

# Remove Q6:|
names(candy_data_2017) <- gsub("Q6 \\| ", "", names(candy_data_2017))



# # Get the column names of each dataset
colnames_2015 <- colnames(candy_data_2015)
colnames_2016 <- colnames(candy_data_2016)
colnames_2017 <- colnames(candy_data_2017)


# Find the column names that are unique to each dataset
diff_names_2015_2016 <- setdiff(colnames_2015, colnames_2016)
diff_names_2016_2015 <- setdiff(colnames_2016, colnames_2015)
diff_names_2015_2017 <- setdiff(colnames_2015, colnames_2017)
diff_names_2017_2015 <- setdiff(colnames_2017, colnames_2015)
diff_names_2016_2017 <- setdiff(colnames_2016, colnames_2017)
diff_names_2017_2016 <- setdiff(colnames_2017, colnames_2016)

diff_names_2016_2015 <- setdiff(colnames_2016, colnames_2015)

# Print the column names that don't match across the three datasets
print("Column names that are unique to candy_data_2015:")
print(diff_names_2015_2016)
# print(diff_names_2015_2017)


# print("Column names that are unique to candy_data_2016:")
# print(diff_names_2016_2015)
# print(diff_names_2016_2017)

# print("Column names that are unique to candy_data_2017:")
# print(diff_names_2017_2015)
# print(diff_names_2017_2016)


# Standardise header data sets 2015-2016-2017

colnames(candy_data_2015)[colnames(candy_data_2015) == "Box’o’ Raisins"] <- "Box of Raisins"
colnames(candy_data_2016)[colnames(candy_data_2016) == "Box'o'Raisins"] <- "Box of Raisins"
colnames(candy_data_2015)[colnames(candy_data_2015) == "Dark Chocolate Hershey"] <- "Hershey's Dark Chocolate"
colnames(candy_data_2015)[colnames(candy_data_2015) == "Hershey’s Kissables"] <- "Hershey's Kisses"
colnames(candy_data_2016)[colnames(candy_data_2016) == "Licorice (yes black)"] <- "Licorice"
colnames(candy_data_2015)[colnames(candy_data_2015) == "JoyJoy (Mit Iodine)"] <- "JoyJoy (Mit Iodine!)"
colnames(candy_data_2016)[colnames(candy_data_2016) == "Sweetums (a friend to diabetes)"] <- "Sweetums"
colnames(candy_data_2016)[colnames(candy_data_2016) == "Your gender:"] <- "Gender"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Q3: AGE"] <- "How old are you?"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Q1: GOING OUT?"] <- "Are you going actually going trick or treating yourself?"
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Anonymous brown globs that come in black and orange wrappers\t(a.k.a. Mary Janes)"] <-
  "Anonymous brown globs that come in black and orange wrappers"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Box'o'Raisins"] <- "Box of Raisins"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Licorice (yes black)"] <- "Licorice"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Sweetums (a friend to diabetes)"] <- "Sweetums"
colnames(candy_data_2017)[colnames(candy_data_2017) == "Q2: GENDER"] <- "Gender"
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Q4: COUNTRY"] <- "Which country do you live in?"
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Q5: STATE, PROVINCE, COUNTY, ETC"] <- "Which state, province, county do you live in?"
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Q8: DESPAIR OTHER"] <- "Please list any items not included above that give you DESPAIR."
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Q7: JOY OTHER"] <- "Please list any items not included above that give you JOY."
colnames(candy_data_2017)[colnames(
  candy_data_2017) == "Independent M&M's"] <- "Third Party M&M's"

# Use mutate to insert a new column with an empty vector of values
candy_data_2016 <- candy_data_2016 %>%
  mutate("Bonkers" = vector("character", nrow(candy_data_2016)),
         "Brach products (not including candy corn)" = vector("character", nrow(candy_data_2016)),
         "Bubble Gum" = vector("character", nrow(candy_data_2016)),
         "Lapel Pins" = vector("character", nrow(candy_data_2016)),
         "Runts" = vector("character", nrow(candy_data_2016)),
         "Mint Leaves" = vector("character", nrow(candy_data_2016)),
         "Mint M&Ms" = vector("character", nrow(candy_data_2016)),
         "Ribbon candy" = vector("character", nrow(candy_data_2016)),
         "Peterson Brand Sidewalk Chalk" = vector("character", nrow(candy_data_2016)),
         "Peanut Butter Bars" = vector("character", nrow(candy_data_2016)),
         "Peanut Butter Jars" = vector("character", nrow(candy_data_2016)),
         "Green Party M&M's" = vector("character", nrow(candy_data_2016)),
         "Abstained from M&M'ing." = vector("character", nrow(candy_data_2016)),
         "Sandwich-sized bags filled with BooBerry Crunch" = vector("character", nrow(candy_data_2016)),
         "Take 5" = vector("character", nrow(candy_data_2016)))


View(candy_data_2016)
names(candy_data_2016)

# Insert a new column with an empty vector of values 2015
candy_data_2015 <- candy_data_2015 %>%
  mutate("Gender" = vector("character", nrow(candy_data_2015)),
         "Which country do you live in?" = vector("character", nrow(candy_data_2015)),
         "Which state, province, county do you live in?" = vector("character", nrow(candy_data_2015)),
         "Bonkers (the candy)" = vector("character", nrow(candy_data_2015)),
         "Bonkers (the board game)" = vector("character", nrow(candy_data_2015)),
         "Chardonnay" = vector("character", nrow(candy_data_2015)),
         "Coffee Crisp" = vector("character", nrow(candy_data_2015)),
         "Dove Bars" = vector("character", nrow(candy_data_2015)),
         "Mike and Ike" = vector("character", nrow(candy_data_2015)),
         "Blue M&M's" = vector("character", nrow(candy_data_2015)),
         "Red M&M's" = vector("character", nrow(candy_data_2015)),
         "Mr. Goodbar" = vector("character", nrow(candy_data_2015)),
         "Peeps" = vector("character", nrow(candy_data_2015)),
         "Reese's Pieces" = vector("character", nrow(candy_data_2015)),
         "Sourpatch Kids (i.e. abominations of nature)" = vector("character", nrow(candy_data_2015)),
         "Sweet Tarts" = vector("character", nrow(candy_data_2015)),
         "Tic Tacs" = vector("character", nrow(candy_data_2015)),
         "Whatchamacallit Bars" = vector("character", nrow(candy_data_2015)),
         "Third Party M&M's" = vector("character", nrow(candy_data_2015)),
         "Green Party M&M's" = vector("character", nrow(candy_data_2015)),
         "Abstained from M&M'ing." = vector("character", nrow(candy_data_2015)),
         "Sandwich-sized bags filled with BooBerry Crunch" = vector("character", nrow(candy_data_2015)),
         "Take 5" = vector("character", nrow(candy_data_2015)))

names(candy_data_2015)


# Insert a new column with an empty vector of values 2017
candy_data_2017 <- candy_data_2017 %>%
  mutate("Bonkers" = vector("character", nrow(candy_data_2017)),
         "Brach products (not including candy corn)" = vector("character", nrow(candy_data_2017)),
         "Bubble Gum" = vector("character", nrow(candy_data_2017)),
         "Lapel Pins" = vector("character", nrow(candy_data_2017)),
         "Mary Janes" = vector("character", nrow(candy_data_2017)),
         "Runts" = vector("character", nrow(candy_data_2017)),
         "Mint Leaves" = vector("character", nrow(candy_data_2017)),
         "Mint M&Ms" = vector("character", nrow(candy_data_2017)),
         "Ribbon candy" = vector("character", nrow(candy_data_2017)),
         "Peterson Brand Sidewalk Chalk" = vector("character", nrow(candy_data_2017)),
         "Peanut Butter Bars" = vector("character", nrow(candy_data_2017)),
         "Peanut Butter Jars" = vector("character", nrow(candy_data_2017)),
         "Please list any items not included above that give you JOY." =
            vector("character", nrow(candy_data_2017)),
         "Please list any items not included above that give you DESPAIR." =
           vector("character", nrow(candy_data_2017)),
         "Guess the number of mints in my hand." = vector("character", nrow(candy_data_2017)))



names(candy_data_2015)

# Remove unrelated questions from the candy survey 2015
candy_data_2015 <- subset(candy_data_2015, select = c(-101:-114))
candy_data_2015 <- subset(candy_data_2015, select = c(-102:-110))
candy_data_2015 <- subset(candy_data_2015, select = c(-1, -97))


ncol(candy_data_2015)
ncol(candy_data_2016)
ncol(candy_data_2017)

names(candy_data_2017)
names(candy_data_2016)
names(candy_data_2015)


# Remove unrelated questions from the candy survey 2016
candy_data_2016 <- subset(candy_data_2016, select = c(-112:-121))
candy_data_2016 <- subset(candy_data_2016, select = c(-112))
candy_data_2016 <- subset(candy_data_2016, select = c(-111))
candy_data_2016 <- subset(candy_data_2016, select = c(-79))
candy_data_2016 <- subset(candy_data_2016, select = c(-108))
candy_data_2016 <- subset(candy_data_2016, select = c(-109))
candy_data_2016 <- subset(candy_data_2016, select = c(-1))

names(candy_data_2016)

# Remove unrelated questions from the candy survey 2017
candy_data_2017 <- subset(candy_data_2017, select = c(-110))
candy_data_2017 <- subset(candy_data_2017, select = -c(`Internal ID`,
                                                       `Real Housewives of Orange County Season 9 Blue-Ray`,
                                                       `Q10: DRESS`,
                                                       `Q11: DAY`,
                                                       `Q12: MEDIA [Science]`,
                                                       `Q12: MEDIA [Yahoo]`,
                                                       `Q12: MEDIA [Daily Dish]`,
                                                       `Q12: MEDIA [ESPN]`,
                                                       `Click Coordinates (x, y)`,
                                                       `Q9: OTHER COMMENTS`))





# Find the column names that are unique to each dataset
diff_names_2015_2016 <- setdiff(colnames(candy_data_2015),colnames(candy_data_2016))
print("Column names that are unique to candy_data_2015:")
print(diff_names_2015_2016)

diff_names_2016_2015 <- setdiff(colnames(candy_data_2016),colnames(candy_data_2015))
print(diff_names_2016_2015)

diff_names_2015_2017 <- setdiff(colnames(candy_data_2015),colnames(candy_data_2017))
print(diff_names_2015_2017)

diff_names_2017_2016 <- setdiff(colnames(candy_data_2017),colnames(candy_data_2016))
print(diff_names_2017_2016)


# check the number of columns
ncol(candy_data_2015) == ncol(candy_data_2017) && ncol(candy_data_2016) == ncol(candy_data_2017)


# Add column year and add the constant value (year)
candy_data_2015 <- candy_data_2015 %>%
    add_column(year = 2015)

candy_data_2016 <- candy_data_2016 %>%
    add_column(year = 2016)

candy_data_2017 <- candy_data_2017 %>%
    add_column(year = 2017)


# Combine the datasets
combined_data_v2 <- bind_rows(candy_data_2015, candy_data_2016, candy_data_2017)


View(combined_data_v2)


# write.xlsx(combined_data_v2, "combined_data_v2.xlsx")
write.csv(combined_data_v2, file = 'combined_data_v2.csv', row.names = FALSE)

