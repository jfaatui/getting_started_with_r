#Restart R session is ctl+shift+f10

install.packages("tidytuesdayR")

# To tidy up column names as lower case with underscore for gaps
install.packages("janitor")

library(dplyr)
library(janitor)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load("2022-02-01")

# As coded above, we are constantly pulling down/calling the tidy tuesday github repository to get the tidy tuesday data.
# Github has max number of times you can make call to same repository - will think you're a bot.
# So instead, save file contents locally to an RDS file, and instead use local RDS file when referring to tidy tuesday data.
saveRDS(tuesdata$breed_traits, "breed_traits.rds")

#breed_traits <- tuesdata$breed_traits

breed_traits <- clean_names(readRDS("breed_traits.rds"))

# Selecting
select(breed_traits, breed)

select(breed_traits, coat_length)

select(breed_traits, c(1, 3, 4))

select(breed_traits, c(breed, coat_length))

select(breed_traits, 1, 2, 6:10)

select(breed_traits, breed, affectionate_with_family, drooling_level:openness_to_strangers)

# Filtering
glimpse(breed_traits) #for seeing column names

filter(breed_traits, drooling_level == 5)

filter(breed_traits, drooling_level == 5 & coat_length == "Short")

filter(breed_traits, drooling_level == 5 | drooling_level == 4)

filter(breed_traits, drooling_level %in% c(1,3,5))

filter(breed_traits, drooling_level %in% c(3,4,5) & coat_length == "Short")

# Not operator
filter(breed_traits, drooling_level != 5)
filter(breed_traits, !drooling_level %in% c(1, 3, 5)) # NOT IN 1, 3 or 5

#Homework
filter(breed_traits, affectionate_with_family %in% c(4,5) & good_with_young_children %in% c(4,5) & good_with_other_dogs %in% c(4,5))


# Native pipes (keyboard shortcut Ctrl + Shift + M)

# "Assign" keyboard shortcut Alt + -
# See ALL KEYBOARD SHORTCUTS (Alt + Shift + K)

# Arrange
arrange(breed_traits, breed)
arrange(breed_traits, desc(breed))
arrange(breed_traits, desc(breed), affectionate_with_family)

# Native Piping - filter then arrange via the pipe
drooly_dogs <- breed_traits |> 
  filter(drooling_level == 5) |> 
  arrange(breed)

# Mutate
View(breed_traits)

# breed_traits |> mutate(.date$"Bark Energy Level")  # if I didn't have cleaned-up names, I'd have to call the column by using this notation

noisy_dogs <- breed_traits |> 
  mutate(bark_energy_level = energy_level * barking_level) |> 
  select(breed, energy_level, barking_level, bark_energy_level) |> 
  arrange(desc(bark_energy_level))

# Case_when - use when you have more than 2 conditions
trainable_dods <- breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  select(breed, trainability_level, trainability_category) |> 
  filter(trainability_category == "Very trainable")

# if_else - use when you have 2 conditions
smmoth_dogs <- breed_traits |> 
  mutate(smooth_coat = if_else(coat_type == "smooth", TRUE, FALSE)) |> 
  select(breed, coat_type, smooth_coat)

# Homework
dogs_that_drool <- breed_traits |> 
  mutate(drool_heaviness = case_when(
    drooling_level < 3 ~ "Light drool",
    drooling_level == 3 ~ "Medium drool",
    drooling_level > 3 ~ "Heavy drool",
  )) |> 
  select(breed, drooling_level, drool_heaviness) |> 
  filter(!drool_heaviness %in% c("Light drool", "Heavy drool")) |> 
  arrange(desc(breed))

# Group by and Summarise
breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  group_by(trainability_category) |> 
  summarise(
    avg_energy_lvl = mean(energy_level),
    count = n() # counts rows
    )

# Get a simple count of rows for each category
breed_traits |> 
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable",
  )) |> 
  count(trainability_category)

#Homework
# Which coat type has the highest average (mean) coat grooming frequency?
breed_traits |> 
  group_by(coat_type) |> 
  summarise(
    avg_coat_grm_freq = mean(coat_grooming_frequency),
    count = n()) |> 
  arrange(desc(avg_coat_grm_freq))

# Create a table of the dogs with the coat type identified above. Do they all have similarly
# high coat-grooming frequency scores?
breed_traits |> 
  filter(coat_type == "Corded") |> 
  select(breed, coat_type, coat_grooming_frequency)

# Which is the most common coat type?
breed_traits |> 
  group_by(coat_type) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

#Binding Rows and Joining tables
# Starting again with the 3 files
# Save all 3 files as a list and create an RDS file
tuesdata <- tidytuesdayR::tt_load("2022-02-01") |> 
  saveRDS("all_good_dogs.rds")

all_dogs <- readRDS("all_good_dogs.rds")

# Piping the breed_traits table into clean names to standardise the names of the columns
# Wrapping in parenthesis lets you see the output in the console AND creates the breed_traits object in the Environment tab
(breed_traits <-  all_dogs$breed_traits |> 
    clean_names())

(breed_ranks <-  all_dogs$breed_rank |> 
    clean_names())

# Left join
(traits_with_ranks <- left_join(breed_traits, breed_ranks, by = "breed"))

# Some names don't match because of spaces between breed names, so create a new additional
# column called key which you populate with the breed names in a clean format
(breed_traits <- all_dogs$breed_traits |> 
    clean_names() |> 
    mutate(key = make_clean_names(breed)))

# Same with the other table:create a new additional column called key which you populate with the breed names in a clean format
# There's a minus breed command to take away this second breed column which is exactly the same field as in
# the other table, as we don't need two breed columns when joining the tables together
(breed_ranks <- all_dogs$breed_rank |> 
    clean_names() |> 
    mutate(key = make_clean_names(breed)) |> 
    select(-breed))

#Join the tables together on the respective "key" columns
(traits_with_ranks <- left_join(breed_traits, breed_ranks, by = "key"))

# Check join was successful by checking "links" column where every row should NOT be null
# so check to see that there are NO null rows in "links" column
# Expecting count = 0 if everything matches
(traits_with_ranks |> 
    filter(is.na(links)) |> 
    nrow())

# BUT can check BEFORE you join tables by using ANTI_JOIN
# So below, I'm asking IF I want to join the two tables by their "key" columns, give me
# a count of anything in breed_traits that can't be found in breed_ranks
# Expecting count = 0 if everything matches
(anti_join(breed_traits, breed_ranks, by = "key") |> 
    nrow())

# Want weighted score where it's positive traits minus negative traits to get an overall score for dog
# This requires taking away negative_traits_score twice
# Because when creating all_traits_score, it summed EVERY single column, including the ones to be used in negative_traits_score
# So negative_traits_score is minused once, to remove the scores of the negative traits so that you have the scores of the traits you want to keep in a modified all_traits_score total
# Then negative traits_score is taken away a second time from this modified all_traits_score total, to actually have a "minus effect" on the modified all_traits_score total
(breed_traits <- all_dogs$breed_traits |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      all_traits_score = rowSums(across(where(is.double)), na.rm = TRUE),
      negative_traits_score = rowSums(across(c(shedding_level:drooling_level, barking_level)), na.rm = TRUE),
      weighted_score = all_traits_score - (2 * negative_traits_score)
    ))

#Want average rank from breed_rank table as you get one for each year for a dog
(breed_ranks <- all_dogs$breed_rank |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      avg_rank = rowMeans(across(where(is.double)), na.rm = TRUE) |> round()
      ) |> 
        select(-breed))

# Join tables again now that one table has a new, added weighted_score column, the other table has a new, added avg_rank column
(traits_with_ranks |> left_join(breed_traits, breed_ranks, by = "key"))

  