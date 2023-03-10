#Restart R session is ctl+shift+f10

install.packages("tidytuesdayR")

# To tidy up column names as lower case with underscore for gaps
install.packages("janitor")

library(dplyr)
library(janitor)

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
