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

<<<<<<< HEAD
# Not operator
filter(breed_traits, drooling_level != 5)
filter(breed_traits, !drooling_level %in% c(1, 3, 5)) # NOT IN 1, 3 or 5

=======
>>>>>>> 9e0b2d209813ad83ec9f542e84daa4a736de7109
filter(breed_traits, affectionate_with_family %in% c(4,5) & good_with_young_children %in% c(4,5) & good_with_other_dogs %in% c(4,5))
