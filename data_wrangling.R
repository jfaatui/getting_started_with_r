install.packages("tidytuesdayR")

library(dplyr)

tuesdata <- tidytuesdayR::tt_load("2022-02-01")

breed_traits <- tuesdata$breed_traits

select(breed_traits, "Breed")

select(breed_traits, "Coat Length")

select(breed_traits, c(1, 3, 4))

select(breed_traits, c("Breed", "Coat Length"))

select(breed_traits, 1, 2, 6:10)

select(breed_traits, "Breed", "Affectionate With Family", "Drooling Level":"Openness To Strangers")
