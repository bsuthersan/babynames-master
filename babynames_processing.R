library(babynames)
library(tidyverse)
library(stringr)

#Read in the data
babynames <- babynames::babynames
namemeanings <- read_csv("https://raw.githubusercontent.com/bsuthersan/babynames/master/namemeanings.csv")

#Process and merge

namemeanings <- namemeanings[ ,1:2]
colnames(namemeanings) <- c("Name", "Meaning")

namemeanings <- namemeanings %>%
  mutate(Name = str_to_title(Name))

babynames <- babynames %>%
  left_join(namemeanings, by = c("name"="Name"))

