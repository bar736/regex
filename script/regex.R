library(tidyverse)
read_csv('data/bird_counts.csv')
bird_counts <- read_csv('data/bird_counts.csv')

# | OR - Allow alternative matches

bird_counts %>%
  mutate(is_colourful = str_detect(species, "Rosella|Parrot"))

bird_counts %>%
  mutate(not_cockatoo = str_detect(species, "Rosella|Parrot|Magpie"))

# [] alternatives for a single character

bird_counts %>%
  mutate(l_vowel = str_detect(location, "l[aeiou]"))

# [abcde] [a-e]
# [0123456789] [0-9]
# [a-zA-Z]

bird_counts %>%
  mutate(double_digits = str_detect(count, "[0-9][0-9]"))

#[^a-z] Any character but not the lowercase letters

bird_counts %>%
  mutate(non_numeric = str_detect(count, '[^0-9]'))

# Alternatives: | [] [^]

#What is a regular expression pattern that would match:
#Text containing either Magpie or cockatoo in species
#Text with either an i, o or u character in location
#Any uppercase text character in counts
#Any character except for 3, 4, or 5 in dates

bird_counts %>% 
  mutate(Q1 = str_detect(species, "Magpie|cockatoo"))

bird_counts %>% 
  mutate(Q2 = str_detect(location, "[iou]"))

bird_counts %>% 
  mutate(Q3 = str_detect(count, "[A-Z]"))

bird_counts %>% 
  mutate(Q4 = str_detect(date, "[^3-5]"))


#Grouping and anchoring: ()

bird_counts %>% 
  mutate(lakes = str_detect(location, "L(ake|BG)")) # will match Lake or LBG

bird_counts %>% 
  mutate(gang_gangs = str_detect(species, "Gang(-| )[Gg]ang"))

# str_detect() T/F
# str_extract() give back the text that matches or NAs


bird_counts %>% 
  mutate(gang_gangs = str_extract(species, "Gang(-| )[Gg]ang"))



