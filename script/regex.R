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

### Functions
# str_detect() T/F
# str_extract() give back the text that matches or NAs


bird_counts %>% 
  mutate(gang_gangs = str_extract(species, "Gang(-| )[Gg]ang"))


#Anchoring
# ^ - start $ - end

bird_counts %>% 
  mutate(ainslie = str_detect(location, "Ainslie"))   #TRUE for any line with Ainslie

bird_counts %>% 
  mutate(starts_ainslie = str_detect(location, "^Ainslie"))  #TRUE for any line with Ainslie at the beggining

bird_counts %>% 
  mutate(starts_ainslie = str_detect(location, "Ainslie$"))  #TRUE for any line with Ainslie at the end

#Using groupings with (), create a regex pattern that will match all the mountains 
#in the location column using str_detect().

bird_counts %>% 
  mutate(all_mountains = str_detect(location, "M(ount|tn|t|t.)"))

#Modify your pattern above with an anchoring to exclude ‘Black Mtn’ from the matched values.

bird_counts %>% 
  mutate(all_mountains = str_detect(location, "^M(ount|tn|t|t.)"))

# Quantifying {}

bird_counts %>% 
  mutate(double_o = str_detect(species, "o{2}"))

# Range {2,5} Match two to 5 times
# {2,} Match at least two times
# {,4} Match up to four times

#Shortcut quantifications
# ? - {0,1}

bird_counts %>% 
  mutate(short_mts = str_extract(location, "Mtn?"))

bird_counts %>% 
  mutate(all_mts = str_extract(location, "M(oun)?tn?"))

# * - {0,}   any number of times and maybe absent
# + - {1,}   any number of times but at least one

bird_counts %>% 
  mutate(all = str_extract(species, ".+"))

bird_counts %>% 
  mutate(gg = str_extract(species, "(.ang )+")) %>%  select(-count)

#Matching dates
#Using the birds data, write a regex pattern using str_detect() or str_extract() 
#that will match values in the date column that start with two digits, 
#followed by a separator (/ or -).

bird_counts %>% 
  mutate(two_digit_date = str_extract(date, "^[0-9]{2}[/|-]"))

#Can you modify this pattern so that it matches dates with
#two digits for the day, month, and year?

bird_counts %>% 
  mutate(two_digit_date = str_extract(date, "^[0-9]{2}[/|-][0-9]{2}[/|-][0-9]{2}$"))

bird_counts %>% 
  mutate(two_digit_date = str_extract(date, "^([0-9]{2}[/-]?){3}$"))

#How to match things that are special characters (. etc)
# Use Escaping with \

bird_counts %>% 
  mutate(has_dot = str_detect(location, "."))

bird_counts %>% 
  mutate(has_dot = str_detect(location, "\\."))

# String "\\." -> pattern \. -> matches text .
# So to find a backslash you need "\\\\"    hahahah

bird_counts %>% 
  mutate(has = str_detect(count, "\\("))


# \\s - whitspace
# \\S - not whitespaces

# \\d - digits [0,9]

# \\w - any word     \\w = [a-zA-Z0-9_]

bird_counts %>% 
  mutate(ful_year = str_extract(date, "\\d{4}"))

bird_counts %>% 
  mutate(two_digits = str_extract(date, "^(\\d{2}\\D?){3}$"))

bird_counts %>% 
  mutate(two_words = str_extract(location, "^\\S+\\s\\S+"))


# \\b - word boundary

bird_counts %>% 
  mutate(with_space =  str_extract(species, "\\sgang\\s"))

bird_counts %>% 
  mutate(with_space =  str_extract(species, "\\bgang\\b"))

# Backreferences
# \\1
bird_counts %>% 
  mutate(doubled = str_extract(species, "(.)\\1"))  #it things with a repeated letter

#Extract the doubled letters from the location column of the birds data using backreferences


bird_counts %>% 
  mutate(doubled=str_extract(location, "(\\w)\\1"))

bird_counts %>% 
  mutate(palindrome = str_extract(date, "(\\d)(\\d)\\D\\2\\1"))


#Have a look at the output from the following two patterns:
bird_counts %>% 
  mutate(
    pattern_one = str_extract(species, "(\\w)(\\w)\\1"),
    pattern_two = str_extract(species, "(\\w)(\\w)\\2")
  )


#REPLACING TEXT with str_replace()

bird_counts %>% 
  mutate(fixed_date = str_replace(date, "Last Sunday", "19/1/20"))

bird_counts %>% 
  mutate(first = str_replace(location, "^\\w+", "First"))

#Replace the “Eight” in the count column with the character “8”
#Replace all hyphens (“-“) in the species column with spaces (“ “)
#Replace the digits for the year in the date column with the text “Year”

bird_counts %>% 
  mutate(count_number = str_replace(count, "Eight", "8"))

bird_counts %>% 
  mutate(no_hyphens = str_replace(species, "-", " "))

bird_counts %>% 
  mutate(year = str_replace(date, "\\d+$", "Year"))

#Replace all
bird_counts %>% 
  mutate(no_caps = str_replace_all(location, "[A-Z]", "_"))

#Delete things with str_remove

bird_counts %>% 
  mutate(devowel = str_remove_all (species, "[aeiou]"))


bird_counts %>% 
  mutate(fancy = str_replace_all(species, "(\\w+)", "*\\1*"))

bird_counts %>% 
  mutate(fancy = str_replace_all(species, "^(\\w).+(\\w)$", "First: \\1, Last: \\2"))

  