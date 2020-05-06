library(tidyverse)
fruit
length(fruit)
fruit[1]
fruit[length(fruit)]

str_detect(fruit, pattern = 'apricot')
fruit[2]

str_detect(fruit, "apple") #2 trues
str_detect(fruit, "rine") #3 trues
str_detect(fruit, "Banana") #0 trues
str_detect(fruit, "berry") #14 trues
str_detect(fruit, " berry") #2 trues


sum(str_detect(fruit, "apple"))
sum(str_detect(fruit, "rine"))
sum(str_detect(fruit, "Banana"))
sum(str_detect(fruit, "berry"))
sum(str_detect(fruit, " berry"))


sum(str_detect(fruit, "melon"))
sum(str_detect(fruit, " fruit"))
sum(str_detect(fruit, "cl"))

#how to find fruits that contain currant, but ignoring the plain currant

fruit
sum(str_detect(fruit, "currant")) #3
sum(str_detect(fruit, ".currant")) #2, Only matches blackcurrant and redcurrant

#using wild cards "."
#How many fruits have the letter e three characters after the letter a?

sum(str_detect(fruit, "a..e"))  # 5 fruits


#Working with text in data frames and patterns to
#find how many have apples on it

framed_fruit <- tibble(fruit_name = fruit)

framed_fruit %>% 
  mutate(has_apple = str_detect(fruit_name, "apple"))   #or

framed_fruit %>% 
  filter(str_detect(fruit_name, "apple"))


# All currants
framed_fruit %>% 
  filter(str_detect(fruit_name, "currant"))
# Not the plain currant
framed_fruit %>% 
  filter(str_detect(fruit_name, ".currant"))
