
setwd("./docs/")

# What packages do we need?

library(tidyverse)
library(lubridate) #install if needed

# Vectors

v <- c(1, 2, 3, 4, 5)
v
typeof(v)
length(v)
v[1] # Use brackets to access a vector element
v[3]
v <- c(v, 6, 7, 8) # use c to add items
v <- c(1, 2, 3, "4", "5", "6") 
v # You cannot mix types in a vector
v <- c(x = 1, y = 2, z = 3) # Named vector
v
v['y']

files <- list.files(path = "data_raw")
files[1] # Character vector
files[2]
length(files) 

# Lists

lt <- list(x = 1, y = "3", z = 1:5)
lt
lt$z # Access named list elements with $
lt[[1]] # Use double brackets to get list elements
lt[[3]] <- 5:7

# List of lists
lt <- list(lt1 = lt, lt2 = lt, lt3 = lt)
lt$lt1

model <- lm(height ~ mass, data = starwars)
typeof(model)
model$coefficients

# Character data (strings)

ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12")
ds <- ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word", values_drop_na = T, names_prefix = "age_") %>%
  select(-id, -dob)

# Jonah's first words

ds$word

# Length, containing strings, counting strings

ds %>% mutate(word_len = str_length(word), 
              has_b = str_detect(word, "b"), 
              num_b = str_count(word, "b"))

# Matching patterns

ds %>% mutate(word_cleaned = str_remove_all(word, "[:punct:]")) #ROTATE
ds %>% mutate(word_cleaned = str_replace_all(word, "[:punct:]"," ")) #ROTATE
ds %>% mutate(vowel_count = str_count(word, "[aeiou]")) #ROTATE
ds %>% mutate(is_three_letter = str_detect(word, "[:alnum:{3}]")) #ROTATE

fnames <- list.files(path="data_raw", full.names = T)
str_split(fnames, "/") #ROTATE
str_replace_all(fnames, "data_raw/|.csv|vocab","") #ROTATE
str_extract_all(fnames, "\\d{2}.\\d{1}|\\d{2}", simplify = T) #ROTATE
str_extract_all(fnames, "\\.[:alpha:]{1,4}", simplify = T) #ROTATE
str_extract_all(fnames, "(?<=\\/)[:alpha:]+", simplify = T) #ROTATE
str_extract_all(ds$word, boundary("word"), simplify = T) #ROTATE

# Combining strings

age <- seq(from = 12, to = 24, by = .5) 
paste0("data_raw/vocab", age, ".csv") #ROTATE
str_glue("data_raw/vocab{age}.csv") #ROTATE
str_glue("Today's date is {today()} and the time is {now()}") #ROTATE
str_glue("Here is pi to {x} digits: {format(pi, digits = x)}", x = 3) #ROTATE
str_glue("Here is pi to {x} digits: {format(pi, digits = x)}", x = 4) #ROTATE
str_glue("Here is pi to {x} digits: {format(pi, digits = x)}", x = 5) #ROTATE
str_glue("Here is pi to {x} digits: {format(pi, digits = x)}", x = 6) #ROTATE

# Working with dates and times

dates <- tibble(dates1 = c("2017-09-01", "2016-09-01", "2015-09-01", "2014-09-01"),
                dates2 = c("9/01/2018", "09/01/2017", "9/1/2016", "9/1/2015"),
                dates3 = c("March 1, 2019", "May 1, 2018", "January 20, 2017", "February 4, 2020"))

# Parsing dates

dates # ROTATE
dates %>% mutate(dates1_parsed = ymd(dates1)) #ROTATE
dates %>% mutate(dates2_parsed = mdy(dates2)) #ROTATE
dates %>% mutate(dates3_parsed = parse_date(dates3, format = "%B %d, %Y")) #ROTATE

# Time is weird

time_length(mdy("05-27-2022")-mdy("05-27-1983"), "years")
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-27-2022"))), "years")
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-28-2022"))), "years")

# Age math

birth_date <- ymd("2001-09-01", "2000-09-01")
test_date <- ymd("2020-09-01", "2019-09-01")
(test_date-birth_date)/365.25
as.period(interval(start = birth_date, end = test_date), unit = "years")

test_date2 <- test_date - years(1) + days(7)
test_date2
(test_date2-birth_date)/365.25
as.period(interval(start = birth_date, end = test_date2), unit = "years")

# Time Zones

t1 <- as_datetime("2022-01-27 10:39:28", tz = "America/Los_Angeles")
t1
tz(t1)
t2 <- as_datetime("2022-01-27 13:39:28", tz = "America/New_York")
t2-t1
t3 <- as_datetime("2021-10-15 10:39:28", tz = "America/Los_Angeles")
t3
as.period(interval(start = t3, end = t1), unit = "days")
with_tz(t3, "America/New_York")
force_tz(t3, "America/New_York")

# Factors represent categories

x <- c(1, 1, 2, 3, 1, 4)
x <- factor(x, levels = c(1,2,3), labels = c("rarely", "neutral", "frequently"))
x

# Factors

ds <- starwars
unique(ds$eye_color)
ds <- ds %>% mutate(eye_f = factor(eye_color))
fct_count(ds$eye_f, sort = T) %>% slice_head(n = 5)
fct_count(ds$eye_f, prop = T) %>% slice_head(n = 5)

# Setting factors levels

ds <- ds %>% select(name, eye_f)
ds #ROTATE
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"))) #ROTATE
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"), labels = c("bl","br"))) #ROTATE
ds %>% mutate(eye_f2 = fct_lump(eye_f, n = 3)) #ROTATE
ds %>% mutate(eye_type = fct_collapse(eye_f, human = c("blue", "brown"), nonhuman = c("yellow", "red"))) #ROTATE

# Reordering factors levels

fct_count(ds$eye_f) #ROTATE
fct_count(fct_rev(ds$eye_f)) #ROTATE
fct_count(fct_infreq(ds$eye_f)) #ROTATE
fct_count(factor(ds$eye_f, levels = c("red", "brown", "blue"))) #ROTATE

#Types of un-tidy data: Column names are variables

relig_income %>% slice_head(n = 5)

#Solution: tidyr's pivot_longer() function

relig_income %>% 
  pivot_longer(cols = -religion, names_to = "income", values_to = "count")

ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12")
ds %>% print(n = 25) #ROTATE
ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word") %>% print(n = 65) #ROTATE
ds %>% pivot_longer(-(id:dob), names_to = "age", names_prefix = "age_", values_to = "word") %>% print(n = 65) #ROTATE
ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word", values_drop_na = T, names_prefix = "age_") %>% print(n = 65) #ROTATE

ds <- read_delim(list.files(path = "data_headers", full.names = T),
                 col_names = c("field", "value"), col_types = c("cc"), delim = " ", n_max = 7, id = "filename")

# Types of untidy data: Multiple variables in one column

print(ds)

ds <- ds %>% pivot_wider(id_cols = "filename", 
                         names_from = "field", 
                         values_from = "value")
ds

# Types of untidy data: Multiple variables in each cell

ds %>% separate(filename, into = c("dir_part1", "dir_part2", "ppt_file","ppt_block","exten"))

# Types of untidy data: Multiple variables in each cell

ds %>% separate(filename, into = c(NA, NA, "ppt_file","ppt_block",NA))

# Types of untidy data: Multiple variables in each cell

ds %>% separate(filename, into = c(NA, NA, "ppt_file","ppt_block",NA)) %>% 
  mutate(across(-Sex, as.numeric))

# Types of untidy data: Multiple variables in each cell

ds %>% separate(filename, into = c("dir","file"), sep = "/")

# Binding Columns

star_wars_numeric <- starwars %>% select(where(is.numeric)) #87x3
star_wars_char <- starwars %>% select(where(is.character)) #87x8
star_wars_combined <- bind_cols(star_wars_numeric, star_wars_char) #87x11
star_wars_combined

# Binding Rows

s1 <- starwars %>% slice_sample(n = 2) #2x14
s2 <- starwars %>% slice_sample(n = 2) #2x14
s3 <- starwars %>% slice_sample(n = 2) #2x14
star_wars_combined <- bind_rows(s1, s2, s3) #6x14
star_wars_combined

# Joining by key value

fnames <- list.files(path = "data_headers", full.names = T)
header <- read_delim(fnames, col_names = c("field", "value"), col_types = c("cc"), delim = " ", n_max = 7, id = "filename") %>% pivot_wider(id_cols = "filename", names_from = "field", values_from = "value")
trials <- read_tsv(fnames, col_names = c("trial_num", "speed_actual", "speed_resp", "correct"), skip = 8, id = "filename")
header #ROTATE
trials #ROTATE
left_join(header, trials, by = "filename") #ROTATE
left_join(slice_sample(header, n = 5), trials, by = "filename") #ROTATE
left_join(slice_sample(trials, n = 5), header, by = "filename") #ROTATE

