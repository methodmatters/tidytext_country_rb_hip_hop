# load packages
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)

# read in raw data
# specify here the directory where the raw data are kept
in_dir <- "/directory/"
# raw data are available in this Github repo: 
# https://github.com/methodmatters/scattertext_country_rb_hip_hop
raw_data <- read_csv(paste0(in_dir, "country_rbhh_blog.csv"))

# function to clean up the raw data - in keeping with scattertext analysis
clean_raw_data <- function(data_f){
  # define junk to remove
  to_remove = c('\u200E', '', "Â€~", "repeat chorus", "instrumental")
  patterns <- str_c(to_remove, collapse="|")
  # remove above elements from all of the lyrics columns
  # https://stackoverflow.com/questions/54818690/case-sensitivity-in-conditional-str-replace-all-in-r
  data_f$lyrics <- str_replace_all(data_f$lyrics, regex(patterns, ignore_case = TRUE), "") 
  data_f$lyrics_clean <- str_replace_all(data_f$lyrics_clean, regex(patterns, ignore_case = TRUE), "") 
  data_f$lyrics_scrubbed <- str_replace_all(data_f$lyrics_scrubbed, regex(patterns, ignore_case = TRUE), "")
  
  # replace weird apostrophe / single quotes
  data_f$lyrics <- gsub("’", "'", data_f$lyrics)
  data_f$lyrics_clean <- gsub("’", "'", data_f$lyrics_clean)
  data_f$lyrics_scrubbed <- gsub("’", "'", data_f$lyrics_scrubbed)
  
  # remove duplicates on song and artist
  # and select columns to keep
  data_f <- data_f %>% distinct(song, artist, .keep_all = TRUE) %>% 
      select(song, artist, genre, lyrics_clean, lyrics_scrubbed)

  return(data_f)

}


# apply the function to the raw data
# to get our cleaned df for analysis
clean_data <- clean_raw_data(raw_data)

# remove raw data file
rm(raw_data)

str(clean_data)

# what does it look like?
head(clean_data) 

# genre count
table(clean_data$genre)

# Step 1: Lyrics Texts to Bigrams

# make a tidy df with the bigrams
song_bigrams <- clean_data %>% select(song, genre, lyrics_clean) %>% 
  # extract bigrams (2-word combinations) from the "lyrics_clean" text column
  unnest_tokens(bigram, lyrics_clean, token = "ngrams", n = 2) %>%
  # count - how many times does each bigram appear in each song? 
  group_by(song, bigram) %>%
  mutate(n_bigram_per_song = n()) %>%
  ungroup() %>%
  # count - how many times does each bigram appear in each genre?
  group_by(genre, bigram) %>%
  mutate(n_bigram_per_genre = n())

# what does it look like?
dim(song_bigrams)
head(song_bigrams)


# Step 2: Counting Gender Bigrams Per Genre
bigram_counts <- song_bigrams %>%
  # we *only* want bigrams that appear in more than 1 song
  # we can make this selection by only keeping rows
  # where n_bigram_per_song is not equal to n_bigram_per_genre
  # (if these are equal, it means that the bigram only appears in 1 song
  # in the given genre)
  filter(n_bigram_per_song != n_bigram_per_genre) %>%
  # count the number of occurrences of each bigram in each genre
  count(genre, bigram, sort = TRUE) %>%
  # split up the bigrams into two columns, one per word
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # only keep bigrams that start with he or she
  filter(word1 %in%  c("he", "she")) %>%
  # removes some repetition - she she
  filter(word1 != word2) %>%
  # rename the bigram count column "total"
  rename(total = n)

# what does it look like?
head(bigram_counts)


# Step 3: Visualize Common Words Paired with "She" and "He"

# colors inspired by:
# https://blog.datawrapper.de/gendercolor/
# #8624f5 - for women
# #1fc3aa - for men

hex_pal = c('#8624f5' , '#1fc3aa')

# country
# https://juliasilge.com/blog/reorder-within/
bigram_counts %>%
  # select only country songs
  filter(genre == 'Country') %>%
  # group by first word of bigram
  group_by(word1) %>%
  # and keep only the 10 most-frequently
  # occurring bigrams
  slice_max(total, n = 10) %>%
  ungroup %>%
  # set factor levels so that graph for women appears 
  # on the left
  # and re-order the dataset - by second bigram word,
  # then the total number of occurrences and first bigram word (she/he)
  mutate(word1 = factor(word1, levels = c('she', 'he')),
         word2 = reorder_within(word2, total, word1)) %>%
  # set up our plot with ggplot
  ggplot(aes(word2, total, fill = word1)) +
  # we want a bar chart with no legent
  geom_col(show.legend = FALSE) +
  # separate panels for she & he, panels
  # can have their own scales 
  # (there are more words for women than men)
  facet_wrap(~word1, scales = "free_y") +
  # set the labels for the chart
  labs(x = NULL,
       y = "Number of occurences following 'she' vs. 'he'",
       title = "Most common words paired with 'she' and 'he' in popular country songs",
       subtitle = "Billboard Top 100 Year End Country Songs (1990-2021)",
       caption = "(N = 2,197)") +
  # flip x and y in chart
  coord_flip() +
  # black and white theme (more basic than standard one)
  theme_bw() +
  # set the colors 
  scale_fill_manual(values = hex_pal) +
  # from tidytext - Reorder a column before plotting with faceting, 
  # such that the values are ordered within each facet.
  # (re-order words separately for she/he facet panels)
  scale_x_reordered() +
  # set limits for axis showing the total number of occurrences
  scale_y_continuous(expand = c(0,5))


# R&B/Hip-Hop
# https://juliasilge.com/blog/reorder-within/
bigram_counts %>%
  # select only R&B/hip-hop songs
  filter(genre == 'R&B/Hip-Hop') %>%
  # group by first word of bigram
  group_by(word1) %>%
  # and keep only the 10 most-frequently
  # occurring bigrams
  slice_max(total, n = 10) %>%
  ungroup %>%
  # set factor levels so that graph for women appears 
  # on the left
  # and re-order the dataset - by second bigram word,
  # then the total number of occurrences and first bigram word (she/he)
  mutate(word1 = factor(word1, levels = c('she', 'he')),
         word2 = reorder_within(word2, total, word1)) %>%
  # set up our plot with ggplot
  ggplot(aes(word2, total, fill = word1)) +
  # we want a bar chart with no legent
  geom_col(show.legend = FALSE) +
  # separate panels for she & he, panels
  # can have their own scales 
  # (there are more words for women than men)
  facet_wrap(~word1, scales = "free_y") +
  # set the labels for the chart
  labs(x = NULL,
       y = "Number of occurences following 'she' vs. 'he'",
       title = "Most common words paired with 'she' and 'he' in popular R&B/hip-hop songs",
       subtitle = "Billboard Top 100 Year End R&B/Hip-Hop Songs (1990-2021)",
       caption = "(N = 2,423)") +
  # flip x and y in chart
  coord_flip() +
  # black and white theme (more basic than standard one)
  theme_bw() +
  # set the colors 
  scale_fill_manual(values = hex_pal) +
  # from tidytext - Reorder a column before plotting with faceting, 
  # such that the values are ordered within each facet.
  # (re-order words separately for she/he facet panels)
  scale_x_reordered() +
  # set limits for axis showing the total number of occurrences
  scale_y_continuous(expand = c(0,10))

# Step 4: Visualize Differences Between Words Paired with "She" and "He"

## Inspiration from these sources:

### https://juliasilge.com/blog/gender-pronouns/
### https://github.com/juliasilge/women-in-film
### https://pudding.cool/2017/08/screen-direction/
### https://www.tidytextmining.com/twitter.html#comparing-word-usage
### https://yalagiants.netlify.app/2019/07/log-odds-ratio-vs-tf-idf-vs-weighted-log-odds/
### http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/

# prepare the data

# country
word_ratios_country <- bigram_counts %>% 
  # select only country music songs
  filter(genre == 'Country') %>% 
  # group by music genre and second bigram word
  group_by(genre, word2) %>%
  # filter any bigrams that appear fewer than 10 times
  filter(sum(total) > 10) %>% 
  ungroup() %>%
  # long to wide transformation, making separate columns
  # for she and he, with the total number of occurrences per gender
  # as data points. Fill empty cells with zero
  spread(word1, total, fill = 0) %>% 
  # https://www.tidytextmining.com/twitter.html#comparing-word-usage
  # ratio, for each bigram: total uses of bigram (+1),
  # divided by the sum of all of the bigram uses (+1)
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  # divide she ratio by he ratio, and take the log2 of the result
  # we use log2 because the interpretability / logic is better
  # with log2 vs. log (increase of 1 indicates doubling of original metric)
  mutate(logratio = log2(she / he)) %>%
  # sort the dataframe by the log ratio variable
  arrange(desc(logratio)) 

# R&B/hip-hop
word_ratios_rbhh <- bigram_counts %>% 
  filter(genre == 'R&B/Hip-Hop') %>% 
  # group by music genre and second bigram word
  group_by(genre, word2) %>%
  # filter any bigrams that appear fewer than 10 times
  filter(sum(total) > 10) %>% 
  ungroup() %>%
  # long to wide transformation, making separate columns
  # for she and he, with the total number of occurrences per gender
  # as data points. Fill empty cells with zero
  spread(word1, total, fill = 0) %>% 
  # https://www.tidytextmining.com/twitter.html#comparing-word-usage
  # ratio, for each bigram: total uses of bigram (+1),
  # divided by the sum of all of the bigram uses (+1)
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  # divide she ratio by he ratio, and take the log2 of the result
  # we use log2 because the interpretability / logic is better
  # with log2 vs. log (increase of 1 indicates doubling of original metric)
  mutate(logratio = log2(she / he)) %>%
  # sort the dataframe by the log ratio variable
  arrange(desc(logratio)) 


# make the plots

# colors
# inspired by: 
# https://blog.datawrapper.de/gendercolor/
# #8624f5 - for women
# #1fc3aa - for men

hex_pal = c('#8624f5' , '#1fc3aa')

# Country Plot
word_ratios_country %>%
  # create a column with the absolute value of the logratio
  # calculated above - we will use this to select which
  # bigrams to plot
  mutate(abslogratio = abs(logratio)) %>%
  # group by whether the log ratio scores are positive or negative
  # (this is a defacto selection of which bigrams appear more with she vs. he)
  group_by(logratio < 0) %>%
  # for each group (positive/negative log ratio scores)
  # take the 15 bigrams with largest absolute value log ratios
  top_n(15, abslogratio) %>% 
  ungroup() %>%
  # reorder the dataset by the second word of the bigram and the log ratio score
  mutate(word = reorder(word2, logratio)) %>%
  # set up the ggplot2 plot
  ggplot(aes(word, logratio, fill =  logratio < 0)) +
  # we want a bar chart
  geom_bar(stat = 'identity') +
  # flip the x and y coordinates
  coord_flip() +
  # set up the axis labels and titles
  labs(x = NULL,
       y = "Relative appearance after 'she' compared to 'he'",
       title = "Words paired with 'she' and 'he' in popular country songs",
       subtitle = "Billboard Top 100 Year End Country Songs (1990-2021)",
       caption = "(N = 2,197)") +
  # specify our custom colors and the legend labels
  scale_fill_manual(values = hex_pal, name = "", 
                    labels = c("More 'she'", "More 'he'")) +
  # choose black and white theme
  theme_bw() +
  # set up x axis (below it says "y" but we've flipped them with coord_flip() above)
  # because an increase of 1 in log2 scores means a doubling of the original 
  # metric, we can indicate this in our axis - e.g. "1" on the original scale
  # becomes 2x in our axis labels...
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", 
                                "Same", "2x", "4x", "8x")) 



# RBHH plot 
word_ratios_rbhh %>%
  # create a column with the absolute value of the logratio
  # calculated above - we will use this to select which
  # bigrams to plot
  mutate(abslogratio = abs(logratio)) %>%
  # group by whether the log ratio scores are positive or negative
  # (this is a defacto selection of which bigrams appear more with she vs. he)
  group_by(logratio < 0) %>%
  # for each group (positive/negative log ratio scores)
  # take the 15 bigrams with largest absolute value log ratios
  top_n(15, abslogratio) %>% 
  ungroup() %>%
  # reorder the dataset by the second word of the bigram and the log ratio score
  mutate(word = reorder(word2, logratio)) %>%
  # set up the ggplot2 plot
  ggplot(aes(word, logratio, fill =  logratio < 0)) +
  # we want a bar chart
  geom_bar(stat = 'identity') +
  # flip the x and y coordinates
  coord_flip() +
  # set up the axis labels and titles
  labs(x = NULL,
       y = "Relative appearance after 'she' compared to 'he'",
       title = "Words paired with 'she' and 'he' in popular R&B/Hip-Hop songs",
       subtitle = "Billboard Top 100 Year End R&B/Hip-Hop Songs (1990-2021)",
       caption = "(N = 2,423)") +
  # specify our custom colors and the legend labels
  scale_fill_manual(values = hex_pal, name = "", 
                    labels = c("More 'she'", "More 'he'")) +
  # choose black and white theme
  theme_bw() +
  # set up x axis (below it says "y" but we've flipped them with coord_flip() above)
  # because an increase of 1 in log2 scores means a doubling of the original 
  # metric, we can indicate this in our axis - e.g. "1" on the original scale
  # becomes 2x in our axis labels...
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", 
                                "Same", "2x", "4x", "8x")) 

# Addendum: Check out bigrams in context

## Country

### She

# she cranks?
# https://en.wikipedia.org/wiki/She_Cranks_My_Tractor
# this is a real thing
clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she cranks", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she make", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she needs", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she wants", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she likes", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she gives", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she fell", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she comes", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she came", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she calls", lyrics_clean, ignore.case = TRUE)) %>% message()

### He

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he stood", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he stood up to say", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he did ", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he made", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he thought", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he saw", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he looked", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he always", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he never", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he told", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he talks", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("he makes", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'Country') %>% select(lyrics_clean) %>% 
  filter(grepl("she fell", lyrics_clean, ignore.case = TRUE)) %>% message()

## R&B/Hip Hop

### She

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she she", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she my", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she twerkin", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she bad", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she workin", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she lookin", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("she rockin", lyrics_clean, ignore.case = TRUE)) %>% message()

### He

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he makes", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he should", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he think", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he feel", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he wasn't", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he wouldn't", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he can't", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he doesn't", lyrics_clean, ignore.case = TRUE)) %>% message()

clean_data %>%  filter(genre == 'R&B/Hip-Hop') %>% select(lyrics_clean) %>% 
  filter(grepl("he knows", lyrics_clean, ignore.case = TRUE)) %>% message()

