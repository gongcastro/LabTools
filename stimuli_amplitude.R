# stimuli_amplitude: Generate amplitude data for each sentence
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

# adapted from Aaron Albin's PraatR user manual
# http://www2.kobe-u.ac.jp/~albin/praatr/index.html

##### set up ############################################

# load packages
library(magrittr) # for manipulating data with pipes
library(ggplot2)  # for data visualization 
library(audio)    # for acoustic analysis
library(tibble)   # for tidy data presentation
library(readr)    # for working with strings
library(readxl)   # for importing Excel files
library(dplyr)    # for wrangling data
library(purrr)    # for working with lists

# specify paths
sentences <- list.files("Stimuli/Acoustic/Sentences") %>% sub("fam_", "", .) %>% sub(".wav", "", .) # locate .wav files
file.paths <- list.files("Stimuli/Acoustic/Sentences", full.names = TRUE) # build path for each file

#### get durations ##############################################
amplitude   <- map(file.paths, load.wave) %>% set_names(sentences) # extract amplitude from each audio
n.amplitude <- map(amplitude, length)                              # number of measurements in each audio
sample.rate <- amplitude[[1]]$rate                                 # get the sample rate of each audio
time <- map(n.amplitude, .f = function(x) (1:x/sample.rate))       # get time domain of each audio from sample rate and number of measurements

# compute the time course of amplitude in each audio 
wave <-
  map(amplitude, as.numeric) %>%                                                       # get the amplitude
  map(~tibble(amplitude = .)) %>%
  map2(time, ~mutate(., time = .y)) %>%
  map2(sentences,                                                                      # add sentence
       ~mutate(.,
               word = case_when(grepl("gon", .y) ~ "gon",                              # add word conditionally
                                grepl("mus", .y) ~ "mus",
                                grepl("for", .y) ~ "for",
                                grepl("pul", .y) ~ "pul"),
               language = ifelse(grepl("cat", .y), "catalan", "spanish"),              # add language conditionally
               id = parse_number(.y),                                                  # add id conditionally
               condition = ifelse(word %in% c("gon", "mus"), "gonmus", "forpul"))) %>% # add condition conditionally
  map_df(bind_rows)                                                                    # merge al datasets in the list into one big dataset

# word onset
word.timing <-
  read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences") %>%
  select(word, id, language, word_onset, word_offset, phoneme_onset, phoneme_offset)

#### export data ##########################################################
write.table(wave, "Data/Stimuli/Amplitude/stimuli_amplitude.txt", sep = "\t", row.names = FALSE)

#### visualise data #######################################################
ggplot() +
  geom_rect(data = word.timing, aes(xmin = word_onset, xmax = word_offset, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.5) +
  geom_line(data = wave, aes(x = time, y = amplitude)) +
  labs(x = "Time (s)", y = "Amplitude (dB)", title = "Amplitude plotted against time", subtitle = "Red boxes indicate target word onset and offset") +
  theme(text = element_text(size = 12)) +
  facet_grid(id ~ word + language) +
  ggsave("Figures/stimuli_amplitude.png")
