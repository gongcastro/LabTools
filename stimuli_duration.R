# Segmentation: duration of sentences
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

# adapted from Aaron Albin's PraatR user manual
# http://www2.kobe-u.ac.jp/~albin/praatr/index.html

#### set up #############################################

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
file.names <- list.files("Stimuli/Acoustic/03_scaled") # locate .wav files
file.paths <- paste0("Stimuli/Acoustic/03_scaled/", file.names) # build path for each file
sentences  <- sub(".wav", "", file.names)                                                   # sentence id

#### build dataset ######################################

# get durations
amplitude   <- map(file.paths, load.wave) %>% set_names(sentences) # extract amplitude from each audio
n.amplitude <- map(amplitude, length)                              # number of measurements in each audio
sample.rate <- amplitude[[1]]$rate                                 # get the sample rate of each audio
time <- map(n.amplitude, .f = function(x) (1:x/sample.rate))       # get time domain of each audio from sample rate and number of measurements

# build dataset
duration <-
  read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences") %>%
  mutate(duration = unlist(map(time, last)))

#### export data ########################################
write.table(duration, "Data/stimuli_duration.txt", sep = "\t", row.names = FALSE)

#### plot durations ########################################
ggplot(duration, aes(x = word, y = duration)) +
  geom_point(size = 3, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "crossbar", colour = "red") +
  labs(x = "Target word", y = "Duration (s)",
       title = "Sentence duration by word and language",
       subtitle = "Red boxes indicate mean and standard error of the mean") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 12)) +
  facet_wrap(.~language) +
  ggsave("Figures/stimuli_duration.png")
