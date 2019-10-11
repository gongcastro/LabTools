# stimuli_spectra: Generate a spectrum for each sentence
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Speech Acquisition and Perception, Pompeu Fabra University

# adapted from Aaron Albin's PraatR user manual
# http://www2.kobe-u.ac.jp/~albin/praatr/index.html

#### set up ################################################

# load packages
library(PraatR)   # for analysing audio files
library(audio)    # for analysing audio files
library(magrittr) # for working with pipes
library(ggplot2)  # for visualising data
library(viridis)  # for plotting colours
library(dplyr)    # for manipulating data
library(tibble)   # for tidy data presentation
library(reshape2) # for manipulating datasets 
library(readxl)   # for importing Excel files
library(readr)    # for importing data
library(purrr)    # for working with lists

# load functions
source("Code/Spectrogram.R")

# specify paths
file.names <- list.files("Stimuli/Acoustic/sentences", ) %>% sub("fam_", "", .) %>% sub(".wav", ".", .) # locate .wav files
file.paths <- list.files("Stimuli/Acoustic/sentences", full.names = TRUE)                             # build path for each file

#### get frequency and intensity across time ####################
audio   <- map(file.paths, load.wave) %>% set_names(., sentences)          # name each element of the audio list
n.audio <- map(audio, length)                                              # get number of time points in each audio
rate    <- audio[[1]]$rate                                                 # extract sample rate (same for all audios) 
time    <- map(audio, .f = function(x) as.vector(1:length(x)/(rate*1000))) # reconstruct time domain from sample rate and number of measurements
amplitude <- map(audio, .f = ~as.vector(.x))
  
# define target areas (where target words are spoken)
word.timing <- read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences")

# build a table
spectra <-
  mapply(audio,
         FUN = Spectrogram,                 # The vector of samples for the audio. Also accepts file paths.
         MoreArgs = list(DynamicRange = 40, # Adjust for each file to make the observed patterns clearer.
                         col = "greyscale", # To make look like Praat. If omitted, uses color palette.
                         ylim = c(0, 6000), # Zoom into the main frequency range used for speech.
                         main = sentences,
                         plot = FALSE)) %>%
  map(., melt) %>%
  map(., as_tibble) %>%
  set_names(., sentences) %>%
  map(., ~rename(., time = Var1, frequency = Var2, intensity = value)) %>%
  map2(.x = ., .y = sentences,
          ~mutate(., sentence = .y,                                           # add sentence
                   word = case_when(grepl("gon", sentence) ~ "gon",           # add word conditionally
                                    grepl("mus", sentence) ~ "mus",
                                    grepl("for", sentence) ~ "for",
                                    grepl("pul", sentence) ~ "pul"),
                   language = case_when(grepl("cat", sentence) ~ "catalan",   # add language conditionally
                                        grepl("spa", sentence) ~ "spanish"),  # add ID conditionally
                   id = parse_number(sentence),
                  time = time/1000)) %>%
  map_df(., bind_rows)  

#### export data #################################################
write.table(spectra, "Data/Stimuli/Amplitude/spectra.txt", sep = "\t", row.names = FALSE)

#### visualization ###############################################
ggplot() +
  geom_raster(data = spectra, aes(x = time, y = frequency, fill = intensity)) +
  geom_rect(data = word.timing, aes(xmin = word_onset, xmax = word_offset, ymin = -Inf, ymax = Inf), fill = "white", alpha = 0.3) +
  labs(x = "Time (s)", y = "Frequency (Hz)", fill = "Intensity") +
  scale_fill_viridis(option = "viridis") +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_blank(),
        legend.position = "top") +
  facet_grid(id ~ word + language) +
  ggsave("Figures/stimuli_spectra.png", width = 10, height = 6)
  


