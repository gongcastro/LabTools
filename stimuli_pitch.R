# stimuli_pitch: pitch analysis by sentence (F0)
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

# adapted from Aaron Albin's PraatR user manual
# http://www2.kobe-u.ac.jp/~albin/praatr/index.html

#### set up ##############################################

# load packages
library(PraatR)   # for acoustic analysis
library(tibble)    # for tidy datasets
library(dplyr)    # for wrangling data
library(readr)    # for working with strings
library(magrittr) # for manipulating data with pipes
library(purrr)    # for working with lists
library(readxl)   # for importing Excel data
library(ggplot2)  # for data visualization 

# set paths
file.names  <- list.files("Stimuli/Acoustic/Sentences") # locate .wav files
file.paths  <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Stimuli/Acoustic/Sentences/", file.names)                                             # build path for each file
sentences   <- file.names %>% sub("fam_", "", .) %>% sub(".wav", "", .) # sentence id
n           <- length(file.names) # number of files

# build paths for .Formant files
pitch.paths       <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Data/Stimuli/Pitch-tier/", file.names)
pitch.tier.paths  <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Data/Stimuli/Pitch/", file.names)

#### get fundamental frequency/pitch ################################

pitch.arguments <- list(0.001, # time step in seconds (0.0 = auto)
                        95,    # pitch floor in Hz
                        370)   # pitch ceiling in Hz

# create a .Pitch file for each audio
mapply("To Pitch...",
       FUN = praat,
       arguments = list(pitch.arguments),  
       input = file.paths,                 # paths to the audio files
       output = pitch.paths,               # paths for the resulting paths of the .Pitch files
       overwrite = TRUE)                   # overwrite the files in the outcome folder if the function is run again

# create a .PitchTier file for each audio (similar to .Pitch, but only Time and F0 are retrieved)
mapply("Down to PitchTier",
       FUN = praat,
       input = pitch.paths,                    # paths to the .Pitch files
       output = pitch.tier.paths,              # paths for the resulting .PitchTier files
       overwrite = TRUE,                       # overwrite the files in the outcome folder if the function is run again
       filetype = "headerless spreadsheet")    # this format is easier to read


# import the .PitchTier files containing the F0 data
pitch <-
  map(pitch.tier.paths, ~read.table(., col.names = c("time", "f0"))) %>% # read the .Pitch object from the pitch.tier.path 
  map(~as_tibble(.)) %>%                                                 # trasnform it to tibble (easier to read)
  map2(., sentences, ~mutate(., sentence = .y)) %>%                      # add the stimuli id (sentence) to identify each row 
  map(., ~mutate(.,                                                      # add word
                 word = case_when(
                   grepl("gon", sentence) ~ "gon",                     
                   grepl("mus", sentence) ~ "mus",
                   grepl("for", sentence) ~ "for",
                   grepl("pul", sentence) ~ "pul",
                   TRUE                   ~ "NA"),
                 language = case_when(
                   grepl("cat", sentence) ~ "catalan", # add sentence
                   grepl("spa", sentence) ~ "spanish",
                   TRUE                   ~ "NA"),
                 id = parse_number(sentence),                            # add sentence number (id)
                 condition = case_when(word %in% c("gon", "mus") ~ "gonmus",       # add condition
                                       TRUE                      ~ "forpul"))) %>%
  set_names(., sentences)                                                         # name each dataset as file

# f0 local maxima
pitch.maxima <-
  pitch %>%
  map(., "f0") %>%
  map(., ~which(c(NA, ., NA) > c(., NA, NA) & c(NA, ., NA) > c(NA, NA, .))) %>% # get indices of local maxima
  map(`-`, 1) %>%
  map2(pitch, ., ~slice(.x, .y)) %>%
  map_df(., bind_rows) # merge datasets of all audios into one

# pitch (merged dataset)
pitch <- map_df(pitch, bind_rows)

# word onset
word.pitch <-
  read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences") %>%
  select(word, id, language, word_onset, word_offset, phoneme_onset, phoneme_offset)

#### export data ##########################################################
write.table(pitch, "Data/Stimuli/Pitch/stimuli_pitch.txt", sep = "\t", row.names = FALSE)
write.table(pitch.maxima, "Data/Stimuli/Pitch/stimuli_pitch-maxima.txt", sep = "\t", row.names = FALSE)

#### visualise data #######################################################
ggplot() +
  geom_rect(data = word.pitch, aes(xmin = word_onset, xmax = word_offset, ymin = 0, ymax = Inf), fill = "red", alpha = 0.5) +
  geom_line(data = pitch, aes(time, f0)) +
  labs(x = "Time (s)", y = "F0 (Hz)") +
  scale_y_continuous(breaks = seq(0, 3000, 500)) +
  theme(text = element_text(size = 12)) +
  facet_grid(id ~ word + language) +
  ggsave("Figures/stimuli_pitch.png")
