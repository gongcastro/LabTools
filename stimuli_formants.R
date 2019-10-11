# stimuli_formants: Extract F1 and F2 data from each sentence
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

# adapted from Aaron Albin's PraatR user manual
# http://www2.kobe-u.ac.jp/~albin/praatr/index.html

#### set up #############################################

# load packages
library(PraatR)    # for acoustic analysis
library(tibble)    # for tidy data presentation
library(tidyr)     # for manipulating datasets
library(dplyr)     # for manipulating data
library(readxl)    # for importing Excel files
library(reshape2)  # for manipulating datasets
library(readr)     # for working with strings
library(purrr)     # for working with lists
library(magrittr)  # for manipulating data with pipes
library(ggplot2)   # for data visualization 

# set paths
file.names  <- list.files("Stimuli/Acoustic/Sentences") # locate .wav files
file.paths  <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Stimuli/Acoustic/Sentences/", file.names)                                             # build path for each file
sentences   <- file.names %>% sub("fam_", "", .) %>% sub(".wav", "", .) # sentence id
n           <- length(file.names) # number of files

# build paths for .Formant files
formant.filenames <- sub(".wav", ".Formant", file.names)
formant.paths     <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Data/Stimuli/Formants/", formant.filenames)

# build paths for tables
formant.table.filenames <- sub(".wav", "_Table.txt", file.names)
formant.table.paths     <- paste0("/Users/GonzaloGGC/projects/segmentation/osf/Data/Stimuli/Formants/", formant.table.filenames)

#### get formants ##########################################

# arguments for extracting formants
formant.arguments <- list(0.001, # time step (s)
                          5,     # max. number of formants
                          5500,  # maximum formant (Hz)
                          0.025, # window length (s)
                          50)    # pre-emphasis from (Hz)

# retrieve formants from .wav files
mapply("To Formant (burg)...",
       FUN = praat,
       arguments = list(formant.arguments), # take the arguments specified above as a list
       input = file.paths,                  # paths to the audio files
       output = formant.paths,              # paths for the resulting paths of the .Pitch files
       overwrite = TRUE)                    # overwrite the files in the outcome folder if the function is run again

# arguments to put formant files in tables
formant.tabulation.arguments <-
  list(TRUE, # include frame number
       TRUE, # include time
       3,    # iime decimals
       TRUE, # include intensity
       3,    # intensity decimals
       TRUE, # include number of formants
       3,    # frequency decimals
       TRUE) # include bandwidths

# put formant files in tables
mapply("Down to Table...",
       FUN = praat,
       arguments = list(formant.tabulation.arguments), # take the arguments specified above as a list
       input = formant.paths,                          # paths to the audio files
       output = formant.table.paths,                   # paths for the resulting paths of the .Pitch files
       filetype = "tab-separated",                     # this format is easier to read
       overwrite = TRUE)                               # overwrite the files in the outcome folder if the function is run again

# build a table with F1, F2, and intensity across time for each .wav file. Add some variables of interest
formants <-
  map(.x = formant.table.paths, ~read.table(.x, header = TRUE, sep = "\t", na.strings = "--undefined--")) %>%
  map(., ~select(., time = time.s., intensity, f1 = F1.Hz., f2 = F2.Hz.)) %>%
  map(., ~melt(., id.vars = c("time", "intensity"), variable.name = "formant", value.name = "frequency")) %>%
  map2(., sentences, ~mutate(.,
                             sentence = .y,                                                   # add sentence
                             word = case_when(
                               grepl("gon", sentence) ~ "gon",                 # add word conditionally
                               grepl("mus", sentence) ~ "mus",                
                               grepl("for", sentence) ~ "for",
                               grepl("pul", sentence) ~ "pul"
                               ),
                             language = ifelse(grepl("cat", sentence), "catalan", "spanish"), # add language conditionally
                             id = parse_number(sentence),                                     # add id
                             phoneme = ifelse(word %in% c("gon", "for"), "o", "u"))) %>%      # add phoneme conditionally
  map_df(., bind_rows) %>%                                                             # merge datasets
  as_tibble() %>% # make a tibble
  right_join(., (read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences") %>% select(-phoneme, -sentence)), by = c("word", "language", "id")) %>% # add word onset information
  drop_na()

# get formants from phonemes of interest
formants.phoneme <-
  formants %>%
  filter(
    time > phoneme_onset,
    time < phoneme_offset
  ) %>%
  group_by(sentence, formant) %>%
  summarise(
    frequency = mean(frequency, na.rm = TRUE),
    word = sample(word, 1),
    language = sample(language, 1),
    id = sample(id, 1),
    phoneme = sample(phoneme, 1),
    filename = sample(filename, 1),
    word_onset = sample(word_onset, 1),
    word_offset = sample(word_offset, 1),
    phoneme_onset = sample(phoneme_onset, 1),
    phoneme_offset = sample(phoneme_offset, 1)
    ) %>%
  ungroup() %>%
  spread(
    key = formant,
    value = frequency
    )

# word onset
word.timing <-
  read_xlsx("Stimuli/stimuli.xlsx", sheet = "sentences") %>%
  select(word, id, language, word_onset, word_offset, phoneme_onset, phoneme_offset)

#### export data #################################################################
write.table(formants, "Data/Stimuli/Formants/stimuli_formants.txt", sep = "\t", row.names = FALSE)

#### visualise data ##############################################################

# plot formants against time
ggplot() +
  geom_rect(data = word.timing, aes(xmin = word_onset, xmax = word_offset, ymin = 0, ymax = Inf),
            fill = "red", alpha = 0.5) +
  geom_smooth(data = formants, aes(x = time, y = frequency, group = formant),
              method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.5, colour = "black", fill = "black") +
  labs(x = "Time (s)", y = "Frequency (Hz)") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank()) +
  facet_grid(id ~ language + word) +
  ggsave("Figures/stimuli_formants-sentences.png")

# plot formants against time (just words of interest)
formants.phoneme2 <-
  formants %>%
  filter(time > word_onset, time < word_offset) %>%
  drop_na() 

ggplot() +
  geom_smooth(data = formants.phoneme2, aes(x = time, y = frequency, group = formant),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", size = 1, show.legend = FALSE) +
  geom_rect(data = word.timing, aes(xmin = phoneme_onset, xmax = phoneme_offset, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.5) +
  geom_text(data = word.timing, aes(x = (phoneme_offset+phoneme_onset)/2, y = 3500,
                                    label = paste0(round(1000*(phoneme_offset-phoneme_onset), 2), " ms")),
            size = 7) +
  labs(x = "Time (s)", y = "Frequency (Hz)", title = "F1 and F2 of target of word plotted against time", subtitle = "Red shaded areas indicate target phoneme onset and offset") +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_blank()) +
  facet_wrap(language ~ word + id, scales = "free_x") +
  ggsave("Figures/stimuli_formants-words.png", height = 20, width = 20)

# plot F2 against F1 as a summary of all stimuli
ggplot(formants.phoneme, aes(x = f2, y = f1, colour = phoneme)) +
  geom_text(aes(label = phoneme), size = 5, show.legend = FALSE) +
  stat_ellipse(show.legend = FALSE) +
  labs(x = "F2 (Hz)", y = "F1 (Hz)", title = "F1 plotted against F2") +
  scale_x_reverse() +
  scale_y_reverse() +
  theme(legend.position = "top",
        text = element_text(size = 12)) +
  facet_wrap(.~language) +
  ggsave("Figures/stimuli_formants-summary.png")
  
