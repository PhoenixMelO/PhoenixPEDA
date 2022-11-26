
  # Packages to install and load
  
library(dplyr)
library(audio)


# All notes have to be defined

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

#Create a vector for the notes of the Star Wars theme.  Using `strsplit()` to convert this to a vector with each note as an entry.  


pitch <- paste(
  "D D D",
  "G D5",
  "C5 B A G5 D5",
  "C5 B A G5 D5",
  "C5 B C5 A D D D",
  "G D5",
  "C5 B A G5 D5",
  "C5 B A G5 D5",
  "C5 B C5 A D D",
  "E E C5 B A G",
  "G A B A E F# D D",
  "E E C5 B A G",
  "D5 A D D",
  "E E C5 B A G",
  "G A B A E F# D5 D5",
  "G5 F5 D#5 D5 C5 A# A G",
  "D5 D D D",
  "G D5",
  "C5 B A G5 D5",
  "C5 B A G5 D5",
  "C5 B C5 A D D D",
  "G D5",
  "C5 B A G5 D5",
  "G5 F5 D#5 Bb5 A5",
  "G5 G G G G"
)


# Now create a similar vector with the durations of each notes as proportion of a beat.  This vector should be the same length as the notes vector.  I like to do this with a line for each bar of music.


duration <- c(
  0.33, 0.33, 0.33, 
  2, 2, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 0.33, 0.33, 0.33, 
  2, 2, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 0.75, 0.25,
  1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
  0.33, 0.33, 0.33, 0.75, 0.25, 1, 0.75, 0.25, 
  1.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  1, 2, 0.75, 0.25,
  1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
  0.33, 0.33, 0.33, 0.75, 0.25, 1, 0.75, 0.25, 
  0.75, 0.25, 0.75, 0.25, 0.75, 0.25, 0.75, 0.25, 
  3, 0.33, 0.33, 0.33,   
  2, 2, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 0.33, 0.33, 0.33, 
  2, 2, 
  0.33, 0.33, 0.33, 2, 1, 
  0.33, 0.33, 0.33, 2, 1, 
  1,  0.33, 0.33, 0.33, 1
)


#Put it all together


starwars <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                       duration = duration)

starwars <-
  starwars %>%
  mutate(octave = substring(pitch, nchar(pitch))  %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)                     



tempo <- 150
sample_rate <- 44100
make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

#To play or save, pick the right tempo and go:
  
 
starwars_wave <-
  mapply(make_sine, starwars$freq, starwars$duration) %>%
  do.call("c", .)
audio::play(starwars_wave)

#You can also save this tune as .wav
#audio::save.wave(starwars_wave, "starwars.wav")




#References
#McNulty, K. (2018). How to play Star Wars and other tunes in R. https://towardsdatascience.com/how-to-play-star-wars-and-other-tunes-in-r-5e837e8b71e4
