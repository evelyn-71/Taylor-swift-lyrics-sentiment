
# PROJECT: ANALYSING TAYLOR SWIFT DISCOGRAPHY -----------------------------------------------------------

# LOG CHANGES -------------------------------------------------------------

# Section A: Data loading and libraries -----------------------------------
setwd("/Users/evelyn/#/Projects/R_Taylor Swift")

# install.packages(c("readxl", "tidyr", "dplyr", "ggplot2", "gridExtra", "stringr", "wordcloud2", "webshot",
                   "htmlwidgets", "stringr", "openxlsx", "taylor", "topicmodels"))

# install.packages("tidyverse")

 # Load libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra) # To arrange ggplot objects
library(stringr)
library(wordcloud2)
library(webshot) # To export HTML widget as png
library(htmlwidgets)
library(stringr)
library(openxlsx) # To export as xlsx file
library(taylor) # For her colour palette
library(topicmodels) # for topic modelling

# Load custom functions
source("C:/#/Projects/Sentiment Analysis functions.R")

## 2. Read data ------------------------------------------------------------
lyrics <- read.csv(file = "songs.csv")
# allsongs <- read.csv(file = "taylor_all_songs.csv")
albumsongs <- read.csv(file = "taylor_album_songs.csv")
lyrics2 <- lyrics
awards <- read.csv(file ="taylor_awards.csv")

colourpalette <- taylor::album_palettes

## 3. Number of unique albums in datasets ----------------------------------
unique(lyrics$Album) # 16
unique(albumsongs$album_name) # 10

## Check if the number of songs in these two datasets match ----------------------------------------------------
albumsongs %>%
  group_by(album_name) %>%
  summarise(numsongs = n()) 

lyrics %>%
  group_by(Album) %>%
  summarise(numsongs = n())

# Note: ONLY "Fearless" and "Red" are in Taylor's Version. This is based on Spotify's API which is in 'albumsongs'

# Section B: Data cleaning ------------------------------------------------
# Unique songs in 'lyrics' album

# lyrics %>%
#   group_by(Album) %>%
#   summarise(numsongs = n())

## Album 1: Taylor Swift ---------------------------------------------------

### 'lyrics' dataset --------------------------------------------------------

lyrics %>%
  filter(Album %in% c("Taylor Swift","Taylor Swift (Best Buy Exclusive)", "Taylor Swift (Big Machine Radio Release Special)" )) %>%
  group_by(Album) %>%
  select(Title)

# Replace album titles
lyrics2$Album <- replace(lyrics2$Album, lyrics2$Album == "Taylor Swift (Best Buy Exclusive)", "Taylor Swift")
lyrics2$Album <- replace(lyrics2$Album, lyrics2$Album == "Taylor Swift (Big Machine Radio Release Special)", "Taylor Swift")
lyrics2$Album <- replace(lyrics2$Album, lyrics2$Album == "2004–2005 Demo CD", # Replace
                        "Taylor Swift") # with this value
# Check
lyrics2 %>%
  filter(Album %in% c("Taylor Swift","Taylor Swift (Best Buy Exclusive)", "Taylor Swift (Big Machine Radio Release Special)" )) %>%
  group_by(Album) %>%
  select(Title)

lyrics2 <- lyrics2[lyrics2$Title !="I Heart ?",]

# Drop one pop version track
lyrics2 <- lyrics2[lyrics2$Title !="Teardrops On My Guitar (Pop Version)",]

### 'albumsongs' dataset --------------------------------------------------------
# albumsongs %>%
#  filter(album_name == "Taylor Swift") %>%
#   select(track_name)


### Change song titles to match 'albumsongs' dataset --------------------------------------------------------
lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Mary’s Song (Oh My My My)", "Mary's Song (Oh My My My)")

lyrics2$Title <- replace(lyrics2$Title, 
                           lyrics2$Title == "Should’ve Said No", "Should've Said No")


lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Cold as You", "Cold As You")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "I’m Only Me When I’m With You", "I'm Only Me When I'm With You")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Picture to Burn", "Picture To Burn")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Tied Together with a Smile", "Tied Together With A Smile")
 
# lyrics2$Title[lyrics2$Album == "Taylor Swift"] %in% albumsongs$track_name[albumsongs$album_name == "Taylor Swift"] 
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Taylor Swift"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Taylor Swift"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

## Album 5: 1989 ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
lyrics2 %>%
  filter(Album %in% c("1989 (Taylor’s Version) [Deluxe]", "1989 (Taylor’s Version) [Tangerine Edition]")) %>%
  group_by(Album) %>%
  select(Title)

# Note: Can drop "1989 (Taylor’s Version) [Deluxe]" since it contains extra song
lyrics2 <- lyrics2[lyrics2$Album != "1989 (Taylor’s Version) [Deluxe]",]

# Check if album was dropped
lyrics2 %>%
  filter(Album %in% c("1989 (Taylor’s Version) [Deluxe]", "1989 (Taylor’s Version) [Tangerine Edition]")) %>%
  group_by(Album) %>%
  select(Title) %>%
  print(n=23)

# Rename album
lyrics2$Album <- replace(lyrics2$Album, lyrics2$Album == "1989 (Taylor’s Version) [Tangerine Edition]", "1989")

# Get song titles
lyrics2 %>%
  filter(Album == "1989") %>%
  select(Title)

### 'albumsongs' dataset --------------------------------------------------------
unique(albumsongs$album_name) 

albumsongs %>%
  filter(album_name == "1989") %>%
  select(track_name)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "1989"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "1989"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

# Rename song titles by first creating the pattern to remove "(Taylor's Version)"
pattern <- " \\(Taylor’s Version\\)"

# Remove the pattern from titles
lyrics2$Title[lyrics2$Album == "1989"] <- str_replace(lyrics2$Title[lyrics2$Album == "1989"], pattern, "")

# Rename song titles by first creating the pattern to remove "[From The Vault]"
pattern2 <- " \\[From The Vault\\]"

lyrics2$Title[lyrics2$Album == "1989"] <- str_replace(lyrics2$Title[lyrics2$Album == "1989"], pattern2, "")

# Remove the pattern from titles
lyrics2$Title <- gsub(pattern2, "", lyrics2$Title)

# Check again
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "1989"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "1989"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

# Drop songs that were TV-only
lyrics2 <- lyrics2[lyrics2$Title != "Is It Over Now?",]
lyrics2 <- lyrics2[lyrics2$Title != "Now That We Don’t Talk",]
lyrics2 <- lyrics2[lyrics2$Title != "Say Don’t Go",]
lyrics2 <- lyrics2[lyrics2$Title != "“Slut!”",]
lyrics2 <- lyrics2[lyrics2$Title != "Suburban Legends",]
lyrics2 <- lyrics2[lyrics2$Title != "Sweeter Than Fiction",]

titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "1989"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "1989"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]


## Album 6: Reputation ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
# lyrics2 %>%
#   filter(Album == "reputation") %>%
#   select(Title)

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 

# albumsongs %>%
#   filter(album_name == "reputation") %>%
#   select(track_name)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "reputation"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "reputation"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Don’t Blame Me", 
                         "Don't Blame Me")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "King of My Heart", 
                         "King Of My Heart")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "New Year’s Day", 
                         "New Year's Day")


lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "This Is Why We Can’t Have Nice Things", 
                         "This Is Why We Can't Have Nice Things")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "...Ready for It?", 
                         "...Ready For It?")

titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "reputation"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "reputation"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]


## Album 7: Lover ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
# lyrics2 %>%
#   filter(Album == "Lover") %>%
#   select(Title)

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 
# 
# albumsongs %>%
#   filter(album_name == "Lover") %>%
#   select(track_name)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Lover"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Lover"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "It’s Nice To Have A Friend", 
                         "It's Nice To Have A Friend")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Soon You’ll Get Better", 
                         "Soon You'll Get Better")

titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Lover"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Lover"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

## Album 8: folklore ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
lyrics2 %>%
  filter(Album == "folklore") %>%
  select(Title)

lyrics2$Album <- replace(lyrics2$Album, 
                         lyrics2$Album == "folklore", 
                         "folklore (Deluxe edition)")

lyrics2 %>%
  filter(Album == "folklore (Deluxe edition)") %>%
  select(Title)

### 'albumsongs' dataset --------------------------------------------------------
unique(albumsongs$album_name) 

albumsongs %>%
  filter(album_name == "folklore") %>%
  select(track_name)

albumsongs$album_name <- replace(albumsongs$album_name, 
                                 albumsongs$album_name == "folklore", # Replace this with 
                         "folklore (Deluxe edition)") # This

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "folklore (Deluxe edition)"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "folklore (Deluxe edition)"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

### Add new song lyrics
thelakessong <- data.frame(
  Title = "the lakes",
  Album = "folklore (Deluxe edition)",
  Lyrics = "
Is it romantic how all my elegies eulogize me?
I'm not cut out for all these cynical clones
These hunters with cell phones


Take me to the Lakes, where all the poets went to die
I don't belong and, my beloved, neither do you
Those Windermere peaks look like a perfect place to cry
I'm setting off, but not without my muse


What should be over, burrowed under my skin
In heart-stopping waves of hurt
I've come too far to watch some namedropping sleaze
Tell me what are my words worth


Take me to the Lakes, where all the poets went to die
I don't belong and, my beloved, neither do you
Those Windermere peaks look like a perfect place to cry
I'm setting off, but not without my muse

I want auroras and sad prose
I want to watch wisteria grow right over my bare feet
'Cause I haven't moved in years
And I want you right here
A red rose grew up out of ice frozen ground
With no one around to tweet it
While I bathe in cliffside pools
With my calamitous love and insurmountable grief

Take me to the Lakes, where all the poets went to die
I don't belong and, my beloved, neither do you
Those Windermere peaks look like a perfect place to cry
I'm setting off, but not without my muse
No, not without you"
)

# Add new song
lyrics2 <- rbind(lyrics2, thelakessong)

## Album 9: evermore ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
# lyrics2 %>%
#   filter(Album == "evermore") %>%
#   select(Title)

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 
# 
# albumsongs %>%
#   filter(album_name == "evermore") %>%
#   select(track_name)

# Drop extra song in the Deluxe version
albumsongs <- albumsongs[albumsongs$track_name != "right where you left me",]
albumsongs <- albumsongs[albumsongs$track_name != "it's time to go",]

# Check if dropped
albumsongs %>%
  filter(album_name == "evermore") %>%
  select(track_name)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "evermore"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "evermore"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "l​ong story short", 
                         "long story short")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "’tis the damn season", 
                         "'tis the damn season")

# Check again
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "evermore"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "evermore"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

## Album 10: Midnights ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
lyrics2 %>%
  filter(Album %in% c("Midnights (3am Edition)", "Midnights (The Late Night Edition)")) %>%
  group_by(Album) %>%
  select(Title) %>%
  print(n=23)

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 
# 
# albumsongs %>%
#   filter(album_name == "Midnights") %>%
#   select(track_name)

# Note: Can drop "Midnights (The Late Night Edition)" since it contains extra song
lyrics2 <- lyrics2[lyrics2$Album != "Midnights (The Late Night Edition)",]

# Drop the three songs from Lavender edition
  lyrics2 <- lyrics2[lyrics2$Title != "Hits Different",]
lyrics2 <- lyrics2[lyrics2$Title != "You're On Your Own, Kid (Strings Remix)",]
lyrics2 <- lyrics2[lyrics2$Title != "Sweet Nothing (Piano Remix)",]

# Rename album
lyrics2$Album <- replace(lyrics2$Album, lyrics2$Album == "Midnights (3am Edition)", "Midnights")

# Check 
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Midnights"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Midnights"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

# Rename song titles
lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Would’ve, Could’ve, Should’ve", 
                         "Would've, Could've, Should've")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "You’re On Your Own, Kid", 
                         "You're On Your Own, Kid")
# Check again
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Midnights"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Midnights"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

## ## Album 2: Fearless ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
# unique(lyrics2$Album)

# Drop album
lyrics2 <- lyrics2 [lyrics2$Album !="The More Fearless (Taylor’s Version) Chapter",]


### Add new song lyrics
fasong <- data.frame(
  Title = "Forever & Always (Taylor's Version) [From The Vault]",
  Album = "Fearless (Taylor's Version)",
  Lyrics = "
Once upon a time
I believe it was a Tuesday when I caught your eye
And we caught onto somethin'
I hold onto the night
You looked me in the eye and told me you loved me
Were you just kiddin'?
'Cause it seems to me
This thing is breaking down, we almost never speak
I don't feel welcome anymore
Baby, what happened? Please, tell me
'Cause, one second, it was perfect
Now you're halfway out the door

And I stare at the phone, he still hasn't called
And then you feel so low you can't feel nothin' at all
And you flashback to when he said, \" Forever and always, \" oh
Oh, and it rains in your bedroom, everythin' is wrong
It rains when you're here and it rains when you're gone
'Cause I was there when you said, \"Forever and always \"

Was I out of line?
Did I say somethin' way too honest, made you run and hide
Like a scared little boy?
I looked into your eyes
Thought I knew you for a minute, now I'm not so sure
So here's to everything coming down to nothin'
Here's to silence that cuts me to the core
Where is this going?
Thought I knew for a minute, but I don't anymore

And I stare at the phone, he still hasn't called
And then you feel so low you can't feel nothin' at all
And you flashback to when he said, \"Forever and always,\" oh
Oh, and it rains in your bedroom, everythin' is wrong
It rains when you're here and it rains when you're gone
'Cause I was there when you said, \"Forever and always\"

You didn't mean it, baby
I don't think so
Oh, woah

Oh, back up, baby, back up
Did you forget everything?
Back up, baby, back up
Did you forget everything?

'Cause it rains in your bedroom, everythin' is wrong
It rains when you're here and it rains when you're gone
'Cause I was there when you said, \"Forever and always\"
Oh, I stare at the phone, he still hasn't called
And then you feel so low you can't feel nothin' at all
And you flashback to when we said, \"Forever and always\"
And it rains in your bedroom, everythin' is wrong
It rains when you're here and it rains when you're gone
'Cause I was there when you said, \"Forever and always\"

You didn't mean it, baby
You said, \"Forever and always,\"yeah and always"
  )




# Add new song
lyrics2 <- rbind(lyrics2, fasong)


### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 


### Change album titles to match 'albumsongs; -------------------------------
lyrics2$Album <- replace(lyrics2$Album, 
                         lyrics2$Album == trimws("Fearless (Taylor’s Version)"),
                         trimws("Fearless (Taylor's Version)"))

lyrics2 %>%
  filter(Album == "Fearless (Taylor's Version)") %>%
  select(Title)

albumsongs %>%
  filter(album_name == "Fearless (Taylor's Version)") %>%
  select(track_name)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------

# Define the pattern to be replaced and the replacement
pattern <- "Taylor’s Version"
replacement <- "Taylor's Version"

pattern2 <- "From the Vault"
replacement2 <- "From The Vault"

# Replace the pattern in the Title column
lyrics2$Title[lyrics2$Album == "Fearless (Taylor's Version)"] <- gsub(pattern, replacement, lyrics2$Title[lyrics2$Album == "Fearless (Taylor's Version)"])

lyrics2$Title[lyrics2$Album == "Fearless (Taylor's Version)"] <- gsub(pattern2, replacement2, lyrics2$Title[lyrics2$Album == "Fearless (Taylor's Version)"])

# Change song titles
albumsongs$track_name <- replace(albumsongs$track_name , 
                                 albumsongs$track_name  == "Forever & Always (Piano Version) [Taylor's Version]",
                         "Forever & Always (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Don’t You (Taylor's Version) [From The Vault]",
                         "Don't You (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Superstar (Taylor's Version)",
                         "SuperStar (Taylor's Version)")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "The Other Side of the Door (Taylor's Version)",
                         "The Other Side Of The Door (Taylor's Version)")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "Today Was a Fairytale (Taylor's Version)",
                         "Today Was A Fairytale (Taylor's Version)")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "You’re Not Sorry (Taylor's Version)",
                         "You're Not Sorry (Taylor's Version)")

lyrics2$Title <- replace(lyrics2$Title, 
                         lyrics2$Title == "That’s When (Taylor's Version) [From The Vault]",
                         "That's When (Taylor's Version) [From The Vault]")


# Check again
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Fearless (Taylor's Version)"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Fearless (Taylor's Version)"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]


## Album 3: Speak Now ----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------
# lyrics2 %>%
#   filter(Album == "Speak Now (Taylor’s Version)") %>%
#   select(Title)

# Rename album
lyrics2$Album <- replace(lyrics2$Album, 
                         lyrics2$Album == "Speak Now (Taylor’s Version)", 
                         "Speak Now")

# lyrics2 %>%
#   filter(Album == "Speak Now") %>%
#   select(Title)

# Remove "Taylor's Version"
# Rename song titles by first creating the pattern to remove " (Taylor's Version)" 
pattern <- "\\s*\\(Taylor['’]s Version\\)"

# Remove the pattern from titles
lyrics2$Title[lyrics2$Album == "Speak Now"] <- str_replace(lyrics2$Title[lyrics2$Album == "Speak Now"], pattern, "")

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 
# 
# albumsongs %>%
#   filter(album_name == "Speak Now") %>%
#   select(track_name)

speaknowsong <- data.frame(
  Title = "If This Was A Movie",
  Album = "Speak Now",
  Lyrics = "
Last night, I heard my own heart beating
Sounded like footsteps on my stairs
Six months gone and I'm still reaching
Even though I know you're not there
I was playing back a thousand memories, baby
Thinking 'bout everything we've been through
Maybe I've been going back too much lately
When time stood still and I had you


Come back, come back, come back to me like
You would, you would if this was a movie
Stand in the rain outside 'til I came out
Come back, come back, come back to me like
You could, you could if you just said you're sorry
I know that we could work it out somehow
But if this was a movie, you'd be here by now


I know people change and these things happen
But I remember how it was back then
Wrapped up in your arms and our friends were laughin'
'Cause nothing like this ever happened to them
Now I'm pacing down the hall, chasing down your street
Flashback to the night when you said to me
That nothing's gonna change, not for me and you
Back before I knew how much I had to lose


Come back, come back, come back to me like
You would, you would if this was a movie
Stand in the rain outside 'til I came out
Come back, come back, come back to me like
You could, you could if you just said you're sorry
I know that we could work it out somehow
But if this was a movie, you'd be here by now


If you're out there, if you're somewhere, if you're moving on
I've been waiting for you every day since you've been gone
I just want it back the way it was before
And I just wanna see you back at my front door


And I say,\"\ Come back, come back, come back to me like
  You would before you said it's  not that easy
Before the fight, before I locked you out
But I take it all back now\"\ 


Come back, come back, come back to me like
You would, you would if this was a movie
Stand in the rain outside ' til I came out
  Come back, come back, come back to me like
  You could, you could if you just said you're sorry
I know that we could work it out somehow
But if this was a movie, you'd be here by now

  You'd be here by now
It's not the kind of ending you wanna see now
  Baby, what about the ending?
    Oh, I thought you'd be here by now
Oh, oh, oh, oh, oh, oh
Thought you'd be here by now"
)

# Add new song
lyrics2 <- rbind(lyrics2, speaknowsong)

### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Speak Now"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Speak Now"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]


## Album 4: Red '----------------------------------------------------
### 'lyrics2' dataset --------------------------------------------------------

# Duplicate this row for the acoustic version
lyrics2 <- rbind(lyrics2, lyrics2[22,])

# Change the song title based on row number
lyrics2$Title[217] <- "State Of Grace (Acoustic Version) [Taylor's Version]"

### 'albumsongs' dataset --------------------------------------------------------
# unique(albumsongs$album_name) 
# 
albumsongs %>%
  filter(album_name == "Red (Taylor's Version)") %>%
  select(track_name)


## Change album title --------------------------------------------------
lyrics2$Album <- replace(lyrics2$Album, 
                         lyrics2$Album == trimws("Red (Taylor’s Version)"),
                         trimws("Red (Taylor's Version)"))

lyrics2 %>%
  filter(Album == "Red (Taylor's Version)") %>%
  select(Title)

albumsongs %>%
  filter(album_name == "Red (Taylor's Version)") %>%
  select(track_name)


### Check if song titles matched 'albumsongs' dataset --------------------------------------------------------
titles_in_lyrics <- lyrics2$Title[lyrics2$Album == "Red (Taylor's Version)"]
track_names_in_albumsongs <- albumsongs$track_name[albumsongs$album_name == "Red (Taylor's Version)"]
titles_in_lyrics[!(titles_in_lyrics %in% track_names_in_albumsongs)]

# Define the pattern to be replaced and the replacement
pattern <- "Taylor’s Version"
replacement <- "Taylor's Version"

pattern2 <- "From the Vault"
replacement2 <- "From The Vault"

# Replace the pattern in the Title column
lyrics2$Title[lyrics2$Album == "Red (Taylor's Version)"] <- gsub(pattern, replacement, lyrics2$Title[lyrics2$Album == "Red (Taylor's Version)"])

lyrics2$Title[lyrics2$Album == "Red (Taylor's Version)"] <- gsub(pattern2, replacement2, lyrics2$Title[lyrics2$Album == "Red (Taylor's Version)"])

# Change song titles
lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "All Too Well (10 Minute Version) (Taylor's Version)",
                         "All Too Well (10 Minute Version) [Taylor's Version] [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Babe (Taylor's Version)",
                         "Babe (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Better Man (Taylor's Version)",
                         "Better Man (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Forever Winter (Taylor's Version)",
                        "Forever Winter (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Message In A Bottle (Taylor's Version)",
                         "Message In A Bottle (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Nothing New (Taylor's Version)",
                         "Nothing New (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "Run (Taylor's Version)",
                         "Run (Taylor's Version) [From The Vault]")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "State of Grace (Taylor's Version)",
                         "State Of Grace (Taylor's Version)")

lyrics2$Title <- replace(lyrics2$Title,
                         lyrics2$Title == "The Very First Night (Taylor's Version)",
                         "The Very First Night (Taylor's Version) [From The Vault]")

# SECTION C: Merging --------------------------------------------------------------
merged <- inner_join(x=lyrics2, y=albumsongs, 
                   by = c( "Title"="track_name","Album" = "album_name"))

merged %>%
  group_by(Album) %>%
  summarise(numbersongs = n())

# Drop useless values
pattern <- NULL
pattern2 <- NULL
replacement <- NULL
replacement2 <- NULL
titles_in_lyrics <- NULL
track_names_in_albumsongs <- NULL
original_acm_records <- NULL
merged$lyrics <- NULL
speaknowsong <- NULL
thelakessong <- NULL
fasong<- NULL 

# Remove words like "Verse", "chorus" etc from lyrics
## Define the words to remove
pattern <- "\\[.*?\\]"

## Remove the words
merged$Lyrics <- str_replace_all(merged$Lyrics, pattern, "")

merged2 <- merged

# View(merged)

# Check on all songs by album 
merged %>%
  filter(Album == "Red (Taylor's Version)") %>%
  select(Title)

merged %>%
  filter(Album == "Lover") %>%
  select(Title)

merged %>%
  filter(Album == "Midnights") %>%
  select(Title)

merged %>%
  filter(Album == "Taylor Swift") %>%
  select(Title)

merged %>%
  filter(Album == "Speak Now") %>%
  select(Title)

merged %>%
  filter(Album == "Fearless (Taylor's Version)") %>%
  select(Title)

merged %>%
  filter(Album == "reputation") %>%
  select(Title)

merged %>%
  filter(Album == "evermore") %>%
  select(Title)

merged %>%
  filter(Album == "folklore (Deluxe edition)") %>%
  select(Title)

# unique(merged$Album)
# 
# [1] "Red (Taylor's Version)"      "Lover"                       "1989"                        "Midnights"                  
# [5] "Taylor Swift"                "Speak Now"                   "Fearless (Taylor's Version)" "reputation"                 
# [9] "folklore (Deluxe edition)"   "evermore"                   


# Export as xlsv because csv cannot read -----------------------------------------------------------
write.xlsx(merged, file ="taylor.xlsx")

# SECTION D: TEXT ANALYTICS -----------------------------------------------
source("C:/#/Projects/Sentiment Analysis functions.R")


# Generate nrc graph   -----------------------------------------------------------------
nrc_total_graph(merged2$Lyrics[merged2$Album == "Taylor Swift"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "Fearless (Taylor's Version)"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "Speak Now"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "Red (Taylor's Version)"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "1989"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "reputation"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "Lover"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "folklore (Deluxe edition)"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "evermore"])
nrc_total_graph(merged2$Lyrics[merged2$Album == "Midnights"])

 


# Generate word clouds ---------------------------------------------------
album_wc(my_text = merged2, album_column = Album, album_name = "Taylor Swift", htmlname = "Taylor Swift.html", filename = "wc_taylor.png")
album_wc(my_text = merged2, album_column = Album, album_name = "Fearless (Taylor's Version)", htmlname = "Fearless.html", filename = "wc_fearless.png")
album_wc(my_text = merged2, album_column = Album, album_name = "Speak Now", htmlname = "Speak Now.html", filename = "wc_speaknow.png")
album_wc(my_text = merged2, album_column = Album, album_name = "Red (Taylor's Version)", htmlname = "red.html", filename = "wc_red.png")
album_wc(my_text = merged2, album_column = Album, album_name = "1989", htmlname = "1989.html", filename = "wc_1989.png")
album_wc(my_text = merged2, album_column = Album, album_name = "reputation", htmlname = "reputation.html", filename = "wc_reputation.png")
album_wc(my_text = merged2, album_column = Album, album_name = "Lover", htmlname = "Lover.html", filename = "wc_Lover.png")
album_wc(my_text = merged2, album_column = Album, album_name = "folklore (Deluxe edition)", htmlname = "folklore.html", filename = "wc_folklore.png")
album_wc(my_text = merged2, album_column = Album, album_name = "evermore", htmlname = "evermore.html", filename = "wc_evermore.png")
album_wc(my_text = merged2, album_column = Album, album_name = "Midnights", htmlname = "Midnights.html", filename = "wc_Midnights.png")

# Unique word count -------------------------------------------------------

# Combine lyrics by album first
combined_lyrics <- merged2 %>%
  group_by(Album)%>%
  summarise(Lyrics2 = paste(Lyrics, collapse = " "))

word_count <- function(data) {
  word_counts <- combined_lyrics %>%
    group_by(Album) %>%
    summarize(total_words = sum(str_count(Lyrics2, "\\S+")),
              unique_words = length(unique(unlist(str_split(Lyrics2, "\\s+")))),
              proportion_unique = 100*(unique_words/total_words)
    )
  return(word_counts)
}

ts_words <- word_count(combined_lyrics)


# Unique words ------------------------------------------------------------

# Chart for number of words used
ggplot(data=ts_words, aes(x=total_words, y=Album)) +
  geom_col(fill="#FFE7BA") +
  geom_text(aes(label=total_words), hjust=1) + 
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Number of Words Used")

# Chart for number of unique words
ggplot(data=ts_words, aes(x=unique_words, y=Album)) +
  geom_col(fill="#CDB7D5") +
  geom_text(aes(label=unique_words), hjust=1) + 
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Number of Unique Words")

# Chart for proportion of unique words
ggplot(data=ts_words, aes(x=proportion_unique, y=Album)) +
  geom_col(fill="#96CDCD") +
  geom_text(aes(label=round(proportion_unique,0)), hjust=1) + 
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Proportion of Unique Words")


# Most common words used --------------------------------------------------
excludethese <- c("ooh", "ah", "yeah", "ha", "uh", "mm", "la", "eeh")

albumnames <- unique(merged2$Album)
albumnames
albumnames <- albumnames[c(5,7,6,1,3,8,2,9,10,4)]

albumcolours <- c("Taylor Swift" = "#1D4737",
                 "Fearless (Taylor's Version)" = "#A47F45",
                 "Lover" = "#EBBED3",
                 "Midnights" = "#5865AB",
                 "Red (Taylor's Version)" = "#731803",
                 "Speak Now" = "#833C63",
                 "1989" = "#92573C",
                 "evermore" = "#D37F55",
                 "folklore (Deluxe edition)" = "#5C5C5C",
                 "reputation" = "black")


for (album in albumnames) {
  # Get the corresponding color for the album
  coloUr <- albumcolours[album]
  
  output <- idf_plot(merged2$Lyrics[merged2$Album==album], 
                     top_n=15,
                     exclude_words = excludethese,
                     bar_colour = coloUr)
  
  print(output)
}
 
# idf_plot(merged2$Lyrics[merged2$Album == "Taylor Swift"], top_n = 15)
# idf_plot(merged2$Lyrics[merged2$Album == "Fearless (Taylor's Version)"], top_n = 15)
# idf_plot(merged2$Lyrics[merged2$Album == "Speak Now"], top_n = 15)


# Topic modelling ---------------------------------------------------------
tidy_lyrics <- tidy(merged2)

test <- merged2 %>%
  mutate(document_id = row_number())

# Perform text processing and TF-IDF calculation
test<- test %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words) %>%
  count(document_id, word) %>%
  bind_tf_idf(word, document_id, n) %>%
  cast_dtm(document_id, word, n)
 
LDA(test, k = 2, control = list(seed = 1234))

tidy(test, matrix = "beta")

# SECTION XXX: Merge with awards [LAST PHASE] --------------------------------------------

# Unique number of awards
unique(awards2$Award)
awards2 <- awards


## Filter to get major awards first ----------------------------------------------
iwantthis <- c("Grammy Awards", "Billboard Music Awards", "American Music Awards", "MTV Video Music Awards", "Brit Awards",
               "Academy of Country Music Awards", "Country Music Association Awards", "People's Choice Awards",
               "Juno Awards", "American Country Awards", "iHeartRadio Music Awards", 
               "MTV Europe Music Awards", "NME Awards", "Billboard Women in Music", "ARIA Music Awards",
               "Global Awards", "Billboard Live Music (Touring) Awards"
              )
awards2 <- awards[awards$Award %in% iwantthis, ]

## Clean 'awards2' to match merged ----------------------------------------------
# Remove ""
pattern <- '^"|"\s*$'

awards2$Recipient.s. <- gsub(pattern, "\\1", awards2$Recipient.s.)


awards2$Recipient.s. <- gsub('^"|"$', '', awards2$Recipient.s.)

# Replace 'Swift' with 'Taylor Swift'
awards2$Recipient.s.[awards2$Recipient.s. == 'Swift'] <- 'Taylor Swift'

 
# library(gtrendsR)
# plot(gtrends(keyword = c("Travis Kelce", "Taylor Swift", "The Eras Tour", "NFL", "Kansas City Chiefs")
#         , time = "today 12-m"))
# https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf  



 
