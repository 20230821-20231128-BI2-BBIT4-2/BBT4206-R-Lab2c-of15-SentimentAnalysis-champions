Business Intelligence Lab Submission Markdown
================
Champions
12/10/2023

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Loading the Student Performance
  Dataset](#loading-the-student-performance-dataset)
- [Loading a required Lexicon](#loading-a-required-lexicon)
- [Innerjoin likes and wishes with correspodning likes and
  wishes](#innerjoin-likes-and-wishes-with-correspodning-likes-and-wishes)
- [Frequency Sentiment per group and per
  Gender](#frequency-sentiment-per-group-and-per-gender)
- [Classifications of Words Per
  sentiment](#classifications-of-words-per-sentiment)
- [Average Per Question per Group](#average-per-question-per-group)

# Student Details

<table>
<colgroup>
<col style="width: 53%" />
<col style="width: 46%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Student ID Numbers and Names of Group Members</strong></td>
<td><ol type="1">
<li><p>134111 - B - Immaculate Juma</p></li>
<li><p>126761 - B - Virginia Wanjiru</p></li>
<li><p>133996 - B- Trevor Ngugi</p></li>
<li><p>135859 - B - Pauline Wairimu</p></li>
<li><p>127707 - B - Clarice Gitonga</p></li>
</ol></td>
</tr>
<tr class="even">
<td><strong>GitHub Classroom Group Name</strong></td>
<td>Champions</td>
</tr>
<tr class="odd">
<td><strong>Course Code</strong></td>
<td>BBT4206</td>
</tr>
<tr class="even">
<td><strong>Course Name</strong></td>
<td>Business Intelligence II</td>
</tr>
<tr class="odd">
<td><strong>Program</strong></td>
<td>Bachelor of Business Information Technology</td>
</tr>
<tr class="even">
<td><strong>Semester Duration</strong></td>
<td>21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023</td>
</tr>
</tbody>
</table>

# Setup Chunk

We start by installing all the required packages

``` r
# STEP 1. Install and Load the Required Packages ----
# The following packages can be installed and loaded before proceeding to the
# subsequent steps.

## dplyr - For data manipulation ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE)
}
require("dplyr")

## ggplot2 - For data visualizations using the Grammar for Graphics package ----
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE)
}
require("ggplot2")

## ggrepel - Additional options for the Grammar for Graphics package ----
if (!is.element("ggrepel", installed.packages()[, 1])) {
  install.packages("ggrepel", dependencies = TRUE)
}
require("ggrepel")

## ggraph - Additional options for the Grammar for Graphics package ----
if (!is.element("ggraph", installed.packages()[, 1])) {
  install.packages("ggraph", dependencies = TRUE)
}
require("ggraph")

## tidytext - For text mining ----
if (!is.element("tidytext", installed.packages()[, 1])) {
  install.packages("tidytext", dependencies = TRUE)
}
require("tidytext")

## tidyr - To tidy messy data ----
if (!is.element("tidyr", installed.packages()[, 1])) {
  install.packages("tidyr", dependencies = TRUE)
}
require("tidyr")

## widyr - To widen, process, and re-tidy a dataset ----
if (!is.element("widyr", installed.packages()[, 1])) {
  install.packages("widyr", dependencies = TRUE)
}
require("widyr")

## gridExtra - to arrange multiple grid-based plots on a page ----
if (!is.element("gridExtra", installed.packages()[, 1])) {
  install.packages("gridExtra", dependencies = TRUE)
}
require("gridExtra")

## knitr - for dynamic report generation ----
if (!is.element("knitr", installed.packages()[, 1])) {
  install.packages("knitr", dependencies = TRUE)
}
require("knitr")

## kableExtra - for nicely formatted output tables ----
if (!is.element("kableExtra", installed.packages()[, 1])) {
  install.packages("kableExtra", dependencies = TRUE)
}

require("kableExtra")

## formattable -  To create a formattable object ----
# A formattable object is an object to which a formatting function and related
# attributes are attached.
if (!is.element("formattable", installed.packages()[, 1])) {
  install.packages("formattable", dependencies = TRUE)
}
require("formattable")

## circlize - To create a cord diagram or visualization ----
# by Gu et al. (2014)
if (!is.element("circlize", installed.packages()[, 1])) {
  install.packages("circlize", dependencies = TRUE)
}
require("circlize")

## memery - For creating data analysis related memes ----
# The memery package generates internet memes that optionally include a
# superimposed inset plot and other atypical features, combining the visual
# impact of an attention-grabbing meme with graphic results of data analysis.
if (!is.element("memery", installed.packages()[, 1])) {
  install.packages("memery", dependencies = TRUE)
}
require("memery")

## magick - For image processing in R ----
if (!is.element("magick", installed.packages()[, 1])) {
  install.packages("magick", dependencies = TRUE)
}
require("magick")

## yarrr - To create a pirate plot ----
if (!is.element("yarrr", installed.packages()[, 1])) {
  install.packages("yarrr", dependencies = TRUE)
}
require("yarrr")

## radarchart - To create interactive radar charts using ChartJS ----
if (!is.element("radarchart", installed.packages()[, 1])) {
  install.packages("radarchart", dependencies = TRUE)
}
require("radarchart")

## igraph - To create ngram network diagrams ----
if (!is.element("igraph", installed.packages()[, 1])) {
  install.packages("igraph", dependencies = TRUE)
}
require("igraph")

## wordcloud2 - For creating wordcloud by using 'wordcloud2.JS ----
if (!is.element("wordcloud2", installed.packages()[, 1])) {
  install.packages("wordcloud2", dependencies = TRUE)
}
require("wordcloud2")

## textdata - Download sentiment lexicons and labeled text data sets ----
if (!is.element("textdata", installed.packages()[, 1])) {
  install.packages("textdata", dependencies = TRUE)
}
require("textdata")

## readr - Load datasets from CSV files ----
if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE)
}
require("readr")

## stringr - For processing characters in a string ----
if (!is.element("stringr", installed.packages()[, 1])) {
  install.packages("stringr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("stringr")

if (!is.element("lexicon", installed.packages()[, 1])) {
  install.packages("lexicon", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("lexicon")
```

------------------------------------------------------------------------

**Note:** the following “*KnitR*” options have been set as the defaults
in this markdown:  
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy.opts = list(width.cutoff = 80), tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

``` r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    warning = FALSE,
    collapse = FALSE,
    tidy = TRUE
)
```

------------------------------------------------------------------------

**Note:** the following “*R Markdown*” options have been set as the
defaults in this markdown:

> output:  
>   
> github_document:  
> toc: yes  
> toc_depth: 4  
> fig_width: 6  
> fig_height: 4  
> df_print: default  
>   
> editor_options:  
> chunk_output_type: console

# Loading the Student Performance Dataset

``` r
# STEP 2. Customize the Visualizations, Tables, and Colour Scheme ---- The
# following defines a blue-grey colour scheme for the visualizations: shades of
# blue and shades of grey
blue_grey_colours_11 <- c("#27408E", "#304FAF", "#536CB5", "#6981c7", "#8da0db",
    "#dde5ec", "#c8c9ca", "#B9BCC2", "#A7AAAF", "#888A8E", "#636569")

blue_grey_colours_6 <- c("#27408E", "#304FAF", "#536CB5", "#B9BCC2", "#A7AAAF", "#888A8E")

blue_grey_colours_4 <- c("#27408E", "#536CB5", "#B9BCC2", "#888A8E")

blue_grey_colours_3 <- c("#6981c7", "#304FAF", "#888A8E")

blue_grey_colours_2 <- c("#27408E", "#888A8E")

blue_grey_colours_1 <- c("#6981c7")

# Custom theme for visualizations
blue_grey_theme <- function() {
    theme(axis.ticks = element_line(linewidth = 1, linetype = "dashed", lineend = NULL,
        color = "#dfdede", arrow = NULL, inherit.blank = FALSE), axis.text = element_text(face = "bold",
        color = "#3f3f41", size = 12, hjust = 0.5), axis.title = element_text(face = "bold",
        color = "#3f3f41", size = 14, hjust = 0.5), plot.title = element_text(face = "bold",
        color = "#3f3f41", size = 16, hjust = 0.5), panel.grid = element_line(linewidth = 0.1,
        linetype = "dashed", lineend = NULL, color = "#dfdede", arrow = NULL, inherit.blank = FALSE),
        panel.background = element_rect(fill = "#f3eeee"), legend.title = element_text(face = "plain",
            color = "#3f3f41", size = 12, hjust = 0), legend.position = "right")
}

# Customize the text tables for consistency using HTML formatting
kable_theme <- function(dat, caption) {
    kable(dat, "html", escape = FALSE, caption = caption) %>%
        kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
            full_width = FALSE)
}
```

``` r
Mid_Term_Course_Evaluation_Form_Preprocessed <- read_csv("../data/Mid_Term_Course_Evaluation_Form_Preprocessed.csv")
```

    ## Rows: 102 Columns: 30
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): Submitted on:, Department, Course, Group, Gender, Q01_Class Demogr...
    ## dbl (22): Absenteeism, Average Course Evaluation Rating, Classes start and e...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
View(Mid_Term_Course_Evaluation_Form_Preprocessed)

## Create a filtered subset of the data ----

# Function to expand contractions
expand_contractions <- function(doc) {
  doc <- gsub("I'm", "I am", doc, ignore.case = TRUE)
  doc <- gsub("you're", "you are", doc, ignore.case = TRUE)
  doc <- gsub("he's", "he is", doc, ignore.case = TRUE)
  doc <- gsub("she's", "she is", doc, ignore.case = TRUE)
  doc <- gsub("it's", "it is", doc, ignore.case = TRUE)
  doc <- gsub("we're", "we are", doc, ignore.case = TRUE)
  doc <- gsub("they're", "they are", doc, ignore.case = TRUE)
  doc <- gsub("I'll", "I will", doc, ignore.case = TRUE)
  doc <- gsub("you'll", "you will", doc, ignore.case = TRUE)
  doc <- gsub("he'll", "he will", doc, ignore.case = TRUE)
  doc <- gsub("she'll", "she will", doc, ignore.case = TRUE)
  doc <- gsub("it'll", "it will", doc, ignore.case = TRUE)
  doc <- gsub("we'll", "we will", doc, ignore.case = TRUE)
  doc <- gsub("they'll", "they will", doc, ignore.case = TRUE)
  doc <- gsub("won't", "will not", doc, ignore.case = TRUE)
  doc <- gsub("can't", "cannot", doc, ignore.case = TRUE)
  doc <- gsub("n't", " not", doc, ignore.case = TRUE)
  return(doc)
}

# Select the class group, gender, average course evaluation rating,
# and most importantly, the likes and wishes from the original dataset
evaluation_likes_and_wishes <- Mid_Term_Course_Evaluation_Form_Preprocessed %>%
  mutate(`Student's Gender` =
           ifelse(Gender == 1, "Male", "Female")) %>%
  rename(`Class Group` = Group) %>%
  rename(Likes = `Likes`) %>% # nolint
  rename(Wishes = `Dislikes`) %>% # nolint
  select(`Class Group`,
         `Student's Gender`, `Average Course Evaluation Rating`,
         Likes, Wishes) %>%
  filter(!is.na(`Average Course Evaluation Rating`)) %>%
  arrange(`Class Group`)

evaluation_likes_and_wishes$Likes <- sapply(
  evaluation_likes_and_wishes$Likes,
  expand_contractions)
evaluation_likes_and_wishes$Wishes <- sapply(
  evaluation_likes_and_wishes$Wishes,
  expand_contractions)

head(evaluation_likes_and_wishes, 10)
```

    ## # A tibble: 10 × 5
    ##    `Class Group`          `Student's Gender` Average Course Evalu…¹ Likes Wishes
    ##    <chr>                  <chr>                               <dbl> <chr> <chr> 
    ##  1 20230821-20231128-BI2… Female                                  5 It i… none  
    ##  2 20230821-20231128-BI2… Female                                  5 The … I do …
    ##  3 20230821-20231128-BI2… Female                                  5 It i… More …
    ##  4 20230821-20231128-BI2… Female                                  4 I li… Use o…
    ##  5 20230821-20231128-BI2… Female                                  5 Ever… i hav…
    ##  6 20230821-20231128-BI2… Female                                  4 The … stude…
    ##  7 20230821-20231128-BI2… Female                                  5 Inte… A bit…
    ##  8 20230821-20231128-BI2… Female                                  4 Its … N/A   
    ##  9 20230821-20231128-BI2… Female                                  4 It i… .     
    ## 10 20230821-20231128-BI2… Female                                  4 Labs… Prope…
    ## # ℹ abbreviated name: ¹​`Average Course Evaluation Rating`

``` r
# Function to remove special characters and convert all text to a standard
# lower case
remove_special_characters <- function(doc) {
  gsub("[^a-zA-Z0-9 ]", "", doc, ignore.case = TRUE)
}

evaluation_likes_and_wishes$Likes <- sapply(evaluation_likes_and_wishes$Likes,
                                            remove_special_characters)
evaluation_likes_and_wishes$Wishes <- sapply(evaluation_likes_and_wishes$Wishes,
                                             remove_special_characters)

# Convert everything to lower case (to standardize the text)
evaluation_likes_and_wishes$Likes <- sapply(evaluation_likes_and_wishes$Likes,
                                            tolower)
evaluation_likes_and_wishes$Wishes <- sapply(evaluation_likes_and_wishes$Wishes,
                                             tolower)

# After removing special characters and converting everything to lower case
head(evaluation_likes_and_wishes, 10)
```

    ## # A tibble: 10 × 5
    ##    `Class Group`          `Student's Gender` Average Course Evalu…¹ Likes Wishes
    ##    <chr>                  <chr>                               <dbl> <chr> <chr> 
    ##  1 20230821-20231128-BI2… Female                                  5 it i… "none"
    ##  2 20230821-20231128-BI2… Female                                  5 the … "i do…
    ##  3 20230821-20231128-BI2… Female                                  5 it i… "more…
    ##  4 20230821-20231128-BI2… Female                                  4 i li… "use …
    ##  5 20230821-20231128-BI2… Female                                  5 ever… "i ha…
    ##  6 20230821-20231128-BI2… Female                                  4 the … "stud…
    ##  7 20230821-20231128-BI2… Female                                  5 inte… "a bi…
    ##  8 20230821-20231128-BI2… Female                                  4 its … "na"  
    ##  9 20230821-20231128-BI2… Female                                  4 it i… ""    
    ## 10 20230821-20231128-BI2… Female                                  4 labs… "prop…
    ## # ℹ abbreviated name: ¹​`Average Course Evaluation Rating`

``` r
write.csv(evaluation_likes_and_wishes,
          file = "data/evaluation_likes_and_wishes.csv",
          row.names = FALSE)

# Function to censor/remove unwanted words
undesirable_words <- c("wow", "lol", "none", "na")

# unnest and remove stopwords, undesirable words, and short words
evaluation_likes_filtered <- evaluation_likes_and_wishes %>% # nolint
  unnest_tokens(word, Likes) %>%
  # do not join where the word is in the list of stopwords
  anti_join(stop_words, by = c("word")) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes (tokenized)` = word) %>%
  select(-Wishes)

write.csv(evaluation_likes_filtered,
          file = "data/evaluation_likes_filtered.csv",
          row.names = FALSE)

evaluation_wishes_filtered <- evaluation_likes_and_wishes %>% # nolint
  unnest_tokens(word, Wishes) %>%
  # do not join where the word is in the list of stopwords
  anti_join(stop_words, by = c("word")) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Wishes (tokenized)` = word) %>%
  select(-Likes)

write.csv(evaluation_wishes_filtered,
          file = "data/evaluation_wishes_filtered.csv",
          row.names = FALSE)
```

# Loading a required Lexicon

``` r
data(hash_nrc_emotions)
nrc <- hash_nrc_emotions
nrc <- nrc %>%
    mutate(word = token, sentiment = emotion) %>%
    select(word, sentiment)
View(nrc)

### AFINN ---- Assigns words with a score that runs between -5 and 5. Negative
### scores indicate negative sentiments and positive scores indicate positive
### sentiments
afinn <- get_sentiments(lexicon = "afinn")
View(afinn)

### Bing ---- Assigns words into positive and negative categories only
bing <- get_sentiments("bing")
View(bing)
```

# Innerjoin likes and wishes with correspodning likes and wishes

``` r
evaluation_likes_filtered_nrc <- evaluation_likes_filtered %>%
    inner_join(nrc, by = join_by(`Likes (tokenized)` == word), relationship = "many-to-many")

evaluation_wishes_filtered_nrc <- evaluation_wishes_filtered %>%
    inner_join(nrc, by = join_by(`Wishes (tokenized)` == word), relationship = "many-to-many")
```

# Frequency Sentiment per group and per Gender

``` r
# svg(filename = 'visualizations/nrc_likes_chord.svg', width = 8.5, height =
# 8.5, pointsize = 12, bg = 'transparent')

# pdf('visualizations/nrc_likes_chord.pdf', width = 8.5, height = 8.5, bg =
# 'transparent', pagecentre = TRUE, paper = 'A4')

grid_col <- c(A = blue_grey_colours_11[1], B = "#f3c487", C = blue_grey_colours_11[5])

nrc_likes_chord <- evaluation_likes_filtered_nrc %>%
    # filter(decade != 'NA' & !sentiment %in% c('positive', 'negative')) %>%
count(sentiment, `Class Group`) %>%
    group_by(`Class Group`, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    filter(sentiment_sum > 10) %>%
    mutate(sentiment = reorder(sentiment, sentiment_sum)) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Class Group'. You can override using the
    ## `.groups` argument.

``` r
circos.clear()
# Set the gap size
circos.par(gap.after = c(rep(5, length(unique(nrc_likes_chord[[1]])) - 1), 15, rep(5,
    length(unique(nrc_likes_chord[[2]])) - 1), 15))

chordDiagram(nrc_likes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Likes per Group")
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%20Six%20Code%20Chunk-1.png)<!-- -->

``` r
# To close the device used to create either the PNG, JPEG, SVG, or PDF.
dev.off()
```

    ## null device 
    ##           1

``` r
# To plot the chord diagram in the IDE:
chordDiagram(nrc_likes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Likes per Group")

## Evaluation Wishes per Group ---- We can save the plots by hard-coding the
## save function as follows: NOTE: Execute one filetype at a time, i.e., either
## PNG, JPEG, SVG, or PDF.  png(filename =
## 'visualizations/nrc_wishes_chord.png', width = 1920, height = 1080, units =
## 'px', pointsize = 12, bg = 'transparent', res = 150)



# svg(filename = 'visualizations/nrc_wishes_chord.svg', width = 8.5, height =
# 8.5, pointsize = 12, bg = 'transparent')

# pdf('visualizations/nrc_wishes_chord.pdf', width = 8.5, height = 8.5, bg =
# 'transparent', pagecentre = TRUE, paper = 'A4')

grid_col <- c(A = blue_grey_colours_11[1], B = "#f3c487", C = blue_grey_colours_11[5])

nrc_wishes_chord <- evaluation_wishes_filtered_nrc %>%
    # filter(decade != 'NA' & !sentiment %in% c('positive', 'negative')) %>%
count(sentiment, `Class Group`) %>%
    group_by(`Class Group`, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    filter(sentiment_sum > 3) %>%
    mutate(sentiment = reorder(sentiment, sentiment_sum)) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Class Group'. You can override using the
    ## `.groups` argument.

``` r
circos.clear()
# Set the gap size
circos.par(gap.after = c(rep(5, length(unique(nrc_wishes_chord[[1]])) - 1), 15, rep(5,
    length(unique(nrc_wishes_chord[[2]])) - 1), 15))

chordDiagram(nrc_wishes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Wishes per Group")

# To close the device used to create either the PNG, JPEG, SVG, or PDF.
dev.off()
```

    ## null device 
    ##           1

``` r
# To plot the chord diagram in the IDE:
chordDiagram(nrc_wishes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Wishes per Group")

## Evaluation Likes per Gender ---- We can save the plots by hard-coding the
## save function as follows: NOTE: Execute one filetype at a time, i.e., either
## PNG, JPEG, SVG, or PDF.  png(filename =
## 'visualizations/nrc_likes_gender_chord.png', width = 1920, height = 1080,
## units = 'px', pointsize = 12, bg = 'transparent', res = 150)



# svg(filename = 'visualizations/nrc_likes_gender_chord.svg', width = 8.5,
# height = 8.5, pointsize = 12, bg = 'transparent')

# pdf('visualizations/nrc_likes_gender_chord.pdf', width = 8.5, height = 8.5,
# bg = 'transparent', pagecentre = TRUE, paper = 'A4')

grid_col <- c(Male = blue_grey_colours_11[1], Female = "#f387f3")

nrc_likes_chord <- evaluation_likes_filtered_nrc %>%
    # filter(decade != 'NA' & !sentiment %in% c('positive', 'negative')) %>%
count(sentiment, `Student's Gender`) %>%
    group_by(`Student's Gender`, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    filter(sentiment_sum > 10) %>%
    mutate(sentiment = reorder(sentiment, sentiment_sum)) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Student's Gender'. You can override using
    ## the `.groups` argument.

``` r
circos.clear()
# Set the gap size
circos.par(gap.after = c(rep(5, length(unique(nrc_likes_chord[[1]])) - 1), 15, rep(5,
    length(unique(nrc_likes_chord[[2]])) - 1), 15))

chordDiagram(nrc_likes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Likes per Gender")

# To close the device used to create either the PNG, JPEG, SVG, or PDF.
dev.off()
```

    ## null device 
    ##           1

``` r
# To plot the chord diagram in the IDE:
chordDiagram(nrc_likes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Likes per Gender")

## Evaluation Wishes per Gender ---- We can save the plots by hard-coding the
## save function as follows: NOTE: Execute one filetype at a time, i.e., either
## PNG, JPEG, SVG, or PDF.  png(filename =
## 'visualizations/nrc_wishes_gender_chord.png', width = 1920, height = 1080,
## units = 'px', pointsize = 12, bg = 'transparent', res = 150)



# svg(filename = 'visualizations/nrc_wishes_gender_chord.svg', width = 8.5,
# height = 8.5, pointsize = 12, bg = 'transparent')

# pdf('visualizations/nrc_wishes_gender_chord.pdf', width = 8.5, height = 8.5,
# bg = 'transparent', pagecentre = TRUE, paper = 'A4')

grid_col <- c(Male = "lightblue", Female = "lightpink")

nrc_wishes_chord <- evaluation_wishes_filtered_nrc %>%
    # filter(decade != 'NA' & !sentiment %in% c('positive', 'negative')) %>%
count(sentiment, `Student's Gender`) %>%
    group_by(`Student's Gender`, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    filter(sentiment_sum > 3) %>%
    mutate(sentiment = reorder(sentiment, sentiment_sum)) %>%
    ungroup()
```

    ## `summarise()` has grouped output by 'Student's Gender'. You can override using
    ## the `.groups` argument.

``` r
circos.clear()
# Set the gap size
circos.par(gap.after = c(rep(5, length(unique(nrc_wishes_chord[[1]])) - 1), 15, rep(5,
    length(unique(nrc_wishes_chord[[2]])) - 1), 15))

chordDiagram(nrc_wishes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Wishes per Gender")

# To close the device used to create either the PNG, JPEG, SVG, or PDF.
dev.off()
```

    ## null device 
    ##           1

``` r
# To plot the chord diagram in the IDE:
chordDiagram(nrc_wishes_chord, grid.col = grid_col, transparency = 0.2)
title("Lexicon-Based Sentiment Analysis of Course Evaluation Wishes per Gender")
```

# Classifications of Words Per sentiment

``` r
evaluation_likes_filtered_nrc %>%
  # filter(`Class Group` %in% "A") %>%
  distinct(`Likes (tokenized)`) %>%
  inner_join(nrc,
             by = join_by(`Likes (tokenized)` == word),
             relationship = "many-to-many") %>%
  ggplot(aes(x = `Likes (tokenized)`, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + # Create a bar for each word per sentiment
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + # Place the words on the y-axis
  xlab(NULL) + ylab(NULL) +
  ggtitle(paste("Classification of Words in Course Evaluation Likes ",
                "based on the NRC Lexicon")) +
  coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%20Seven%20Code%20Chunk-1.png)<!-- -->

``` r
## Evaluation Wishes ----
evaluation_wishes_filtered_nrc %>%
  # filter(`Class Group` %in% "A") %>%
  distinct(`Wishes (tokenized)`) %>%
  inner_join(nrc,
             by = join_by(`Wishes (tokenized)` == word),
             relationship = "many-to-many") %>%
  ggplot(aes(x = `Wishes (tokenized)`, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + # Create a bar for each word per sentiment
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + # Place the words on the y-axis
  xlab(NULL) + ylab(NULL) +
  ggtitle(paste("Classification of Words in Course Evaluation Wishes ",
                "based on the NRC Lexicon")) +
  coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%20Seven%20Code%20Chunk-2.png)<!-- -->

# Average Per Question per Group

``` r
evaluation_rating_per_question_per_group <- Mid_Term_Course_Evaluation_Form_Preprocessed %>% # nolint
  rename(`Class Group` = Group) %>%
  filter(!is.na(`Average Course Evaluation Rating`)) %>%
  group_by(`Class Group`) %>%
  summarize(
    `A. I am enjoying the subject` =
      mean(`Average Course Evaluation Rating`),
    `B. Classes start and end on time` =
      mean(`Classes start and end on time`),
    `C. The learning environment is participative, involves learning by doing and is group-based` = # nolint
      mean(`The learning environment is participative, involves learning by doing and is group-based`), # nolint
    `D. The subject content is delivered according to the course outline and meets my expectations` = # nolint
      mean(`The subject content is delivered according to the course outline and meets my expectations`), # nolint
    `E. The topics are clear and logically developed` =
      mean(`The topics are clear and logically developed`),
    `F. I am developing my oral and writing skills` =
      mean(`I am developing my oral and writing skills`),
    `G. I am developing my reflective and critical reasoning skills` =
      mean(`I am developing my reflective and critical reasoning skills`), # nolint
    `H. The assessment methods are assisting me to learn` =
      mean(`The assessment methods are assisting me to learn`),
    `I. I receive relevant feedback` =
      mean(`I receive relevant feedback`),
    `J. I read the recommended readings and notes` =
      mean(`I read the recommended readings and notes`),
    `K. I use the eLearning material posted` =
      mean(`I use the eLearning material posted`),
    `L. Mean Overall Course Evaluation Rating` =
      mean(`Average Course Evaluation Rating`),
  ) %>%
  # If we had to sort the results
  # arrange(`Mean Average Course Evaluation Rating`) %>%
  select(
    `Class Group`,
    `A. I am enjoying the subject`,
    `B. Classes start and end on time`,
    `C. The learning environment is participative, involves learning by doing and is group-based`, # nolint
    `D. The subject content is delivered according to the course outline and meets my expectations`, # nolint
    `E. The topics are clear and logically developed`,
    `F. I am developing my oral and writing skills`,
    `G. I am developing my reflective and critical reasoning skills`, # nolint
    `H. The assessment methods are assisting me to learn`,
    `I. I receive relevant feedback`,
    `J. I read the recommended readings and notes`,
    `K. I use the eLearning material posted`,
    `L. Mean Overall Course Evaluation Rating`
  )

View(evaluation_rating_per_question_per_group)

evaluation_rating_per_question_per_group_long_data <- evaluation_rating_per_question_per_group %>% # nolint
  pivot_longer(
    cols = -`Class Group`,
    names_to = "Evaluation Question",
    values_to = "Mean Value")

View(evaluation_rating_per_question_per_group_long_data)

evaluation_rating_per_question_per_group_long_data <- # nolint
  evaluation_rating_per_question_per_group_long_data %>%
  mutate(`Evaluation Question` =
           factor(`Evaluation Question`,
                  levels =
                    c("A. I am enjoying the subject",
                      "B. Classes start and end on time",
                      "C. The learning environment is participative, involves learning by doing and is group-based", # nolint
                      "D. The subject content is delivered according to the course outline and meets my expectations", # nolint
                      "E. The topics are clear and logically developed",
                      "F. I am developing my oral and writing skills",
                      "G. I am developing my reflective and critical reasoning skills", # nolint
                      "H. The assessment methods are assisting me to learn",
                      "I. I receive relevant feedback",
                      "J. I read the recommended readings and notes",
                      "K. I use the eLearning material posted",
                      "L. Mean Overall Course Evaluation Rating")
           )) %>%
  mutate(`Class Group` = factor(`Class Group`, levels = c("A", "B", "C")))

View(evaluation_rating_per_question_per_group_long_data)

# This is done to enable word wrapping when the plot is created
evaluation_rating_per_question_per_group_long_data$`Evaluation Question` <- # nolint
  str_wrap(evaluation_rating_per_question_per_group_long_data$`Evaluation Question`, # nolint
           width = 30)

### Visualizations (Grouped Vertical Bar Chart) ----
# ggplot2 visualization samples are available here:
# https://r-graph-gallery.com/index.html

ggplot(evaluation_rating_per_question_per_group_long_data,
       aes(fill = `Class Group`, y = `Mean Value`, x = `Evaluation Question`,
           label = `Mean Value`)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +  # Flip the coordinates to make it vertical
  geom_text(position = position_dodge(width = 0.9),
            hjust = 1, vjust = 0.5) +  # Add text labels
  labs(title = "Standard Course Evaluation Score per Question per Group",
       x = "Standard Course Evaluation Questions", y = "Mean Value") +
  scale_fill_manual(values = blue_grey_colours_3) +
  blue_grey_theme() +
  geom_hline(yintercept = 4, color = "#b90c0c",
             linetype = "dashed", size = 1)
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%20nine%20Code%20Chunk-1.png)<!-- -->

``` r
## Average per Question per Gender ----
evaluation_rating_per_question_per_gender <- Mid_Term_Course_Evaluation_Form_Preprocessed %>% # nolint
  mutate(`Student's Gender` =
           ifelse(Gender == 1, "Male", "Female")) %>%
  filter(!is.na(`Average Course Evaluation Rating`)) %>%
  group_by(`Student's Gender`) %>%
  summarize(
    `A. I am enjoying the subject` =
      mean(`Average Course Evaluation Rating`),
    `B. Classes start and end on time` =
      mean(`Classes start and end on time`),
    `C. The learning environment is participative, involves learning by doing and is group-based` = # nolint
      mean(`The learning environment is participative, involves learning by doing and is group-based`), # nolint
    `D. The subject content is delivered according to the course outline and meets my expectations` = # nolint
      mean(`The subject content is delivered according to the course outline and meets my expectations`), # nolint
    `E. The topics are clear and logically developed` =
      mean(`The topics are clear and logically developed`),
    `F. I am developing my oral and writing skills` =
      mean(`I am developing my oral and writing skills`),
    `G. I am developing my reflective and critical reasoning skills` =
      mean(`I am developing my reflective and critical reasoning skills`), # nolint
    `H. The assessment methods are assisting me to learn` =
      mean(`The assessment methods are assisting me to learn`),
    `I. I receive relevant feedback` =
      mean(`I receive relevant feedback`),
    `J. I read the recommended readings and notes` =
      mean(`I read the recommended readings and notes`),
    `K. I use the eLearning material posted` =
      mean(`I use the eLearning material posted`),
    `L. Mean Overall Course Evaluation Rating` =
      mean(`Average Course Evaluation Rating`),
  ) %>%
  # If we had to sort the results
  # arrange(`Mean Average Course Evaluation Rating`) %>%
  select(
    `Student's Gender`,
    `A. I am enjoying the subject`,
    `B. Classes start and end on time`,
    `C. The learning environment is participative, involves learning by doing and is group-based`, # nolint
    `D. The subject content is delivered according to the course outline and meets my expectations`, # nolint
    `E. The topics are clear and logically developed`,
    `F. I am developing my oral and writing skills`,
    `G. I am developing my reflective and critical reasoning skills`, # nolint
    `H. The assessment methods are assisting me to learn`,
    `I. I receive relevant feedback`,
    `J. I read the recommended readings and notes`,
    `K. I use the eLearning material posted`,
    `L. Mean Overall Course Evaluation Rating`
  )

View(evaluation_rating_per_question_per_gender)

evaluation_rating_per_question_per_gender_long_data <- evaluation_rating_per_question_per_gender %>% # nolint
  pivot_longer(
    cols = -`Student's Gender`,
    names_to = "Evaluation Question",
    values_to = "Mean Value")

View(evaluation_rating_per_question_per_gender_long_data)

evaluation_rating_per_question_per_gender_long_data <- # nolint
  evaluation_rating_per_question_per_gender_long_data %>%
  mutate(`Evaluation Question` =
           factor(`Evaluation Question`,
                  levels =
                    c("A. I am enjoying the subject",
                      "B. Classes start and end on time",
                      "C. The learning environment is participative, involves learning by doing and is group-based", # nolint
                      "D. The subject content is delivered according to the course outline and meets my expectations", # nolint
                      "E. The topics are clear and logically developed",
                      "F. I am developing my oral and writing skills",
                      "G. I am developing my reflective and critical reasoning skills", # nolint
                      "H. The assessment methods are assisting me to learn",
                      "I. I receive relevant feedback",
                      "J. I read the recommended readings and notes",
                      "K. I use the eLearning material posted",
                      "L. Mean Overall Course Evaluation Rating")
           )) %>%
  mutate(`Student's Gender` =
           factor(`Student's Gender`, levels = c("Male", "Female")))

View(evaluation_rating_per_question_per_gender_long_data)

# This is done to enable word wrapping when the plot is created
evaluation_rating_per_question_per_gender_long_data$`Evaluation Question` <- # nolint
  str_wrap(evaluation_rating_per_question_per_gender_long_data$`Evaluation Question`, # nolint
           width = 30)

### Visualizations (Grouped Vertical Bar Chart) ----
# ggplot2 visualization samples are available here:
# https://r-graph-gallery.com/index.html

ggplot(evaluation_rating_per_question_per_gender_long_data,
       aes(fill = `Student's Gender`, y = `Mean Value`,
           x = `Evaluation Question`, label = `Mean Value`)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +  # Flip the coordinates to make it vertical
  geom_text(position = position_dodge(width = 0.9),
            hjust = 1, vjust = 0.5) +  # Add text labels
  labs(title = "Standard Course Evaluation Score per Question per Gender",
       x = "Standard Course Evaluation Questions", y = "Mean Value") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  blue_grey_theme() +
  geom_hline(yintercept = 4, color = "#b90c0c",
             linetype = "dashed", size = 1)
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%20nine%20Code%20Chunk-2.png)<!-- -->

**etc.** as per the lab submission requirements. Be neat and communicate
in a clear and logical manner.
