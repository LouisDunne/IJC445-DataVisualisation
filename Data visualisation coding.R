# This section installs and loads the tidyverse package. This provides tools
# for data manipulation, cleaning, and visualisation.
install.packages("tidyverse")
library(tidyverse)

# This section converts selected variables into the appropriate data types,
# ensuring all audio features are in numeric format for quantitative
# analysis and visualisation
music_cw<-music_clean %>%
  mutate(
    ranking=as.integer(ranking),
    year=as.integer(year),
    danceability=as.numeric(danceability),
    energy=as.numeric(energy),
    valence=as.numeric(valence),
    tempo=as.numeric(tempo),
    loudness=as.numeric(loudness),
    acousticness=as.numeric(acousticness),
    speechiness=as.numeric(speechiness),
    instrumentalness=as.numeric(instrumentalness),
    liveness=as.numeric(liveness),
    duration_ms=as.numeric(duration_ms))

# This section provided a quick overview of the cleaned data set, displaying
# variable names, data types, and example values to verify the structure
# of the data.
glimpse(music_cw)

# This section selects the key audio features used in the analysis, removing 
# missing values and converting son duration from milliseconds to minutes
# for easier interpretation.
music_features<-music_cw %>%
  select(
    year,
    loudness,
    acousticness,
    speechiness,
    instrumentalness,
    liveness,
    duration_ms
  ) %>%
  drop_na() %>%
  mutate(
    duration_min=duration_ms/60000)

# This section checks the structure of the cleaned data set.
glimpse(music_features)

# This section calculates summary statistics for the selected audio features,
# reporting both minimum and maximum values to understand the range of each 
# variable.
music_features %>%
  summarise(
    across(
      c(loudness, acousticness, speechiness,
        instrumentalness, liveness, duration_min),
      list(min=~min(.), max=~max(.))
    )
  )

# This section rechecks the cleaned data set after summary calculations.
glimpse(music_features)

# This section explores the distribution of instrumentalness across charting
# songs. A histogram with an overlaid density curve is used as an initial
# exploratory step to assess skewness and concentration near zero.
# Insights from this informed later refinements of the visualisation.
ggplot(music_features, aes(x=instrumentalness)) +
  geom_histogram(bins=30, fill="steelblue", colour="white") +
  geom_density(colour="darkred", linewidth=1) +
  labs(
    title="Distribution of Instrumentalness in Billboard Top 100 Songs",
    x="Instrumentalness",
    y="Count"
  )

# This section refines the initial instrumentalness distribution
# visualisation by applying a log-scaled x-axis. The log transformation
# improves visibility of variation among low non-zero values in a highly
# skewed distribution. This version was selected for reporting as it more 
# effectively reveals underlying patterns in the data.
ggplot(music_features, aes(x=instrumentalness + 1e-6)) +
  geom_histogram(bins=30, fill="steelblue", colour="white") +
  scale_x_log10() +
  labs(
    title="Distribution of Instrumentalness (log scale)",
    x="Instrumentalness (log scale)",
    y="Count"
  )

# This section categorises instrumentalness into ordered levels to preserve
# nuance beyond a binary classification. Instrumentalness values are grouped
# into four ranges to improve interpretability of a highly skewed
# distribution. A colour scale is applied to reflect increasing
# levels of instrumental content from low to high.
music_features %>%
  mutate(
    instrumental_level=cut(
      instrumentalness,
      breaks=c(0, 0.25, 0.5, 0.75, 1),
      include.lowest=TRUE,
      labels=c(
        "Very low instrumentalness",
        "Low-moderate instrumentalness",
        "Moderate-high instrumentalness",
        "High instrumentalness"
      )
    )
  ) %>%
  ggplot(aes(x=instrumental_level, fill=instrumental_level)) +
  geom_bar() +
  scale_fill_manual(
    values=c(
      "Very low instrumentalness"="darkred",
      "Low-moderate instrumentalness"="lightsalmon",
      "Moderate-high instrumentalness"="lightblue",
      "High instrumentalness"="darkblue"
    )
  ) +
  labs(
    title="Levels of instrumentalness in Billboard Hot 100 songs",
    x="Instrumentalness level",
    y="Number of songs",
    fill="Instrumentalness"
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=0.5),
    axis.text.x=element_text(angle=20, hjust=1)
  )
  
# This section explores the relationship between song duration and loudness.
# A scatter plot is used to show individual songs, with transparency applied
# to reduce over plotting. A two-dimensional density contour is added to
# highlight areas of higher concentration.
ggplot(music_features, aes(x=duration_min, y=loudness)) +
  geom_point(
    alpha=0.20,
    size=1.3,
    colour="darkorange"
  ) +
  geom_density_2d(colour="black") +
  labs(
    title="Relationship between song duration and loudness",
    x="Duration (minutes)",
    y="Loudness (dB)"
  ) +
  theme_minimal()

# This section categorises song duration into discrete ranges for grouped
# comparison. Creating duration bins supports summary visualisations and 
# comparison of loudness across song lengths.
music_features<-music_features %>%
  mutate(
    duration_bin=cut(
      duration_min,
      breaks=c(0,3,4,5,6, Inf),
      labels=c("<3", "3-4", "4-5", "5-6", ">6")
      
    )
  )

#This section compares loudness distributions across song duration categories.
# A boxplot is used to summarise central tendency and variability while
# highlighting outliers. This supports comparison of loudness patterns
# across different song lengths.
ggplot(music_features, aes(x=duration_bin, y=loudness)) +
  geom_boxplot(
    fill="aquamarine",
    alpha=0.7,
    colour="black",
    size=0.4
  ) +
  labs(
    title="Loudness distribution across song duration categories",
    x="Song duration (minutes)",
    y="Loudness (dB)"
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(size=14, face="bold"),
    axis.title=element_text(size=11)
  )

# This section installs and loads the GGally package. This is used to create
# correlation-based visualisations for multivariate analysis.
install.packages("GGally")
library(GGally)

# This section creates a correlation matrix of selected musical features.
# Pairwise Pearson correlation coefficients are calculated to examine
# relationships between varaibles. Numeric labels are included to support
# precise interpretation of correlation strength and direction.
music_features %>%
  select(loudness, acousticness, speechiness,
         instrumentalness, liveness, duration_min) %>%
  GGally::ggcorr(label=TRUE)

# This section refines the correlation matrix to improve clarity and
# interpretability. Correlation values are rounded and displayed numerically,
# with a diverging colour scale centred at zero.The colour encoding
# highlights both the strength and direction of relationships between
# musical features.
music_features %>%
  select(loudness, acousticness, speechiness,
         instrumentalness, liveness, duration_min) %>%
  GGally::ggcorr(
    label=TRUE,
    label_round=2,
    low="#4575b4",
    mid="white",
    high="#d73027",
    midpoint=0
  ) +
  labs(
    title="Correlation Between Selected Musical Features",
    subtitle="Pairwise Pearson correlation coefficients"
  )


