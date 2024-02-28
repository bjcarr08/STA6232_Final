######## SCRATCH (DS2 Final)
######## QUESTION 1

library(tidyverse)

#### Dataset 1: Spotify

# danceability ~ loudness + energy + playlist_genre
  
# data
spotify <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# check columns for NA values
data.frame("NAs" = colSums(is.na(spotify))) %>% 
  filter(NAs > 0)

# subset data & factor playlist_genre
spotify1 <- spotify %>% 
  select(danceability, loudness, energy, playlist_genre) %>%
  mutate(playlist_genre = as.factor(playlist_genre))



#### 1a-i
#### Let's do some data exploration. 
#### What are the possible responses in playlist_genre? 
#### How many songs fall into each genre?

(playlist_genre_RANKED <- count(spotify1, playlist_genre, sort=T))

spotify1 %>%
  mutate(playlist_genre = factor(playlist_genre, levels = playlist_genre_RANKED$playlist_genre, ordered=T)) %>%
  ggplot(aes(playlist_genre, col=playlist_genre, fill=playlist_genre)) +
  geom_bar(alpha=0.3) +
  theme_bw() +
  theme(legend.position="none")

#### 1a-ii
#### Let's do some data exploration
#### What is the mean danceability, loudness, and energy for each level of playlist_genre?

spotify1 %>%
  group_by(playlist_genre) %>%
  summarise_all(., mean)

spotify1 %>%
  pivot_longer(cols=danceability:energy) %>%
  filter(value > -30) %>%
  group_by(playlist_genre, name) %>%
  ggplot(aes(x=playlist_genre, y=value, group=playlist_genre, col=playlist_genre, fill=playlist_genre)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position="none") +
  facet_wrap(name~., scales="free")

#### 1b
#### Consider modeling danceability as a function of 
#### loudness, energy, playlist_genre, and the interaction between loudness and energy

#### 1b-i
#### What method (distribution) do you think is appropriate to apply here? 
#### Provide evidence to support your idea.

genres <- as.character(playlist_genre_RANKED$playlist_genre)

normHist_FUN <- function(i, xaxt="n"){
  rcompanion::plotNormalHistogram(spotify1$danceability[spotify1$playlist_genre==genres[i]], 
                                  prob=T,
                                  xaxt=xaxt,
                                  yaxt="n",
                                  col=rainbow(6,alpha=0.4)[i],
                                  border=colorspace::darken(rainbow(6)[i], 0.2),
                                  linecol=1,
                                  xlim=c(0,1))
  text(x=0, y=1.5, labels=genres[i], col=colorspace::darken(rainbow(6)[i], 0.3), cex=1.5, adj=c(0,0.5))
}

par(mfrow=c(6,1), mar=c(2,0.5,0.1,0.5))
for(i in 1:5){normHist_FUN(i)}
normHist_FUN(6, "s")


set.seed(1000)
i = sample(1:nrow(spotify1), 1000)
par(mfrow=c(2,2), mar=c(6.1,5.3,1.1,2.1))
rcompanion::plotNormalHistogram(spotify1$loudness[spotify1$loudness >= -25], xlab="loudness")
plot(spotify1$danceability[i] ~ spotify1$loudness[i], xlim=c(-25,0), xlab="loudness", ylab="danceability")
par(mar=c(5,5.3,2.2,2.1))
rcompanion::plotNormalHistogram(spotify1$energy, xlab="energy")
plot(spotify1$danceability[i] ~ spotify1$energy[i], xlab="energy", ylab="danceability")


par(mfrow=c(1,1), mar=c(5,5,2,2))
plot(spotify1$loudness[i] ~ spotify1$energy[i], xlab="energy", ylab="loudness")



# re-scale danceability to have strictly positive values & estimate boxcox lambda
danceability <- spotify1$danceability + 1
danceability_lambda <- car::powerTransform(danceability)$lambda
danceability_BC <- ((danceability^danceability_lambda) - 1) / danceability_lambda
par(mfrow=c(2,1), mar=c(4,5,2,2))
rcompanion::plotNormalHistogram(spotify1$danceability, main="danceability", xlab="")
rcompanion::plotNormalHistogram(danceability_BC, main="re-scaled boxcox of danceability", xlab="")


# loudness outliers
loudOut <- spotify[spotify$loudness <= -25,]

# re-scale loudness to have strictly positive values & estimate boxcox lambda
loudness <- spotify1$loudness - min(spotify1$loudness) + 1
loudness_lambda <- car::powerTransform(loudness)$lambda
loudness_BC <- ((loudness^loudness_lambda) - 1) / loudness_lambda
par(mfrow=c(2,1), mar=c(4,5,2,2))
rcompanion::plotNormalHistogram(spotify$loudness, main="loudness", xlab="")
rcompanion::plotNormalHistogram(loudness_BC, main="re-scaled boxcox of loudness", xlab="")


# estimate boxcox lambda for energy
energy_lambda <- car::powerTransform(spotify1$energy)$lambda
energy_BC <- ((spotify1$energy^energy_lambda) - 1) / energy_lambda
par(mfrow=c(2,1), mar=c(4,5,2,2))
rcompanion::plotNormalHistogram(spotify1$energy, main="energy", xlab="")
rcompanion::plotNormalHistogram(energy_BC, main="boxcox of energy", xlab="")


## what if I make energy a binary & split at 0.75, or maybe 0.5 ??



#### 1b-ii
#### What is the resulting model using the approach from 1b-i? 

#spotify1$loudness_BC <- loudness_BC

m_reduced <- glm(danceability ~ 1, data=spotify1)
m1 <- glm(danceability ~ loudness + energy + playlist_genre + loudness:energy, data=spotify1)
#m2 <- glm(danceability ~ loudness_BC + energy + playlist_genre + loudness_BC:energy, data=spotify1)

par(mfrow=c(2,2), mar=c(5,5,5,2))
plot(m1)
#plot(m2)

summary(m1)
car::Anova(m1, type=3)
anova(m_reduced, m1, test="LRT")


#### 1b-iii
#### Check the relevant assumptions of the model
#### Are we okay to proceed with statistical inference?

# residuals are NOT normal
# residuals vs fitted plot showing 2 very distinct groups

# Danceability Normal Histogram
rcompanion::plotNormalHistogram(spotify$danceability, 
                                prob=F,
                                xlab="",
                                main="Normal Histogram of Danceability",
                                linecol=1,
                                xlim=c(0,1))


# Danceability Normal Histogram's by Genre
genres <- as.character(playlist_genre_RANKED$playlist_genre)

normHist_FUN <- function(i){
  rcompanion::plotNormalHistogram(spotify$danceability[spotify$playlist_genre==genres[i]], 
                                  prob=T,
                                  ylab="",
                                  xlab="",
                                  col=rainbow(6,alpha=0.3)[i],
                                  border=colorspace::darken(rainbow(6)[i], 0.2),
                                  linecol=1,
                                  xlim=c(0,1))
  text(x=0.1, y=1.5, labels=genres[i], col=colorspace::darken(rainbow(6)[i], 0.3), cex=2, adj=c(0,0.5))
}

par(mfrow=c(3,2), mar=c(4,5,4,4))
for(i in 1:6){normHist_FUN(i)}







spotify %>%
  group_by(playlist_genre) %>%
  ggplot() + 
  geom_histogram(aes(danceability, 
                     y=after_stat(density), 
                     group=playlist_genre, 
                     fill=playlist_genre, 
                     col=playlist_genre),
                 alpha=0.3) +
  stat_function(fun=dnorm, 
                args=list(mean(spotify$danceability), sd(spotify$danceability)))
















#
