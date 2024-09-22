DF <- read.csv("./spotify.csv") # load dataset
View(DF) #View dataset

model = lm(popularity ~ duration_ms + danceability + energy + acousticness + instrumentalness, data = DF)

print(summary(model))
