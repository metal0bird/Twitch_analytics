df=read.csv("Downloads/twitchdata_final.csv",header=T)
head(df)
library(dplyr)
df=mutate(df,Watch.Stream.ratio=Watch.time.Minutes./Stream.time.minutes.)
head(df)
head(arrange(df,-Watch.time.Minutes.))
head(arrange(df,-Watch.Stream.ratio))
head(arrange(df,-Peak.viewers))
df
df=mutate(df,followergained.total.ratio=Followers.gained/Followers)
head(arrange(df,-followergained.total.ratio))     

df2=select(df,c(Channel,Watch.Stream.ratio,Followers,followergained.total.ratio,Average.viewers,Partnered,Mature,Language))
head(df2)

data=as.numeric(factor(df2$Partnered))
data=table(data)
data
pie(data,labels=levels(factor(df2$Partnered)),main="partnered streamers")

data=as.numeric(factor(df2$Language))
data=table(data)
data
pie(data,labels=levels(factor(df2$Language)),main="languages")
plot(df2$Average.viewers,df2$Watch.Stream.ratio)
cor(df2$Average.viewers,df2$Watch.Stream.ratio)
plot(df2$followergained.total.ratio,df2$Watch.Stream.ratio)
cor(df2$followergained.total.ratio,df2$Watch.Stream.ratio)

# correlation matrix

df_num<-subset(df,select=-c(Channel,Partnered,Mature,Language))
df_num
res<-cor(df_num)
round(res,2)

# heat map

install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)


#linear regression
install.packages("randomForest")
library(randomForest)
library(ggplot2)

set.seed(4543)
data(mtcars)
rf.fit <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                       keep.forest=FALSE, importance=TRUE)

x<-c(unlist(df$Watch.time.Minutes.),unlist(df$Stream.time.minutes.),unlist(df$Peak.viewers),unlist(df$Average.viewers))
y<-df$Followers.gained
relation <- lm(y~x)

install.packages("gapminder")
library(gapminder)
library(tidyverse)

ggplot(df, aes(Watch.Stream.ratio, Followers.gained)) +
  geom_point()

lm(Watch.Stream.ratio ~ Followers.gained, data = df)
summary(lm(Watch.Stream.ratio ~ Followers.gained, data = df))

head(df)
df.subset <- df[c('Watch.time.Minutes.','Stream.time.minutes.','Peak.viewers','Average.viewers','Followers','Followers.gained','Views.gained')]
str(df.subset)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

df.subset.n <- as.data.frame(lapply(df.subset[,c(1,2,3,4,5,6,7)], normalize))
head(df.subset.n)

set.seed(123)
df.d <- sample(1:nrow(df.subset.n),size=nrow(df.subset.n)*0.7,replace = FALSE) 

train.df <- df.subset[df.d,] 
test.df <- df.subset[-df.d,] 

train.df_labels <- df.subset[df.d,7]
test.df_labels <-df.subset[-df.d,7]

#Install class package

# Load class package
library(class)
NROW(train.df_labels) 

knn.26 <- knn(train=train.df, test=test.df, cl=train.df_labels, k=26)
knn.27 <- knn(train=train.df, test=test.df, cl=train.df_labels, k=27)

knn.26
test.df_labels


#logistic models

glm(Average.viewers ~ Followers, data = df.subset.n, family = binomial)

logmod <- glm(Average.viewers ~ Followers, data = df.subset.n, family = binomial)
summary(logmod)

summary(glm(Average.viewers ~ Followers + Views.gained, data = df.subset.n, family="binomial"))

mtmod <- glm(Average.viewers ~ Followers + Views.gained, data = df.subset.n, family="binomial")
head(predict(mtmod, df.subset.n))

head(predict(mtmod, df.subset.n, type = "response"))

install.packages("pscl")
pscl::pR2(mtmod)
summary(logmod)
summary(mtmod)$aic

library(knitr)
install.packages("kableExtra")
library(kableExtra)
library(tibble)
tibble("If the difference in AIC is:" = c("0-2", "2-4", "4+"),
       "The model with lowest AIC is:" = c("Identical to the other one", "Maybe better", "Definitely better")) %>%
  kable() %>%
  kable_styling("striped")
summary(logmod)$aic - summary(mtmod)$aic
