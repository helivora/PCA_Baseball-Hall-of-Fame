####
#### Career statistics for Major League Baseball players, as well as indicators for 
#### whether the players are in the Hall of Fame (HOF).
####

## Load data.
DTA <- read.csv("hof_data.csv")
head(DTA)

## Extract a few offensive statistics (numerical variables).
num_vars <- c("H", "HR", "RBI", "AVG", "SLG", "OBP")
X <- as.matrix(DTA[, num_vars])
p <- ncol(X)

##
## Principal component analysis.
##
pca <- prcomp(X) # centers by default, see ?prcomp
pca
summary(pca)
names(pca)

# Principal components
pca$rotation

# Scores
scores <- pca$x
plot(scores[,1:2])
cor(scores[,1:2])
pairs(scores)

# Variance explained
variance_explained <- pca$sdev^2/sum(pca$sdev^2)
plot(variance_explained,type="b",xlab="PC",ylab="Variance explained")

## Summary statistics.
X_st <- scale(X, center = TRUE, scale = FALSE)
x_bar <- colMeans(X)
S <- var(X)
eigen(S) # should give PCs
pca$rotation
X_st[1:10,]%*%eigen(S)$vectors
sqrt(eigen(S)$values) # should give variances of scores

# Correlation matrix
R <- cor(X)
eigen(R)
sqrt(eigen(R)$values) 

pca <- prcomp(X,center=TRUE,scale=TRUE)
pca

variance_explained <- pca$sdev^2/sum(pca$sdev^2)
plot(variance_explained,type="b",xlab="PC",ylab="Variance explained")
