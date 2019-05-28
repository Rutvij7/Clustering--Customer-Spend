# Clustering # Simple Analysis on Customer Spend in Retail Store #

### Hierarchy Clustering 

customer = read.csv('spend.csv')
attach(customer)

## EDA

dim(spend)
# [1] 10  7

summary(spend)

hist(Avg_Mthly_Spend, col = c('seagreen'))

# >>>>>>  ADD MORE EDA FEATURES <<<<<<<<

##  SCALING

cust.scale = customer[,c(3,7)]
cust.scale = scale(cust.scale)

distance = dist(cust.scale, method = 'euclidean')
print(distance, digits = 3)

clust = hclust(distance, method = 'average')

plot(clust, labels = as.character(customer[,2]))
clust$height

rect.hclust(clust, k=4, border = 'green')

## CLUSTER PROFILING 

customer$cluster = cutree(clust, k=4)

avg = aggregate(customer[, -c(1,2,8)], list(customer$cluster), mean)

clust.profile = data.frame(cluster = avg[,1],
                           freq = as.vector(table(customer$cluster)),
                           avg[,-1])
View(clust.profile)
