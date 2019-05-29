# Clustering # Simple Analysis on Customer Spend in Retail Store #

### Hierarchy Clustering 

customer = read.csv('spend.csv')
attach(customer)

## EDA

dim(spend)
# [1] 10  7

str(customer)

summary(spend)

hist(Avg_Mthly_Spend, col = c('seagreen'))

# NO sign of any outlier
boxplot(Avg_Mthly_Spend)


##  SCALING

cust.scale = customer[,c(3:7)]
cust.scale = scale(cust.scale)

distance = dist(cust.scale, method = 'euclidean')
print(distance, digits = 3)

clust = hclust(distance, method = 'average')

plot(clust, labels = as.character(customer[,2]))
clust$height

rect.hclust(clust, k=3, border = 'green')

## CLUSTER PROFILING 

customer$cluster = cutree(clust, k=3)

avg = aggregate(customer[, -c(1,2,8)], list(customer$cluster), mean)

clust.profile = data.frame(cluster = avg[,1],
                           freq = as.vector(table(customer$cluster)),
                           avg[,-1])

print(clust.profile, digits = 3)
View(clust.profile)
write.table(clust.profile,file = 'Cust_Spend_Output_table.csv', sep = ',')

