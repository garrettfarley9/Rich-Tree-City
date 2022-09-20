library(readr)
library(dlookr)
library(ggplot2)
##Read in data##
geotree<-read_csv("geoID_Rich-Tree-City.csv")

###Hypothesis###
#The Arbor Day Foundation tree cities in Missouri have a higher home value than 
#those cities that aren't in Missouri
#H0: mu_tree - mu_nontree = 0
#Ha: mu_tree - mu_nontree > 0

###Transform Data##
geotree$log<- transform(geotree$X2020.01.31, method = "log")

###Visualization###
g<-ggplot(data = geotree) + 
        geom_point(mapping = aes(x = TreeCity, y = log)) +
        geom_hline(mapping = aes(yintercept = 12.05)) +
        geom_hline(mapping = aes(yintercept = 11.65))
###Statistical Analysis###
alpha = .95
#upper.tail
n_tree<-97
n_nontree<-738
mu_tree<-mean(geotree$log[geotree$TreeCity==1])
mu_nontree<-mean(geotree$log[geotree$TreeCity==0])
s_tree<-var(geotree$log[geotree$TreeCity==1])
s_nontree<-var(geotree$log[geotree$TreeCity==0])
s_tree_nontree<-sqrt((s_tree**2/n_tree)+(s_nontree**2/n_nontree))

t<- t.test(geotree$log[geotree$TreeCity==1], geotree$log[geotree$TreeCity==0], 
       alternative = "greater")
f<- var.test(geotree$log[geotree$TreeCity == 1], geotree$log[geotree$TreeCity==0])

#fit1<-lm(TreeCity ~ , geotree)
fit2<-lm(log ~ TreeCity, geotree)
fit1<-lm(X2020.01.31 ~ TreeCity, geotree)

#Write graph
