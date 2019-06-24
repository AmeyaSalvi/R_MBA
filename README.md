# Importing packages required for Market Basket Analysis
install.packages("arules")
install.packages("arulesViz")
install.packages("datasets")

# Load the installed packages
library("arules")
library("arulesViz")
library("datasets")

# To perform the M B A let us load the dataset Groceries
data("Groceries")

# Extra
Groceries
length(Groceries)

# If not able to print on console due to maximum limit exceeded
#options(max.print=1000000)



#to write this data in a file, direct output to a file called "Groceries.csv"
#sink (file = 'Groceries.csv', append = FALSE, type = c("output"), split = FALSE)
#x<-inspect(Groceries[1:length(Groceries)])

#to stop writing to this file
#sink (file = NULL)
#End of Extra

# Let us examine the first thirty rows of the data
inspect(Groceries[1:30])


# To get a Summary of the data
summary(Groceries)
# Shows us that 
# The data is stored in sparse format
# we have 9835 orders in our dataset for 169 items
# whole milk is the most frequently occuring item with 2513 occurances followed by other vegetables and rolls/buns
# there are 2159 orders with just 1 item, 1643 orders with 2 items and so on.
# the average number of items in an order is 4.409 and the median is 3



# Plot the 15 most frequently ordered items
itemFrequencyPlot(Groceries,topN=15,type="relative")
# From the graph we see that the minimum support for atleast 15 items would be close to 0.08




# The Graph in absolute terms for 15 most frequent items ordered 
itemFrequencyPlot(Groceries,topN=15,type="absolute")


# to find out frequencies of each item and display n itmes of highest frequency

x <- itemFrequency(Groceries,type="absolute")
sort_x <- sort (x, decreasing = TRUE)
N <- 15
sort_x[1:N]




 
# Market Basket Analysis
rules <- apriori(Groceries, parameter = list(supp = 0.007, conf = 0.25, minlen=2))


#extra
# writing rules in file()
sink (file = 'Rules.csv', append = FALSE, type = c("output"), split = FALSE)
inspect(rules) 

#to stop writing to this file
#sink (file = NULL)
#inspect(rules) 
# end of extra

# Let us understand the data
summary(rules)
# There are 363 rules with min support as 0.007 and confidence as 0.25
# Out of these 363, 137 are for 2 items, 214 are for 3 items and 12 are for 4 items


# Removing repetitive rules  .. not correct logic
#redundant <- which (colSums (is.subset (rules, rules)) > 1)
#rules <- rules[-redundant]

# Getting rules of 2 lenght only
rules <- apriori(Groceries, parameter = list(supp = 0.007, conf = 0.25, minlen=2,maxlen=2))
summary(rules)


# Visualizing the data
# Connecting Graph Visual
#plot (rules[1:20],method="graph",interactive=TRUE,shading="confidence")
plot (rules[1:20],method="graph",engine = 'interactive',shading="confidence")

# Scatter-plot Visual
plot (rules[1:20],method="grouped",engine = 'interactive',shading="confidence")

plot (rules, measure=c("support", "lift"), shading="confidence")





# Display the first 5 entries in rules
inspect(rules[1:5])




# Sort rules with Lift and display the first 5 entries
inspect(sort(rules, by="lift")[1:5])




# Sort rules with Confidence and display the first 5 entries
inspect(sort(rules, by="confidence")[1:5])





# To find out what customers buy BEFORE buying an item (say "other vegetables")
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="other vegetables"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])




# Similarly to find out what customers buy AFTER buying an item (say "whole milk")
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

