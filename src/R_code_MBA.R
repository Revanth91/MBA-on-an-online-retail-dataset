library(plyr)
library(arules)

#to convert CSV to arff
data=read.csv("C:/Users/ACAL/Desktop/New folder2/dataset5_factor.csv",
              header=T)
write.arff(x=data,file= "C:/Users/ACAL/Downloads/dataset5_factor.arff")


items = ddply(dataset5,c("InvoiceNo"), function(x)paste(x$Description, collapse = ","))
head(items)

#Saving the foramtted file as csv
write.csv(items,"C:/Users/Sreejith/Documents/FAll 2019/idb/Integrared databases/project/R_Items_List.csv",quote=FALSE, row.names = TRUE)

#creating the baskets
baskets = read.transactions("C:/Users/Sreejith/Documents/FAll 2019/idb/Integrared databases/project/R_Items_List.csv", format='basket',sep=",")

#summary(baskets)
#generating the rules
basket_rules = apriori(baskets,parameter = list(sup = 0.005, conf = 0.90))

#sorting the rules generated
basket_rules = sort(basket_rules, by='confidence', decreasing = TRUE)
#inspecting the 25 rules gerated
inspect(basket_rules[1:25])

frequentItems <- eclat (baskets, parameter = list(supp = 0.007, maxlen = 15))
inspect(frequentItems)

#Visualization
itemFrequencyPlot(baskets, topN=10, type="absolute", main="Frequency")

#To identify the products that customers brought along with 'key fob'
basket_rules3 = apriori(baskets, parameter=list(supp=0.002,conf = 0.8),
                        appearance = list(default="lhs",rhs="KEY FOB"),
                        control = list(verbose=F))
#sorting the rules by lift
basket_rules3 = sort(basket_rules3, decreasing=TRUE,by="lift")
#inspecting the top 5 rules
inspect(basket_rules3[1:5])