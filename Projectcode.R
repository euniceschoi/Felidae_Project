#define read.table for population trend count
df <- read.table(text='Count    PopulationTrend
2  Unknown 
5 Stable
30  Decreasing', header=TRUE)

library("ggplot2")
#plot population trend onto a bar graph
ggplot(data=df, aes(x=PopulationTrend, y=Count)) + 
  geom_bar(aes(fill=PopulationTrend), stat="identity")


