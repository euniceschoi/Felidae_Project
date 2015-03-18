#define read.table for population trend count
df <- read.table(text='Count    PopulationTrend
2  Unknown 
5 Stable
30  Decreasing', header=TRUE)

library("ggplot2")
#plot population trend onto a bar graph
ggplot(data=df, aes(x=PopulationTrend, y=Count)) + 
  geom_bar(aes(fill=PopulationTrend), stat="identity")

#define read.table for threatlevel count 
df <- read.table(text='Count    ThreatLevel
9  Vulnerable
7 Endangered
1 CriticallyEndangered
9 NearThreatened
11  LeastConcerned', header=TRUE)

library("ggplot2")
#order x axis values
ThreatLevel <- c("LeastConcerned", "NearThreatened", "Vulnerable", "Endangered", "CriticallyEndangered")
#plot threatlevel trend onto bar graph
ggplot(data=df, aes(x = ThreatLevel, y=Count)) + 
  scale_x_discrete(limits = ThreatLevel) + 
  geom_bar(aes(fill=ThreatLevel), stat="identity")
