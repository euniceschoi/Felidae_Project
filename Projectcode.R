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

#read table with species separated by threat and continent
df <- read.table(text='Count    ThreatLevel  Continent
                3 LeastConcerned  NorthAm
4 NearThreatened  NorthAm
1 Vulnerable  NorthAm
2 LeastConcerned  SouthAm
2 NearThreatened  SouthAm
2 Vulnerable  SouthAm
1 Endangered  SouthAm
2 LeastConcerned  Eu
1 CriticallyEndangered  Eu
5 LeastConcerned  Asia
5 NearThreatened  Asia
5 Vulnerable  Asia
5 Endangered  Asia
3 LeastConcerned  Africa
3 NearThreatened  Africa
3 Vulnerable  Africa
1 NearThreatened  Aus
1 Vulnerable  Aus', header=TRUE)

#plot using ggplot
require(ggplot2)
p <- ggplot(df, aes(Continent, Count, fill=ThreatLevel)) + 
    geom_bar(stat='identity') +
    theme_bw(base_size=16)
print(p)
