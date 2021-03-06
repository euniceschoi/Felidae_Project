curl 'http://www.iucnredlist.org/search' -H 'Host: www.iucnredlist.org' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:35.0) Gecko/20100101 Firefox/35.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Referer: http://www.iucnredlist.org/' -H 'Cookie: _newiucnredlist_session=2f736adea4e261166188977cec387f23; __utma=107924634.1582988870.1424392749.1424392749.1424392749.1; __utmb=107924634.2.10.1424392749; __utmc=107924634; __utmz=107924634.1424392749.1.1.utmcsr=iucn.org|utmccn=(referral)|utmcmd=referral|utmcct=/knowledge/tools/databases/; __utmt=1' -H 'Connection: keep-alive' -H 'If-None-Match: "34c4a2eb0a0d43839b67ba3d5c22f1a7"' -H 'Cache-Control: max-age=0' > felidae_web.html
#curl website and save data onto file

from bs4 import BeautifulSoup
soup = BeautifulSoup(open("felidae_web.html"))
#open a new file to write onto
file= open("felidae_data.html", "w", "r")
#find description of species
felidae_pane=soup.find(id="results")
felidae_li= felidae_pane.find_all("li")
for li in felidae_li:
  status= li.find(class_="desc")
  for string in status.strings:
    #write webscraped data onto the new file
    file.write(string.strip())

#open new file called poptrend
file = open("poptrend.txt","w",)
#Search for population trends in felidae file
for line in open('felidae_web'):
    rec = line.strip()
    if rec.startswith('Pop'):
        #write lines population trends onto the new file 
        file.write(line)
        file.close()

#open poptrend file
file = open(r"poptrend.txt", "r", encoding="utf-8-sig")
from collections import Counter
#count number of each trend
wordcount = Counter(file.read().split())
#print population trend next to count
for item in wordcount.items(): print("{}\t{}".format(*item))



#open new file called threatlevel
file = open("threatlevel.txt","w",)
#Search for threatlevels in felidae file
for line in open('felidae_web'):
    rec = line.strip()
    if rec.startswith('Status'):
        #write lines population trends onto the new file 
        file.write(line)
        file.close()

#open threatlevel file
file = open(r"threatlevel.txt", "r", encoding="utf-8-sig")
from collections import Counter
#count number of each threat level
wordcount = Counter(file.read().split())
#print threat level next to count
for item in wordcount.items(): print("{}\t{}".format(*item))

#open file with all links
file = open("felidae_links.txt").readlines()
import subprocess
import sys
#count
count = 0
#write loop that curls each link into files with numerical labels
for line in file:
        count+=1
        print "links{}.html".format(count), line
        subprocess.call(['curl','-L',' -o',"links{}.html".format(count),line.strip()])

import glob
import re
import codecs
#import program
all_html = glob.glob("links*.html") #glob links
file= codecs.open("felidae_geog.txt", "w", "utf-8")
for html_file in all_html: #loop through html file
    with open(html_file, "r") as rfile:
        #find scientific name and countries
        soup = BeautifulSoup(rfile)
        name = soup.find('span', {'class': 'sciname'}).text 
        georange = soup.find('div', {'class': 'group'}).text  
        print (name) + (georange) + '\n' + '\n'
        #write data to a file
        file.write((name)+(georange) + '\n' + '\n') 
