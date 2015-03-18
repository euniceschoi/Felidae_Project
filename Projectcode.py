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
