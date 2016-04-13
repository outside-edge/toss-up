'''
Download Cricket Data
Last Edited: 05.27.15

@author: Gaurav Sood

'''

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import math
import csv
import sys
import time
import os
import simplejson
import unicodedata
from BeautifulSoup import BeautifulSoup, SoupStrainer

for match_type in ['list%20a', 'first%20class', 'odi', 'test', 't20i', 't20']: 
    results = []
    r = requests.get('http://search.espncricinfo.com/ci/content/match/search.html?all=1;page=0;search=' + match_type)
    soup = BeautifulSoup(r.text)
    last_match = int(soup.findAll('span', attrs={'class':'PaginationNmbrs'})[-1].text)
    last_page = int(math.ceil(float(last_match)/float(20)))
    for i in range(0, last_page):
        time.sleep(1)
        results_page = requests.get("http://search.espncricinfo.com/ci/content/match/search.html?search={0};all=1;page={1}".format(match_type, i))
        soupy = BeautifulSoup(results_page.text)
        for new_host in soupy.findAll('a', {'class' : 'srchPlyrNmTxt'}):
            try:
                new_host = new_host['href']
            except:
                continue
            new_host = unicodedata.normalize('NFKD', new_host).encode('ascii','ignore')
            #print(type(str.split(new_host)[3]))
            print str.split(new_host, "/")[4].split('.')[0]
            results.append(str.split(new_host, "/")[4].split('.')[0])

    with open("matches-{0}.json".format(match_type), "wb") as f:
        simplejson.dump(results, f)
