### Toss-up: The Impact of Winning the Toss on Probability of Winning

We analyze data from nearly 43,000 first-class men's cricket matches -- a near census of the relevant population. And we make a series of discoveries that upend some conventional wisdom, and understanding based on analysis of much smaller datasets --- in fact, one [prominent previous study (pdf)](http://people.stat.sfu.ca/~tim/papers/cricket.pdf) basis its analysis on just about 1% of the data we have.

#### Data

* **Match Level Data**: We got our data from [espncricinfo.com](http://espncricinfo.com). We went about downloading and parsing the data a couple of different ways. Gaurav just [scraped and parsed](https://github.com/soodoku/get-cricket-data) the HTML pages. Derek, clearly the sharper of the two, realized that espncricinfo also provides a nice json API and developed [a python module](https://github.com/dwillis/python-espncricinfo).  
Aware of the duplication of work, in this repository, we only provide scripts and data that aren't available elsewhere (except for the final dataset we use). These include, a script to [download match ids](scripts/01_download_data.py), [match ids by match type (json)](data/json/), a script for [making the requests and parsing the requests](scripts/02_parse_cric.py) using the json data, and [output for ODI matches](data/odi_partial.csv) based on the script. However, the [final dataset we use](data/final_output.csv) is the same as posted on Gaurav's [repository](https://github.com/soodoku/get-cricket-data).

* **Rankings Data**: [parse_rankings](scripts/03_parse_rankings.py) gets monthly rankings for ODIs from 1981-2013 and for tests from 1952-2013. ICC changed its site in 2014 so that it only shows the most recent rankings. The script outputs [odi rankings](data/odi_ranks.csv) and [test rankings](data/test_ranks.csv).

#### Analysis, Write-up, And Figures

We began by [merging the ranking and the match data](scripts/04_merge_ranking_data.R). We next [analyzed the data](scripts/05_cricket.R). The script produces [these figures](figs/). The tex and pdf files for the final write-up can be found [here](write_up/).

#### Authors

Gaurav Sood and Derek Willis

#### License

Scripts, figures, and writing are released under [CC BY 2.0](https://creativecommons.org/licenses/by/2.0/).
