import csv
import requests
from BeautifulSoup import BeautifulSoup

MONTH_MAPPING = {
    'JANUARY': 1,
    'FEBRUARY': 2,
    'MARCH': 3,
    'APRIL': 4,
    'MAY': 5,
    'JUNE': 6,
    'JULY': 7,
    'AUGUST': 8,
    'SEPTEMBER': 9,
    'OCTOBER': 10,
    'NOVEMBER': 11,
    'DECEMBER': 12
}

FORMAT_RANGES = {
    'test': range(1952, 2014),
    'odi': range(1981, 2014)
}

def parser(format):
    with open("../data/rankings_%s.csv" % format, "wb") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['format', 'year', 'month', 'rank', 'country', 'rating'])
        for year in FORMAT_RANGES[format]:
            rankings = fetch_rankings(year, format)
            for ranking in rankings:
                writer.writerow(ranking)

def fetch_rankings(year, format):
    results = []
    url = "http://web.archive.org/web/20130320093711/http://www.icc-cricket.com/match_zone/%s_ranking.php?year=%s" % (format, year)
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    month_links = soup.findAll('a', attrs={'style':'color:#666666;'})
    months = [MONTH_MAPPING[m.text] for m in month_links]
    tables = soup.findAll('table', attrs={'class':'dataBox topMargin'})
    combo = zip(months, tables)
    for month, table in combo:
        for row in table.findAll('tr')[1:-1]:
            rank, country, rating = [td.text for td in row.findAll('td')]
            results.append([format, year, month, int(rank), country, int(rating)])
    return results

def fetch_current_rankings(format):
    """
    format is one of: 'test', 'odi' or 't20i'
    """
    r = requests.get("http://www.icc-cricket.com/api/getRankings")
    rankings_json = r.json()
    return [r['rankings'] for r in rankings_json if r['matchType'] == format.upper()][0]
