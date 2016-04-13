import csv
import time
import requests

countries = ['England', 'Australia', 'India', 'Pakistan', 'South Africa', 'New Zealand', 'Sri Lanka', 'West Indies', 'Zimbabwe', 'Bangladesh', 'Kenya', 'Ireland', 'Canada', 'Netherlands', 'Scotland', 'Afghanistan', 'USA', 'United Arab Emirates']

def identify_grounds():
    matches = open("../data/final_output.csv")
    reader = csv.reader(matches)
    reader.next()
    match_list = list(reader)
    grounds_raw = [x[9] for x in match_list if x[9].strip() != '']
    grounds = list(set(grounds_raw))
    with open("../data/grounds.csv", "wb") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['ground_id', 'ground', 'country', 'continent', 'latitude', 'longitude'])
        for ground in grounds:
            time.sleep(1)
            ground_matches = [int(x[12]) for x in match_list if x[9] == ground]
            latest_match = max(ground_matches)
            r = requests.get('http://www.espncricinfo.com/match/engine/match/%s.json' % str(latest_match))
            country = r.json()['match']['country_name']
            continent = r.json()['match']['continent_name']
            ground_id = r.json()['match']['ground_id']
            latitude = r.json()['match']['ground_latitude']
            longitude = r.json()['match']['ground_longitude']
            writer.writerow([ground_id, ground.strip(), country, continent, latitude, longitude])
