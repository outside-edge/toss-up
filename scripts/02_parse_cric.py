'''
Parse Downloaded Cricket Data

'''

import time
import csv
import simplejson
from espncricinfo.match import Match

headers = ["team1", "team1_id", "team2", "team2_id", "win_toss", "bat_or_bowl", "outcome", "win_game", "date", "day_n_night", "ground", "rain", "duckworth_lewis", "match_id", "type_of_match", "match_type_id", "home_team_id", "umpire_1_id", "umpire_1_name", "umpire_1_country", "umpire_2_id", "umpire_2_name", "umpire_2_country", "tv_umpire_id", "tv_umpire_name", "tv_umpire_country", "referree_id", "referee_name", "referee_country", "url"]

matches = simplejson.loads(open('all_matches.json').read())
matches = list(set(matches)) # dedupe

##################################START PROCESSING DATA#########################################
with open("final_output.csv", "wb") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(headers)
    for match in matches:
        time.sleep(1)
        try:
            m = Match(int(match))
            if m.match_json()['match_status'] == 'forthcoming':
                continue
            if m.result() == '':
                continue
            if m.rain_rule() == 'D/L method':
                duckworth_lewis = 1
            else:
                duckworth_lewis = 0
            try:
                m.team_2()['team_name']
            except KeyError:
                continue
            try:
                m.team_1()['team_name']
            except KeyError:
                continue
            if m.match_json()['international_class_card'] != "":
                match_type_id = m.match_json()['international_class_id']
            else:
                match_type_id = m.match_json()['general_class_id']
            try:
                if len(m.officials()) > 0:
                    umpires = [o for o in m.officials() if o['player_type_name'] == 'umpire']
                    tv_ump = [o for o in m.officials() if o['player_type_name'] == 'tv umpire']
                    match_ref = [o for o in m.officials() if o['player_type_name'] == 'referee']
                    if len(umpires) == 2:
                        ump_1 = umpires[0]
                        ump_2 = umpires[1]
                    elif len(umpires) == 0:
                        ump_1 = {'object_id': None, 'known_as': None, 'team_name': None}
                        ump_2 = {'object_id': None, 'known_as': None, 'team_name': None}
                    else:
                        ump_1 = umpires[0]
                        ump_2 = {'object_id': None, 'known_as': None, 'team_name': None}
                else:
                    ump_1 = {'object_id': None, 'known_as': None, 'team_name': None}
                    ump_2 = {'object_id': None, 'known_as': None, 'team_name': None}
            except ValueError:
                raise
            if len(tv_ump) > 0:
                tvu_id = tv_ump[0]['object_id']
                tvu_name = tv_ump[0]['known_as']
                tvu_country = tv_ump[0]['team_name']
            else:
                tvu_id = None
                tvu_name = None
                tvu_country = None
            if len(match_ref) > 0:
                mr_id = match_ref[0]['object_id']
                mr_name = match_ref[0]['known_as']
                mr_country = match_ref[0]['team_name']
            else:
                mr_id = None
                mr_name = None
                mr_country = None
            writer.writerow([m.team_1()['team_name'], m.team_1_id(), m.team_2()['team_name'], m.team_2_id(), m.toss_winner(), m.toss_decision(), m.result(), m.match_json()['winner_team_id'], m.date(), m.lighting(), m.ground_name(), None, duckworth_lewis, match, m.match_class(), match_type_id, m.match_json()['home_team_id'], ump_1['object_id'], ump_1['known_as'], ump_1['team_name'], ump_2['object_id'], ump_2['known_as'], ump_2['team_name'], tvu_id, tvu_name, tvu_country, mr_id, mr_name, mr_country, m.match_url])
        except simplejson.scanner.JSONDecodeError:
            continue

##################################FINISHED#########################################
print "DONE." #"Wrote output to %s" %(FINAL_OUTPUT_FILE)
