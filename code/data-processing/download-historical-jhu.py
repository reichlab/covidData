import pandas as pd
import requests
import json
import os
import time
import datetime
import argparse

# optional commandline argument to only download most recent history
parser = argparse.ArgumentParser()
parser.add_argument("-r","--recent", help="download only most recent history", action="store_true")
args = parser.parse_args()
only_download_most_recent = False
if args.recent:
    only_download_most_recent = True
    
# create directory to save files if it doesn't already exist
if (not os.path.isdir('../../data-raw/JHU/')):
    os.mkdir('../../data-raw/JHU/')

# which files we will download
base_files = [
    'time_series_covid19_deaths_US.csv',
    'time_series_covid19_confirmed_US.csv'
]


for base_file in base_files:
    # retrieve information about all commits that modified the file we want
    all_commits = []

    page = 0
    while True:
        page += 1
        r = requests.get(
            'https://api.github.com/repos/CSSEGISandData/COVID-19/commits',
            params = {
                'path': 'csse_covid_19_data/csse_covid_19_time_series/' + base_file,
                'page': str(page)
            }
        )
        
        if (not r.ok) or (r.text == '[]'):
            break
        
        all_commits += json.loads(r.text or r.content)

    # date of each commit
    commit_dates = [
        commit['commit']['author']['date'][0:10] for commit in all_commits
    ]


    
    # sha for the last commit made each day
    commit_shas_to_get = {}
    for index, commit_date in enumerate(commit_dates):
        # location in which to save file
        result_path =  '../../data-raw/JHU/' + commit_date + '_' + base_file
        
        # delete file if it was downloaded on the commit date since it may not
        # be the last commit that day
        if os.path.isfile(result_path):
            creation_time = os.path.getctime(result_path)
            creation_date = time.strftime(
                "%Y-%m-%d",
                time.gmtime(creation_time)
            )
            if creation_date == commit_date:
                os.remove(result_path)
        
        # record as a sha to download if applicable
        commit_date_as_date = datetime.date(
                int(commit_date[0:4]),
                int(commit_date[5:7]),
                int(commit_date[8:10]))
        commit_weekday = commit_date_as_date.weekday()
        if only_download_most_recent:
            if (commit_date not in commit_shas_to_get) and \
                (not os.path.isfile(result_path)) and \
                (datetime.datetime.today().date() - commit_date_as_date < datetime.timedelta(7)):
                commit_shas_to_get[commit_date] = all_commits[index]['sha']
                break
        else:
            if (commit_date not in commit_shas_to_get) and \
                (not os.path.isfile(result_path)) and \
                (commit_weekday == 0 or commit_weekday == 6 or
                    datetime.datetime.today().date() - commit_date_as_date < datetime.timedelta(7)):
                commit_shas_to_get[commit_date] = all_commits[index]['sha']
    
    # download and save the csvs
    for commit_date, commit_sha in commit_shas_to_get.items():
        df = pd.read_csv(
            'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/' +
            commit_sha +
            '/csse_covid_19_data/csse_covid_19_time_series/' +
            base_file)
        
        result_path =  '../../data-raw/JHU/' + commit_date + '_' + base_file
        df.to_csv(result_path, index=False)
