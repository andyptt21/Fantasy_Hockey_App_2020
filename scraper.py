## I've been using td class "v-top"
import time
import re
import pandas as pd
from pandas.io.html import read_html
from selenium import webdriver
driver = webdriver.Chrome()
driver.get("https://www.fantrax.com/fantasy/league/8i8nwftijzzq6mwq/standings?startDate=2019-10-02&endDate=2020-04-04&hideGoBackDays=true&period=22&timeStartType=PERIOD_ONLY&timeframeType=YEAR_TO_DATE&view=SCHEDULE&pageNumber=1")
time.sleep(5)


## Weekly matchup stats
def matchup_scraper(num):
    table = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[' + num + ']/ultimate-table/div/section/div')
    table_html = table.get_attribute('innerHTML')
    df = read_html(table_html)[0]
    teams = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[' + num + ']/ultimate-table/div')
    teams_html = teams.get_attribute('innerHTML')
    # categories = re.findall('">.*?</a></th>',teams_html)
    # for x in range(0,len(categories)):
    #     categories[x] = re.findall(';">.*?</a></th>',categories[x])
    #     categories[x] = str(categories[x])[6:]  
    #     categories[x] = categories[x][:-11]
    # df.columns = categories
    teams = re.findall("</figure>.*?<!---->",teams_html)
    for x in range(0,len(teams)):
        teams[x] = teams[x][10:]
        teams[x] = teams[x][:-8]
    df['Team'] = teams
    df['matchup'] = [num] * len(teams)
    #df.columns.values[0:4] = ['CatWins','CatLosses','CatTies','CatPts']
    df.columns = ['CatWins','CatLosses','CatTies','CatPts',
                  'Goals','Assists','Points','PlusMinus',
                  'PIM','SOG','Hits','PPP','ATOI','SHP',
                  'Blocks','Wins','GAA','Saves','G.Points',
                  'G.TOI','G.PIM','Team','matchup']
    return(df)

matchup1 = matchup_scraper('1')
matchup_df = matchup1.append(matchup_scraper('2'))

list = range(3,23)
for x in list:
    try:
        matchup_df = matchup_df.append(matchup_scraper(str(x)))
    except:
        break

## Calculate season stats and record from matchup stats in R
driver.get('https://www.fantrax.com/fantasy/league/8i8nwftijzzq6mwq/standings?startDate=2019-10-02&endDate=2020-04-04&hideGoBackDays=true&period=5&timeStartType=PERIOD_ONLY&timeframeType=YEAR_TO_DATE&view=SEASON_STATS&pageNumber=1')
time.sleep(5)
table = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[2]/ultimate-table/div/section/div')
table_html = table.get_attribute('innerHTML')
season_df = read_html(table_html)[0]

teams = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[2]/ultimate-table/div')
teams_html = teams.get_attribute('innerHTML')
teams = re.findall("</figure>.*?<!---->", teams_html)
# categories = re.findall('">.*?</a></th>', teams_html)
for x in range(0,len(teams)):
    teams[x] = teams[x][10:]
    teams[x] = teams[x][:-8]
# for x in range(0,len(categories)):
#     categories[x] = re.findall(';">.*?</a></th>',categories[x])
#     categories[x] = str(categories[x])[6:]  
#     categories[x] = categories[x][:-11]
# season_df.columns = categories
season_df.columns = ['CatWins','CatLosses','CatTies','CatPts',
                     'Goals','Assists','Points','PlusMinus',
                     'PIM','SOG','PPP','SHP','Hits',
                     'Blocks','ATOI','Wins','GAA','Saves',
                     'G.PIM','G.TOI','G.Points']
season_df['Team'] = teams
season_df = season_df.iloc[:,4:22]


driver.quit()
