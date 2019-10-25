## I've been using td class "v-top"
import time
import re
import pandas as pd
from pandas.io.html import read_html
from selenium import webdriver
driver = webdriver.Chrome()
driver.get("https://www.fantrax.com/fantasy/league/8i8nwftijzzq6mwq/standings?startDate=2019-10-02&endDate=2020-04-04&hideGoBackDays=true&period=3&timeStartType=PERIOD_ONLY&timeframeType=YEAR_TO_DATE&view=SCHEDULE&pageNumber=1")
time.sleep(5)


## Weekly matchup stats
def matchup_scraper(num):
    table = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[' + num + ']/ultimate-table/div/section/div')
    table_html = table.get_attribute('innerHTML')
    df = read_html(table_html)[0]
    teams = driver.find_element_by_xpath('/html/body/app-root/div/div[1]/div/app-league-standings/div/section/league-standings-tables/div/div[' + num + ']/ultimate-table/div')
    teams_html = teams.get_attribute('innerHTML')
    teams = re.findall("</figure>.*?<!---->",teams_html)
    for x in range(0,len(teams)):
        teams[x] = teams[x][10:]
        teams[x] = teams[x][:-8]
    df['Team'] = teams
    df.columns = ['CatWins','CatLosses','CatTies','CatPts',
                  'Goals','Assists','Points','PlusMinus',
                  'PIM','SOG','Hits','PPP','ATOI','SHP',
                  'Blocks','Wins','GAA','Saves','G.Points',
                  'G.TOI','G.PIM','Team']
    return(df)

matchup1 = matchup_scraper('1')
matchup_df = matchup1.append(matchup_scraper('2'))
matchup_df = matchup_df.append(matchup_scraper('3'))

## Calculate season stats and record from matchup stats in R
 # driver.get('http://fantasy.espn.com/hockey/league/standings?leagueId=21656')
 # time.sleep(5)
# table = driver.find_element_by_xpath('//*[@id="espn-analytics"]/div/div[5]/div[2]/div[1]/div/div/div[4]/section/table/tbody/tr/td[2]')
# table_html = table.get_attribute('innerHTML')
# season_df = read_html(table_html)[0]
# season_df = season_df.iloc[2:15,0:15]
# season_df.columns = season_df.iloc[0]
# season_df = season_df.drop([2])

# table = driver.find_element_by_xpath('//*[@id="espn-analytics"]/div/div[5]/div[2]/div[1]/div/div/div[4]/section/table/tbody/tr/td[1]')
# table_html = table.get_attribute('innerHTML')
# season_names_df = read_html(table_html)[0]
# season_names_df = season_names_df.iloc[:,1]
# season_df = pd.concat([season_names_df.reset_index(drop=True), season_df.reset_index(drop=True)], axis=1)

# def record_scraper(num):
#     table = driver.find_element_by_xpath('//*[@id="espn-analytics"]/div/div[5]/div[2]/div[1]/div/div/div[3]/div[' + num +']/section/table/tbody/tr/td')
#     table_html = table.get_attribute('innerHTML')
#     df = read_html(table_html)[0]
#     df = df.iloc[2:5,0:4]
#     return(df)


# record1 = record_scraper('1')
# record_df = record1.append(record_scraper('2'))
# record_df = record_df.append(record_scraper('3'))
# record_df = record_df.append(record_scraper('4'))

driver.quit()



