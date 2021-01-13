import pandas as pd
import numpy as np

df_2015 = pd.read_csv('2015.csv')
df_2015['Year'] = 2015
df_2015.columns = ['Country', 'Region', 'Rank', 'HappinessScore',
       'Standard Error', 'Economy', 'Family',
       'Health', 'Freedom', 'GovernmentCorruption',
       'Generosity', 'Dystopia Residual', 'Year']
df_2016 = pd.read_csv('2016.csv')
df_2016['Year'] = 2016
df_2016.columns = ['Country', 'Region', 'Rank', 'HappinessScore',
       'CI_low', 'CI_up',
       'Economy', 'Family', 'Health',
       'Freedom', 'GovernmentCorruption', 'Generosity',
       'Dystopia Residual', 'Year']
df_2017 = pd.read_csv('2017.csv')
df_2017['Year'] = 2017
df_2017.columns = ['Country', 'Rank', 'HappinessScore', 'CI_up',
       'CI_low', 'Economy', 'Family',
       'Health', 'Freedom', 'Generosity',
       'GovernmentCorruption', 'Dystopia Residual', 'Year']
df_2018 = pd.read_csv('2018.csv')
df_2018['Year'] = 2018
df_2018.columns = ['Rank', 'Country', 'HappinessScore', 'Economy',
       'Family', 'Health',
       'Freedom', 'Generosity',
       'GovernmentCorruption', 'Year']
df_2019 = pd.read_csv('2019.csv')
df_2019['Year'] = 2019
df_2019.columns = ['Rank', 'Country', 'HappinessScore', 'Economy',
       'Family', 'Health',
       'Freedom', 'Generosity',
       'GovernmentCorruption', 'Year']

print('Check for same columns:')
for d in [df_2015, df_2016, df_2017, df_2018, df_2019]:
    print(d.columns)

df = df_2015.copy()
df_list = [df_2016, df_2017, df_2018, df_2019]

for data in df_list:
    df = df.append(data)

print('Unique years in data: ', df['Year'].unique())
print('Number of nulls in region before processing: ', df['Region'].isnull().sum())

df['Country'] = df['Country'].replace({'Trinidad & Tobago': 'Trinidad and Tobago', 'Hong Kong S.A.R., China': 'Hong Kong',
                                      'Taiwan Province of China': 'Taiwan', 'Somaliland region': 'Somaliland Region',
                                      'North Macedonia': 'Macedonia'})

dic = {}

for row in df_2015[['Country', 'Region']].values:
    dic[row[0]] = row[1]
    
dic['Puerto Rico'] = 'Latin America and Caribbean'
dic['Belize'] = 'Latin America and Caribbean'
dic['Northern Cyprus'] = 'Middle East and Northern Africa'
dic['Somalia'] = 'Sub-Saharan Africa'
dic['South Sudan'] = 'Sub-Saharan Africa'
dic['Namibia'] = 'Sub-Saharan Africa'
dic['Gambia'] = 'Sub-Saharan Africa'
dic['Somaliland Region'] = 'Sub-Saharan Africa'


df['Region'] = df.apply(lambda row: dic[row['Country']] if (row['Country'] in dic.keys()) else np.nan, axis= 1)

df = df.drop(['Standard Error', 'CI_up', 'CI_low', 'Dystopia Residual'], axis= 1)
df.to_csv('FullData.csv')

print('Number of nulls in region after processing: ', df['Region'].isnull().sum())
display(df)
