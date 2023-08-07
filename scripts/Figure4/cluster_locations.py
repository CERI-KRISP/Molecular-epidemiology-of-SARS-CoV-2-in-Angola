import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter

import numpy as np

metadata_df=pd.read_excel('angola_metadata_c16.xlsx')
#print(metadata_df)

'''
ID_match=pd.read_csv('A23_new.tsv',sep='\t')
print(ID_match)

alignment_seqs=pd.read_csv('A_23_new_IDs.csv')
print(alignment_seqs)

alignment_seqs=pd.merge(alignment_seqs, ID_match, how='left')
print(alignment_seqs)

cluster_df=pd.merge(alignment_seqs, metadata_df, how='left')[['new_ID','country','division','location']]
'''
cluster_df=metadata_df[['strain','country','division','location']]
print(cluster_df)

def make_address(df):
    if pd.isnull(df['division']):
        address=str(df['country'])
    elif pd.isnull(df['location']):
    #else:
        address=str(df['division']+','+df['country'])
    else:
        address=str(df['location']+','+df['division']+','+df['country'])
    return address

cluster_df['address']=cluster_df.apply(lambda x: make_address(x), axis=1)
print(cluster_df)

#cluster_df.to_excel('text.xlsx')


geolocator = Nominatim(user_agent="houriiyah.tegally@gmail.com")
geocode = RateLimiter(geolocator.geocode, min_delay_seconds=1.5)
'''
location = geolocator.geocode("Western Cape Province, South Africa")
print(location.address)
print((location.latitude, location.longitude))
print(location)
'''

def get_geolocation(x):
    try:
        location=geolocator.geocode(x)
    except:
        print(x, 'test')
        return x
    print(x,'xx', location, 'sucess')
    return location

#cluster_df=cluster_df[:10]
geolocation_df=pd.DataFrame()
geolocation_df['address']=cluster_df['address'].drop_duplicates()
print(geolocation_df['address'])
geolocation_df['geolocation']=geolocation_df['address'].apply(lambda x: get_geolocation(x))

cluster_df=pd.merge(cluster_df, geolocation_df, how='left')
cluster_df.to_csv('test.csv')


cluster_df['latitude']=cluster_df['geolocation'].apply(lambda x: x.latitude)
cluster_df['longitude']=cluster_df['geolocation'].apply(lambda x: x.longitude)

print(cluster_df)


#location_df=cluster_df[['new_ID','latitude','longitude']]
#location_df=location_df.rename(columns={'new_ID':'taxa'})

location_df=cluster_df[['strain','latitude','longitude']]
location_df=location_df.rename(columns={'strain':'taxa'})
print(location_df)


location_df.to_csv('angola_metadata_c16.txt', sep='\t', index=False)
