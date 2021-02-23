import zipfile, io
import requests
from creds import qualtrics_token
import pandas.io.sql as sqlio
import numpy as np
import pandas as pd
import scipy


def fx(d):
    def fx_d(user_id, stim):
        order = d[(d['field'] == 'order') & (d['user_id']== user_id)]['value'].values[0].split(',')
        clean_stim = stim.replace("betty", 'r').replace("wilma", "f").replace(".png","").replace("ake", "")
        if clean_stim == "acc":
            return 20.5
        return order.index(clean_stim)
    return fx_d


def label(x):
    if 'betty' in x:
        return 'r'
    elif 'wilma' in x:
        return 'f'
    else:
        return 'a'
     

def render_data(conn, exp_id):
	demo_query = "select * from demographics"
	stim_query = "select * from stimulus"

	demo_dat = sqlio.read_sql_query(demo_query, conn)
	stim_dat = sqlio.read_sql_query(stim_query, conn)

	d = demo_dat[(demo_dat['experiment_id'] == exp_id)]
	s = stim_dat[(stim_dat['experiment_id'] == exp_id)]

	s['order'] = np.vectorize(fx(d))(s['user_id'], s['stim'])
	s['veracity'] = s['stim'].map(label)
	return d,s

def timing(users):
    timedata = []
    for user in users:
        try:
            start_feed = d[(d['session_id'] == user) & (d['field'] == 'order')]['time_stamp'].values[0]
    #         start = d[(d['session_id'] == user) & (d['field'] == 'has_twitter')]['time_stamp'].values[0]
            crt_end = d[(d['session_id'] == user) & (d['field'] == 'crt_race')]['time_stamp'].values[0]
            demo_end = d[(d['session_id'] == user) & (d['field'] == 'length')]['time_stamp'].values[0]
            
            start_feed = datetime.strptime(start_feed,  '%Y-%m-%d %H:%M:%S.%f')
            crt_end = datetime.strptime(crt_end,  '%Y-%m-%d %H:%M:%S.%f')
            demo_end = datetime.strptime(demo_end,  '%Y-%m-%d %H:%M:%S.%f')
        except:
            continue
    #     survey_length = (demo_end - start).astype('timedelta64[m]').astype(int)
    #     start_length = (start_feed - start).astype('timedelta64[m]').astype(int)
    #     feed_length = (crt_end - start_feed).astype('timedelta64[m]').astype(int)
        feed_length = (crt_end - start_feed).seconds/60
    #     demo_length = (demo_end - crt_end).astype('timedelta64[m]').astype(int)
        demo_length = (demo_end - crt_end).seconds/60
        print("survey_length: {}, start_length:{}, feed_length:{}, demo_length: {}".format(survey_length,start_length,feed_length,demo_length ))
        timedata.append((survey_length,start_length,feed_length,demo_length))
        
    
def d2of(d, users):
    dfo = d[(d['session_id'].isin(users)) & (d['field'] == 'order')][['value', 'session_id', 'condition']].reset_index()
    orders  = dfo[~dfo.duplicated(subset=['session_id'])]['value']
    order_frame = pd.DataFrame(orders.apply(lambda x:x.split(',')).values.tolist(), columns=["pos{}".format(i) for i in range(0,40)]).applymap(lambda x: x[0])
    order_frame.index = dfo[~dfo.duplicated(subset=['session_id'])]['session_id']
    return order_frame
    

def autolabel(rects, xpos='center'):
    """
    Attach a text label above each bar in *rects*, displaying its height.

    *xpos* indicates which side to place the text w.r.t. the center of
    the bar. It can be one of the following {'center', 'right', 'left'}.
    """

    ha = {'center': 'center', 'right': 'left', 'left': 'right'}
    offset = {'center': 0, 'right': 1, 'left': -1}


def mean_confidence_interval(data, confidence=0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return h


def dump_data(surveyId, dataCenter ='co1'):
	apiToken = qualtrics_token
	dataCenter = 'co1'

	# start export file creation
	baseUrl = 'https://{0}.qualtrics.com/API/v3/surveys/{1}/export-responses'.format(dataCenter, surveyId)
	headers = {
	    "x-api-token": apiToken,
	    "Content-Type": "application/json"
	    }

	data = {
	    "format": "csv",
	    "timeZone": "America/Denver"
	}

	response = requests.post(baseUrl, json=data, headers=headers)
	responseJSON = response.json()
	print(response.text)

	# get status of export file
	progressId = responseJSON['result']['progressId']
	baseUrl = 'https://{0}.qualtrics.com/API/v3/surveys/{1}/export-responses/{2}'.format(dataCenter, surveyId, progressId)
	headers = {
	    "x-api-token": apiToken,
	    "Content-Type": "application/json"
	    }

	response = requests.get(baseUrl, json=data, headers=headers)
	responseJSON = response.json()
	print(response.text)

	# download export file
	fileId = responseJSON['result']['fileId']
	baseUrl = 'https://{0}.qualtrics.com/API/v3/surveys/{1}/export-responses/{2}/file'.format(dataCenter, surveyId, fileId)
	headers = {
	    "x-api-token": apiToken,
	    "Content-Type": "application/json"
	    }

	response = requests.get(baseUrl, json=data, headers=headers)
	print(response.ok) # check that we got a response
	z = zipfile.ZipFile(io.BytesIO(response.content))
	test = z.extractall("/Users/zive/GDrive/research/yourfeed_analysis/data/{}	".format(surveyId)) # you can put a path in here to unzip the file to a certain path