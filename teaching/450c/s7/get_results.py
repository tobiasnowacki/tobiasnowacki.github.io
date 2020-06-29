# Import libraries
import requests
import csv
import pandas as pd

def get_election(year, start, stop):
	# get csvs for range of numeric values and given year
	iters = list(range(start, stop))
	year = str(year)
	all_files = ['https://www.andrewteale.me.uk/leap/results/' + year + '/' + str(i) + '.csv' for i in iters]
	df_list = []
	for f in all_files:
		out = pd.read_csv(f, names = colnames)
		print(f)
		if len(out) > 5:
			df_list.append(out)
	if len(df_list) > 1:
		combined = pd.concat(df_list)
		return(combined)
	return()

# Column names for resulting .csvs
colnames = ['Council', 'Ward', 'Code', 'ONS_ref', 'Candidate', 'Party', 'Votes', 'Elected']

# for each year between 2003 and 2015, concat csv and save as file
year_list = list(range(2005, 2020))
big_df = []
intervals = [1, 100, 200, 300, 400, 500, 600]
rg = range(0, 5)

for y in year_list:
	year_list = []
	for i in rg:
		grab = get_election(y, intervals[i], intervals[i+1])
		if len(grab) > 1:
			year_list.append(grab)
	year_df = pd.concat(year_list)
	year_df.to_csv(str(y) + '_combined.csv', index = False)

# Problems to fix: url timeout... (maybe break down into smaller things)