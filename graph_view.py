#!/usr/bin/env python3

# I made this to create a way to view the graphs of the algorithms
# NOTE install plotly for python with "pip install plotly"

import plotly
from plotly.graph_objs import Scatter, Line, Marker, Figure, Data, Layout
import re

location_pattern = re.compile(r"[0-9]+ [0-9\.]+ [0-9\.]+")  # match locations
results_pattern = re.compile(r"\([0-9\ ]+\)")  # match locations
data_points = {}
paths = []

# READ THINGS

def add_data_points(string, data_points):
	match = location_pattern.findall(string)
	for x in match:
		data_points[int(x.split(' ')[0])] = [float(v) for v in x.split(' ')[1:]]
	return data_points

def add_paths(string, paths):
	match = results_pattern.findall(string)
	paths.extend([[int(v) for v in x.strip('()').split(' ')] for x in match])
	return paths

with open('out.txt', 'r') as fd:
	for line in fd:
		if ":CUSTOMER.LOCATIONS" in line:
			data_points = add_data_points(line, data_points)
			for line in fd:
				if line == '\n': break
				data_points = add_data_points(line, data_points)
		if ":VEHICLE-ROUTES" in line:
			line = line[:line.index(':',15)] # remove the remainder of the line after the second found ':'
			paths = add_paths(line, paths)
			for line in fd:
				if ':NUMBER-UNVISITED-LOCATIONS' in line or ':INSERTED-PAIR' in line: break
				paths = add_paths(line, paths)

## PLOT THINGS
print(paths)

node_trace = Scatter(
	x=[], y=[], text=[], mode='markers', name='Locations', textposition='bottom',
	marker=Marker( size=9, color='#555'))

full_set = set(data_points.keys())
reached_set = set([x for s in paths for x in s])
for n in data_points: #set([0]).union(full_set - reached_set):
	node_trace['x'].append(data_points[n][0])
	node_trace['y'].append(data_points[n][1])
	node_trace['text'].append(str(n))

edges = []
for i, path in enumerate(paths):
	edges.append(Scatter( x=[], y=[], name='Vehicle %d' % i, text=[],
		line=Line( width=3, autocolorscale=True),
		marker=Marker(size=15),
		hoverinfo='text', mode='lines+markers'))
	for l in range(len(path)):
		x , y = data_points[path[l]]
		edges[i]['x'].append(x)
		edges[i]['y'].append(y)
		edges[i]['text'].append("%d  [ %.1f %.1f ]" % (path[l], data_points[path[l]][0], data_points[path[l]][1]))

updatemenus = list([
	dict( buttons = list([
		dict(label="Off", method='restyle', args=['mode',['markers']+['lines+markers']*len(edges)]),
		dict(label="On", method='restyle', args=['mode',['markers+text']+['lines+markers']*len(edges)]),
	]))])

fig = Figure(data=Data([node_trace,] + edges),
			 layout=Layout( title='Search Graph', hovermode='closest', updatemenus=updatemenus))
plotly.offline.plot(fig, filename='search_graph.html', auto_open=False)

