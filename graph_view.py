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

with open('out.txt', 'r') as fd:
	for line in fd:
		if ":CUSTOMER.LOCATIONS" in line:
			for line in fd:  # NOTE this will skip the line with :CUSTOMER.LOCATIONS so if any points come after this they wont be read
				if ":CUSTOMER.DEMAND" in line:
					break
				else:
					match = location_pattern.findall(line)
					for x in match:
						data_points[int(x.split(' ')[0])] = [float(v) for v in x.split(' ')[1:]]
		if ":VEHICLE-ROUTES" in line:
			match = results_pattern.findall(line)
			paths.extend([[int(v) for v in x.strip('()').split(' ')] for x in match])

## PLOT THINGS

node_trace = Scatter(
	x=[], y=[], text=[], mode='markers', hoverinfo='text', name='Locations',
	marker=Marker( size=9, color='#888'))

for n in data_points:
	node_trace['x'].append(data_points[n][0])
	node_trace['y'].append(data_points[n][1])
	node_trace['text'].append("%d  [ %.2f %.2f ]" % (n, data_points[n][0], data_points[n][1]))

edges = []
for i, path in enumerate(paths):
	edges.append(Scatter( x=[], y=[], name='Vehicle %d' % i,
		line=Line( width=3, autocolorscale=True),
		marker=Marker(size=10),
		hoverinfo='none', mode='lines+markers'))
	for l in range(len(path)-1):
		x0 , y0 = data_points[path[l]]
		x1 , y1 = data_points[path[l+1]]
		edges[i]['x'] += [x0, x1, None]
		edges[i]['y'] += [y0, y1, None]

fig = Figure(data=Data([node_trace,] + edges),
			 layout=Layout( title='Search Graph', hovermode='closest'))
plotly.offline.plot(fig, filename='search_graph.html')

