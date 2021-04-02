import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import csv

start = 1752 # Excel index for first observation of desired period
end = 2101 # Excel index for last observation of desired period
stability = 'unstable'
speed = 'slow'
period = 26

with open('Slow 150sec for barber polls.csv') as csv_reader:
    reader=csv.reader(csv_reader)
    reader_list=[]
    for i in reader:
        reader_list.append(i)
csv_reader.close()
data=np.asarray(reader_list).astype(float)[start-2:end-1]


median = data[:, 0]
IQR = data[:, 1]
cent_median = data[:, 2]
cent_IQR = data[:, 3]
seconds=np.linspace(26*.4,375*.4, data.shape[0])


fig = plt.figure(figsize=(4, 4))
ax = fig.add_subplot(111, projection='3d')
ax.xaxis.set_tick_params(labelsize=6)
ax.yaxis.set_tick_params(labelsize=6)
ax.zaxis.set_tick_params(labelsize=6)

ax.plot3D(median, IQR, seconds, 'blue', alpha=.6)
ax.plot3D(cent_median, cent_IQR, seconds, 'red', alpha=.6)

for i in xrange(0, 20, 1):
    ax.view_init(elev=20, azim=i)

ax.set_xlim(0, 2)
ax.set_ylim(0, 1)
ax.set_zlim(0, 150)

ax.set_title('%s, %s, period %d'%(stability, speed, period), fontsize=9)
ax.set_xlabel('Median', fontsize=7)
ax.set_ylabel('IQR', fontsize=7)
ax.set_zlabel('Seconds', fontsize=7)
ax.invert_zaxis()

fig.savefig("%s_%s_period%d.pdf"%(stability, speed, period)) 

plt.show()