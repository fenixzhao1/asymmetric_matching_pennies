
# coding: utf-8

# In[49]:


def pswitch(q1, q2, current_matrix=0):
   transition_probabilities = [
       [ 1, 0 ], [   0,   0 ],
       [   0,   0 ], [ 0, 1 ]
   ]
   p11, p12, p21, p22 = [
       p[current_matrix]
       for p in transition_probabilities
   ]
   Pmax = .034064
   return (p11 * q1 * q2 +
           p12 * q1 * (1 - q2) +
           p21 * (1 - q1) * q2 +
           p22 * (1 - q1) * (1 - q2)) * Pmax


# In[50]:


import random
from collections import defaultdict
samples = []
for sample in range(10000):
    i = 0
    while not random.uniform(0, 1) < pswitch(1, 1):
        i += 1
    samples.append(i)


# In[51]:


import matplotlib.pyplot as plt

plt.hist(samples, bins=100)
plt.show()


# In[52]:


import numpy
λ = 10
samples = [numpy.random.exponential(scale=1/λ) for i in range(10000)]
plt.hist(samples, bins=100)
plt.show()


# In[ ]:




