# -*- coding: utf-8 -*-
"""
Spyder Editor

"""

import hddm
import pandas as pd
import matplotlib.pyplot as plt
import pickle
from kabuki.analyze import gelman_rubin
from kabuki.analyze import check_geweke

#set path
cd C:\...
#load data
data_hddm = hddm.load_csv('hddm_data_inpu.csv', sep = ",")

data_hddm.columns = ['response', 'rt', 'condition', 'subj_idx']

# patch to save properly
def my_save(self, fname):
    with open(fname, 'wb') as f:
        pickle.dump(self, f)
hddm.HDDM.my_save = my_save

# vary v, z and a
m_zav_hddm = hddm.HDDM(data_hddm,  include='all', depends_on={'v': 'condition', 'a' : 'condition', 'z' : 'condition'})
# find a good starting point which helps with the convergence.
m_zav_hddm.find_starting_values()
# start drawing 5000 samples and discarding 2000 as burn-in
m_zav_hddm.sample(5000, burn=2000, dbname='traces.db', db='pickle')
m_zav_hddm.my_save('m_zav_hddm')
m_zav_hddm.print_stats()
mzav_stats = pd.DataFrame(m_zav_hddm.gen_stats())
#save to csv
mzav_stats.to_csv('hddm_mzav_stats.csv')

print check_geweke(m_zav_hddm)

m_zav_hddm.plot_posteriors()
m_zav_hddm.plot_posterior_predictive()