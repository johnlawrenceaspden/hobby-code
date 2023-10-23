#!/usr/bin/env python3
import matplotlib.pyplot as plt
import datetime
import numpy as np
import pandas as pd
import io
import re

# plt.ion() #for interactive mode, plt.show will update graph and return

# Just cut and paste spreadsheet data here, it will deal with missing values
data_copied_from_fatods = """
21/10/23	94.3		0
20/10/23	95.2	36.59	1

21/10/23	94.3	36.48	0	1
20/10/23	95.2	36.59	1	1
19/10/23	94.6	36.68	0	1
18/10/23	94.5	36.66	0	1
17/10/23	94.6		1	1
16/10/23	95.8	37.36	0	1
15/10/23	95.9	37.12	0	1
14/10/23	95.1	36.63	0	1
13/10/23	95.5	36.66	0	1
12/10/23	95.5	36.72	0	1
11/10/23			0	1
10/10/23		36.6	0	1
09/10/23		36.7	
08/10/23		36.5	1	1
07/10/23		36.7	0	1
06/10/23		36.7	0	1
05/10/23		36.6	0	1
04/10/23		
03/10/23		36.7	0	1
02/10/23		36.5	1	1
01/10/23		36.7	0	1

21/10/23	94.3
20/10/23	95.2
19/10/23	94.6
15/05/23	
14/05/23	
"""

df=pd.read_csv(io.StringIO(data_copied_from_fatods.replace('\t',',')), names=["date", "weight","temperature", "T4", "NDT"])

plt.plot(df.date, df.weight, label="weight", linewidth=3)
plt.plot(df.date, df.temperature, label="temperature")
plt.plot(df.date, df.T4, label="temperature")
plt.plot(df.date, df.NDT, label="NDT")


plt.title("The Heart Attack Diet")
plt.show()

