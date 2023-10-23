#!/usr/bin/env python3
import matplotlib.pyplot as plt
import datetime
import numpy as np
import pandas as pd

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

data = data_copied_from_fatods

# throw away everything that is not a pair
d = [l.split() for l in data.splitlines() if len(l.split()) == 2]
# print(d)

d2 = [(datetime.datetime.strptime(date, "%d/%m/%y"), float(w)) for (date, w) in d]
# print(d2)

df = pd.DataFrame(
    {
        "date": np.array([a for (a, b) in d2]),
        "weight": [b for (a, b) in d2],
    }
)

plt.plot(df.date, df.weight, label="weight", linewidth=3)


plt.title("The Heart Attack Diet")
plt.show()
