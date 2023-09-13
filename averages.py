import pandas as pd
import numpy as np
def cumulativemeans(averages):
    x = []
    for j in range(len(averages)+1):
        x.append(np.prod((averages[j:])))
    return x

