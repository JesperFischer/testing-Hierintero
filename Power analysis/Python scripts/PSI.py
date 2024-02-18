
from pyvirtualdisplay import Display

# Start a virtual display
with Display():
    # Import PsychoPy
    from psychopy.data import PsiHandler

import numpy as np
import math
import pandas as pd 


def get_stim(lapse,alpha,beta,ids,trials,subjects,sessions):
  
  df = pd.DataFrame(columns=['lapse','alpha','beta','participant_id','trials','subs','X',"resp","sessions"])
  
  s = -1
  for ss in range(sessions):
    for i in range(subjects):
      s = s+1
      resp = np.empty(trials[s], dtype=object)
      
      f = PsiHandler(
              nTrials=trials[s],
              intensRange=[-50.5, 50.5],
              alphaRange=[-50.5, 50.5],
              betaRange=[0.1, 25],
              intensPrecision=1,
              alphaPrecision=1,
              betaPrecision=0.1,
              delta=0.02,
              stepType="lin",
              expectedMin=0,
          )
          
      f.next()
      for i in range(trials[s]):
          resp[i] = np.random.binomial(1, lapse[s]+(1-2*lapse[s])*(0.5+0.5*math.erf((f.intensities[i]-alpha[s])/(beta[s]*np.sqrt(2)))))
          f.addResponse(resp[i])
          if(i != (trials[s]-1)):
            f.next()
          else:
            break
        
      
      data = {"lapse":lapse[s],
              "beta":beta[s],
              "alpha":alpha[s],
              "participant_id":ids[s],
              "trials":trials[s],
              "subs":i,
              "X":f.intensities,
              "resp":resp,
              "sessions":ss+1}
      
      df1 = pd.DataFrame(data)
      df = pd.concat([df, df1])
    
  #df.to_csv('/home/jespere1/Hierarchical-Interoception/slurm/psi_stim.csv', index=False)

  return(df)
