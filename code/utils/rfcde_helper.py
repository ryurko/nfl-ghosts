# PURPOSE: Define helper functions to fit RFCDE models using the python version
#          of the package, and then a function to spit out the predictions

import numpy as np
import rfcde

def fit_yac_rfcde(x_matrix, y_vector, N_TREES, MTRY, NB, NS):
  
  train_forest = rfcde.RFCDE(n_trees = N_TREES, mtry = MTRY, n_basis = NB, node_size = NS)
  
  train_forest.train(x_matrix, y_vector)
  return train_forest


