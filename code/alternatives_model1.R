# phen:phen_n3 included

summary(model1) # p=0.0443
moran_model1 # Moran I statistic standard deviate = 84.549, p-value < 2.2e-16
summary(model1_ME) # One vector, p=0.0445, high autocorrelation left (Moran I = 73.446, p-value < 2.2e-16)
summary(model1_MEa) # Two vectors, p=0.0705, low autocorrelation left (Moran I = 2.6791, p-value = 0.003691) --> USE
# Look correlations

# phen:phen_n3 not included

summary(model1b) # p=0.0436
summary(model1b_ME) #Two vectors, p=0.0516, worse with one vector
moran_model1b_ME # Moran I statistic standard deviate = 2.1873, p-value = 0.01436

