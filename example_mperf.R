#--- example of mperf usage
#--- this function was developed based on https://doi.org/10.1016/C2016-0-01552-8

#--- load the mperf function
#--- change the path accordingly!
source('C:/Murilo/CSSP-Brazil/code/bin/mperf.R')

#--- suppose you have 16 observations
obs = rnorm(16,5,2)

#--- then you run a model for these 16 observations
l_points = seq(1,16)
sim = obs * NA
for(s in l_points){
  
  #--- your model
  sim[s] = obs[s] * rnorm(1,1,0.2)
  
}

#--- you can add to a data.frame
df = data.frame(points = l_points,
                obs    = obs,
                sim    = sim)

#--- with mperf you can get statistical indexes of performance
results = 
  mperf(sim = df$sim,
        obs = df$obs,
        dchart = T,
        vnam = 'test-mperf')

print(results)
