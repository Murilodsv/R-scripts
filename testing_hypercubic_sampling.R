#install.packages("lhs")
library(lhs)
library(ggplot2)

l_samples = c(10,50,100,250,500,1000,5000, 10000)

df_gg = list()
for(n_samples in l_samples){
  
  LHS_df = setNames(data.frame(randomLHS(n_samples,2)),c('x','y'))
  RNO_df = data.frame(x=rnorm(n_samples,0.5,0.25),
                      y=rnorm(n_samples,0.5,0.25))
  
  LHS_df$Sampling = "LHS"
  RNO_df$Sampling = "RNO"
  
  df_res = rbind(LHS_df, RNO_df)
  df_res$n = n_samples
  df_gg[[length(df_gg)+1]] = df_res
  
}
df_gg = do.call(rbind, df_gg)

gg = 
ggplot(df_gg, aes(x=x,y=y)) + 
  geom_point() + 
  geom_vline(xintercept = 0, colour = "red")+
  geom_vline(xintercept = 1, colour = "red")+
  geom_hline(yintercept = 0, colour = "red")+
  geom_hline(yintercept = 1, colour = "red")+
  facet_wrap(n~Sampling,
             nrow = length(l_samples)) + 
  theme_bw()


LHS_manypars = randomLHS(1000,10)
plot(data.frame(LHS_manypars))

