#----------------------#
#--- Function usage ---#
#----------------------#

# Set parameters:

d     = 1
y.ini = 2
y.end = 18
tb    = 3000
tm    = 4000
te    = 5000
delta = 1
dt    = 0.1

#--- Input data
t     = seq(-500,7500, by = dt)

#--- Integral Form
y = flex.sigm.fun(0,y.ini,y.end,tb,tm,te,t,delta)

#--- First derivative form
dy = flex.sigm.fun(1,y.ini,y.end,tb,tm,te,t,delta)

#--- Relative response
ry = flex.sigm.fun(2,y.ini,y.end,tb,tm,te,t,delta)

#--- Plot results
plot(y ~ t,  type = "l", col = "blue", ylim = c(0,y.end))
plot(ry~ t,  type = "l", col = "red")
plot((dy*dt) ~ t,  type = "l", col = "red")

