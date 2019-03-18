#--- Agave Model Flexible Sigmoid Fucntion
#--- MDSV - Mar/2019

flex.sigm.fun = function(d,y.ini,y.end,tb,tm,te,t,delta){
  
  #--- A flexible sigmoid function of determinate growth.
  #--- https://doi.org/10.1093/aob/mcg029
  
  #--- Few modification were done:
  #---  (i)     y.ini and y.end were included in place of wmax to account for initial conditions
  #---  (ii)    option for cm = 1, for relative rate curve
  #---  (iii)   parameters and time is now scaled to cope with different initial tb and te
  
  
  #--- Input parameters
  # integer d               # Derivative type (Integral form [d=0], 1st derivative [d=1], 1st derivative but cm is set to 1 to return relative response [d=2])
  # real    y.ini           # Initial value,  when t = tb: y = y.ini
  # real    y.end           # Final value,    when t = te: y = y.end
  # real    tb              # Initial time of growth
  # real    tm              # Time of maximun growth rate
  # real    te              # Time for growth end
  # real    t               # Current time
  # real    delta           # Asymmetry shape factor
  
  #--- Variables
  # real    cm              # Maximum Growth Rate (inflexion point) [dw/dt]
  
  #--- Outputs
  # real    y               # Current Biomass in time t
  # real    dy              # Current Growth Rate in time t [dy/dt]
  
  fnm = "sigm.fun"
  
  if(missing(d)){d = 0}         # Integral Form as default
  if(missing(delta)){delta = 1} # No Asymmetry as default
  
  #--- parameter correction for growing period
  tb.cor  = 0
  tm.cor  = tm - tb
  te.cor  = te - tb
  
  #--- t correction for growing period
  t.cor             = t
  t.cor[t.cor<tb]   = 0
  t.cor[t.cor>te]   = 1
  t.cor[t.cor >= tb & t.cor <= te] = 1 - ((t.cor[t.cor >= tb & t.cor <= te] - te) / (tb - te))
  t.cor = t.cor * te.cor
  
  if(d == 0){
    
    #--- Check Mandatory Input Data
    sapply(c(y.ini,y.end,tm,te,t),function(x) miss.wrn(x,fnm))
    
    #--- Integral Form
    y   = y.ini + (y.end - y.ini) * (1. + (te.cor-t.cor)/(te.cor-tm.cor)) * (t.cor/te.cor)^(te.cor/(te.cor-tm.cor))
    
    #--- Constraint to wmax (either forvector, single value or gridded data)
    if(length(y) == length(y.end)){
      y[t>=te] = y.end[t>=te]  
    }else{
      y[t>=te] = y.end  
    }
    
    return(y)
  }
  
  if(d == 1){
    
    #--- Check Mandatory Input Data
    sapply(c(y.ini,y.end,tb,tm,te,t),function(x) miss.wrn(x,fnm))
    
    #--- 1st Derivative Form
    cm  = ((2. * te.cor - tm.cor) / (te.cor * (te.cor-tm.cor)) * (tm.cor / te.cor) ^ (tm.cor/(te.cor-tm.cor))) * (y.end - y.ini)
    dy  = cm * (((te.cor-t.cor)/(te.cor-tm.cor)) * ((t.cor-tb.cor)/(tm.cor-tb.cor)) ^ ((tm.cor-tb.cor)/(te.cor-tm.cor))) ^ delta
    
    #--- constrait to time limits
    dy[t >= te] = 0
    dy[t <  tb] = 0
    
    return(dy)
  }
  
  if(d == 2){
    
    #--- Check Mandatory Input Data
    sapply(c(y.ini,y.end,tb,tm,te,t),function(x) miss.wrn(x,fnm))
    
    #--- 1st Derivative Form but cm = 1 to return relative rate response
    cm  = 1
    ry  = cm * (((te.cor-t.cor)/(te.cor-tm.cor)) * ((t.cor-tb.cor)/(tm.cor-tb.cor)) ^ ((tm.cor-tb.cor)/(te.cor-tm.cor))) ^ delta
    
    #--- constrait to time limits
    ry[t >= te] = 0
    ry[t <  tb] = 0
    
    return(ry)
    
  }
  
}


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

