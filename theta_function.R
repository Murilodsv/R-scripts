fun = function(x) {
  
  a = x[1]
  b = x[2]
  c = x[3]
  
  theta = 10^(log10((freo-c)/a)/b)
  
  rmse = sqrt(sum((theta - obs)^2)/length(obs))
  print(rmse)
  
  
}
