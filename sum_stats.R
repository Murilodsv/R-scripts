#--- custom summary statistics using base aggregate function from R
#--- MSV, 2023-04-13
sum_stats = function(df_sum, targ_col, by_cols){
  
  #--- df_sum     : dataframe holding the data
  #--- targ_col   : the dataframe column name to aggregate the data [unique value]
  #--- by_cols    : the dataframe column names as the key indexers for aggregation [unique or vector of values]
  
  if(missing(targ_col)){targ_col="value"}
  
  #--- aggregation formula
  agg_formula = formula(paste0(targ_col,'~',paste(by_cols,collapse = "+")))
  
  #--- ids
  df_ids = aggregate(agg_formula, df_sum, mean)[,1:length(by_cols)]
  if(length(by_cols) == 1){df_ids = setNames(data.frame(df_ids),by_cols)}
  
  #--- custom quantiles
  q05 = function(x){quantile(x,0.05)}
  q25 = function(x){quantile(x,0.25)}
  q75 = function(x){quantile(x,0.75)}
  q95 = function(x){quantile(x,0.95)}
  
  #--- get stats
  df_res = data.frame(mean = aggregate(agg_formula, df_sum, mean  )[,length(by_cols)+1],
                      sd   = aggregate(agg_formula, df_sum, sd    )[,length(by_cols)+1],
                      min  = aggregate(agg_formula, df_sum, min   )[,length(by_cols)+1],
                      q05  = aggregate(agg_formula, df_sum, q05   )[,length(by_cols)+1],
                      q25  = aggregate(agg_formula, df_sum, q25   )[,length(by_cols)+1],
                      med  = aggregate(agg_formula, df_sum, median)[,length(by_cols)+1],
                      q75  = aggregate(agg_formula, df_sum, q75   )[,length(by_cols)+1],
                      q95  = aggregate(agg_formula, df_sum, q95   )[,length(by_cols)+1],
                      max  = aggregate(agg_formula, df_sum, q95   )[,length(by_cols)+1])
  
  #--- bind and return
  df_res = cbind(df_ids, df_res)
  return(df_res)
  
}
