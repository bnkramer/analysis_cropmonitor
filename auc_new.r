auc = function(df, span = 0.3){
  
  # standard normalize between 0 - 1
  normalize = function(x){
    (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
  }
  
  # sort things, database isn't ordered  
  gcc = df$gcc[order(df$date)]
  date = df$date[order(df$date)]
  
  # create a complete time series (all dates, non sparse)
  full_date_range = seq(min(df$date),max(df$date),by = "days")  
  
  # smooth the data using a loess fit
  fit = loess(gcc ~ as.numeric(date), span = 0.3)
  fit_gcc = predict(fit, as.numeric(full_date_range), se = TRUE)
  greenness_smooth = fit_gcc$fit
  
  # normalize
  fit_gcc = normalize(fit_gcc)
  
  # grab first occurence of value equal to max_gcc
  max_loc = which(fit_gcc == max(fit_gcc))[1]
  
  # sum everything under the curve
  auc = sum(fit_gcc)
  
  # grab sum between start and max location
  auc_growth = sum(fit_gcc[1:max_loc])
  
  # return everything nicely
  return(list("auc" = auc,
              "auc_growth" = auc_growth))
}