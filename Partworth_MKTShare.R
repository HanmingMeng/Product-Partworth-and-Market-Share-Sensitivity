wtp = function(name,url_for_design_matrix_and_preference = 'design matrix&preference.xlsx',url_for_design_competitor_cost = 'my design&competitor&cost.xlsx'){
  df1 = readxl::read_excel(paste(name,url_for_design_matrix_and_preference, sep = '_'))
  df2 = readxl::read_excel(url_for_design_competitor_cost)
  ##regression
  colnames(df1) = c('Id','Profiles','Rank','75inch','85inch','4k','Sony','Price')
  model = lm(df1$Rank~df1$`75inch`+df1$`85inch`+df1$`4k`+df1$Sony+df1$Price)
  
  high_price = max(df2[2:3,7])
  low_price = min(df2[2:3,7])
  util = (high_price-low_price)/abs(coef(summary(model))[6, 1])
  output1 = t(data.frame(
    'Attributes' = c('Intercept','Screen 75 inch','Screen 85 inch','Resolution','Sony = 1','Price (low = 0; hi =1)'),
    'Partworth' = coef(summary(model))[, 1],
    'se' = coef(summary(model))[, 2],
    'tval' = coef(summary(model))[, 3],
    'WTP' = coef(summary(model))[, 1]*util
  ))
  colnames(output1) = output1[1,]
  output1 = output1[-1,]
  attribute_range = c(max(abs(coef(summary(model))[2, 1]-coef(summary(model))[3, 1]),abs(coef(summary(model))[2, 1]),abs(coef(summary(model))[3, 1])),abs(coef(summary(model))[4, 1]),abs(coef(summary(model))[5, 1]),abs(coef(summary(model))[6, 1]))
  
  output2 = t(data.frame(
    'Attributes' = c('Screen Size','Screen Resolution','Brand Name','Price'),
    'Range' = attribute_range,
    'Importance' = c(attribute_range[1]/sum(attribute_range),attribute_range[2]/sum(attribute_range),attribute_range[3]/sum(attribute_range),attribute_range[4]/sum(attribute_range))
  ))
  colnames(output2) = output2[1,]
  output2 = output2[-1,]
  
  cost = sum(df2[1,2:6]*df2[4,2:6])
  design = df2[1:3,-1]
  price = c()
  sales = c()
  profits = c()
  for (i in -20:20){
    tmp = design
    new_price = df2$`Price (low = 0; hi =1)`[1]+i*100
    tmp$`Price (low = 0; hi =1)`[1] = new_price
    
    tmp$`Price (low = 0; hi =1)` = ( tmp$`Price (low = 0; hi =1)`-low_price)/(high_price-low_price)
    utility = as.matrix(tmp) %*% as.matrix(coef(summary(model))[, 1])
    market_share = exp(utility)[1]/sum(exp(utility))
    sale = market_share*100
    profit = (new_price - cost)*sale
    
    
    price = append(price, new_price)
    sales = append(sales, sale)
    profits = append(profits, profit)
  }
  
  #output
  sink(paste(name,'output.txt',sep = '_'))
  print(output1)
  cat('-----------------This is the dividing line-----------------\n')
  print(output2)
  cat('-----------------This is the dividing line-----------------\n')
  cat('Max price is',price[which(profits == max(profits))],'\nMax profit is',max(profits))
  sink()
  
  jpeg(file = paste(name,'sales.jpg',sep = '_'))
  plot(sales~price,type = 'l',main = 'Sales = Share x Market Size')
  jpeg(file = paste(name,'profits.jpg',sep = '_'))
  plot(profits~price,type = 'l',main = 'Profit = Margin x Sales')
  dev.off()
}

wtp('WEI_YU','design matrix&preference.xlsx','my design&competitor&cost.xlsx')
wtp('YUCHEN_ZENG','design matrix&preference.xlsx','my design&competitor&cost.xlsx')
wtp('XI_WANG','design matrix&preference.xlsx','my design&competitor&cost.xlsx')
wtp('ETHAN_MENG','design matrix&preference.xlsx','my design&competitor&cost.xlsx')
wtp('RUXIN_ZHANG','design matrix&preference.xlsx','my design&competitor&cost.xlsx')