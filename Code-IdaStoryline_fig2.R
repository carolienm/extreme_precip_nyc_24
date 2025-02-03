setwd(getwd())

# Libraries # 
{
  library(Hmisc)
  library(readxl)
  library(extRemes)
  library(Kendall)
  library(trend)
  library(plotrix)
  library(logspline)
  library(locfit)
  library(ismev)
  library(biwavelet)
  library(svMisc)
}
# Read Observation Data # 
Years_historical = as.matrix(read_excel("centralpark_data_with_climate_2023.xlsx", sheet = "centralpark_data", range = "A2:A77", col_names = FALSE)) # (calendar years 1948-2022)
AMWSR = as.matrix(read_excel("centralpark_data_with_climate_2023.xlsx", sheet = "centralpark_data", range = "G2:G77", col_names = FALSE))            # inches/hr (calendar years 1948-2022)
CDD = as.matrix(read_excel("centralpark_data_with_climate_2023.xlsx", sheet = "centralpark_data", range = "E2:E77", col_names = FALSE))            # deg-F (calendar years 1948-2022)
Temperature_data = as.matrix(read_excel("centralpark_data_with_climate_2023.xlsx", sheet = "daily_temperature", range = "A1:I27395", col_names = TRUE))            

CDD_projected_data = as.matrix(read_excel("centralpark_data_with_climate_2023.xlsx", sheet = "CMIP6_Projected_CDD", range = "B2:C68", col_names = FALSE))

CDD = AMWSR
for(i in range(Years_historical)[1]:range(Years_historical)[2])
{
  ind = which( (Temperature_data[,3]==i) & (Temperature_data[,1] > 4) & (Temperature_data[,1] < 11))
  CDD[(i-1947),1] = sum(Temperature_data[ind,9],na.rm=T)  # 9th column is CDD with baseline 65 deg F
}

#warm season CDD for 2023 calculated by CM in python, so manually adding it here: 
CDD[76]=1201

CDD_degC = (CDD)*(5/9)
CDD_projected_low_degC = (CDD_projected_data[,1])*(5/9)  # second column is low-emission 
CDD_projected_high_degC = (CDD_projected_data[,2])*(5/9) # third column is high-emission 

Ida_Rain = AMWSR[74]
Sep29_Rain = AMWSR[76]

####### -- #######

## Analyses ## 
# Step 1: Trend Checks in AMWSR 

  # Mann Kendall on AMR using Sen Slope 
  x = AMWSR
  t = Years_historical
  
  mod1 = mk.test(x)
  mod2 = sens.slope(x)
  mktau = mod1$estimates[3]
  mkpval_AMR = mod1$p.value
  mksenslope_AMR = mod2$estimates

  rbind(c("MK Sen's Slope","p-value"),c(round(mksenslope_AMR,4),round(mkpval_AMR,4)))

  # Mann Kendall on CDD using Sen Slope 
  x = CDD_degC
  t = Years_historical
  
  mod1 = mk.test(x)
  mod2 = sens.slope(x)
  mktau = mod1$estimates[3]
  mkpval_CDD = mod1$p.value
  mksenslope_CDD = mod2$estimates
  
  rbind(c("MK Sen's Slope","p-value"),c(round(mksenslope_CDD,4),round(mkpval_CDD,2)))

    # Observation: Trend is significant at the 5% level # 

  # Mann Kendall Randomized Experiment to see if Ida-only is causing trend 
  x = AMWSR
  N = 10000
  mksenslope_nullreplace = matrix(NA,0,N)
  # Initializes the progress bar
  pb <- txtProgressBar(min = 0, max = N, style = 3, width = 50, char = "=")   # Progress Bar
  for (i in 1:N)
  {
    xboot = sample(x,replace=T)
    mksenslope_nullreplace[i] = sens.slope(xboot)$estimates
    setTxtProgressBar(pb, i)
  }

  # Time-series of MK Sen Slope after the first 30 years to see when trend pick up
  x = AMWSR
  mksen_forwardtendency = NULL
  mkpval_forwardtendency = NULL
  for(i in 31:length(x))
  {
    xt = x[1:i]
    mksen_forwardtendency = c(mksen_forwardtendency,sens.slope(xt)$estimates)
    mkpval_forwardtendency = c(mkpval_forwardtendency,sens.slope(xt)$p.value)
  }

  
  ## Figures ## 
  
  #unitl 1992
  yikes=lm(AMWSR[1:45]~Years_historical[1:45])
  #until 2003
  yikes2=lm(AMWSR[1:56]~Years_historical[1:56])
  
  #fib 2b years to highlight
  short=Years_historical[31:length(Years_historical)]
  short[mkpval_forwardtendency<=.1]
  
  
  # figure 2 cm 1/22 putting all 3 together
  fout=paste("Fig2abcmetric.tiff",sep="")
  tiff(fout, height=20, width=12,units='in',compression="lzw+p",res=350)
  par(mfrow=c(3,1),cex.main=2,cex.lab=2, cex.axis=2,mar = c(4, 5, 5, 3))
  #a
  plot(Years_historical,AMWSR,type="b",xlab="Years",ylab="Annual Maximum Warm Season Hourly Rainfall (in/hr)",col="white", font=2,font.lab=2,axes=F)
  title('(A) Observations')
  axis(side=1, at=c(1950,1960,1970,1980,1990,2000,2010,2020),font=2,font.lab=2)
  axis(side=2, at=c(0,0.5,1,1.5,2,2.5,3,3.5,4),font=2,font.lab=2)
  abline(v = Years_historical, lwd=1, lty = 2, col = "gray90")
  abline(h = c(0,0.5,1,1.5,2,2.5,3,3.5,4), lwd=1, lty = 2, col = "gray90")
  points(Years_historical,AMWSR,type="b")
  abline(lm(AMWSR~Years_historical),lty=2,lwd=2,col="brown",cex=2)
  segments(x0 = 1948, x1 = 1992, y0 = yikes$coefficients[1] + yikes$coefficients[2]*1948, y1 = yikes$coefficients[1]+ yikes$coefficients[2]*1992,lwd=2,lty=2,col="blue")
  segments(x0 = 1948, x1 = 2003, y0 = yikes2$coefficients[1] + yikes2$coefficients[2]*1948, y1 = yikes2$coefficients[1]+ yikes2$coefficients[2]*2003,lwd=2,lty=2,col="orange")
  abline(h=1.75,lty=2,col="black")
  text(1960,1.8,"1.75 in/hr- NYC Sewer Design Rainfall",cex=2)
  text(2019,3.5,"Ida",font=2,cex=2)
  text(2019,3.5,"Ida",font=2,cex=2)
  text(1965,3,"Mann-Kendall Sen's Slope",font=2,cex=2)
  text(1965,2.8,paste("Estimate:", round(mksenslope_AMR*10,2), "in/hr/decade"),cex=2)
  text(1965,2.6,paste("p-value:", round(mkpval_AMR,4)),cex=2)
  # set parameter new=True for a new axis 
  par(new = TRUE)          
  # Draw second plot using axis y2 
  plot(Years_historical,AMWSR*25.4, pch = 15, col = "1", axes = FALSE, xlab = "",ylab="",font=2,font.lab=2) 
  #b
  axis(side = 4, at = pretty(range(AMWSR*25.4)),font=2,font.lab=2)      # Add second axis
  mtext("(mm/hr)", side = 4, line = 2,font=2,font.lab=2)  
  
  plot(Years_historical,c(rep(NA,30),mkpval_forwardtendency),type="b",col="white", xlab="Years",ylab="Mann-Kendall Trend Test p-value",font=2,cex=0,font.lab=2, axes=F)
  title('(B) Trends in Significance')
  axis(side=1, at=c(1950,1960,1970,1980,1990,2000,2010,2020),font=2,font.lab=2,)
  axis(side=2, at=c(0,0.05,0.1,0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),font=2,font.lab=2)
  abline(v = Years_historical, lwd=1, lty = 2, col = "gray90")
  abline(h = c(0,0.05,0.1,0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), lwd=1, lty = 2, col = "gray90")
  #shaded highlight
  rect(1991, 0, 1993,1, col = rgb(0.4,0.4,0.4,alpha=0.5),border = NA)
  rect(1995, 0, 1996,1, col = rgb(0.4,0.4,0.4,alpha=0.5),border = NA)
  rect(2020, 0, 2023,1, col = rgb(0.4,0.4,0.4,alpha=0.5),border = NA)
  abline(h=0.1,lty=3,col="black")
  text(1960,0.12,"10% rejection level",cex=2)
  abline(h=0.05,lty=2,col="black")
  text(1960,0.07,"5% rejection level",cex=2)
  points(Years_historical,c(rep(NA,30),mkpval_forwardtendency),type="b",cex=0)
  text(Years_historical, c(rep(NA,30),mkpval_forwardtendency), labels=(c(rep(NA,30),mkpval_forwardtendency)<0.1)*t, cex=0.75)
  #c
  plot(Years_historical,c(rep(NA,30),IdaRP_forwardtendency),type="b", xlab="Years",ylab="Ida's Return Period (in years)",font=2,font.lab=2,axes=F)
  title('(C) Tendency in Return Period')
  axis(side=1, at=c(1950,1960,1970,1980,1990,2000,2010,2020),font=2,font.lab=2)
  axis(side=2, at=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),font=2,font.lab=2)
  abline(v = Years_historical, lwd=1, lty = 2, col = "gray90")
  abline(h = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000), lwd=1, lty = 2, col = "gray90")
  rp_trendmod = lowess(Years_historical[31:length(Years_historical)],IdaRP_forwardtendency)
  lines(rp_trendmod,lwd=1, lty=2, col="brown")
  dev.off()
  
  
#below is reference for the NS models (but is done in python using the r2py script)
    
# Step 2: Stationary and Non-stationary GEV Models
  
  # Gumbel stationary model using extRemes packages
  fit <- fevd(x = AMWSR, method = "MLE", type="GEV")
  location_parameter = fit$results$par[1]
  scale_parameter = fit$results$par[2]
  shape_parameter = fit$results$par[3]
  
  location_se = sqrt(distill(fit)[5])
  scale_se = sqrt(distill(fit)[9])
  shape_se = sqrt(distill(fit)[13])
  
  location_pval = 1-pnorm((location_parameter/location_se))
  scale_pval = 1-pnorm((scale_parameter/scale_se))
  shape_pval = 1-pnorm((shape_parameter/shape_se))
  
  print(fit)
  rbind(c("Location","Scale","Shape"),round(c(location_pval,scale_pval,shape_pval),2))
  
    # Shape is not significant - so can assume Gumbel for a simpler model
  
  # Time-series of Ida Return Period after the first 30 years to see how it changes with recent extremes
  x = AMWSR
  IdaRP_forwardtendency = NULL
  for(i in 31:length(x))
  {
    xt = x[1:i]
    
    fit <- fevd(xt, method = "MLE", type="Gumbel")
    gev.location = fit$results$par[1]
    gev.scale = fit$results$par[2]
    gev.shape = 0
    #gev.shape = fit$results$par[3]
    
    idarp = 1/(1-pevd(Ida_Rain,loc = gev.location, scale = gev.scale, shape = gev.shape, type="GEV"))
    IdaRP_forwardtendency = c(IdaRP_forwardtendency,idarp) 
  }
  
  # Explore time and temperature variables using a non-stationary Gumbel model 
  y = AMWSR
  x1 = 1:length(y)  # scaling will be expressed in terms of in/hr per year
  x2 = CDD_degC*0.01  # scaling will be expressed in terms of in/hr per 100 degree C change 
  
  # extracting only the slope coefficient (AMWSR(t) ~ Gumbel(location(t),scale(t))); location(t) = a + b1*t or b2*CDD(t); scale(t) = a + b1*t or b2*CDD(t)
  b_location_Time = NULL
  se_b_location_Time = NULL
  b_scale_Time = NULL
  se_b_scale_Time = NULL
  
  b_location_CDD = NULL
  se_b_location_CDD = NULL
  b_scale_CDD = NULL
  se_b_scale_CDD = NULL
  
  for(i in 31:length(x))
  {
    yt = y[1:i]
    x1t = x1[1:i]
    x2t = x2[1:i]
    
    fit_ns1 <- fevd(yt, location.fun = ~x1t, scale.fun = ~x1t, method = "MLE", type="Gumbel")
    fit_ns2 <- fevd(yt, location.fun = ~x2t, scale.fun = ~x2t, method = "MLE", type="Gumbel")
    
    b_location_Time = c(b_location_Time,fit_ns1$results$par[2])
    se_b_location_Time = c(se_b_location_Time,sqrt(distill(fit_ns1)["mu1.mu1"]))
    b_scale_Time = c(b_scale_Time,fit_ns1$results$par[4])
    se_b_scale_Time = c(se_b_scale_Time,sqrt(distill(fit_ns1)["sigma1.sigma1"]))
    
    b_location_CDD = c(b_location_CDD,fit_ns2$results$par[2])
    se_b_location_CDD = c(se_b_location_CDD,sqrt(distill(fit_ns2)["mu1.mu1"]))
    b_scale_CDD = c(b_scale_CDD,fit_ns2$results$par[4])
    se_b_scale_CDD = c(se_b_scale_CDD,sqrt(distill(fit_ns2)["sigma1.sigma1"]))
  }
  
  # Stationary and Non-stationary model simulations to estimate P(Ida) in the next K years  
  
  # M0: Gumbel Parameters are updated - but still IID; temporal dependence is not assumed 
  # M1: Gumbel Parameters are time-varying on t  
  # M2: Gumbel Parameters are time-varying on CDD  
  
  plan_period1 = 10 # number of years of future timeline for planning
  plan_period2 = 25 # number of years of future timeline for planning
  plan_period3 = 50 # number of years of future timeline for planning

  x = AMWSR
  
  C_Ida = 1 # cost of damage from Ida estimated at 360M USD 
  
  Exp_Cost_M0_plan1 = NULL
  Exp_Cost_M0_plan2 = NULL
  Exp_Cost_M0_plan3 = NULL

  Exp_Cost_M1_plan1 = NULL
  Exp_Cost_M1_plan2 = NULL
  Exp_Cost_M1_plan3 = NULL

  Exp_Cost_M2_low_plan1 = NULL
  Exp_Cost_M2_low_plan2 = NULL
  Exp_Cost_M2_low_plan3 = NULL
  Exp_Cost_M2_high_plan1 = NULL
  Exp_Cost_M2_high_plan2 = NULL
  Exp_Cost_M2_high_plan3 = NULL
  
  # scaling will be expressed in terms of in/hr per 100 degree C change 
  full_CDD_low  = 0.01*c(CDD_degC,CDD_projected_low_degC[2:67])  # appending historical CDD with CMIP6 low-emission adjusted CDD
  full_CDD_high = 0.01*c(CDD_degC,CDD_projected_high_degC[2:67]) # appending historical CDD with CMIP6 high-emission adjusted CDD
  full_years = c(Years_historical,c(2024:2089)) # track of years 
  
  for(i in 45:length(x)) # starting the model from 1992 onward (but the results may be shown only at 2 points; 1992 (first time trend was significant) and 2021 (model built after Ida))
  {
    xt = x[1:i]
    
    predictor_time = 1:i
    predictor_cdd = full_CDD_low[1:i]
    
  
    # stationary model (parameters estimated using data till-date)
    fit <- fevd(xt, method = "MLE", type="Gumbel")
    gev.location = fit$results$par[1]
    gev.scale = fit$results$par[2]
    gev.shape = 0
    #gev.shape = fit$results$par[3]

    pida_stationary = 1-pevd(Ida_Rain,loc = gev.location, scale = gev.scale, shape = gev.shape, type="GEV")
    
    # based on expected cost (unit cost) derived from the decision tree (see appendix)
    Exp_Cost_M0_plan1 = c(Exp_Cost_M0_plan1, plan_period1*C_Ida*pida_stationary)
    Exp_Cost_M0_plan2 = c(Exp_Cost_M0_plan2, plan_period2*C_Ida*pida_stationary)
    Exp_Cost_M0_plan3 = c(Exp_Cost_M0_plan3, plan_period3*C_Ida*pida_stationary)

    
    # non-stationary model (time as predictor)
    fit_ns_time <- fevd(xt, location.fun = ~predictor_time, scale.fun = ~predictor_time, method = "MLE", type="Gumbel")
    
    predictor_future = (i+1):(i+plan_period3)
    full_time = c(predictor_time,predictor_future)
    
    future_gumbel_location = fit_ns_time$results$par[1] + fit_ns_time$results$par[2]*full_time
    future_gumbel_scale = fit_ns_time$results$par[3] + fit_ns_time$results$par[4]*full_time
    pida_nonstationary = NULL; for(k in 1:length(full_time)){pida_nonstationary = c(pida_nonstationary,(1-pevd(Ida_Rain,loc=future_gumbel_location[k],scale=future_gumbel_scale[k],shape=0,type="GEV")))}
    
    # based on expected cost (unit cost) derived from the decision tree (see appendix)
    Exp_Cost_M1_plan1 = c(Exp_Cost_M1_plan1, C_Ida*sum(pida_nonstationary[(i+1):(i+plan_period1)]))
    Exp_Cost_M1_plan2 = c(Exp_Cost_M1_plan2, C_Ida*sum(pida_nonstationary[(i+1):(i+plan_period2)]))
    Exp_Cost_M1_plan3 = c(Exp_Cost_M1_plan3, C_Ida*sum(pida_nonstationary[(i+1):(i+plan_period3)]))


    # non-stationary model (cdd as predictor)
    fit_ns_cdd <- fevd(xt, location.fun = ~predictor_cdd, scale.fun = ~predictor_cdd, method = "MLE", type="Gumbel")
    
    # low emission scenario 
    future_gumbel_location = fit_ns_cdd$results$par[1] + fit_ns_cdd$results$par[2]*full_CDD_low
    future_gumbel_scale = fit_ns_cdd$results$par[3] + fit_ns_cdd$results$par[4]*full_CDD_low
    pida_nonstationary_low = NULL; for(k in 1:length(full_CDD_low)){pida_nonstationary_low = c(pida_nonstationary_low,(1-pevd(Ida_Rain,loc=future_gumbel_location[k],scale=future_gumbel_scale[k],shape=0,type="GEV")))}
    
    # based on expected cost (unit cost) derived from the decision tree (see appendix)
    Exp_Cost_M2_low_plan1 = c(Exp_Cost_M2_low_plan1, C_Ida*sum(pida_nonstationary_low[(i+1):(i+plan_period1)]))
    Exp_Cost_M2_low_plan2 = c(Exp_Cost_M2_low_plan2, C_Ida*sum(pida_nonstationary_low[(i+1):(i+plan_period2)]))
    Exp_Cost_M2_low_plan3 = c(Exp_Cost_M2_low_plan3, C_Ida*sum(pida_nonstationary_low[(i+1):(i+plan_period3)]))

    # high emission scenario 
    future_gumbel_location = fit_ns_cdd$results$par[1] + fit_ns_cdd$results$par[2]*full_CDD_high
    future_gumbel_scale = fit_ns_cdd$results$par[3] + fit_ns_cdd$results$par[4]*full_CDD_high
    pida_nonstationary_high = NULL; for(k in 1:length(full_CDD_high)){pida_nonstationary_high = c(pida_nonstationary_high,(1-pevd(Ida_Rain,loc=future_gumbel_location[k],scale=future_gumbel_scale[k],shape=0,type="GEV")))}
    
    # based on expected cost (unit cost) derived from the decision tree (see appendix)
    Exp_Cost_M2_high_plan1 = c(Exp_Cost_M2_high_plan1, C_Ida*sum(pida_nonstationary_high[(i+1):(i+plan_period1)]))
    Exp_Cost_M2_high_plan2 = c(Exp_Cost_M2_high_plan2, C_Ida*sum(pida_nonstationary_high[(i+1):(i+plan_period2)]))
    Exp_Cost_M2_high_plan3 = c(Exp_Cost_M2_high_plan3, C_Ida*sum(pida_nonstationary_high[(i+1):(i+plan_period3)]))
    
    print(i)
  }
