CO2_effects <- function(NDVIadj, C4frac, CO2, refyear=1980,y_str=1982,y_end=2014){
  NDVIadj <- as.numeric(NDVIadj)
  # ========== Get the CO2 data ==========
  if ((C4frac > 1)||(C4frac<0)){
    print(C4frac)
    stop("C4frac must be between 0 and 1")
  }

  franks_FvC <- function(Ca){
    theta = 0.7         # shape of the light response curve
    gamma_star = 40.0   # CO2 compensation point
    return ((theta * Ca - gamma_star) / (theta * Ca + 2.0 * gamma_star))
  }

  # ========== Calculate the baseline ==========
  str_loc <- which(rownames(CO2)==y_str,)
  end_loc <- which(rownames(CO2)==y_end,)
  baseline_Ca = CO2[rownames(CO2)==refyear,]
  baseAnet    = franks_FvC(baseline_Ca)
  # # ========== get the CO2 concentration of the dataset years ==========
  # +++++ build a matching CO2 series +++++
  Ca <- as.numeric( CO2[str_loc:end_loc,] )

  Anet           = franks_FvC(Ca)
  model_response = Anet / baseAnet
  # ========== NDVI with CO2 effects ==========
  NDVI_CO2 <- as.numeric(NDVIadj)*model_response/(1-C4frac+model_response*C4frac)
  return(NDVI_CO2)
}
