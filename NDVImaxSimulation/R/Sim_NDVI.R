#===================================================
#=========== Annual NDVImax calculation ============
#===================================================
NDVI_sim <- function(acc_p,acc_t,std_p,std_p_bf=NULL,std_t,std_t_bf=NULL,parm,str_y=1982,historical=FALSE){
  BH  <- as.numeric(parm$Break.Height)
  SC  <- as.numeric(parm$Slope.Change)
  SCT <- as.numeric(parm$Slope.ChangeTmp)
  sl_p<- as.numeric(parm$pre.slope)
  sl_t<- as.numeric(parm$temp.slope)
  b   <- as.numeric(parm$intercept)
    # Make a function that based on linear relationship
    ln_ndvi <- function(B, slp_p, acc_p, slp_t, acc_t){
      slp_t[is.na(slp_t)] <- 0
      acc_t[is.na(acc_t)] <- 0
      acc_p[is.na(acc_p)] <- 0
      ndvi_cm6 <- B+slp_p*as.numeric(acc_p)+slp_t*as.numeric(acc_t)
      return(ndvi_cm6)
    }
    # Make a function that based on multi-linear relationship
    bp_loc <- as.data.frame(parm$bp.year-str_y+1)
    multiln_ndvi <- function(B, slp_p,std_p,std_p.bf, slp_t,std_t,std_t.bf, BrkH, SlpC, SlpCT,bp,his=historical){
      slp_t[is.na(slp_t)] <- 0
      SlpCT[is.na(SlpCT)] <- 0
      SlpC[is.na(SlpC)]   <- 0
      std_t[is.na(std_t)] <- 0
      std_p[is.na(std_p)] <- 0
      std_t.bf[is.na(std_t.bf)]<- 0
      std_p.bf[is.na(std_p.bf)]<- 0
      bp <- as.numeric(bp)
      #read in the break point year, the acp and osp period are different at each sides of the breakpoint
      if (!his) {
        ndvi_cm6    <- B + slp_p*as.numeric(std_p) + slp_t*as.numeric(std_t)+ BrkH + SlpC*as.numeric(std_p) + SlpCT*as.numeric(std_t)
      } else {
        ndvi_cm6.b4 <- B + slp_p*as.numeric(std_p.bf) + slp_t*as.numeric(std_t.bf)
        ndvi_cm6.af <- B + slp_p*as.numeric(std_p) + slp_t*as.numeric(std_t) + BrkH + SlpC*as.numeric(std_p) + SlpCT*as.numeric(std_t)
        ndvi_cm6    <- as.numeric(c(ndvi_cm6.b4[1:bp],ndvi_cm6.af[(bp+1):length(std_p)]))
      }

      return(ndvi_cm6)
    }
  # Make a loop to calculate NDVI
  sim_ndvi <- c()
    if (is.na(BH)){
      sim_ndvi <- ln_ndvi(b, sl_p, std_p, sl_t, std_t)
    } else{
      sim_ndvi <- multiln_ndvi(b, sl_p, std_p,std_p_bf, sl_t, std_t,std_t_bf, BH, SC, SCT,bp_loc,his = historical)
    }
  return(sim_ndvi)
}
