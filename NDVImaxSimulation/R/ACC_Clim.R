#============= Function used to calculate optimal accumulated ==================
#============= precipitation and temperature, which has the   ==================
#============= most higher R2 with annul max NDVI ==============================

#======== add the packages that needed ========
library(lubridate)
'''
library(TSS.RESTREND)

rawmax.df  <- AnMaxVI(CTSR.VI)
CTSR.VIadj <- franksCO2(CTSR.VI, C4[1,])
adjmax.df  <- AnMaxVI(CTSR.VIadj)
max_mon <-adjmax.df$Max.month
'''
#============= build a function to get a list =======================
#============= conclude optimal accumulated P and T =================
ACC_Clim <- function(pr,tas,acp,osp,tacp,tosp,max_mon,str_y=1982){

  first_loc<- which(year(colnames(pr))==str_y)
  max_loc  <- seq(first_loc[1]+max_mon-1,dim(pr)[2], 12)
  #===== calculate optimal accumulated precipitation =========
  op_acp <- c()
  for (i in 1:length(max_loc)) {
    str_acp<- as.numeric(max_loc[i])-acp-osp+1
    end_acp<- as.numeric(max_loc[i])-osp
    ocp    <- sum(as.numeric(pr[str_acp:end_acp]))
    op_acp <- c(op_acp,ocp)
  }
  #===== calculate optimal mean temperature =========
  if(!is.na(tosp)){
    op_avt<- c()
    for (j in 1:length(max_loc)) {
       str_avt<- as.numeric(max_loc[j])-tosp-tacp+1
       end_avt<- as.numeric(max_loc[j])-tosp
       avt    <- mean(as.numeric(tas[str_avt:end_avt]))
       op_avt <- c(op_avt,avt)
    }
  }else{
    op_avt <- rep(0,length.out=length(max_loc))
  }
  return(list(op_acp,op_avt))
}




