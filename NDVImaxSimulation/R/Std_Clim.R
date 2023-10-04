#==== For historical secanerios, standard score need to be calculated ==========
#==== Because the breakpoint makes the lineaner relationship changed  ==========
Std_Clim <- function(op_acp,op_acp_bf,op_avt,op_avt_bf,historical=TRUE){
  #calculate standard score of Pr
  mean_p<- mean(as.numeric(op_acp))
  dev_p <- sd(as.numeric(op_acp))
  std_p <- (op_acp-mean_p)/dev_p
  #calculate standard score of T
  mean_t<- mean(as.numeric(op_avt))
  dev_t <- sd(as.numeric(op_avt))
  std_t <- (op_avt-mean_t)/dev_t
  # Calculate the standard score before the break point
  if(!historical){
    return(list(std_p,std_t))
    } else {
      mean_p_bf<- mean(as.numeric(op_acp_bf))
      dev_p_bf <- sd(as.numeric(op_acp_bf))
      std_p_bf <- (op_acp_bf-mean_p_bf)/dev_p_bf

      mean_t_bf<- mean(as.numeric(op_avt_bf))
      dev_t_bf <- sd(as.numeric(op_avt_bf))
      std_t_bf <- (op_avt_bf-mean_t_bf)/dev_t_bf
      return(list(std_p,std_t,std_p_bf, std_t_bf))
  }

}


#sim.cmip6 <- mpi.remote.exec(apply_func, (pr[line, ], tas[line,], mons[line,], parm[line, ]),simplify = TRUE, comm = 1, ret = TRUE )
