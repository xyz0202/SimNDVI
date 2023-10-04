\name{Std_Clim}
\alias{Std_Clim}
\title{
Function that used to calculate standard score of accumulated P and T
}
\description{
When there is a breakpoint in the time series, standard score should be calculated
using the optimal accumulation and offset periods on ench side of the BP
}
\usage{
Std_Clim(
op_acp,
op_acp_bf,
op_avt,
op_avt_bf,
parm,
historical=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{op_acp}{optimal accumulated precipitation}
  \item{op_acp_bf}{optimal accumulated precipitation before breakpoint}
  \item{op_avt}{optimal accumulated temperature}
  \item{op_avt_bf}{optimal accumulated temperature before breakpoint}
  \item{historical}{scenarios of simulation, if historical set it TRUE, else
  set FALSE}
}
\details{
}
\value{
returning a list containing:
  \bold{std_p} standard score of precipitation
  \bold{std_t} standard score of temperature
  \bold{std_p_bf} standard score of precipitation bdfore breakpoint
  \bold{std_t_bf} standard score of temperature bdfore breakpoint
}
\references{
 Arden burrell: https://github.com/ArdenB/TSSRESTREND/
}
\author{
 Xinyue Zhang
 E-mail: xyzhang2021@gmail.com
}
\note{
If set historical=FALSE, std_p_bf and std_t_bf will be returned as missing values
}


\examples{
standard_score <- Std_Clim(op_acp,op_acp_bf,op_avt,op_avt_bf,historical=TRUE)
std_p   <-standard_score[[1]] #standard score of P
std_t   <-standard_score[[2]] #standard score of T
std_p_bf<-standard_score[[3]] #standard score of P before BP
std_t_bf<-standard_score[[4]] #standard score of T before BP
}
