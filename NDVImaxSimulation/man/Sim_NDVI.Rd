\name{Sim_NDVI}
\alias{Sim_NDVI}
\title{
Function use TSSRESTREND coefficients to calculate NDVI
}
\description{
This function use optimal accumulated P and T and other coefficients from
TSSRESTREND to calculate NDVI
}
\usage{
Sim_NDVI(
std_p,
std_p_bf=NULL,
std_t,
std_t_bf=NULL,
parm,
str_y=1982,
historical=FALSE)
}

\arguments{
  \item{std_p}{standard score of precipitation}
  \item{std_p_bf}{standard score of precipitation before breakpoint}
  \item{std_t}{standard score of temperature}
  \item{std_t_bf}{standard score of temperature before breakpoint}
  \item{parm}{parameters that including slope(P,T), intercept, slope change(P,T),break height}
  \item{str_y}{year that NDVI start}
  \item{historical}{scenarios of simulation, if historical set it TRUE, else
  set FALSE}
}

\value{
returning numeric simulated NDVImax:
  \bold{sim_ndvi} simulated NDVImax
}
\references{
 Arden burrell: https://github.com/ArdenB/TSSRESTREND/
}
\author{
 Xinyue Zhang
 E-mail: xyzhang2021@gmail.com
}

\examples{

sim_ndvi <- NDVI_sim (std_p,std_p_bf=std_p_bf,std_t,std_t_bf=std_t_bf,parm,str_y=1982,historical=TRUE)
}

