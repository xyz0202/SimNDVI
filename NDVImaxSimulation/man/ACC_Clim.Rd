\name{ACC_Clim}
\alias{ACC_Clim}
\title{
Calculate optimal accumulated precipitation and temperature through accumulation
periods that get from TSSRESTREND
}
\description{
TSSRESTREND find the best combination of acp and osp, which have a strong
relationship with NDVImax. This function use the best combination to get the
optimal accumulated P and T
}
\usage{
ACC_Clim(
pr,
tas,
acp,
osp,
tacp,
tosp,
max_mon,
str_y=1982
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pr}{precipitation datasets}
  \item{tas}{temperature datasets}
  \item{acp}{Optimal accumulation period of precipitation}
  \item{osp}{Optimal offset period of precipitation}
  \item{tacp}{Optimal accumulation period of temperature}
  \item{tosp}{Optimal offset period of temperature}
  \item{max_mon}{The mons that NDVImax happens}
  \item{str_y}{start year of NDVI, set "1982" originally}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing:
   \bold{op_acp} accumulated precipitation calculated by osp and acp
   \bold{op_avt} accumulated temperature (mean) calculated by tosp and tacp
}
\references{
 Arden burrell: https://github.com/ArdenB/TSSRESTREND/
}
\author{
 Xinyue Zhang
 E-mail: xyzhang2021@gmail.com
}

\examples{

a <- ACC_Clim (pr=precipitation,tas=temperature,acp=2,osp=0,tacp=4,tosp=1,max_mon=8,str_y=1982)
# precipitation and temperature should be numeric datasets
}

