\name{CO2_effects}
\alias{CO2_effects}
\title{
add co2 effects on the simulated NDVImax
}
\description{
The simulated NDVimax using TSSRESTREND removed CO2 effects. So after get the
simulation, add CO2 effects on it.
}
\usage{
CO2_effects(
NDVIadj,
C4frac,
CO2,
refyear=1980
)
}

\arguments{
  \item{NDVIadj}{simulated results throught function "Sim_NDVI"}
  \item{C4frac}{The fraction of C4 vegetation}
  \item{CO2}{CO2 concentration of different years}
  \item{refyear}{The year as the baseline of CO2 concentration.}
}

\value{
returning numeric NDVImax which consrdered CO2 effects:
  \bold{NDVI_CO2} simulated NDVImax with CO2 effects
}
\references{
 Arden burrell: https://github.com/ArdenB/TSSRESTREND/
}
\author{
 Xinyue Zhang
 E-mail: xyzhang2021@gmail.com
}


\examples{
NDVI_CO2<- CO2_effects(NDVIadj, C4frac, CO2, refyear=1980)
}

