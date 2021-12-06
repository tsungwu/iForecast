\name{data-sets}


\alias{ES_15m}

\alias{ES_Daily}

\alias{macrodata}

\alias{bc}

\title{Economic and Financial Data Sets}

\encoding{UTF-8}

\description{

ES_15m is 15-min realized absolute variance of e-mini S&P 500. macrodata contains monthly US unemployment(unrate), ES_Daily is daily realized absolute variance of e-mini S&P 500. macrodata contains monthly US unemployment(unrate) and  and year-to-year changes in three regional business cycle indices (OECD, NAFTA, and G7).
bc contains monthly business cycle data, bc is binary indicator(0=recession, 1=boom) of Taiwan's business cycle phases, IPI_TWN is industrial production index of Taiwan, LD_OECD, LD_G7, and LD_NAFTA are leading indicators of OECD, G7 and NAFTA regions; all four are monthly rate of changes.
}


\usage{
data(ES_15m)
data(macrodata)
data(ES_Daily)
data(bc)
}



\value{

    an object of class \code{"zoo"}.

}


\keyword{datasets}

