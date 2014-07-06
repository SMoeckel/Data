\name{BvDdata}
\alias{BvDdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BvD TP Catalyst comparables data input
}
\description{
The function read.csv2 is used with appropriate parameters to read .csv files with appropirate options.
}
\usage{
BvDdata(Sfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sfile}{
Sfile is a .csv file located in the respective workspace directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a data frame
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Anvis Transfer Pricing Policy Review, 2014
}
\author{
Dr. Steffen R. Möckel
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  ContRaD_01  <- BvDdata("ContRaD_01.csv")

## The function is currently defined as
function (Sfile) 
{
    dinput <- read.csv2(Sfile, header = TRUE, sep = ";", dec = ",", 
        na.strings = "n.a.")
    dinput
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
