\name{FacMean}
\alias{FacMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates a data frames factor means, sums, ...
}
\description{
As the comparables' data are retrieved for several years, FacMean simplifies the task to translage annual data in corresponding period averages.
}
\usage{
FacMean(SFrame, Fac, fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SFrame}{
The source data frame contains both, numerical values and factors.
}
  \item{Fac}{
A factor colum taken from SFrame, the source data frame.
}
  \item{fun}{
An arithmetic functioin such as mean, sum, ...
}
}
\details{
Should SFrame contain more than one factors, these factors are identified adjusted to the new size (number of rows) of the resulting data frame.
}
\value{
%%  ~Describe the value returned
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
Warnings: 
## The name convention (columns of the output data frame) requires review.
## Defaults for the arithmetic function are not yet provided.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==> PContRaD_01 <- FacMean(ContRaD_01,ContRaD$Company,mean)
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (SFrame, Fac, fun) 
{
    index <- 1:ncol(SFrame)
    row <- 1:nrow(SFrame)
    numCols <- vapply(SFrame, is.numeric, logical(1))
    facCols <- vapply(SFrame, is.factor, logical(1))
    index <- index[numCols]
    VarName <- c(names(SFrame[facCols]), paste("P", names(SFrame[index]), 
        sep = ""))
    d <- SFrame[, facCols]
    d <- d[-row[duplicated(Fac)], ]
    for (i in 1:length(index)) {
        d <- cbind(d, tapply(SFrame[[index[i]]], Fac, fun))
    }
    colnames(d) <- VarName
    d
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
