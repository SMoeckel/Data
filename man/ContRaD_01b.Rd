\name{ContRaD_01b}
\alias{ContRaD_01b}
\docType{data}
\title{
Contract R&D comparables data search 01b
}
\description{
Data frame provides financial and other data for further analysis
}
\usage{data(ContRaD)}
\format{
  A data frame with 68 observations on the following 51 variables.
  \describe{
    \item{\code{FY}}{a factor with levels \code{31/12/2009} \code{31/12/2010} \code{31/12/2011} \code{31/12/2012} - indicates the respective fiscal year end}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Comp}}{a factor with levels \code{APTIV SOLUTIONS (UK) LTD} \code{ARTELIA DENMARK APS} \code{AUDIO VISUAL SERVICES CORPORATION LIMITED} \code{AVC LIVE LIMITED} \code{CENTROVICE} \code{CITOXLAB SCANTOX A/S} \code{CONSTRUCTION INDUSTRY RESEARCH AND INFORMATIONASSOCIATION} \code{CVS (COMMERCIAL VALUERS & SURVEYORS) LIMITED} \code{DANAK DEN DANSKE AKKREDITERINGSFOND} \code{GECKO DIRECT LIMITED} \code{GLEEDS TECHNOLOGY LIMITED} \code{GREENRAY TURBINES (LINCOLN) LIMITED} \code{HANNAH, REED AND ASSOCIATES LIMITED} \code{INTEGRATED ENGINEERING SERVICES (ABERDEEN) LIMITED} \code{JOHN ROBERTSON ARCHITECTS LTD} \code{TUN ABDUL RAZAK RESEARCH CENTRE} \code{VALVE (ENGINEERING) SERVICES LIMITED} \code{WISSENBERG A/S RADGIVENDE INGENIORER F.R.I.}}
    \item{\code{II}}{a factor with levels \code{ D} \code{ U} \code{A-} \code{B+}; Bureau von Dijk's independence indicator}
    \item{\code{Curr}}{a factor with levels \code{DKK} \code{GBP}; 3 digit currency descriptor}
    \item{\code{RepCurr}}{a factor with levels \code{kEUR}; identifies the reporting currency and dimension}
    \item{\code{Xrate}}{a numeric vector; exchange rate used to calculate Euro values}
    \item{\code{FA}}{a numeric vector; fixed assets (total)}
    \item{\code{FA_IT}}{a numeric vector; intangible fixed assets}
    \item{\code{FA_TA}}{a numeric vector; tangible fixed assets}
    \item{\code{FA_Ot}}{a numeric vector; other fixed asset items}
    \item{\code{CA}}{a numeric vector; current assets (total)}
    \item{\code{CA_Inv}}{a numeric vector; current assets - inventories}
    \item{\code{CA_Rec}}{a numeric vector; current assets - receivables}
    \item{\code{CA_Ot}}{a numeric vector; current assets - other items}
    \item{\code{CA_Csh}}{a numeric vector; current assets - cash}
    \item{\code{Equity}}{a numeric vector; shareholders funds (total)}
    \item{\code{NCL}}{a numeric vector; non-current liabilities (total)}
    \item{\code{NCL_LTD}}{a numeric vector; non-current liabilities (long-term debt)}
    \item{\code{NCL_Ot}}{a numeric vector; non-current liabilities (other items)}
    \item{\code{NCL_Prov}}{a numeric vector; non-current liabilities (provisions)}
    \item{\code{CL}}{a numeric vector; current liabilities (total)}
    \item{\code{CL_Loans}}{a numeric vector; current liabilities (loans)}
    \item{\code{CL_Payables}}{a numeric vector; current liabilities (payables)}
    \item{\code{CL_Ot}}{a numeric vector; current liabilities (other items)}
    \item{\code{HC}}{a numeric vector; headcount}
    \item{\code{Turnover}}{a numeric vector; net sales }
    \item{\code{Sales}}{a logical vector}
    \item{\code{CoS}}{a numeric vector; Cost of Sales}
    \item{\code{Gprofit}}{a numeric vector; gross profit}
    \item{\code{OpEx}}{a numeric vector; operating expenses}
    \item{\code{EBIT}}{a numeric vector; earnings before interest and tax}
    \item{\code{FiInc}}{a numeric vector; financial income (total)}
    \item{\code{FiEx}}{a numeric vector; financial expenses (total)}
    \item{\code{FinRes}}{a numeric vector; net financial result}
    \item{\code{PBTExt}}{a numeric vector; profit before tax and extraordinary result}
    \item{\code{Tax}}{a numeric vector; taxes (income)}
    \item{\code{PAT}}{a numeric vector; profit after tax}
    \item{\code{ExtRev}}{a logical vector; extraordinary revenues}
    \item{\code{ExtExp}}{a logical vector; extraordinary expenses}
    \item{\code{ExtRes}}{a logical vector; extraordinary result}
    \item{\code{NetInc}}{a numeric vector; net income}
    \item{\code{Xsales}}{a numeric vector; export turnover}
    \item{\code{MatCoS}}{a logical vector; material cost}
    \item{\code{SW}}{a numeric vector; salaries and wages/cost of employees}
    \item{\code{Dep}}{a numeric vector; depreciation}
    \item{\code{IntExp}}{a numeric vector; interest paid}
    \item{\code{RaDExp}}{a logical vector; research and development expenses}
    \item{\code{CF}}{a numeric vector; cash flow}
    \item{\code{AV}}{a numeric vector; added value}
    \item{\code{EBITDA}}{a numeric vector; earnings before interest and depreciation}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Bureau van Dijk, TP Catalyst, Release 49 
}
\references{
Anvis Transfer Pricing Project 2014
}
\examples{
data(ContRaD)
## maybe str(ContRaD_01b) ; plot(ContRaD_01b) ...
}
\keyword{datasets}
