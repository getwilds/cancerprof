% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo-non-english-language.R
\name{demo_language}
\alias{demo_language}
\title{Access to Non-English Language}
\usage{
demo_language(area, areatype, language)
}
\arguments{
\item{area}{A state/territory abbreviation or USA.}

\item{areatype}{One of the following values:
\itemize{
\item \code{"county"}
\item \code{"hsa"} (Health Service Area)
\item \code{"state"}.
}}

\item{language}{The only permissible value is
\code{"language isolation"}.}
}
\value{
A data frame with the following columns:
Area Type, Area Code, Percent, Households, Rank.
}
\description{
This function returns a data frame about language demographics
from State Cancer Profiles.
}
\examples{
demo_language(
  area = "WA",
  areatype = "county",
  language = "language isolation"
)

demo_language(
  area = "dc",
  areatype = "hsa",
  language = "language isolation"
)

demo_language(
  area = "usa",
  areatype = "state",
  language = "language isolation"
)
}
\seealso{
Other demographics: 
\code{\link{demo_crowding}()},
\code{\link{demo_education}()},
\code{\link{demo_food}()},
\code{\link{demo_income}()},
\code{\link{demo_insurance}()},
\code{\link{demo_mobility}()},
\code{\link{demo_population}()},
\code{\link{demo_poverty}()},
\code{\link{demo_svi}()},
\code{\link{demo_workforce}()}
}
\concept{demographics}
