% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo-food-access.R
\name{demo_food}
\alias{demo_food}
\title{Access to Food Insecurity Data}
\usage{
demo_food(area, areatype, food, race = NULL)
}
\arguments{
\item{area}{A state/territory abbreviation or USA.}

\item{areatype}{Either \code{"county"} or \code{"state"}.}

\item{food}{One of the following values:
\itemize{
\item \code{"food insecurity"}
\item \code{"limited access to healthy food"}.
}}

\item{race}{One of the following values:
\itemize{
\item \code{"All Races (includes Hispanic)"}
\item \code{"White non-Hispanic"}
\item \code{"Black (includes Hispanic)"}
\item \verb{"Hispanic (Any Race)}.
}}
}
\value{
A data frame with the following columns:
Area Type, Area Code, Value, People.
}
\description{
This function returns a data frame about food demographics from
State Cancer Profiles.
}
\examples{
\dontrun{
demo_food(
  area = "wa",
  areatype = "county",
  food = "food insecurity",
  race = "black"
)

demo_food(
  area = "usa",
  areatype = "state",
  food = "limited access to healthy food"
)

demo_food(
  area = "pr",
  areatype = "county",
  food = "food insecurity",
  race = "all races (includes hispanic)"
)
}
}
\seealso{
Other demographics: 
\code{\link{demo_crowding}()},
\code{\link{demo_education}()},
\code{\link{demo_income}()},
\code{\link{demo_insurance}()},
\code{\link{demo_language}()},
\code{\link{demo_mobility}()},
\code{\link{demo_population}()},
\code{\link{demo_poverty}()},
\code{\link{demo_svi}()},
\code{\link{demo_workforce}()}
}
\concept{demographics}
