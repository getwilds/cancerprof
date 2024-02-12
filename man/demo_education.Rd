% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo-education.R
\name{demo_education}
\alias{demo_education}
\title{Access to Education Data}
\usage{
demo_education(area, areatype, education, sex = NULL, race = NULL)
}
\arguments{
\item{area}{A state/territory abbreviation or USA.}

\item{areatype}{Either "county", "hsa" (Health service area), or "state"}

\item{education}{Either "less than 9th grade", "at least high school", "at least bachelors degree"}

\item{sex}{Either "both sexes", "male", "female"}

\item{race}{One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
"white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
"asian or pacific islander (includes hispanic)","hispanic (any race)}
}
\value{
A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
}
\description{
This function returns a data frame from Education in State Cancer Profiles
}
\examples{
\dontrun{
demo_education("wa", "county", "at least high school", "males")
demo_education("usa", "state", "at least bachelors degree", "both sexes", 
               "all races (includes hispanic)")
demo_education("pr", "hsa", "less than 9th grade")
}
}