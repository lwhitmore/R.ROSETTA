\name{viewRules}
\alias{viewRules}
\title{
View rules in IF-THEN form.
}
\description{
Display rules in the IF-THEN form. Prints the basic rule statistics.
}
\usage{
viewRules(rules, setDiscLabels=FALSE, newDiscLabels=c("down","nochange", "up"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rules}{
A data frame of rule table in a form of rosetta $main output.
}
  \item{setDiscLabels}{
Logical. If TRUE the new discretization labels can be applied. Default is FALSE.
}
    \item{pAdjust}{
A character vector. New label names for replacing discretization states.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A data frame containing IF-THEN rules and statistics: rule length, RHS accuracy, RHS support, and rule p-value from hypergeometric distribution.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mateusz Garbulowski
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(R.ROSETTA)

out <- rosetta(autcon)
rules <- out$main

IfThenRules <- viewRules(rules)

#change 1,2,3 gene levels into down,nochange,up
IfThenRulesDNU <- viewRules(rules, setDiscLabels=TRUE, newDiscLabels=c("down","nochange", "up"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line