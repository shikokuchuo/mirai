% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daemons.R
\name{serialization}
\alias{serialization}
\title{Custom Serialization Functions}
\usage{
serialization(refhook = list(), class = "", vec = FALSE)
}
\arguments{
\item{refhook}{\strong{either} a list or pairlist of two functions: the
signature for the first must accept a reference object inheriting from
'class' (or a list of such objects) and return a raw vector, and the
second must accept a raw vector and return reference objects (or a list
of such objects), \cr \strong{or else} NULL to reset.}

\item{class}{[default ""] a character string representing the class of object
that these serialization function will be applied to, e.g. 'ArrowTabular'
or 'torch_tensor'.}

\item{vec}{[default FALSE] the serialization functions accept and return
reference object individually e.g. \code{arrow::write_to_raw} and
\code{arrow::read_ipc_stream}. If TRUE, the serialization functions are
vectorized and accept and return a list of reference objects, e.g.
\code{torch::torch_serialize} and \code{torch::torch_load}.}
}
\value{
Invisibly, the pairlist of currently-registered 'refhook' functions.
    A message is printed to the console when functions are successfully
    registered or reset.
}
\description{
Registers custom serialization and unserialization functions for sending and
    receiving external pointer reference objects.
}
\details{
Calling without any arguments returns the pairlist of
    currently-registered 'refhook' functions.

    This function may be called prior to or after setting daemons, with the
    registered functions applying across all compute profiles.
}
\examples{
r <- serialization(list(function(x) serialize(x, NULL), unserialize))
print(serialization())
serialization(r)

serialization(NULL)
print(serialization())

}
