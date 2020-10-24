\name{NEWS}
\title{News for Package 'koRpus.lang.en'}
\encoding{UTF-8}
\section{Changes in koRpus.lang.en version 0.1-4 (2020-10-24)}{
  \subsection{fixed}{
    \itemize{
      \item removed hard coded abbreviations from global TT.tknz.opts in preset, to
        enable checks for file names by koRpus
      \item added missing BNC tag AJS and 15 ambiguity tags
    }
  }
  \subsection{changed}{
    \itemize{
      \item removed the lexicon lookup from the preset, as it is also no longer part
        of the TreeTagger script
      \item renamed abbreviations and parameter files omitting the "-utf8" suffix
    }
  }
}
\section{Changes in koRpus.lang.en version 0.1-3 (2019-05-13)}{
  \subsection{changed}{
    \itemize{
      \item reverted temporary transitional changes from 0.1-2
    }
  }
  \subsection{added}{
    \itemize{
      \item now also supports the BNC tagset (thanks to Elen Le Foll)
      \item added missing PENN tags NNP, NNPS, and PRP$, and explained why koRpus
        defaults to the BNC definition for the conflicting tag PRP in the
        documentation. 
    }
  }
}
\section{Changes in koRpus.lang.en version 0.1-2 (2018-03-20)}{
  \subsection{fixed}{
    \itemize{
      \item package name in license header
    }
  }
  \subsection{changed}{
    \itemize{
      \item updated README.md
    }
  }
  \subsection{added}{
    \itemize{
      \item support optional arguments for \code{koRpus::set.lang.support()}
    }
  }
  \subsection{temporary}{
    \itemize{
      \item this is planned to be a CRAN-only transitional release. since this
        package needs koRpus >=0.11-2 but the CRAN version (0.10-2) should be upgraded
        after this package was already accepted, we'll work around this by
        deactivating functionality unavailable to older koRpus versions, if such an old
        package is used
      \item temporarily lowered koRpus dependency to 0.10-2
      \item added a check for koRpus version to \code{.onAttach()}, so that the language
        support which is already present won't be tampered with; i.e., you can safely
        load this package with older koRpus packages, it just won't add anything
    }
  }
}
\section{Changes in koRpus.lang.en version 0.1-1 (2018-01-05)}{
  \subsection{changed}{
    \itemize{
      \item initial release as standalone package
    }
  }
}
