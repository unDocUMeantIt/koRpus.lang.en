# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.lang.en.
#
# koRpus.lang.en is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus.lang.en is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.lang.en.  If not, see <http://www.gnu.org/licenses/>.


# this is an internal file providing language support.
# please refer to inst/README.languages for details

#' Language support for English
#' 
#' This function adds support for English to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' being loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "en" to be used with koRpus
#'  \item \code{treetag}: The additional preset "en", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.en:hyph.support.en]{sylly.en}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf}
#' @export
#' @importFrom koRpus set.lang.support
#' @examples
#' lang.support.en()

lang.support.en <- function(...) {
  koRpus::set.lang.support("treetag",
    list("en"=list(
      ## preset: "en"
      lang="en",
      encoding="UTF-8",
      preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
        TT.abbrev       <- file.path(TT.lib, "english-abbreviations")
        TT.lexicon      <- file.path(TT.lib, "english-lexicon.txt")
        TT.filter       <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
        TT.lookup       <- file.path(TT.cmd, "lookup.perl")
        # TT.tokenizer TT.tknz.opts "|" TT.lookup.command TT.tagger TT.opts TT.params TT.filter.command
        if(isTRUE(unix.OS)){
          # preset for unix systems
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
              TT.tagger         = file.path(TT.bin, "tree-tagger"),
              TT.abbrev         = TT.abbrev,
              TT.params         = file.path(TT.lib, "english-utf8.par"),
              TT.lexicon        = TT.lexicon,
              TT.lookup         = TT.lookup,
              TT.filter         = TT.filter,

              TT.tknz.opts      = paste("-e"),
              TT.lookup.command = paste("perl", TT.lookup, TT.lexicon, "|"),
              TT.filter.command = paste("|", TT.filter),
              TT.pre.tagger     = "grep -v '^$' |"
            )
          )
        } else {
          # preset for windows systems
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
              TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev         = TT.abbrev,
              TT.params         = file.path(TT.lib, "english-utf8.par"),
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = TT.filter,

              TT.tknz.opts      = paste("-e -a", TT.abbrev),
              TT.lookup.command = c(),
              TT.filter.command = paste("|", TT.filter),
              TT.pre.tagger     = c()
            )
          )
        }
      })
    ),
    ...
  )

  koRpus::set.lang.support("kRp.POS.tags",
    ## tag and class definitions
    # en -- english
    list("en"=list(
      tag.class.def.words=matrix(c(
        "CC", "conjunction", "Coordinating conjunction",
        "CD", "number", "Cardinal number",
        "DT", "determiner", "Determiner",
        "EX", "existential", "Existential there",
        "FW", "foreign", "Foreign word",
        "IN", "preposition", "Preposition or subordinating conjunction",
        "IN/that", "preposition", "Preposition or subordinating conjunction",
        "JJ", "adjective", "Adjective",
        "JJR", "adjective", "Adjective, comparative",
        "JJS", "adjective", "Adjective, superlative",
        "LS", "listmarker", "List item marker",
        "MD", "modal", "Modal",
        "NN", "noun", "Noun, singular or mass",
        "NNS", "noun", "Noun, plural",
        "NP", "name", "Proper noun, singular",
        "NPS", "name", "Proper noun, plural",
        "NS", "noun", "Noun, plural", # undocumented, bug in parameter file?
        "PDT", "predeterminer", "Predeterminer",
        "POS", "possesive", "Possessive ending",
        "PP", "pronoun", "Personal pronoun",
        "PP$", "pronoun", "Possessive pronoun",
        "RB", "adverb", "Adverb",
        "RBR", "adverb", "Adverb, comparative",
        "RBS", "adverb", "Adverb, superlative",
        "RP", "particle", " Particle",
        "SYM", "symbol", "Symbol",
        "TO", "to", "to",
        "UH", "interjection", "Interjection",
        "VB", "verb", "Verb, base form of \"to be\"",
        "VBD", "verb", "Verb, past tense of \"to be\"",
        "VBG", "verb", "Verb, gerund or present participle of \"to be\"",
        "VBN", "verb", "Verb, past participle of \"to be\"",
        "VBP", "verb", "Verb, non-3rd person singular present of \"to be\"",
        "VBZ", "verb", "Verb, 3rd person singular present of \"to be\"",
        "VH", "verb", "Verb, base form of \"to have\"",
        "VHD", "verb", "Verb, past tense of \"to have\"",
        "VHG", "verb", "Verb, gerund or present participle of \"to have\"",
        "VHN", "verb", "Verb, past participle of \"to have\"",
        "VHP", "verb", "Verb, non-3rd person singular present of \"to have\"",
        "VHZ", "verb", "Verb, 3rd person singular present of \"to have\"",
        "VV", "verb", "Verb, base form",
        "VVD", "verb", "Verb, past tense",
        "VVG", "verb", "Verb, gerund or present participle",
        "VVN", "verb", "Verb, past participle",
        "VVP", "verb", "Verb, non-3rd person singular present",
        "VVZ", "verb", "Verb, 3rd person singular present",
        "WDT", "determiner", "Wh-determiner",
        "WP", "pronoun", "Wh-pronoun",
        "WP$", "pronoun", "Possessive wh-pronoun",
        "WRB", "adverb", "Wh-adverb"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.punct=matrix(c(
        ",", "comma", "Comma", # not in guidelines
        "(", "punctuation", "Opening bracket", # not in guidelines
        ")", "punctuation", "Closing bracket", # not in guidelines
        ":", "punctuation", "Punctuation", # not in guidelines
        "``", "punctuation", "Quote", # not in guidelines
        "''", "punctuation", "End quote", # not in guidelines
        "#", "punctuation", "Punctuation", # not in guidelines
        "$", "punctuation", "Punctuation" # not in guidelines
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.sentc=matrix(c(
        "SENT", "fullstop", "Sentence ending punctuation" # not in guidelines
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
      )
    ),
    ...
  )
}

# this internal, non-exported function causes the language support to be
# properly added when the package gets loaded
#' @importFrom sylly.en hyph.support.en
#' @importFrom utils packageVersion
.onAttach <- function(...) {
  # TODO: remove the if condition after successfull transition of this package and koRpus v0.11 to CRAN
  # this was added to make sure that the package can be checked on CRAN with older but incompatible
  # versions of koRpus; they already included support for english
  if(packageVersion("koRpus") >= "0.11.2"){
    lang.support.en()
    sylly.en::hyph.support.en()
  } else {}
}
