# Copyright 2010-2020 Meik Michalke <meik.michalke@hhu.de>
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
#' The POS tags cover tag definitions from multiple sources. Please note that there is one tag, "PRP", that
#' is defined in both PENN[3] and BNC[4] tagsets, but with different meanings: The PENN tag marks
#' personal pronouns, whereas the BNC tag marks prepositions (except "of"). Since the conflicting tag
#' is not being used by TreeTagger's PENN parameter set, but in its BNC set, koRpus also uses the BNC
#' definition. Keep this in mind if you use this language support package with alternative taggers.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "en" to be used with koRpus
#'  \item \code{treetag}: The additional preset "en", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2], additional tags from the PENN treebank project[3], and the BNC tagset[4] used in
#'    an alternative TreeTagger parameter set.
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.en:hyph.support.en]{sylly.en}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf}
#'
#' [3] \url{https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html}
#'
#' [4] \url{http://www.natcorp.ox.ac.uk/docs/c5spec.html}
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
        TT.tokenizer    <- file.path(TT.cmd, "utf8-tokenize.perl")
        TT.abbrev       <- file.path(TT.lib, "english-abbreviations")
        TT.filter       <- "perl -pe 's/\\tV[BDHV]/\\tVB/;s/IN\\/that/\\tIN/;'"
        TT.params       <- file.path(TT.lib, "english.par")
        # TT.tokenizer TT.tknz.opts "|" TT.lookup.command TT.tagger TT.opts TT.params TT.filter.command
        if(isTRUE(unix.OS)){
          # preset for unix systems
          return(
            list(
              TT.tokenizer      = TT.tokenizer,
              TT.tagger         = file.path(TT.bin, "tree-tagger"),
              TT.abbrev         = TT.abbrev,
              TT.params         = TT.params,
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = TT.filter,

              TT.tknz.opts      = "-e",
              TT.lookup.command = c(),
              TT.filter.command = paste("|", TT.filter),
              TT.pre.tagger     = "grep -v '^$' |"
            )
          )
        } else {
          # preset for windows systems
          return(
            list(
              TT.tokenizer      = TT.tokenizer,
              TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev         = TT.abbrev,
              TT.params         = TT.params,
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = TT.filter,

              TT.tknz.opts      = "-e",
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
    # PENN tags, see: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf
    # additional PENN tags, see: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
    # BNC tags, see: http://www.natcorp.ox.ac.uk/docs/c5spec.html
    #   BNC tags kindly provided by Elen Le Foll
    list("en"=list(
      tag.class.def.words=matrix(c(
        ## PENN tags
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
        # "POS", "possesive", "Possessive ending", # see BNC tags below
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
        # "VBD", "verb", "Verb, past tense of \"to be\"", # see BNC tags below
        # "VBG", "verb", "Verb, gerund or present participle of \"to be\"", # see BNC tags below
        # "VBN", "verb", "Verb, past participle of \"to be\"", # see BNC tags below
        "VBP", "verb", "Verb, non-3rd person singular present of \"to be\"",
        # "VBZ", "verb", "Verb, 3rd person singular present of \"to be\"", # see BNC tags below
        "VH", "verb", "Verb, base form of \"to have\"",
        # "VHD", "verb", "Verb, past tense of \"to have\"", # see BNC tags below
        # "VHG", "verb", "Verb, gerund or present participle of \"to have\"", # see BNC tags below
        # "VHN", "verb", "Verb, past participle of \"to have\"", # see BNC tags below
        "VHP", "verb", "Verb, non-3rd person singular present of \"to have\"",
        # "VHZ", "verb", "Verb, 3rd person singular present of \"to have\"", # see BNC tags below
        "VV", "verb", "Verb, base form",
        # "VVD", "verb", "Verb, past tense", # see BNC tags below
        # "VVG", "verb", "Verb, gerund or present participle", # see BNC tags below
        # "VVN", "verb", "Verb, past participle", # see BNC tags below
        "VVP", "verb", "Verb, non-3rd person singular present",
        # "VVZ", "verb", "Verb, 3rd person singular present", # see BNC tags below
        "WDT", "determiner", "Wh-determiner",
        "WP", "pronoun", "Wh-pronoun",
        "WP$", "pronoun", "Possessive wh-pronoun",
        "WRB", "adverb", "Wh-adverb",
        ## additional PENN tags
        "NNP", "name", "Proper noun, singular",
        "NNPS", "name", "Proper noun, plural",
        #"PRP", "pronoun", "Personal pronoun", # contradicts BNC tag, see below!
        "PRP$", "pronoun", "Possessive pronoun",
        ## BNC tags
        "AJ0", "adjective", "Adjective, general or positive",
        "AJC", "adjective", "Adjective, comparative",
        "AJS", "adjective", "Adjective, superlative",
        "AT0", "article", "Article, which typically begins a noun phrase", # includes no
        "AV0", "adverb", "Adverb  not subclassified as AVP or AVQ", # N.B. Unlike,adverbs  adjectives, are not tagged
          # as positive, comparative, or superlative. This is because of the relative rarity of comparative and
          # superlative adverbs.
        "AVP", "particle", "Adverb particle", # N.B. AVP is used for such "prepositional adverbs", whether or not they
          # are used idiomatically in a phrasal verb, e.g. in 'Come out here' and 'I can't hold out any longer',
          # the same AVP tag is used for out.
        "AVQ", "adverb", "Wh-adverb", # N.B. The same tag is used, whether the word occurs in interrogative or relative use.
        "CJC", "conjunction", "Coordinating conjunction",
        "CJS", "conjunction", "Subordinating conjunction",
        "CJT", "conjunction", "The subordinating conjunction that", # N.B. that is tagged CJT when it introduces not
          # only a nominal clause, but also a relative clause, as in 'the day that follows Christmas'.
        "CRD", "number", "Cardinal number",
        "DPS", "determiner", "Possessive determiner",
        "DT0", "determiner", "General determiner", # i.e. a determiner which is not a DTQ. Here a determiner is
          # defined as a word which typically occurs either as the first word in a noun phrase, or as the head
          # of a noun phrase. E.g. This is tagged DT0 both in 'This is my house' and in 'This house is mine'.
        "DTQ", "determiner", "Wh-determiner", # The category of determiner here is defined as for DT0 above.
          # These words are tagged as wh-determiners whether they occur in interrogative use or in relative use.
        "EX0", "existential", "Existential there",
        "ITJ", "interjection", "Interjection or other isolate",
        "NN0", "noun", "Common noun, neutral for number", # N.B. Singular collective nouns such as committee and
          # team are tagged NN0, on the grounds that they are capable of taking singular or plural agreement with
          # the following verb, e.g. 'The committee disagrees/disagree'
        "NN1", "noun", "Singular common noun",
        "NN2", "noun", "Plural common noun",
        "NP0", "noun", "Proper noun", # N.B. the distinction between singular and plural proper nouns is not
          # indicated in the tagset, plural proper nouns being a comparative rarity.
        "ORD", "numeral", "Ordinal numeral", # N.B. The ORD tag is used whether these words are used in a nominal
          # or in an adverbial role. Next and last, as "general ordinals", are also assigned to this category.
        "PNI", "pronoun", "Indefinite pronoun", # N.B. This tag applies to words which always function as (heads of)
          # noun phrases. Words like some and these, which can also occur before a noun head in an article-like
          # function, are tagged as determiners (see DT0 and AT0 above).
        "PNP", "pronoun", "Personal pronoun", # N.B. Possessive pronouns like ours and theirs are tagged as personal pronouns.
        "PNQ", "pronoun", "Wh-pronoun", # N.B. These words are tagged as wh-pronouns whether they occur in interrogative
          # or in relative use.
        "PNX", "pronoun", "Reflexive pronoun",
        "POS", "possessive", "Possessive or genitive marker \'s or \'",
        "PRF", "preposition", "Preposition, of", # Because of its frequency and its almost exclusively postnominal
          # function, of is assigned a special tag of its own.
        "PRP", "preposition", "Preposition, except of",
        "TO0", "to", "Infinitive marker to",
        "UNC", "unclassified", "Unclassified items", # Items tagged UNC include foreign (non-English) words,
          # special typographical symbols, formulae, and (in spoken language) hesitation fillers such as er and erm.
        "VBB", "verb", "Verb, present tense forms of BE, except for is, \'s",
        "VBD", "verb", "Verb, past tense forms of BE, was, were",
        "VBG", "verb", "Verb, gerund or present participle (-ing form) of BE, being",
        "VBI", "verb", "Verb, infinitive form of BE, be",
        "VBN", "verb", "Verb, past participle form of BE, been",
        "VBZ", "verb", "Verb, 3rd person singular present (-s form) of BE, is, \'s",
        "VDB", "verb", "Verb, finite base form of DO, do",
        "VDD", "verb", "Verb, past tense form of DO, did",
        "VDG", "verb", "Verb, gerund or present participle (-ing form) of DO, doing",
        "VDI", "verb", "Verb, infinitive form of DO, do",
        "VDN", "verb", "Verb, past participle form of DO, done",
        "VDZ", "verb", "Verb, 3rd person singular present (-s form) of DO, does, \'s",
        "VHB", "verb", "Verb, finite base form of HAVE, have, \'ve",
        "VHD", "verb", "Verb, past tense form of HAVE, had, \'d",
        "VHG", "verb", "Verb, gerund or present participle (-ing form) of HAVE, having",
        "VHI", "verb", "Verb, infinitive form of HAVE, have",
        "VHN", "verb", "Verb, past participle form of HAVE, had",
        "VHZ", "verb", "Verb, 3rd person singular present (-s form) of HAVE, has, \'s",
        "VM0", "verb", "Modal auxiliary verb",
        "VVB", "verb", "Finite base form of lexical verbs", # N.B. This tag includes the imperative and present subjunctive
        "VVD", "verb", "Past tense form of lexical verbs",
        "VVG", "verb", "Gerund or present participle (-ing form) of lexical verbs",
        "VVI", "verb", "Infinitive form of lexical verbs",
        "VVN", "verb", "Past participle form of lexical verbs",
        "VVZ", "verb", "3rd person singular present (-s form) of lexical verbs",
        "XX0", "negation", "Negative particle not or n\'t",
        "ZZ0", "letter", "Alphabetical symbols, e.g. A, a, B, b, c, d",
         # abiguity tags
        "AJ0-AV0", "ambiguous", "Adjective, general or positive/Adverb  not subclassified as AVP or AVQ (ambiguity tag)",
        "AJ0-VVN", "ambiguous", "Adjective, general or positive/Past participle form of lexical verbs (ambiguity tag)",
        "AJ0-VVD", "ambiguous", "Adjective, general or positive/Past tense form of lexical verbs (ambiguity tag)",
        "AJ0-NN1", "ambiguous", "Adjective, general or positive/Singular common noun (ambiguity tag)",
        "AJ0-VVG", "ambiguous", "Adjective, general or positive/Gerund or present participle (-ing form) of lexical verbs (ambiguity tag)",
        "AVP-PRP", "ambiguous", "Adverb particle/Preposition, except of (ambiguity tag)",
        "AVQ-CJS", "ambiguous", "Wh-adverb/Subordinating conjunction (ambiguity tag)",
        "CJS-PRP", "ambiguous", "Subordinating conjunction/Preposition, except of (ambiguity tag)",
        "CJT-DT0", "ambiguous", "The subordinating conjunction that/General determiner (ambiguity tag)",
        "CRD-PNI", "ambiguous", "Cardinal number/Indefinite pronoun (ambiguity tag)",
        "NN1-NP0", "ambiguous", "Singular common noun/Proper noun (ambiguity tag)",
        "NN1-VVB", "ambiguous", "Singular common noun/Finite base form of lexical verbs (ambiguity tag)",
        "NN1-VVG", "ambiguous", "Singular common noun/Gerund or present participle (-ing form) of lexical verbs (ambiguity tag)",
        "NN2-VVZ", "ambiguous", "Plural common noun/Verb, 3rd person singular present (-s form) of HAVE, has, \'s (ambiguity tag)",
        "VVD-VVN", "ambiguous", "Past tense form of lexical verbs/Past participle form of lexical verbs (ambiguity tag)"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.punct=matrix(c(
        ",", "comma", "Comma", # not in guidelines
        "(", "punctuation", "Opening bracket", # not in guidelines
        ")", "punctuation", "Closing bracket", # not in guidelines
        ":", "punctuation", "Punctuation", # not in guidelines
        "``", "punctuation", "Quote", # not in guidelines
        "''", "punctuation", "End quote", # not in guidelines
        "#", "punctuation", "Punctuation", # not in guidelines
        "$", "punctuation", "Punctuation", # not in guidelines
        ## BNC tags
        "PUL", "punctuation", "Punctuation, left bracket",
        "PUN", "punctuation", "Punctuation, general separating mark", # i.e. . , ! , , ; - or ?
        "PUQ", "punctuation", "Punctuation, quotation mark",
        "PUR", "punctuation", "Punctuation, right bracket"
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
.onAttach <- function(...) {
  lang.support.en()
  sylly.en::hyph.support.en()
}
