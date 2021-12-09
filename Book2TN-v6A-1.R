


library(stringr)

nDigitString <- function(num, digs) {
  st <- as.character(num)
  zeros <- digs - str_length(st)
  for (i in 1:zeros) {
    st <- paste0("0", st)
  }
  return(st)
}

#' Title Truenumbers from a book (vector of character strings)
#'
#' @param theBook, book structure as found in janeaustenR
#' @param subjectRoot, name of the book
#' @param line number to begin, so front matter can be skipped
#'
#' @description Because these gutenberg library texts have no explicit structure,
#' we make the assumption that text has been pre-processed so lines in the array preceded and followed by two
#' < and > characters are heading material, and that successive non-blank lines
#' constitute paragraphs separated by single blank lines.  Thus, content is a sequence
#' of header lines and paragraphs of sentences.
#'
#' The position of each header or sentence in this sequence is called its _ordinal_.
#'
#' Heading line TNs have subject: subjectRoot/heading:<heading-number>
#'
#' Sentences have subject: subjectRoot/section:<heading-number>/paragraph:<number>/sentence:<number>
#'
#' Since we don't know structurally whether headings denote chapters, volumes, etc.
#' we give each sentence a section number equal to the number of the last heading line
#' that preceded the sentence.
#'
#' Sentences will have a TN for their, text, word count, and ordinal.  Headings will only have their
#' text and ordinal.
#'
#' @export
#'
tnBooksFromLines <- function(theBook, subjectRoot, startLine = 1) {
  sentence <- ""    # this variable accumulates a sentence across line boundaries
  sentencenum <- 0  # keeps track of the sentence number within a paragraph
  ordinal <- 0      # the sequence number in the stream of headers and sentences
  headingnum <- 0   # keeps track of the last heading line number to use as the section of running text
  headingState <- FALSE # true if processing a heading
  heading <- ""
  paragraph <- 0
  afterheading <-FALSE

  bklen <- length(theBook)
  tn <- list()
  for (j in startLine:bklen) {
    # loop through lines in theBook
    line <- theBook[j]

      # Look for headings between <...>
      if (startsWith(str_trim(line, side = "left"), '<'))
      {
        headingState <- TRUE
        heading <- ""
        headingnum <- headingnum + 1  # increment the heading number
      }
      if (headingState) {
        heading <- paste0(heading, line)
        if (endsWith(str_trim(line,side = "right"), '>')) {
          headingState <- FALSE

        # Make tnums for the header

        tn[[length(tn) + 1]] <-
          tnum.makeObject(paste0(subjectRoot, "/heading:", nDigitString(headingnum, 4)),
                          "text",
                          heading)
        tn[[length(tn) + 1]] <-
          tnum.makeObject(paste0(subjectRoot, "/heading:", nDigitString(headingnum, 4)),
                          "ordinal",
                          ordinal + 1)

        ordinal <- ordinal + 1
        paragraph <- 1
        sentencenum <- 1
        afterheading <- TRUE
      }
    } else {
      # running text

      if(line == ""){
        if(!afterheading){
          paragraph <- paragraph + 1
          sentencenum <- 1
        }
      } else {
        afterheading <- FALSE
      }

      ln <-
        str_replace_all(line, "Mrs. ", "Mrs") #HACK to make period only sentence delimiters
      ln <- str_replace_all(ln, "Mr. ", "Mr " )
      ln <- str_replace_all(ln, "Esq. ", "Esq ")

      # accumulate sentences across line boundaries, and make TNs for each
      if (stringr::str_detect(ln, "\\.\\s|\\?\\s|\\.\\\"\\s|\\.$")) {
        matchs <- stringr::str_locate_all(pattern = '\\.\\s|\\?\\s|\\.\\\"\\s|\\.$', ln)
        beg <- 0
        matchnum <- length(matchs[[1]][, 1])
        for (i in 1:matchnum) {

          strt <- matchs[[1]][, 1][[i]]
          fnsh <- matchs[[1]][, 2][[i]]
          sentence <- paste0(sentence, substr(ln, beg, fnsh), collapse = "")
          beg <- strt
          subj <-
            paste0(
              tolower(subjectRoot),
              "/section:",
              nDigitString(headingnum, 4),
              "/paragraph:",
              nDigitString(paragraph, 4),
              "/sentence:",
              nDigitString(sentencenum, 4),
              collapse = ''
            )
          ## for debugging: print(sentence)
          sentence <-
            str_replace_all(sentence, "\"", "/iQ/") # fix quotes

          # make 3 TNs
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj, "number:section", headingnum)
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj, "number:paragraph", paragraph)
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj, "number:sentence", sentencenum)
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj, "text", trimws(sentence))
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj, "ordinal", ordinal + 1)
          tn[[length(tn) + 1]] <-
            tnum.makeObject(subj,
                            "count:word",
                            stringr::str_count(trimws(sentence), " ") + 1)

          if (length(tn) >= 50) {
            res <- tnum.postObjects(tn)
            tn <- list()
          }
          sentencenum <- sentencenum + 1
          ordinal <- ordinal + 1

          segend <- nchar(ln)
          if(i < matchnum){
            segend <- matchs[[1]][, 2][[i+1]]
          }

          if(fnsh < nchar(ln)){
            sentence <- paste0(substr(ln, fnsh + 1, segend), " ", collapse = " ")
          } else {
            sentence <- ""
          }
        }

      } else {
        sentence <- paste0(sentence, ln, " ", collapse = " ")
      }
    }
  }
}
