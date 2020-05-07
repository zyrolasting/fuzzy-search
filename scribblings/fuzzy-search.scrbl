#lang scribble/manual
@require[@for-label[fuzzy-search
                    fuzzy-search/require
                    racket/base
                    racket/contract
                    racket/file]]

@title{fuzzy-search}
@author{Forrest Smith (90%), Sage Gerard (10%)}

This module provides approximate string matching procedures
loosely based on Sublime Text's approach.


@section{Definitions}

In the context of this collection, the following terms apply:

@itemlist[
@item{@deftech{needle}: A user-defined string used as search input.}

@item{@deftech{haystack}: A string that may contain @tech{needles}.}

@item{@deftech{match}: A position in a @tech{haystack} where a @tech{needle}'s character appears.}

@item{@deftech{score}: An exact integer that represents the relevance of a @tech{match}.}

@item{@deftech{scoring procedure}: A procedure that returns a @tech{score}.}
]


@section{Approximate Matching}

@defmodule[fuzzy-search]

@defproc[(fuzzy-search [needle string?]
                       [haystack string?]
                       [assign-score score/c forrest-skinner]
                       [#:max-match-count max-match-count exact-positive-integer? 256]
                       [#:max-depth max-depth exact-positive-integer? 10]
                       [#:case-sensitive? case-sensitive? any/c #f])
                       (values boolean?
                               exact-integer?
                               (hash/c exact-nonnegative-integer?
                                       exact-nonnegative-integer?
                                       #:immutable #t))]{
Searches @racket[haystack] for the given @racket[needle]. Returns
three values:

@itemlist[#:style 'ordered

@item{@racket[#t] if a match was found.}

@item{The value of @racket[(assign-score H haystack (string-length
haystack) (length (hash-keys H)))], where @racket[H] is the returned
hash table.}

@item{A hash table @racket[H], such that @racket[(string-ref haystack (hash-ref H N))]
returns the first character of the @racket[N]th (zero-based) match. This hash table
will produce the highest score for the given @racket[haystack].}
]

The search will record a match for every contiguous character in
@racket[needle] found in @racket[haystack]. If the number of matches
exceeds @racket[max-match-count], then the search will conclude as if
no match occurred, and with a score of 0.

Recursive searches in substrings of @racket[haystack] will commence in
order to find higher scoring matches. @racket[max-depth] limits the
recursion depth of the search. While searches are themselves called in
tail position, @racket[max-depth] can still help reduce the runtime of
the search.

When @racket[case-sensitive?] is @racket[#f], characters from the
needle and haystack are first normalized with @racket[char-downcase]
before being compared.
}


@defthing[score/c
  (-> (hash/c exact-nonnegative-integer?
              exact-nonnegative-integer?
              #:immutable #t)
      string?
      exact-positive-integer?
      exact-positive-integer?
      exact-integer?)]{
Defines a @tech{scoring procedure} that assigns a score
with awareness of all @tech[#:key "match"]{matches} in a @tech{haystack}.

This procedure accepts the following arguments, in order:

@itemlist[#:style 'ordered

@item{An immutable @racket[hasheq]. The meaning of the hash is the same as it is in @racket[fuzzy-search].}

@item{A @tech{haystack}.}

@item{The length of the haystack.}

@item{The number of matches found in the haystack.}
]
}

@defthing[score/match/c
  (-> (hash/c exact-nonnegative-integer?
              exact-nonnegative-integer?
              #:immutable #t)
      string?
      exact-positive-integer?
      exact-positive-integer?
      exact-integer?
      exact-integer?
      exact-integer?)]{
Defines a @tech{scoring procedure} that assigns a score with awareness
of @italic{a particular @tech{match}} in a @tech{haystack}.

Like @racket[score/c], but accepts two more arguments that represent a
key and the associated value from the hash table. This allows
per-occurrence scoring when used with @racket[score/match].
}

@defthing[score/pair/c (-> char? char? exact-integer?)]{
Defines a @tech{scoring procedure} that assigns a score in context
of a character that MAY appear in a @tech{match}, followed by a character
that DOES appear in a match.
}

@defthing[score/forrest score/c]{
A @tech{scoring procedure} based on
@hyperlink["https://www.forrestthewoods.com/blog/reverse_engineering_sublime_texts_fuzzy_match/"]{Forrest Smith's approximation} of Sublime Text's behavior.

@racketblock[
(define score/forrest
  (score/all
   (λ _ 100)
   (score/all/leading-letter -5 -15)
   (score/all/unmatched-letter -1)
   (score/match
    (score/match/sequence 15)
    (score/match/first-letter 15)
    (score/match/pair
     (score/pair/camel-case 30)
     (score/pair/prefixed 30 '(#\space #\_))))))]
}

@defform[(score-if t v)]{
Expands to @racket[(if t v 0)]. Useful for scoring a match
conditionally in expression position of a sum.
}


@defproc[(score/all [procs score/c] ...) score/c]{

Equivalent to:

@racketblock[
(λ args
  (for/sum ([f (in-list procs)])
    (apply f args)))]

Useful for combining several @racket[score/c] procedures.
}

@defproc[(score/all/unmatched-letter [v exact-integer?]) score/c]{
Returns a @tech{scoring procedure}. That procedure returns @racket[v]
times the number of unmatched characters in a search.
}

@defproc[(score/all/leading-letter [v exact-integer?] [limit exact-positive-integer?]) score/c]{
Returns a @tech{scoring procedure}. That procedure returns a score of @racket[v] times
the position of the first matching character in a @tech{haystack}, capped
to a magnitude of @racket[limit].

Use to base a penalty or reward on how far the first match is from the beginning of its
@tech{haystack}.
}

@defproc[(score/match [procedures score/match/c] ...) score/c]{
Like @racket[score/all], except the given @racket[procedures] must accept
two more arguments according to @racket[score/match/c]. Those procedures
are used to assign scores to individual matches.
}

@defproc[(score/match/sequence [v exact-integer?] ...) score/match/c]{
Returns a @tech{scoring procedure} for use in @racket[score/match].

The scoring procedure assigns a @tech{score} of @racket[v]
if a given @tech{match} (other than the first) starts
immediately after the prior match in their @tech{haystack}.
}

@defproc[(score/match/first-letter [v exact-integer?]) score/match/c]{
Returns a @tech{scoring procedure} for use in @racket[score/match].

That procedure returns a @tech{score} of @racket[v] if the given @tech{match}
starts at the beginning of the @tech{haystack}.
}

@defproc[(score/match/pair [procedures score/pair/c] ...) score/match/c]{
Like @racket[score/all], except the @racket[procedures] are meant to
assign @tech{scores} based on adjacent characters in the @tech{haystack}.

The returned procedure is also meant for use in @racket[score/match].
}


@defproc[(score/pair/camel-case [v exact-integer?]) score/pair/c]{
Returns a @tech{scoring procedure} for use in @racket[score/match/pair].

That procedure returns a @tech{score} of @racket[v] if a
given pair of characters changes from lowercase to uppercase.
It returns a score of @racket[0] otherwise.
}


@defproc[(score/pair/prefixed [v exact-integer?]
                              [chars (listof char?)]) score/pair/c]{
Returns a @tech{scoring procedure} for use in @racket[score/match/pair].

That procedure returns a @tech{score} of @racket[v] if the first of a
given pair of characters appears in @racket[chars]. It returns a score of
@racket[0] otherwise.
}

@section{Fuzzy Requires}

@defmodule[fuzzy-search/require]

@defform[(fuzzy needle ...)]{
When used as a @racket[require] subform, @racket[fuzzy] expands to
@racket[(combine-in (file relative-path) ...)], where each
@racket[relative-path] is the highest-scoring relative path in
proximate directories to a given Racket module.

The filesystem search entails use of @racket[find-files] starting from
the parent directory of the module using @racket[fuzzy], or
@racket[(current-directory)] if the source directory is unknown. Paths
relative to the file using @racket[fuzzy] are then treated as
@tech{haystacks} for @racket[fuzzy-search].

This is useful if you are prototyping a Racket package and are likely
to shuffle files around. Normally when you move a file, all relative
path @racket[require] subforms @italic{will} break. @racket[fuzzy]
forms @italic{may} break. A break would occur if a @racket[needle]
targeting a file becomes ambiguous, or if the intended file moves out
of the search scope.


@racketblock[
(require fuzzy-search/require
         (fuzzy "legacy"    @code:comment{../legacy.rkt}
                "widgets")) @code:comment{utils/widgets.rkt}]


@bold{Do not use @racket[fuzzy] in user-facing code.} Use it only to
delay your commitment to concrete file paths. Even then, be careful
not to use overly vague needles. @racket[(require (fuzzy "a"))] is a
dice roll.
}


@section{Credits and Project Information}
The implementation is based on
@hyperlink["https://www.forrestthewoods.com/blog/reverse_engineering_sublime_texts_fuzzy_match/"]{Forrest
Smith's work} with Sublime Text's fuzzy search. That link will take
you to his content and public domain source code. Thanks, Forrest!

You can find the source code of this package
@hyperlink["https://github.com/zyrolasting/fuzzy-search"]{here}.

My own contributions are as follows:

@itemlist[
@item{Port the @hyperlink["https://github.com/forrestthewoods/lib_fts/blob/master/code/fts_fuzzy_match.js"]{JavaScript implementation of Forrest's fuzzy match} to Racket.}
@item{Refactor the code to allow scoring procedures and combinators as part of the public API.}
@item{Make case-sensitivity an option.}
@item{Expose match positions in program output.}
@item{Package for distribution on the Racket catalog.}
@item{Add the @racket[fuzzy] require subform.}
]

If you found my additions useful, then please consider
@hyperlink["https://sagegerard.com/subscribe.html"]{sponsoring my
work}. I count on your support to continue writing libraries like this
one.
