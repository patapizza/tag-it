#! /usr/bin/env racket

#lang racket

; @todo: build sparse matrix object

(require htdp/matrix)

; filter out duplicate values
; @in
;   lst: a list
; @out
;   lst with unique values
(define filter-unique
  (lambda (lst)
    (let ([dict (make-hash)])
      (filter (lambda (arg)
		(if (dict-has-key? dict arg)
		  #f
		  (begin
		    (dict-set! dict arg 1)
		    #t)))
	      lst))))

; basic testing
(let ([lst '(1 1 2 3 4 2 5 1 42)])
  (displayln lst)
  (displayln (filter-unique lst)))

; retrieve list of word/tag
; @in
;   ins: list of files
; @out
;   wts: list of word/tag
(define get-wts
  (lambda ([ins (find-files (lambda (arg)
			      ;(regexp-match-exact? #px".+c[a-r][0-9]{2}" arg))
			      (regexp-match-exact? #px".+ca01" arg))
			    (build-path (current-directory) "brown"))]
	   [wts '()])
    (if (null? ins)
      (flatten wts)
      (get-wts (cdr ins) (list wts (filter (lambda (arg)
				   (> (string-length arg) 2))
				 (regexp-split #rx"[ \n\t]+" (file->string (car ins)))))))))

; basic testing
(displayln (get-wts))

; count occurrences
; @in
;   lst: a list
; @out
;   dict: dictionary of item -> number of occurrences
(define list->dict
  (lambda (lst
	    [dict '()])
    (if (null? lst)
      dict
      (list->dict (cdr lst) (dict-update dict (car lst) add1 0)))))

; basic testing
(let ([lst '(1 1 2 3 4 2 5 1 42)])
  (displayln lst)
  (displayln (list->dict lst)))

; extract words and tags in two lists
; @in
;   wts: list of word/tag
; @out
;   lsts: pair of (words list, tags list)
; @todo: make it generic with delimiter as input
(define explode-list
  (lambda (wts
	    [lsts '(() ())])
    (if (null? wts)
      (list (flatten (car lsts)) (flatten (cadr lsts)))
      (let ([pair (regexp-split #rx"/" (car wts))])
	(explode-list (cdr wts) (list
				  (cons (car lsts) (car pair))
				  (cons (cadr lsts) (cadr pair))))))))

; basic testing
(let ([lst '("1/2" "3/4" "5/6" "7/8")])
  (displayln lst)
  (displayln (explode-list lst)))

; build a list of pairs (item, index)
; @in
;   lst: a list
; @out
;   a list of pairs (item, index)

(define index-list
  (lambda (lst
	    [i 0])
    (if (null? lst)
      '()
      (cons (cons (car lst) i) (index-list (cdr lst) (+ i 1))))))

; basic testing
(displayln (index-list '("t1" "t2" "t3" "t4")))

; build the row labelled tag for the B matrix
; @in
;   wts-dict: dictionary of (word/tag, count)
;   words: list of unique words
;   tag: tag labelling the row
;   tag-count: number of occurrences of tag
; @out
;   row: the row labelled tag in B matrix
(define get-b-row
  (lambda (wts-dict words tag tag-count
		    [row '()])
    (if (null? words)
      (flatten row)
      (get-b-row wts-dict (cdr words) tag tag-count (list row (/ (dict-ref wts-dict (string-append (car words) "/" tag) 0) tag-count))))))

; basic testing
(let ([wts-dict '(("w1/t1" . 1) ("w2/t1" . 2) ("w3/t2" . 1) ("w1/t2" . 4) ("w2/t2" . 5) ("w4/t3" . 2))]
      [words '("w1" "w2" "w3" "w4")]
      [tag "t2"]
      [tag-count 10])
  (displayln (get-b-row wts-dict words tag tag-count)))

; build B matrix
; @in
;   wts-dict: dictionary of (word/tag, count)
;   tags-dict: dictionary of (tag, count)
;   words: list of unique words
;   tags: list of unique tags
; @out
;   matrix: the matrix of probabilities of words appearing
(define get-b-matrix
  (lambda (wts-dict tags-dict words tags
		    [matrix '()])
    (if (null? tags)
      (begin
	(printf "(~a x ~a) matrix --> ~a\n" (dict-count tags-dict) (length words) (length (flatten matrix)))
        (make-matrix (dict-count tags-dict) (length words) (flatten matrix)))
      (get-b-matrix wts-dict tags-dict words (cdr tags) (list matrix (get-b-row wts-dict words (car tags) (dict-ref tags-dict (car tags))))))))

; basic testing
(let ([wts-dict '(("w1/t1" . 1) ("w2/t1" . 2) ("w3/t2" . 1) ("w1/t2" . 4) ("w2/t2" . 5) ("w4/t3" . 2))]
      [tags-dict '(("t1" . 3) ("t2" . 10) ("t3" . 2))]
      [words '("w1" "w2" "w3" "w4")]
      [tags '("t1" "t2" "t3")])
  (displayln (matrix-render (get-b-matrix wts-dict tags-dict words tags))))

; @todo: add start-of-sentence <s> and end-of-sentence </s> symbols

; count pairs of following tags
; @in
;   tag1: the first tag
;   tag2: the following tag
;   tags: the tags list
; @out
;   count: the amount of times tag1 is followed by tag2
(define count-next
  (lambda (tag1 tag2 tags
		[count 0])
    (if (null? (cdr tags))
      count
      (count-next tag1 tag2 (cdr tags) 
		  (if (and
			(string=? (car tags) tag1)
			(string=? (cadr tags) tag2))
		    (+ count 1)
		    count)))))

; basic testing
(let ([tag1 "t1"]
      [tag2 "t2"]
      [tags '("t1" "t1" "t2" "t3" "t2" "t4" "t5" "t1" "t2" "t2" "t3" "t1" "t2" "t2" "t5")])
  (displayln (count-next tag1 tag2 tags)))

; build the row labelled tag for the A matrix
; @in
;   tags: list of tags as in text
;   tag: tag labelling the row
;   tag-count: number of occurrences of tag
;   taglist: list of unique tags
; @out
;   row: the row labelled tag in A matrix
(define get-a-row
  (lambda (tags tag tag-count taglist
		     [row '()])
    (if (null? taglist)
      (flatten row)
      (get-a-row tags tag tag-count (cdr taglist) (list row (/ (count-next tag (car taglist) tags) tag-count))))))

; basic testing
(let ([tags '("t1" "t2" "t2" "t3" "t1" "t4" "t2" "t5" "t1" "t1" "t2" "t3" "t5")]
      [tag "t1"]
      [tag-count 4]
      [taglist '("t1" "t2" "t3" "t4" "t5")])
  (displayln (get-a-row tags tag tag-count taglist)))

; build the A matrix
; @in
;   tags-dict: dictionary of (tag, count)
;   tags: list of tags
;   taglist: index of (unique) tags
;   taglist-left: rows left to build
; @out
;   matrix: the matrix of probabilities of tags appearing
(define get-a-matrix
  (lambda (tags-dict tags taglist
		     [taglist-left taglist]
		     [matrix '()])
    (if (null? taglist-left)
      (make-matrix (length taglist) (length taglist) (flatten matrix))
      (get-a-matrix tags-dict tags taglist (cdr taglist-left) (list matrix (get-a-row tags (car taglist-left) (dict-ref tags-dict (car taglist-left)) taglist))))))

; basic testing
(let ([tags-dict '(("t1" . 5) ("t2" . 3) ("t3" . 1) ("t4" . 3) ("t5" . 2))]
      [tags '("t1" "t4" "t5" "t2" "t1" "t1" "t3" "t2" "t1" "t4" "t5" "t1" "t4" "t2")]
      [taglist '("t1" "t2" "t3" "t4" "t5")])
  (displayln (matrix-render (get-a-matrix tags-dict tags taglist))))

; @todo: address the unknown words issue

; perform the initialization step of the Viterbi algorithm
; @in
;   a-matrix: the A matrix
;   b-matrix: the B matrix
;   o1: col index of the first word (observation) in the B matrix
;   tags-index: list of (tag, index) pairs
; @out
;   viterbi: a matrix whose first column is initialized
;   backptr: a matrix whose first column is initialized
(define viterbi-init
  (lambda (a-matrix b-matrix o1 tags-index
		    [viterbi '()]
		    [backptr '()])
    (let ([i (cadar tags-index)])
      (if (null? taglist)
	(list viterbi backptr)
	(viterbi-init a-matrix b-matrix o1
		      (cdr tags-index)
		      (list viterbi (* (matrix-ref 0 i a-matrix)
				       (matrix-ref i o1 b-matrix)))
		      (append backptr 0))))))

; @todo: optimize vars

; retrieve list of word/tag
(let* ([wts (get-wts)]
; make a dictionary word/tag -> count
       [wts-dict (list->dict wts)]
; extract words and tags in two lists
       [wts-exploded (explode-list wts)]
       [words (car wts-exploded)]
       [tags (cadr wts-exploded)]
; make a dictionary tag -> count
       [tags-dict (list->dict tags)]
; retrieve its size
       [tags-size (dict-count tags-dict)]
; filter out duplicates of words list
       [words-unique (filter-unique words)]
; retrieve its size
       [words-size (length words-unique)]
; filter out duplicates of tags list
       [tags-unique (filter-unique tags)]
; build tags index (make-hash)?
       [tags-index (index-list tags-unique)]
; build words index (make-hash)?
       [words-index (index-list words-unique)]
; build A matrix
       [a-matrix (get-a-matrix tags-dict tags tags-unique)]
; build B matrix
       [b-matrix (get-b-matrix wts-dict tags-dict words tags-unique)])
    (printf "a-matrix: ~a\n b-matrix: ~a\n" (matrix-render a-matrix) (matrix-render b-matrix)))
