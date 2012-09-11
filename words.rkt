#! /usr/bin/env racket

#lang racket

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
  (display lst)
  (newline)
  (display (filter-unique lst))
  (newline))

; retrieve list of word/tag
; @in
;   ins: list of files
; @out
;   wts: list of word/tag
(define get-wts
  (lambda ([ins (find-files (lambda (arg)
			      (regexp-match-exact? #px".+c[a][0-9]{2}" arg))
			    (build-path (current-directory) "brown"))]
	   [wts '()])
    (if (null? ins)
      (flatten wts)
      (get-wts (cdr ins) (list wts (filter (lambda (arg)
				   (> (string-length arg) 2))
				 (regexp-split #rx"[ \n\t]+" (file->string (car ins)))))))))

; basic testing
(display (get-wts))
(newline)

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
  (display lst)
  (newline)
  (display (list->dict lst))
  (newline))

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
  (display lst)
  (newline)
  (display (explode-list lst))
  (newline))

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
      row
      (get-b-row wts-dict (cdr words) tag tag-count (append row (/ (dict-ref wts-dict (string-append (car words) "/" tag))))))))

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
      matrix
      (get-b-matrix wts-dict tags-dict words (cdr tags) (append matrix (get-b-row wts-dict words (car tags) (dict-ref tags-dict (car tags))))))))

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
    (if (null? (cadr tags))
      count
      (count-next tag1 tag2 (cdr tags) 
		  (if (and
			(string=? (car tags) tag1)
			(string=? (cadr tags) tag2))
		    (+ count 1)
		    count)))))

; build the row labelled tag for the A matrix
; @in
;   tags-dict: dictionary of (tag, count)
;   tags: list of tags as in text
;   tag: tag labelling the row
;   tag-count: number of occurrences of tag
;   taglist: list of unique tags
; @out
;   row: the row labelled tag in A matrix
(define get-a-row
  (lambda (tags-dict tags tag tag-count taglist
		     [row '()])
    (if (null? taglist)
      row
      (get-a-row tags-dict tags tag tag-count (cdr taglist) (append row (/ (count-next tag (car taglist) tags) tag-count))))))

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
      matrix
      (get-a-matrix tags-dict tags taglist (cdr taglist-left) (append matrix (get-a-row tags-dict tags (car taglist-left) (dict-ref tags-dict (car taglist-left)) taglist))))))

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
;   retrieve its size
       [tags-size (dict-count tags-dict)]
; filter out duplicates of words list
       [words-unique (filter-unique words)]
;   retrieve its size
       [words-size (length words-unique)]
; build word index?: (list->dict words)?
; build tag index
       [taglist (filter-unique tags)]
; build A matrix
       [a-matrix (get-a-matrix tags-dict tags taglist)]
; build B matrix
       [b-matrix (get-b-matrix wts-dict tags-dict words tags)])
    (printf "a-matrix: ~a\n b-matrix: ~a\n" a-matrix b-matrix))
