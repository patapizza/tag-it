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
       [words-size (length words-unique)])
  (printf "tag size: ~a words size: ~a" tags-size words-size)
  (newline))
; build word index
; build tag index
; build A and B matrices
