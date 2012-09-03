#! /usr/bin/env racket

#lang racket

(require htdp/matrix)

(define make-counts
  (lambda ([ins (find-files (lambda (arg)
			      (regexp-match-exact? #px".+c[a][0-9]{2}" arg))
			    (build-path (current-directory) "brown"))]
	   [dicts '(() ())])
    (let ([dicts (get-wt (filter (lambda (arg)
				   (> (string-length arg) 2))
				 (regexp-split #rx"[ \n\t]+" (file->string (car ins)))) dicts)])
      (if (null? (cdr ins))
	dicts
	(make-counts (cdr ins) dicts)))))

(define get-wt
  (lambda (wts dicts)
    (let* ([wt (car wts)]
	   [wts (cdr wts)]
	   [dicts (list
		    (dict-update (car dicts) wt add1 0)
		    (dict-update (cadr dicts) (cadr (regexp-split #rx"/" wt)) add1 0))])
      (if (null? wts)
	dicts
	(get-wt wts dicts)))))

;(for-each
;  (lambda (arg)
;    (display arg))
;  (dict->list (car (make-counts))))

(display (length (cadr (make-counts))))

; filters out duplicate values
(define filter-unique
  (lambda (lst)
    (let ([dict '()])
      (filter (lambda (arg)
		(if (dict-has-key? dict arg)
		  #t
		  (begin
		    (dict-set dict arg 1)
		    #f)))
	      lst))))

; retrieve list of word/tag
; make a dictionary word/tag -> count
; extract words and tags in two lists
; make a dictionary tag -> count
;   retrieve its size
; filter out duplicates of words list
;   retrieve its size
; build word index
; build tag index
; build A and B matrices

(let ([wts (car (make-counts))]
      [tags (cadr (make-counts))])
  (build-matrix (length tags) (length words) add1))
