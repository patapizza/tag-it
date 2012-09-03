#! /usr/bin/env racket

#lang racket

(define make-counts
  (lambda ([ins (find-files (lambda (arg)
			      (regexp-match-exact? #px".+c[a-r][0-9]{2}" arg))
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
	  [dicts (list
		   (dict-update (car dicts) wt add1 0)
		   (dict-update (cadr dicts) (cadr (regexp-split #rx"/" wt)) add1 0))])
      (if (null? (cdr wts))
	dicts
	(get-wt (cdr wts) dicts)))))

(for-each
  (lambda (arg)
    (display arg))
  (dict->list (car (make-counts))))
