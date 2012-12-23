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

; add start-of-sentence symbols
; @in
;   wts: list of word/tag
; @out
;   wts-markers: list of word/tag with start-of-sentence symbols
(define add-sos
  (lambda (wts
            [wts-markers '("<s>/<s>")])
    (if (null? wts)
      (flatten wts-markers)
      (add-sos (cdr wts) (cons wts-markers 
                                   (if (string=? (car wts) "./.")
                                    (cons (car wts) "<s>/<s>")
                                    (car wts)))))))

; basic testing
(displayln (add-sos (get-wts)))

; filter out start-of-sentence symbols
; @in
;   tags-sos: list of tags with start-of-sentence symbols
; @out
;   tags: list of tags, without start-of-sentence symbols
(define filter-sos
  (lambda (tags-sos
            [tags '()])
    (if (null? tags-sos)
      (flatten tags)
      (filter-sos (cdr tags-sos) (if (string=? (car tags-sos) "<s>")
                                   tags
                                   (cons tags (car tags-sos)))))))

; basic testing
(displayln (filter-sos '("<s>" "vb" "nn" "jj" "<s>" "whatever" "<s>")))

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
      (get-b-row wts-dict (cdr words) tag tag-count (list row (/ (+ 1 (dict-ref wts-dict (string-append (car words) "/" tag) 0)) (+ (dict-count wts-dict) tag-count)))))))

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
        (make-matrix (- (dict-count tags-dict) 1) (length words) (flatten matrix)))
      (get-b-matrix wts-dict tags-dict words (cdr tags) (list matrix (get-b-row wts-dict words (car tags) (dict-ref tags-dict (car tags))))))))

; basic testing
(let ([wts-dict '(("w1/t1" . 1) ("w2/t1" . 2) ("w3/t2" . 1) ("w1/t2" . 4) ("w2/t2" . 5) ("w4/t3" . 2))]
      [tags-dict '(("<s>" . 1) ("t1" . 3) ("t2" . 10) ("t3" . 2))]
      [words '("w1" "w2" "w3" "w4")]
      [tags '("t1" "t2" "t3")])
  (displayln (matrix-render (get-b-matrix wts-dict tags-dict words tags))))

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

; @todo: (?) add start and end transition probabilities for beginning and final states

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
		     [taglist-left (flatten (cons "<s>" taglist))]
		     [matrix '()])
    (if (null? taglist-left)
      (make-matrix (+ (length taglist) 1) (length taglist) (flatten matrix))
      (get-a-matrix tags-dict tags taglist (cdr taglist-left) (list matrix (get-a-row tags (car taglist-left) (dict-ref tags-dict (car taglist-left)) taglist))))))

; basic testing
(let ([tags-dict '(("<s>" . 1) ("t1" . 5) ("t2" . 3) ("t3" . 1) ("t4" . 3) ("t5" . 2))]
      [tags '("t1" "t4" "t5" "t2" "t1" "t1" "t3" "t2" "t1" "t4" "t5" "t1" "t4" "t2")]
      [taglist '("t1" "t2" "t3" "t4" "t5")])
  (displayln (matrix-render (get-a-matrix tags-dict tags taglist))))

; @todo: address the unknown words issue
; @todo: optimize by building a list instead of operating on the matrix itself

; perform the initialization step of the Viterbi algorithm
; @in
;   a-matrix: the A matrix
;   b-matrix: the B matrix
;   o1: col index of the first word (observation) in the B matrix
;   tags-index: list of (tag, index) pairs
;   viterbi: a (len(tags) + 2) x len(obs) matrix
; @out
;   viterbi: a matrix whose first column is initialized
(define viterbi-init
  (lambda (a-matrix b-matrix o1 tags-index viterbi)
    (if (null? tags-index)
      viterbi
      (let ([i (cdar tags-index)])
	(viterbi-init a-matrix b-matrix o1
		      (cdr tags-index)
		      (matrix-set viterbi (+ i 1) 1 (* (matrix-ref a-matrix 0 i)
						 (matrix-ref b-matrix i o1))))))))

; perform the recursion step of the Viterbi algorithm
; @in
;   a-matrix: the A matrix
;   b-matrix: the B matrix
;   obs: col indexes of observations in the B matrix
;   tags-index: list of (tag, index) pairs
;   viterbi: a (len(tags) + 2) x len(obs) initialized matrix
;   backptr: a (len(tags) + 2) x len(obs) initialized matrix
;   t: observation index
; @out
;   viterbi: an initialized matrix, but the last col
;   backptr: an initialized matrix, but the last col
(define viterbi-loop
  (lambda (a-matrix b-matrix obs tags-index viterbi backptr
		    [t 2])
    (if (null? obs)
      (list viterbi backptr)
      (letrec ([inner-loop (lambda (vit back
				      [tags-left tags-index])
			  (if (null? tags-left)
			    (list vit back)
			    (let ([s (cdar tags-left)])
			      (inner-loop
			        (matrix-set vit (+ s 1) t (get-max vit s t a-matrix b-matrix tags-index (car obs)))
				(matrix-set back (+ s 1) t (cdr (get-arg-max vit s t a-matrix tags-index)))
				(cdr tags-left)))))]
	    [pair (inner-loop viterbi backptr)])
	(viterbi-loop a-matrix b-matrix (cdr obs) tags-index (car pair) (cadr pair) (+ t 1))))))

(define viterbi-end
  (lambda (pair a-matrix s t tags-index)
    (let ([vit (car pair)]
	  [back (cadr pair)])
      (displayln (matrix-render vit))
      (displayln (matrix-render back))
      (list
        (matrix-set vit (+ s 1) t (get-max-end vit s t a-matrix tags-index))
	(matrix-set back (+ s 1) t (get-arg-max-end vit s t a-matrix tags-index))))))

; @todo: combine get-max and get-arg-max in one proc
; @todo: ditto for get-max-end and get-arg-max-end

; return the maximum probability
; @in
;   viterbi: the Viterbi matrix
;   s: the current state (tag)
;   t: the current step (observation)
;   a-matrix: the A matrix
;   b-matrix: the B matrix
;   tags-index: list of (tag, index) pairs
;   obs: the index of observation at step t in matrix B
; @out
;   res: the maximum probability
(define get-max
  (lambda (viterbi s t a-matrix b-matrix tags-index obs
		   [res 0])
    (if (null? tags-index)
      res
      (let* ([ss (cdar tags-index)]
	     [cur (* (* (matrix-ref viterbi (+ ss 1) (- t 1))
			(matrix-ref a-matrix (+ ss 1) s))
		     (matrix-ref b-matrix s obs))])
	(get-max viterbi s t a-matrix b-matrix (cdr tags-index) obs (if (< res cur)
								    cur
								    res))))))

(define get-max-end
  (lambda (viterbi s t a-matrix tags-index
		   [res 0])
    (if (null? tags-index)
      res
      (let* ([ss (cdar tags-index)]
	     [cur (* (matrix-ref viterbi (+ ss 1) t)
		     (matrix-ref a-matrix (+ ss 1) s))])
	(get-max-end viterbi s t a-matrix (cdr tags-index) (if (< res cur)
							     cur
							     res))))))

; return a (max, index) pair of the maximum probability
; @in
;   viterbi: the Viterbi matrix
;   s: the current state (tag)
;   t: the current step (observation)
;   a-matrix: the A matrix
;   tags-index: list of (tag, index) pairs
; @out
;   res: a (max, index) pair
(define get-arg-max
  (lambda (viterbi s t a-matrix tags-index
		   [res '(0 . 0)])
    (if (null? tags-index)
      res
      (let* ([ss (cdar tags-index)]
	     [cur (* (matrix-ref viterbi (+ ss 1) (- t 1))
		     (matrix-ref a-matrix (+ ss 1) s))])
	(get-arg-max viterbi s t a-matrix (cdr tags-index) (if (< (car res) cur)
							     (cons cur (+ ss 1))
							     res))))))

(define get-arg-max-end
  (lambda (viterbi s t a-matrix tags-index
		   [res '(0 . 0)])
    (if (null? tags-index)
      res
      (let* ([ss (cdar tags-index)]
	     [cur (* (matrix-ref viterbi (+ ss 1) t)
		     (matrix-ref a-matrix ss s))])
	(get-arg-max-end viterbi s t a-matrix (cdr tags-index) (if (< (car res) cur)
								 (cons cur (+ ss 1))
								 res))))))

; @todo: address the unknown word issue

; build a list of observations' indexes
; @in
;   obs: list of observations
;   words-index: hashmap of (word, index)
; @out
;   obs-index: list of observations' indexes
(define index-words
  (lambda (obs words-index
	       [obs-index '()])
    (if (null? obs)
      (flatten obs-index)
      (index-words (cdr obs) words-index (list obs-index (hash-ref words-index (car obs) 0))))))

; apply part-of-speech tagging on text
; @in
;   obs: text to tag
; @out
;   pos: list of pairs (observation, tag)
(define tag-it
  (lambda (obs)
    '()))

; @todo: optimize vars

; retrieve list of word/tag
(let* ([wts (add-sos (get-wts))]
; make a dictionary word/tag -> count
       [wts-dict (list->dict wts)]
; extract words and tags in two lists
       [wts-exploded (explode-list wts)]
       [words (filter-sos (car wts-exploded))]
       [tags-sos (cadr wts-exploded)]
; make a dictionary tag -> count
       [tags-dict (list->dict tags-sos)]
; retrieve its size, minus start-of-sentence symbol
       [tags-size (- (dict-count tags-dict) 1)]
; filter out duplicates of words list
       [words-unique (filter-unique words)]
; retrieve its size
       [words-size (length words-unique)]
; remove start-of-sentence symbols
       [tags (filter-sos tags-sos)]
; filter out duplicates of tags list
       [tags-unique (filter-unique tags)]
; build tags index (make-hash)?
       [tags-index (index-list tags-unique)]
; build words index
       [words-index (make-hash (index-list words-unique))]
; build A matrix
       [a-matrix (get-a-matrix tags-dict tags-sos tags-unique)]
; build B matrix
       [b-matrix (get-b-matrix wts-dict tags-dict words tags-unique)]
; the observations (words) to tag
       [obs '("I" "want" "to" "try" "it")]
; number of rows of viterbi and backptr matrices
       [n (+ (length tags-unique) 2)]
; number of cols of viterbi and backptr matrices
       [m (+ (length obs)  1)]
; 0-filled list to populate viterbi and backptr matrices
       [lst-0 (build-list (* n m) (lambda (x) 0))])
    (printf "a-matrix: ~a\n b-matrix: ~a\n" (matrix-render a-matrix) (matrix-render b-matrix))
    (printf "vit-init: ~a\n" (matrix-render (viterbi-init a-matrix b-matrix (hash-ref words-index (car obs)) tags-index (make-matrix n m lst-0))))
    (printf "I: ~a want: ~a to: ~a try: ~a it ~a\n" (hash-ref words-index "I" -1) (hash-ref words-index "want" -1) (hash-ref words-index "to" -1) (hash-ref words-index "try" -1) (hash-ref words-index "it" -1))
    (displayln (matrix-render (car (viterbi-end
				     (viterbi-loop
				       a-matrix
				       b-matrix
				       (index-words (cdr obs) words-index)
				       tags-index
				       (viterbi-init
				         a-matrix
				         b-matrix
				         (hash-ref words-index (car obs))
				         tags-index
				         (make-matrix n m lst-0))
				       (make-matrix n m lst-0))
				         a-matrix
					 (- n 3)
					 (- m 1)
					 tags-index)))))
