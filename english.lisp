;English sentence maker
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  (cond ((listp phrase)
	 (mappend #'generate phrase))
	((rewrites phrase)
	 (generate (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

#|(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (Article) (Noun)))

(defun verb-phrase ()
  (append (Verb) (noun-phrase)))

(defun Article ()
  (one-of '(the a)))

(defun Noun ()
  (one-of '(man ball woman table)))

(defun Verb ()
  (one-of '(hit took saw liked)))
|#
(defun one-of (set)
  (list (random-elt set)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase (generate-tree (random-elt (rewrites phrase)))))
	(t (list phrase))))

(defun generate-all (phrase)
  (cond ((null phrase) (list nil))
	((listp phrase)
	 (combine-all (generate-all (first phrase))
		      (generate-all (rest phrase))))
	((rewrites phrase)
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))

(defun conbine-all (xlist ylist)
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (append x y))
		       xlist))
	   ylist))
