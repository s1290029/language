;output first-name and last-name
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General))

(setf names '((First Last) (Joseph Joe) (Coca Cola)))

(defun last-name (name)
  (first (last name)))
#|
(defun first-name (name)
  (first name))
|#

(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
#|
(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun mappend (fn the-list)
  (if (null the-list) nil
      (append (funcall fn (first the-list))
	      (mappend fn (rest the-list)))))
|#
