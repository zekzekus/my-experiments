;;;; 2006-11-09 00:23:45
;;;; MU Bulmacasý
(in-package :lisp-deneme)

(defvar *miu* '(M I U))  ;; Biçimsel dizge.
(defvar *ilksav* '(M I)) ;; Baþlangýç dizgesi.
(defvar *hedef* '(M U))  ;; Bulunmasý istenen dizge.

(defun gecerli-p (dizge)
  (not (set-difference dizge *miu*)))

(defun k1 (dizge)
  (when (gecerli-p dizge)
    (if (equal (last dizge) '(I))
        (append dizge '(U))
        nil)))

(defun k2 (dizge)
  (when (gecerli-p dizge)
    (if (equal (first dizge) 'M)
        (append dizge (rest dizge))
        nil)))

(defun k3 (dizge)
  (when (gecerli-p dizge)
    (let ((sayac1 0)
          (sayac2 0))
      (dolist (i dizge nil)
        (incf sayac1)
        (if (equal i 'I)
            (incf sayac2)
            (setf sayac2 0))
        (when (= sayac2 3)
          (return (nconc (subseq dizge 0 (- sayac1 3)) (cons 'U (nthcdr sayac1 dizge)))))))))

(defun k4 (dizge)
  (when (gecerli-p dizge)
    (let ((sayac1 0)
          (sayac2 0))
      (dolist (i dizge nil)
        (incf sayac1)
        (if (equal i 'U)
            (incf sayac2)
            (setf sayac2 0))
        (when (= sayac2 2)
          (return (remove 'U dizge :start (- sayac1 2) :end sayac1)))))))