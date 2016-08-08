;;;; calcu-qt.lisp

(in-package #:calcu-qt)

(in-readtable :qtools)

(defparameter *default-capacity* 10)

(defparameter *button-layout*
  '((nil nil nil ce ac)
    (m mr m+ m- mc)
    (b7 b8 b9 % sqrt)
    (b4 b5 b6 b* b/)
    (b1 b2 b3 b+ b-)
    (b0 dot pm (b= 1 2))))

(defparameter *button-commands*
  '((ce "CE")
    (ac "AC")
    (m "M")
    (mr "MR")
    (mc "MC")
    (m+ "M+")
    (m- "M-")
    (sqrt "sqrt")
    (% "%")
    (b+ "+")
    (b- "-")
    (b* "*")
    (b/ "/")
    (b= "=")
    (pm "pm")
    (dot ".")
    (b0 "0")
    (b1 "1")
    (b2 "2")
    (b3 "3")
    (b4 "4")
    (b5 "5")
    (b6 "6")
    (b7 "7")
    (b8 "8")
    (b9 "9")
    ))

(defun button-command (button &optional (button-commands *button-commands*))
  (second (assoc button *button-commands*)))

(defparameter *commands*
  `(("0" ,(lambda (calc) (calcu-core:feed-char #\0 calc)))
    ("1" ,(lambda (calc) (calcu-core:feed-char #\1 calc)))
    ("2" ,(lambda (calc) (calcu-core:feed-char #\2 calc)))
    ("3" ,(lambda (calc) (calcu-core:feed-char #\3 calc)))
    ("4" ,(lambda (calc) (calcu-core:feed-char #\4 calc)))
    ("5" ,(lambda (calc) (calcu-core:feed-char #\5 calc)))
    ("6" ,(lambda (calc) (calcu-core:feed-char #\6 calc)))
    ("7" ,(lambda (calc) (calcu-core:feed-char #\7 calc)))
    ("8" ,(lambda (calc) (calcu-core:feed-char #\8 calc)))
    ("9" ,(lambda (calc) (calcu-core:feed-char #\9 calc)))
    ("." ,(lambda (calc) (calcu-core:feed-char #\. calc)))
    ("pm" ,(lambda (calc) (calcu-core:feed-pm calc)))
    ("+" ,(lambda (calc) (calcu-core:feed-binary-operation '+ calc)))
    ("-" ,(lambda (calc) (calcu-core:feed-binary-operation '- calc)))
    ("*" ,(lambda (calc) (calcu-core:feed-binary-operation '* calc)))
    ("/" ,(lambda (calc) (calcu-core:feed-binary-operation '/ calc)))
    ("=" ,(lambda (calc) (calcu-core:compute calc t nil)))
    ("%" ,(lambda (calc) (calcu-core:compute calc t nil t)))
    ("AC" ,(lambda (calc) (calcu-core:reset-calculator calc)))
    ("CE" ,(lambda (calc) (calcu-core:c calc)))
    ("M" ,(lambda (calc) (calcu-core:m calc)))
    ("MR" ,(lambda (calc) (calcu-core:mr calc)))
    ("M+" ,(lambda (calc) (calcu-core:m+ calc)))
    ("M-" ,(lambda (calc) (calcu-core:m- calc)))
    ("MC" ,(lambda (calc) (calcu-core:mc calc)))
    ("sqrt" ,(lambda (calc) (calcu-core:feed-unary-operation 'sqrt calc)))))

(defun command->function (command &optional (commands *commands*))
  (second (assoc command commands :test #'string=)))

(define-widget calculator (QWidget)
  ((brain :reader brain)))

(defmethod initialize-instance ((calc calculator) &rest initargs &key (capacity *default-capacity*) &allow-other-key)
  (declare (ignore initargs))
  (setf (slot-value calc 'brain) (calcu-core:make-calculator capacity))
  (call-next-method))

(defmacro define-button (name label)
  `(define-subwidget (calculator ,name) (q+:make-qpushbutton ,label calculator)
     (q+:resize ,name 30 30)))

(define-button ce "CE")
(define-button ac "AC")
(define-button m "M")
(define-button mr "MR")
(define-button mc "MC")
(define-button m+ "M+")
(define-button m- "M-")
(define-button sqrt "√")
(define-button % "%")
(define-button b+ "+")
(define-button b- "-")
(define-button b* "×")
(define-button b/ "÷")
(define-button b= "=")
(define-button dot "·")
(define-button pm "±")
(define-button b0 "0")
(define-button b1 "1")
(define-button b2 "2")
(define-button b3 "3")
(define-button b4 "4")
(define-button b5 "5")
(define-button b6 "6")
(define-button b7 "7")
(define-button b8 "8")
(define-button b9 "9")

(define-subwidget (calculator display) (q+:make-qlineedit calculator)
  (q+:set-read-only display t)
  (setf (q+:alignment display) 2)
  (setf (q+:font display) (q+:make-qfont "Fixed" 15)))


(define-signal (calculator update) ())
(define-signal (calculator command) (string))

(define-signal (calculator feed-char) (string))

(define-slot (calculator command) ((cmd string))
  (declare (connected calculator (command string)))
  (funcall (command->function cmd) brain)
  (signal! calculator (update)))


(defun display-string (calc)
  (let* ((s (calcu-core::display-string calc))
	 (p1 (position #\Newline s))
	 (p2 (position #\Newline s :end (1- (length s)) :from-end t)))
    (subseq s (1+ p1) p2)))

(define-slot (calculator update) ()
  (declare (connected calculator (update)))
  (q+:set-text display (display-string brain)))

(define-subwidget (calculator layout) (q+:make-qgridlayout calculator)
  (q+:add-widget layout (slot-value calculator 'display) 0 0 1 5)
  (loop
     for i from 1
     for row in *button-layout*
     do (loop
	   for j from 0
	   for b in row
	   when b
	   do (if (atom b)
		  (q+:add-widget layout (slot-value calculator b) i j)
		  (q+:add-widget layout (slot-value calculator (first b)) i j (second b) (third b)))))
  (mapcar (lambda (b-cmd)
	    (destructuring-bind (b cmd) b-cmd
	      (connect (slot-value calculator b)
		       "clicked()"
		       (lambda ()
			 (signal! calculator (command string) cmd)))))
	  *button-commands*))

(defun main ()
  (with-main-window (window (print (make-instance 'calculator :capacity *default-capacity*)))))
