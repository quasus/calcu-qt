;;;; calcu-qt.asd

(asdf:defsystem #:calcu-qt
  :description "A QT interface for calcu-core"
  :author "Stanislav Kondratyev"
  :license "WTFPL"
  :defsystem-depends-on (:qtools)
  :depends-on (#:calcu-core
               #:qtcore
	       #:qtgui)
  :build-operation "qt-program-op"
  :build-pathname "calcu"
  :entry-point "calcu-qt:main"
  :serial t
  :components ((:file "package")
               (:file "calcu-qt")))

