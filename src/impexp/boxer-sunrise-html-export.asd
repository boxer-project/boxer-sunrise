(asdf:defsystem "boxer-sunrise-html-export"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on ("boxer-sunrise"
               :cffi
               :html-entities
               :qbase64
               :zpng
               )
  :components ((:file "full-html-export"))
  :description "HTML Export system. In it's own system due to a large number of dependencies.")
