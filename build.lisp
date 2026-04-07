(require :asdf)
(load "~/quicklisp/setup.lisp")
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :harlie)
(sb-ext:save-lisp-and-die "consort"
                          :toplevel (lambda ()
                                      (harlie:run-bot)
                                      (loop (sleep 3600)))
                          :executable t
                          :compression t)
