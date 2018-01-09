;;; -*- Mode: LISP; Syntax: LFE -*-
;;;
;;; lfe_gensym.lfe --- `gensym' hacks for LFE.
;;;
;;; Copyright (c) 2018 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    09 Jan 2018 02:59:49
;;;
;;;{{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

(defmacro with-gensyms body
  "WITH-GENSYMS symbols body

Execute BODY in a context where the variables in SYMBOLS are bound to fresh
gensyms."
  (let ((syms (car body))
        (body (cdr body)))
    `(let ,(lists:map (lambda (x)
                        `(,x (lfe_gensym:gensym)))
                      syms)
       ,@body)))

(defmacro once-only body
  "ONCE-ONLY symbols body

Execute BODY in a context where the values bound to the variables in SYMBOLS
are bound to fresh gensyms, and the variables in SYMBOLS are bound to the
corresponding gensym."
  (let* ((symbols (car body))
         (body    (cdr body))
         (gensyms (lists:map (lambda (_)
                               (lfe_gensym:gensym))
                             symbols)))
    ``(let (,,@(lists:map (lambda (elt)
                            ``(,',(element 1 elt) ,,(element 2 elt)))
                          (lists:zip gensyms symbols)))
        ,(let ,(lists:map (lambda (elt)
                            `(,(element 1 elt) ',(element 2 elt)))
                          (lists:zip symbols gensyms))
           ,@body))))

;;; lfe_gensym.lfe ends here.
