;;; -*- Mode: LFE -*-
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
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
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
