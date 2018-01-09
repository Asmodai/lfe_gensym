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
  (let ((syms (car body))
        (body (cdr body)))
    `(let ,(lists:map (lambda (x)
                        `(,x (lfe_gensym:gensym)))
                      syms)
       ,@body)))

(defmacro once-only body
  (let* ((names (car body))
         (body (cdr body))
         (gensyms (lists:map (lambda (x)
                               (lfe_gensym:gensym))
                             names)))
    `(let (,@(lists:map (lambda (g)
                          `(,g (lfe-gensym:gensym)))
               gensyms))
       `(let (,,@(lists:map (lambda (n g)
                              ``(,n ,g))
                   (lists:zip names gensyms)))
          ,(let (,@(lists:map (lambda (n g)
                                `(,n, g))
                     (lists:zip names gensyms)))
             ,@body)))))

;;; lfe_gensym.lfe ends here.
