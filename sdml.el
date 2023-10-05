;;; sdml.el --- SDML Model -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: 
;; Version: 0.0.1

;;; License:

;; Copyright (c) 2023 Simon Johnston

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Complete description goes here.

;;; Code:

(require 'cl-lib)
(require 'seq)

;; --------------------------------------------------------------------------
;; Section heading
;; --------------------------------------------------------------------------

(defun qualified-id-p (value)
  (let ((ids (string-split value ":")))
    (if (= (length ids) 2)
        (seq-every-p #'identifierp ids))))

(defun identifierp (value)
  (and (stringp value) (string-match "\\^[]\\$" value)))

(defun identifier-ref-p (value)
  (or (identifierp value) (qualified-id-p value)))

(defun iri-ref-p (value)
  (string value))

(cl-defstruct module
  (:constructor module--create)
  name base body)

(defun module-create (name &key base body)
  (unless (and (identifierp name)
               (if base (iri-ref-p base) t)
               (if body (module/body-p body)))
    (error))
  (module--create :name name :base base :body body))

(cl-defstruct module/body
  (:constructor module/body--create)
  imports annotations definitions)



(provide 'sdml)

;;; sdml.el ends here
