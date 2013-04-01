;;; users.lisp --- 
;; 
;; Filename: users.lisp
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: Tue Mar 12 12:04:43 2013 (-0400)
;; Version: 
;; Last-Updated: Sat Mar 16 14:39:22 2013 (-0400)
;;           By: fade
;;     Update #: 4
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(in-package :harlie)

(defclass channel-user ()
  ((channel-user :initarg :channel-user :initform nil :accessor channel-user)
   (current-handle :initarg :current-handle :initform nil :accessor current-handle)
   (prev-handle :initarg :prev-handle :initform nil :accessor prev-handle)))

(defclass authenticated-channel-user (channel-user)
  ((authed-p :initarg :authed-p :initform nil :accessor authed-p)))

(defun make-channel-user (nick-message)
  (make-instance 'channel-user
                 :channel-user (user nick-message)
                 :current-handle ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; users.lisp ends here
