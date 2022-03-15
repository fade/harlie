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
  ((user-id :col-type serial :accessor user-id)
   (channel-user :initarg :channel-user :col-type text :initform "" :accessor channel-user)
   (current-handle :initarg :current-handle :col-type text :initform "" :accessor current-handle)
   (prev-handle :initarg :prev-handle :col-type text :initform "" :accessor prev-handle)
   (email-address :initarg :email-address :col-type text :initform nil :accessor email-address)
   (channel :initarg :channel :col-type text :initform nil :accessor channel)
   (server :initarg :server :col-type text :initform nil :accessor server)
   (authenticated :initarg :authenticated :col-type boolean :initform nil :accessor authenticated)
   (ignored :initarg :ignore :col-type boolean :initform nil :accessor ignored))
  (:metaclass postmodern:dao-class)
  (:documentation "This table holds data relevant to the persistent channel users, about whom we would like to calculate things in service to their whims.")
  (table-name channel-users)
  (:keys user-id))

(defun get-user-for-id (id)
  "Given an ID of type integer, return the associated channel user handle."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((u (select-dao 'channel-user  (:= 'user-id id))))
      (if (and (listp u) (>= (length u) 1))
          (first u)
          nil))))

(defun get-user-for-handle (handle)
  "Given a HANDLE, return the associated user"
  (with-connection (psql-botdb-credentials *bot-config*)
    (format t "~2&[HANDLE] : ~A [TYPE] : ~A" handle (type-of handle))
    (let ((u (select-dao 'channel-user (:= 'current_handle handle))))
      (if (and (listp u) (>= (length u) 1))
          (first u)
          nil))))

;; (defgeneric update-channel-user ((user channel-user) &key (:email :prev-handle :current-handle )))

(defclass user-alias ()
  ((id :col-type serial :accessor alias-id)
   (alias-for-id :col-type integer :col-references ((channel-user user-id) :cascade)
                 :initarg :alias-for-id :accessor alias-for-id)
   (user-alias :initarg :user-alias :col-type text :initform "" :accessor user-alias))
  (:metaclass postmodern:dao-class)
  (:documentation "This table class holds the known aliases for any given channel user, if any.")
  (table-name user-aliases)
  (:keys alias-for-id user-alias))

(defun get-aliases-for-userid (userid)
  "Given a USERID of type integer, return the aliases associated with
that id."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((a (select-dao 'user-aliases (:= 'alias-for-id userid))))
      a)))

(defun add-alias-for-user (user alias)
  "Given a USER (string), add an ALIAS (string) to the user-aliases table."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((a (make-instance 'user-alias
                            :alias-for-id (user-id (get-user-for-handle user))
                            :user-alias alias)))
      a)))

(defun zero-users ()
  "Let's just... start all over again."
  (zero-user-aliases)
  (zero-channel-users)
  (make-user-aliases))

(defun zero-channel-users ()
  "destroy and recreate the table to hold a channel's persistent users."
  (with-connection (psql-botdb-credentials *bot-config*)
    (when (pomo:table-exists-p 'channel-user)
      (query (:drop-table 'channel-user)))
    (execute (dao-table-definition 'channel-user))))

(defun zero-user-aliases ()
  "destroy the talbe that holds the aliases for channel users."
  (with-connection (psql-botdb-credentials *bot-config*)
    (when (pomo:table-exists-p 'user-alias)
      (execute (:drop-table 'user-alias)))))

(defun make-user-aliases ()
  (execute (dao-table-definition 'user-alias)))


(defun make-channel-user (nick-message)
  "given an irc user handle in nick-message, create an instance of the
'channel-user dao class."
  (format t "~2&KLEEVO! [ ~A ]~2%" (describe nick-message))
  (make-instance 'channel-user
                 :channel-user nick-message
                 :current-handle nick-message
                 :prev-handle nil
                 :authenticated nil))

(defun make-new-channel-user (nick)
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((uobject (make-instance 'channel-user
                           :channel-user nick
                           :current-handle nick
                           :prev-handle nil
                           :authenticated nil)))
      (save-dao uobject))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; users.lisp ends here
