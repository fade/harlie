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

(defclass bot-channel ()
  ((bot-channel-id :col-type serial
                   :reader bot-channel-id)
   (channel-name :col-type text
            :initarg :channel-name
            :unique t
            :initform ""
            :accessor channel-name)
   (server :col-type text
           :initarg :server
           :initform "" :accessor server))
  
  (:documentation "Metadata for each channel the bot monitors.")
  (:metaclass postmodern:dao-class)
  ;; (:table-name bot-channels)
  (:keys bot-channel-id channel-name))

(defun make-bot-channel (channel-name &optional (server-name nil))
  (format t "Creating a channel object for channel: ~A" channel-name)
  (with-connection (psql-botdb-credentials *bot-config*)
   (let ((bco (make-instance 'bot-channel
                             :channel-name channel-name
                             :server server-name)))
     (if (save-dao bco)
         (values bco)
         nil))))

(defun get-bot-channel-for-name (name)
  "Given a NAME, return the associated BOT-CHANNEL object."
  (with-connection (psql-botdb-credentials *bot-config*)
    (format t "~2&CHANNEL: ~A~2%" name)
    (let ((c (select-dao 'bot-channel (:= 'channel name))))
      (if (and (listp c) (>= (length c) 1))
          (first c)
          nil))))

(defclass channel-user ()
  ((channel-id :col-type integer :col-references ((bot-channel bot-channel-id)) :initarg :channel-id :accessor channel-id)
   (user-id :col-type integer :col-references ((harlie-user harlie-user-id)) :initarg :user-id :accessor user-id)
   (ignored :col-type boolean :initarg :ignored :accessor ignored))
  (:metaclass postmodern:dao-class)
  (:documentation "A table to bridge between a channel and the users it contains.")
  (:keys channel-id user-id))


(defclass harlie-user ()
  ((harlie-user-id :col-type serial
                   :reader harlie-user-id)
   
   (harlie-user-name :col-type text
                     :initarg :harlie-user-name
                     :initform ""
                     :accessor harlie-user-name)
   
   (current-handle :col-type text
                   :initarg :current-handle
                   :initform ""
                   :accessor current-handle )
   
   (prev-handle :col-type text
                :initarg :prev-handle
                :initform ""
                :accessor prev-handle)

   (email-address :col-type (or db-null text)
                  :initarg :email-address
                  :initform ""
                  :accessor email-address)

   (authenticated? :col-type boolean
                   :initarg :authenticated?
                   :initform nil
                   :accessor authenticated?)

   ;; follows some persistent states we want tracked
   (ignored :col-type boolean
            :initarg :ignored
            :initform nil
            :accessor ignored)

   (first-seen :col-type timestamptz
               :initarg :first-seen
               :initform (local-time:now)
               :accessor first-seen)

   (last-seen :col-type timestamptz
              :initarg :last-seen
              :initform (local-time:now)
              :accessor last-seen)

   (harlie-user-memo :col-type text
                     :initarg :harlie-user-memo
                     :initform ""
                     :accessor harlie-user-memo))
  
  (:metaclass postmodern:dao-class)
  ;; (:table-name harlie-users)
  (:documentation "This table holds the user metadata for users being served by the bot.")
  (:keys harlie-user-id))

(defun get-user-for-id (id)
  "Given an ID of type integer, return the associated channel user handle."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((u (select-dao 'harlie-user  (:= 'harlie-user-id id))))
      (if (and (listp u) (>= (length u) 1))
          (first u)
          nil))))

(defun get-user-for-handle (handle &key channel)
  "Given a HANDLE, return the associated user"
  (with-connection (psql-botdb-credentials *bot-config*)
    (format t "~2&[HANDLE] : ~A [CHANNEL] : ~A" handle channel)
    (if channel
        (let ((u (select-dao 'harlie-user (:= 'current_handle handle))))
          (if (and (listp u) (>= (length u) 1))
              (first u)
              nil))
        (let ((u (select-dao 'channel-user (:= 'current_handle handle))))
          (if (and (listp u) (>= (length u) 1))
              (first u)
              nil)))))

;; (defgeneric update-channel-user ((user channel-user) &key (:email :prev-handle :current-handle )))

(defun zero-channel-user ()
  (with-connection (psql-botdb-credentials *bot-config*)
    (when (pomo:table-exists-p 'channel-user)
      (execute (drop-table 'channel-user)))
    (execute (dao-table-definition 'channel-user))))

(defun zero-users ()
  "Let's just... start all over again."
  (setf *users* (make-hash-table :test 'equalp :synchronized t))
  (zero-channel-user)
  (zero-bot-channel)
  (zero-harlie-users))

(defun zero-bot-channel ()
  "Destroy and recreate the table to hold the channels the bot joins."
  (with-connection (psql-botdb-credentials *bot-config*)
    (when (pomo:table-exists-p 'bot-channel)
      (execute (drop-table 'bot-channel)))
    (execute (dao-table-definition 'bot-channel))))

;; (defun zero-channel-users ()
;;   "destroy and recreate the table to hold a channel's persistent users."
;;   (with-connection (psql-botdb-credentials *bot-config*)
;;     (when (pomo:table-exists-p 'channel-user)
;;       (query (:drop-table 'channel-user)))
;;     (execute (dao-table-definition 'channel-user))))

(defun zero-harlie-users ()
  "destroy the table that holds the users known to the bot."
  (with-connection (psql-botdb-credentials *bot-config*)
    (when (pomo:table-exists-p 'harlie-user)
      (execute (:drop-table 'harlie-user)))
    (execute (dao-table-definition 'harlie-user))))

;; (defun zero-user-aliases ()
;;   "destroy the talbe that holds the aliases for channel users."
;;   (with-connection (psql-botdb-credentials *bot-config*)
;;     (when (pomo:table-exists-p 'user-alias)
;;       (execute (:drop-table 'user-alias)))))

;; (defun make-user-aliases ()
;;   (execute (dao-table-definition 'user-alias)))


(defun make-harlie-user (nick-message)
  "given an irc user handle in nick-message, create an instance of the
'harlie-user dao class."
  (format t "~2&KLEEVO! [ ~A ]~2%" (describe nick-message))
  (make-instance 'harlie-user
                 :harlie-user nick-message
                 :current-handle nick-message
                 :prev-handle nil
                 :authenticated nil))

(defun make-new-harlie-user (nick)
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((uobject (make-instance 'harlie-user
                           :harlie-user-name nick
                           :current-handle nick
                           :prev-handle nil
                           :authenticated nil)))
      (if (save-dao uobject)
          (values uobject)
          nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; users.lisp ends here
