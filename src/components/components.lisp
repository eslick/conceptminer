;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          components.lisp
;;;; Purpose:       Support for conceptminer components
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;
;;;; Top level helpers for building our concept components 
;;;; (i.e. default transactional messages for elephant)

(in-package :conceptminer)

(defmacro defcomponent-ele (name &rest body-forms)
  "Ensure proper elephant transaction wrapping inside components"
  `(defcomponent ,name
       ,@body-forms
     (:transaction-start
      (with-miner-store ()
	(start-ele-transaction :store-controller *conceptminer-db*)))
     (:transaction-commit
      (with-miner-store ()
	(commit-transaction)))
     (:transaction-abort
      (with-miner-store ()
	(abort-transaction)))))

