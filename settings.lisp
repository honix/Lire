;;
;; Lire - settable variables
;;

(in-package :lire)

;; Window

(defparameter *initial-width*  800)
(defparameter *initial-height* 600)

;; Canvas

(defparameter *grid-size*          50) ; px
(defparameter *zoom-max*           2)
(defparameter *zoom-min*           0.2)
(defparameter *double-click-limit* 0.3)

;; Node

(defparameter *node-height*      15) ; px
(defparameter *node-text-height* (* *node-height* 0.7))
(defparameter *node-width-char*  (* *node-text-height* 0.54)) ; ?
(defparameter *node-width-bumps* 10) ; px
