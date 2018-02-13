;;
;; Lire - settable variables
;;

(in-package :lire)

;; Window

(defparameter *initial-width*    800)
(defparameter *initial-height*   600)

;; Canvas

(defparameter *grid-size*          50) ; px
(defparameter *zoom-max*           2)
(defparameter *zoom-min*           0.2)
(defparameter *double-click-limit* 0.3)

;; Menu

(defparameter *menu-width*         100) ; px
(defparameter *menu-button-height* 30) ; px
(defparameter *menu-offset*        10) ; px

;; Node

(defparameter *node-height*      15) ; px
(defparameter *node-text-height* (* *node-height* 0.7))
(defparameter *node-width-char*  (* *node-text-height* 0.54)) ; ?
(defparameter *node-width-bumps* 10) ; px

;; Color themes

(progn
  (defparameter *color-theme*      :light)
  (defparameter *background-color* (case *color-theme*
                                     (:light '(0.9 0.9 0.9 1))
                                     (:dark  '(0.1 0.1 0.1 1))))
  (defparameter *alt-color*        (case *color-theme*
                                     (:light '(0 0 0 1))
                                     (:dark  '(1 1 1 1))))
  (defparameter *dimm-color*       (let ((c *alt-color*))
                                     (list (first  c)
                                           (second c)
                                           (third  c)
                                           0.35)))
  (defparameter *normal-color*     (case *color-theme*
                                     (:light '(0 0 1 1))
                                     (:dark  '(0 1 1 1))))
  (defparameter *warn-color*       (case *color-theme*
                                     (:light '(1 0.25 0 1))
                                     (:dark  '(1 0.75 0 1))))
  (defparameter *selector-color*   (let ((c *normal-color*))
                                     (list (first  c)
                                           (second c)
                                           (third  c)
                                           0.25))))

