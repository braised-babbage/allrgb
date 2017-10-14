(defpackage :allrgb
  (:use :common-lisp :zpng :fare-memoization))

(in-package :allrgb)

;;; RGB

(defstruct (rgb (:type list) (:conc-name nil))
  r g b)

(defun dist (p q)
  (let ((dr (- (r p) (r q)))
	(dg (- (g p) (g q)))
	(db (- (b p) (b q))))
    (+ (* dr dr) (* dg dg) (* db db))))


(defun rgb-triplets (max-intensity levels)
  "Generate a vector of all rgb triplets, with
  specified number of intensity levels from zero to max-intensity."
  (let ((step (floor max-intensity levels))
	(triplets (make-array (expt levels 3)))
	(i 0))
    (loop for r below levels
       do (loop for g below levels
	     do (loop for b below levels
		   do (progn (setf (aref triplets i)
				   (make-rgb :r (* r step)
					     :g (* g step)
					     :b (* b step)))
			     (incf i)))))
    triplets))

(defun shuffle-vector (vector)
  "Randomly permutes a vector, in place."
  (let ((len (length vector)))
    (dotimes (i (length vector) vector)
      ;; [0,i) is shuffled. Pick a random element from [i,len) and swap with i.
      (swap vector i (random-from i len)))))

(defun swap (vector i j)
  "Swap elements i and j of the vector."
  (let ((tmp (aref vector i)))
    (setf (aref vector i) (aref vector j))
    (setf (aref vector j) tmp)))

(defun random-from (i j)
  "Choose a random integer from the interval [i,j)."
  (+ i (random (- j i))))

;;; Image

(defun make-empty-image (width height)
  (make-array (list width height) :initial-element nil))

(defun image-rgb (img x y)
  "Get the rgb value of an image at a given pixel."
  (aref img x y))

(defsetf image-rgb (img x y) (rgb)
  `(setf (aref ,img ,x ,y) ,rgb))


(defparameter *width* 256
  "The width, in pixels, of the image.")
(defparameter *height* 128
  "The height, in pixels, of the image.")

(defun out-of-bounds (x y)
  "Check whether the point is outside of the image."
  (or (>= x *width*)
      (<  x 0)
      (>= y *height*)
      (< y 0)))

(defun neighbors (x y)
  "Return the locations of neighboring pixels. This generally yields 
  an 8-pixel neighborhood, except at image boundaries. "
  (let ((candidates
	 (loop for i in '(-1 0 1)
	    append (loop for j in '(-1 0 1)
		      collect (cons (+ x i)
				    (+ y j)))
	    into result-list
	    finally (return result-list))))
    (remove-if #'(lambda (pt)
		   (let ((x1 (car pt))
			 (y1 (cdr pt)))
		     (or (and (eq x x1) (eq y y1))
			 (out-of-bounds x1 y1))))
	       candidates)))

(memoize 'neighbors)

;;; Pixel search / placement

(defvar *frontier* nil
  "The candidate pixels to assign a given rgb value.")

(defparameter *agg-fn* #'min
  "The function used to aggregate the quality of fit amongst all neighbors.")

(defun rgb-pixel-fit (img rgb x y)
  "Measure the quality of an rgb value at a given pixel."
  (apply *agg-fn* (mapcar #'(lambda (pt)
			    (dist rgb (image-rgb img (car pt) (cdr pt))))
			  (remove-if-not #'(lambda (pt)
					     (visited img (car pt) (cdr pt)))
					(neighbors x y)))))

(defun minimum (list key)
  "Get the minimum element of list, according to the specified key."
  (when list
    (let* ((m0 (first list))
	   (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
	      (when (< e1 m1)
		(psetf m0 e0 m1 e1)))
	    list)
      m0)))


(defun best-pixel (img rgb)
  "Finds the best location from the frontier to place the rgb value."
  (minimum *frontier*
	   #'(lambda (pt)
	       (rgb-pixel-fit img rgb (car pt) (cdr pt)))))

(defun visited (img x y)
  (aref img x y))

(defun visit (x y)
  ;; does nothing right now...
  (cons x y))

(defun assign-color (img x y rgb)
  "Assign rgb value to the image at a specified pixel."
  (setf (aref img x y) rgb)
  (visit x y)
  (setf *frontier*
	(remove-if #'(lambda (pt) (visited img (car pt) (cdr pt)))
		   (union (neighbors x y)
			  *frontier*))))

;;; Interface

(defun make-image (width height levels)
  "Returns an image, with the specified size and levels of
  RGB intensity, such that each RGB color is assigned to exactly
  one pixel."
  (assert (= (* width height)
	     (* levels levels levels)))
  (let ((*width* width)
	(*height* height)
	(*frontier* nil)
	(midx (floor width 2))
	(midy (floor height 2))
	(colors (shuffle-vector (rgb-triplets 256 levels)))
	(img (make-empty-image width height)))
    (assign-color img midx midy (aref colors 0))
    (loop for i from 1 below (length colors)
       for rgb = (aref colors i)
       for pt = (best-pixel img rgb)
       do (assign-color img (car pt) (cdr pt) rgb))
    img))


(defun draw-png (img width height filename)
  "Render the image to a png file at the specified filename."
  (let* ((png (make-instance 'png :color-type :truecolor
			     :width width
			     :height height))
	 (data (data-array png)))
    (dotimes (i width (write-png png filename))
      (dotimes (j height)
	(let ((rgb (aref img i j)))
	  (setf (aref data j i 0) (r rgb))
	  (setf (aref data j i 1) (g rgb))
	  (setf (aref data j i 2) (b rgb)))))))



(defun useful-pairs (max)
  "Finds all pairs (x,y) where x^2 = y^3 and x,y <= max."
  (loop for x from 1 to max
     append (loop for y from 1 to max
	       if (= (* x x) (* y y y))
	       collect (list x y))
     into pairs
     finally (return pairs)))

