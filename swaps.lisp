(ql:quickload :alexandria)
(ql:quickload :postmodern)
(ql:quickload :yason)

(postmodern:connect-toplevel "tezos" "tezos" "tezos" "localhost" :port 5432)
(setf result (nth 0
		  (postmodern:query "select * from peppermint.operations limit 1")))

(defun get_address (command)
   (gethash "to_address" (gethash "args" (yason:parse command))))

(setf target (gethash "to_address" (gethash "args" (yason:parse
						    (nth 4 result)))))


(postmodern:doquery (:select 'command :from 'peppermint.operations) (command)
  (format t "target: ~A\n" (get_address command)))

(defclass uniswapv1 ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor x)
   (k :initarg :k :accessor k)
   (cost :initarg :cost :accessor cost)))

(defun init-uniswapv1 (x y cost)
  (make-instance 'uniswapv1 :x x :y y :k (* x y) :cost cost))

(defun init-uniswapv1 (x y cost)
  (let ((a (make-instance 'uniswapv1)))
    (setf (slot-value a 'x) x)
    (setf (slot-value a 'y) y)
    (setf (slot-value a 'k) (* x y))
    (setf (slot-value a 'cost) cost)
    a))

(defmethod p ((a uniswapv1))
  (format t "x: ~S y: ~S k: ~S cost: ~S\n"
	  (slot-value a 'x)
	  (slot-value a 'y)
	  (slot-value a 'k)
	  (slot-value a 'cost)))

(defun swap (x y k swapped-x)
  (- y (/ k (+ x swapped-x))))

(defmethod swap-x (swapped-x (a uniswapv1))
  (let* ((x (slot-value a 'x))
	(y (slot-value a 'y))
	(k (slot-value a 'k))
	(new-x (- x swapped-x))
	(new-y (swap x y k swapped-x)))
    (make-instance 'uniswapv1 :x new-x :y new-y :k k :cost (slot-value a 'cost))))

(defmethod swap-y (swapped-y (a uniswapv1))
  (let* ((x (slot-value a 'x))
	(y (slot-value a 'y))
	(k (slot-value a 'k))
	(new-x (swap y x k swapped-y))
	(new-y (- y swapped-y)))
    (make-instance 'uniswapv1 :x new-x :y new-y :k k :cost (slot-value a 'cost))))



(setf a-uniswapv1 (init-uniswapv1 17987987987987987987 980989080980980809 0.05))
(p a-uniswapv1)
(p (swap-x 5000000 a-uniswapv1))
