(ql:quickload :postmodern)
(ql:quickload :str)
(ql:quickload :yason)


;;;; (postmodern:connect-toplevel "tezos" "tezos" "tezos" "localhost" :port 5432)
(postmodern:connect-toplevel "amm" "amm" "amm" "localhost" :port 5433)
(setq postmodern:*escape-sql-names-p* nil)


;;;;----------------------------------------------------------------------------
;;;; Uniswap v1 class

(defclass uniswapv1 ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (k :initform nil :accessor k)
   (zeroes :initarg :zeroes :accessor zeroes)
   (cost :initarg :cost :accessor cost)))

(defmethod init ((u uniswapv1))
  (setf (k u) (* (x u) (y u)))
  u)

(defmethod p ((a uniswapv1))
  (format t "x: ~S y: ~S k: ~S x-in-y ~$ y-in-x ~$ cost: ~S\n"
	  (x a)
	  (y a)
	  (k a)
	  (x-in-y a)
	  (y-in-x a)
	  (cost a)))

(defmethod x-in-y ((u uniswapv1))
  (/ (/ (slot-value u 'x) (slot-value u 'y)) (expt 10 (zeroes u))))

(defmethod y-in-x ((u uniswapv1))
  (/ (/ (slot-value u 'y) (slot-value u 'x)) (expt 10 (zeroes u))))

(defun swap (x y k swapped-x)
  (- y (/ k (+ x swapped-x))))

(defmethod swap-x (swapped-x (a uniswapv1))
  (let* ((x (slot-value a 'x))
	(y (slot-value a 'y))
	(k (slot-value a 'k))
	(new-x (- x swapped-x))
	(new-y (swap x y k swapped-x)))
    (make-instance 'uniswapv1 :x new-x :y new-y :k k :cost (slot-value a 'cost)
		   :zeroes (zeroes a))))

(defmethod swap-y (swapped-y (a uniswapv1))
  (let* ((x (slot-value a 'x))
	(y (slot-value a 'y))
	(k (slot-value a 'k))
	(new-x (swap y x k swapped-y))
	(new-y (- y swapped-y)))
    (make-instance 'uniswapv1 :x new-x :y new-y :k k :cost (slot-value a 'cost)
		   :zeroes (zeroes a))))

;;;;----------------------------------------------------------------------------
;;;; Classes for loading swaps from disk

(defclass ondisk ()
  ((schema-name :initarg :schema-name :accessor schema-name)
   (table-name :initarg :table-name :accessor table-name)
   (x-col :initarg :x-col :accessor x-col)
   (y-col :initarg :y-col :accessor y-col)
   (cost :initarg :cost :accessor cost)
   (zeroes :initarg :zeroes :accessor zeroes)
   (query :initform nil :accessor query)
   (swap-class :initarg :swap-class :accessor swap-class)))

(defmethod init-ondisk ((o ondisk))
  (setf (query o) (postmodern:prepare
		      (str:concat
		       "SELECT \"" (x-col o) "\", \"" (y-col o) "\" FROM "
		       "\"" (schema-name o) "\".\"" (table-name o) "\""
		       "ORDER BY ID DESC LIMIT 1")))
  o)

;;;; load the latest from the disk, and return an initialized swap object
(defmethod get-latest-swap ((o ondisk))
  (let* ((result (nth 0 (funcall (query o))))
	 (x-val (nth 0 result))
	 (y-val (nth 1 result)))
    (init (make-instance (swap-class o) :x x-val :y y-val :cost (cost o)
					:zeroes (zeroes o)))))

;;;;----------------------------------------------------------------------------

(setf vortex-kUSD-XTX
      (make-instance 'ondisk :schema-name "Vortex kUSD-XTZ DEX"
			     :table-name "storage"
			     :x-col "tokenPool" :y-col "xtzPool"
			     :cost 0.0025 :zeroes 10
			     :swap-class 'uniswapv1))

(setf swap
      (postmodern:with-connection '("amm" "amm" "amm" "127.0.0.1" :port 5433)
	(let* ((od (init-ondisk vortex-kUSD-XTX))
	       (swap (get-latest-swap od)))
	  swap)))

(p swap)
