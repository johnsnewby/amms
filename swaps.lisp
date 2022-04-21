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

;;;;----------------------------------------------------------------------------
;;;; Classes for loading swaps from disk

(defclass ondisk ()
  ((schema-name :initarg :schema-name :accessor schema-name)
   (table-name :initarg :table-name :accessor table-name)
   (x-col :initarg :x-col :accessor x-col)
   (y-col :initarg :y-col :accessor y-col)
   (cost :initarg :cost :accessor cost)
   (zeroes :initarg :zeroes :accessor zeroes)
   (query :initform nil :accessor query)))

(defmethod init ((o ondisk))
  (if (query o)
      ()
      (setf (query o) (postmodern:prepare
			  (str:concat
			   "SELECT \"" (x-col o) "\", \"" (y-col o) "\" FROM "
			   "\"" (schema-name o) "\".\"" (table-name o) "\""
			   "ORDER BY ID DESC LIMIT 1")))))

(defmethod x-in-y ((o ondisk))
  (/ y x))

(defmethod y-in-x ((o ondisk))
  (/ x y))

(defmethod ld ((o ondisk)) ; load from the disk, and return an initialized swap object
  (let* ((result (nth 0 (funcall (query o))))
	 (x-col (nth 0 result))
	 (y-col (nth 1 result)))
    (setf (slot-value o 'x-col) x-col)
    (setf (slot-value o 'y-col) y-col))
  o)

;;;;----------------------------------------------------------------------------

(setf vortex-kUSD-XTX
      (make-instance 'ondisk :schema-name "Vortex kUSD-XTZ DEX"
			     :table-name "storage"
			     :x-col "tokenPool" :y-col "xtzPool"
			     :cost 0.0025 :zeroes 10))
(init vortex-kUSD-XTX)

(setf vortex-USDtz-XTZ
      (make-instance 'ondisk :schema-name "Vortex USDtz-XTZ DEX"
			     :table-name "storage"
			     :x-col "tokenPool" :y-col "xtzPool"
			     :cost 0.0025 :zeroes 10))
(init vortex-USDtz-XTZ)
(setq _vortex-USDtz-XTZ (ld vortex-USDtz-XTZ))
(x-in-y _vortex-USDtz-XTZ)


(setq x (ld vortex-kUSD-XTX))

;(postmodern:with-schema ("\"PLENTY\"" :if-not-exist :error)
  (postmodern:query "SELECT * from \"Vortex kUSD-XTZ DEX\".storage LIMIT 1")
(query vortex-kUSD-XTX)
(concatenate "foo" "bar")

(setf a-uniswapv1 (init-uniswapv1 17987987987987987987 980989080980980809 0.05))
(p a-uniswapv1)
(p (swap-x 5000000 a-uniswapv1))
