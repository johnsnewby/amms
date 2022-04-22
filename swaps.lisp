(ql:quickload :postmodern)
(ql:quickload :str)
(ql:quickload :yason)


;;;; (postmodern:connect-toplevel "tezos" "tezos" "tezos" "localhost" :port 5432)
(postmodern:connect-toplevel "amm" "amm" "amm" "localhost" :port 5433)
(setq postmodern:*escape-sql-names-p* nil)

(defvar *postgres-connection-string*
  '("tezos" "tezos" "tezos" "localhost" :port 5432))

;;;;----------------------------------------------------------------------------
;;;; Uniswap v1 class

(defclass uniswapv1 ()
  ((token :initarg :token :accessor token)
   (mutez :initarg :mutez :accessor mutez)
   (k :initform nil :accessor k)
   (token-zeroes :initarg :token-zeroes :accessor token-zeroes)
   (mutez-zeroes :initform 6 :accessor mutez-zeroes)
   (cost :initarg :cost :accessor cost)))

(defmethod init ((u uniswapv1))
  (setf (k u) (* (token u) (mutez u)))
  u)

(defmethod p ((u uniswapv1))
  (format t
	  "token: ~S mutez: ~S k: ~S token-per-tez ~7$ tez-per-token ~7$ cost: ~S
"
	  (token u)
	  (mutez u)
	  (k u)
	  (token-per-mutez u)
	  (tez-per-token u)
	  (cost u)))

(defmethod token-per-mutez ((u uniswapv1))
  (/ (token u) (mutez u)))

(defmethod token-per-tez ((u uniswapv1))
  (*
   (token-per-mutez u)
   (expt 10 (- (token-zeroes u) (mutez-zeroes u)))))

(defmethod mutez-per-token ((u uniswapv1))
  (/ (mutez u) (token u)))

(defmethod tez-per-token ((u uniswapv1))
  (* (expt 10 (mutez-zeroes u)) (mutez-per-token u)))

(defun swap (x y k swapped)
  (- y (/ k (+ x swapped))))


;;;; simulate a swap of token, and return a new uniswap object with the
;;;; results
(defmethod swap-token ((a uniswapv1) swapped-token )
  (let* ((token (slot-value a 'token))
	(mutez (slot-value a 'mutez))
	(k (slot-value a 'k))
	(new-token (- token swapped-token))
	(new-mutez (swap token mutez k swapped-token)))
    (init (make-instance 'uniswapv1 :token new-token mutez new-mutez :cost (cost a)
		   :token-zeroes (token-zeroes a)))))

(defmethod swap-mutez ((a uniswapv1) swapped-mutez)
  (let* ((token (slot-value a 'token))Z
	(mutez (slot-value a 'mutez))
	(k (slot-value a 'k))
	(new-token (swap mutez token k swapped-mutez))
	(new-mutez (- mutez swapped-mutez)))
    (make-instance 'uniswapv1 :token new-token mutez new-mutez :k k :cost (slot-value a 'cost)
		   :token-zeroes (token-zeroes a))))

;;;;----------------------------------------------------------------------------
;;;; Classes for loading swaps from disk

(defclass ondisk ()
  ((schema-name :initarg :schema-name :accessor schema-name)
   (table-name :initarg :table-name :accessor table-name)
   (token-col :initarg :token-col :accessor token-col)
   (mutez-col :initarg :mutez-col :accessor mutez-col)
   (cost :initarg :cost :accessor cost)
   (token-zeroes :initarg :token-zeroes :accessor token-zeroes)
   (quermutez :initform nil :accessor query)
   (swap-class :initarg :swap-class :accessor swap-class)))

(defmethod init-ondisk ((o ondisk))
  (setf (query o) (postmodern:prepare
		      (str:concat
		       "SELECT \"" (token-col o) "\", \"" (mutez-col o) "\" FROM "
		       "\"" (schema-name o) "\".\"" (table-name o) "\""
		       "ORDER BY ID DESC LIMIT 1")))
  o)

;;;; load the latest from the disk, and return an initialized swap object
(defmethod get-latest-swap ((o ondisk))
  (let* ((result (nth 0 (funcall (query o))))
	 (token-val (nth 0 result))
	 (mutez-val (nth 1 result)))
    (init (make-instance (swap-class o)
			 :token token-val
			 :mutez mutez-val
			 :cost (cost o)
			 :token-zeroes (token-zeroes o)))))

;;;;----------------------------------------------------------------------------

(progn
  (defvar vortex-kUSD-XTZ)
  (defvar vortex-USDtz-XTZ)
  (format t "
Vortex kUSD-XTZ
")
  (setf vortex-kUSD-XTZ
	(make-instance 'ondisk :schema-name "Vortex kUSD-XTZ DEX"
			       :table-name "storage"
			       :token-col "tokenPool" :mutez-col "xtzPool"
			       :cost 9972/10000 :token-zeroes 10
			       :swap-class 'uniswapv1))
  (p (setf swap
      (postmodern:with-connection '("amm" "amm" "amm" "127.0.0.1" :port 5433)
	(let* ((od (init-ondisk vortex-kUSD-XTZ))
	       (swap (get-latest-swap od)))
	  swap))))
  (format t "Vortex USDtz-XTZ
")
  (setf vortex-USDtz-XTZ
	(make-instance 'ondisk :schema-name "Vortex USDtz-XTZ DEX"
			       :table-name "storage"
			       :token-col "tokenPool" :mutez-col "xtzPool"
			       :cost 9972/10000 :token-zeroes 0
			       :swap-class 'uniswapv1))
  (p (setf swap
      (postmodern:with-connection '("amm" "amm" "amm" "127.0.0.1" :port 5433)
	(let* ((od (init-ondisk vortex-USDtz-XTZ))
	       (swap (get-latest-swap od)))
	  swap)))))

(swap (token swap) (mutez swap) (k swap) 100)
