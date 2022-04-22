(ql:quickload :fiveam)
(ql:quickload :postmodern)
(ql:quickload :str)
(ql:quickload :yason)

(setq postmodern:*escape-sql-names-p* nil)

(defvar *postgres-connection-string*
  '("tezos" "tezos" "tezos" "localhost" :port 5432))

;;;;----------------------------------------------------------------------------
;;;; Uniswap v1 class
;;;; https://github.com/Smartlinkhub/DEX/blob/master/dex.mligo

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
	  "token: ~S mutez: ~S k: ~S normalised-token-per-tez ~7$ tez-per-normalised-token ~7$ cost: ~S
"	  (token u)
	  (mutez u)
	  (k u)
	  (normalised-token-per-tez u)
	  (tez-per-normalised-token u)
	  (cost u)))

(defmethod token-per-mutez ((u uniswapv1))
  (/ (token u) (mutez u)))

(defmethod normalised-token-per-tez ((u uniswapv1))
  (/ (token-per-mutez u) (expt 10 (- (token-zeroes u) (mutez-zeroes u)))))

(defmethod mutez-per-token ((u uniswapv1))
  (/ (mutez u)  (token u)))

(defmethod tez-per-normalised-token ((u uniswapv1))
  (/ (mutez-per-token u) (expt 10 (- (mutez-zeroes u) (token-zeroes u)))))

(defun swap (x y k swapped-x)
  ;;            let bought = natural_to_mutez (((tokensSold * 9972n * (mutez_to_natural storage.xtzPool)) / (storage.tokenPool * 10000n + (tokensSold * 9972n)))) in

  (format t "~$ ~$ ~$ ~$
" x y k swapped-x)
  (*
  (- y (/ k (+ x swapped-x))))


;;;; simulate a swap of token for mutez, and return a list of
;;; (mutez-returned new-swap)
(defmethod swap-token ((u uniswapv1) swapped-token )
  (format t "CoSt ~S
" (cost u))
  (let* ((token (token u))
	 (mutez (mutez u))
	 (k (k u))
	 (mutez-swapped (swap token mutez k swapped-token))
	 (mutez-out (* mutez-swapped) (cost u))
	 (new-mutez (- mutez mutez-swapped))
	 (new-token (+ token swapped-token)))
    `(
      ,mutez-out
      ,(init (make-instance 'uniswapv1
			    :token new-token
			    :mutez new-mutez
			    :cost (cost u)
			    :token-zeroes (token-zeroes u))))))

(defmethod swap-mutez ((a uniswapv1) swapped-mutez)
  (let* ((token (token a))
	 (mutez (mutez a))
	 (k (k a))
	 (token-out (* (swap mutez token k swapped-mutez)) (cost u))
	 (new-mutez (+ mutez swapped-mutez))
	 (new-token (- token token-out)))
    `(
      ,token-out
      ,(init (make-instance 'uniswapv1
			    :token new-token
			    mutez new-mutez :k k
			    :cost (cost u)
			    :token-zeroes (token-zeroes a))))))

;;;;----------------------------------------------------------------------------
;;;; Classes for loading swaps from disk

(defclass ondisk ()
  ((schema-name :initarg :schema-name :accessor schema-name)
   (table-name :initarg :table-name :accessor table-name)
   (token-col :initarg :token-col :accessor token-col)
   (mutez-col :initarg :mutez-col :accessor mutez-col)
   (cost :initarg :cost :accessor cost)
   (token-zeroes :initarg :token-zeroes :accessor token-zeroes)
   (mutez-zeroes :initarg :mutez-zeroes :initform 6 :accessor mutez-zeroes)
   (quermutez :initform nil :accessor query)
   (swap-class :initarg :swap-class :accessor swap-class)))

(defmethod init-ondisk ((o ondisk))
  (setf (query o) (postmodern:prepare
		      (str:concat
		       "SELECT \"" (token-col o) "\", \"" (mutez-col o) "\" FROM "
		       "\"" (schema-name o) "\".\"" (table-name o) "\""
		       "ORDER BY ID DESC LIMIT 1") :list))
  o)

;;;; load the latest from the disk, and return an initialized swap object
(defmethod get-latest-swap ((o ondisk))
  (let* ((result (funcall (query o)))
	 (token-val (nth 0 result))
	 (mutez-val (nth 1 result)))
    (init (make-instance (swap-class o)
			 :token token-val
			 :mutez mutez-val
			 :cost (cost o)
			 :token-zeroes (token-zeroes o)))))

;;;;----------------------------------------------------------------------------

;;;; Tests!

(fiveam:def-suite my-system
  :description "Test everything")

(fiveam:def-suite test-swaps
  :description "."
  :in my-system)

(fiveam:in-suite test-swaps)

(fiveam:test test-swap1
  (let*
      ((swap (init (make-instance 'uniswapv1
			    :token 46373645851255859
			    :mutez 15032325881
			    :cost (/ 9972 10000)
			    :token-zeroes 6)))
       (result (swap-token swap 137777989641182))
       (xtz-back (car result))
       (new-swap (car (cdr result))))
    (p swap)
    (fiveam:is (= (floor xtz-back) 44405039))
    (fiveam:is (= (floor (mutez new-swap)) 14987907444))
    (fiveam:is (= (floor (token new-swap)) 46511423840897041))))

(fiveam:run! 'my-system)

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
			       :cost 9972/10000 :token-zeroes 18
			       :swap-class 'uniswapv1))
  (p (setf swap
	   (postmodern:with-connection '("amm" "amm" "amm" "127.0.0.1"
					 :port 5433 :pooled-p t)
	(let* ((od (init-ondisk vortex-kUSD-XTZ))
	       (swap (get-latest-swap od)))
	  swap))))
  (format t "Vortex USDtz-XTZ
")
  (setf vortex-USDtz-XTZ
	(make-instance 'ondisk :schema-name "Vortex USDtz-XTZ DEX"
			       :table-name "storage"
			       :token-col "tokenPool" :mutez-col "xtzPool"
			       :cost 9972/10000 :token-zeroes 6
			       :swap-class 'uniswapv1))
  (p (setf swap
	   (postmodern:with-connection '("amm" "amm" "amm" "127.0.0.1"
					 :port 5433 :pooled-p t)
	(let* ((od (init-ondisk vortex-USDtz-XTZ))
	       (swap (get-latest-swap od)))
	  swap)))))

(format t "~7$
" (car (swap-token swap 100)))
