;; amm-pair.clar
;; Simple AMM pair (constant product) for two SIP-010 tokens

(define-trait token-trait
    ((transfer? (uint principal principal (optional (buff 34))) (response bool uint))))

(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_ALREADY_INITIALIZED u101)
(define-constant ERR_NOT_INITIALIZED u102)
(define-constant ERR_ZERO_AMOUNT u103)
(define-constant ERR_TRANSFER_FAIL u104)
(define-constant ERR_INSUFFICIENT_LIQUIDITY u105)
(define-constant ERR_INVALID_RATIO u106)
(define-constant ERR_INSUFFICIENT_BALANCE u107)
(define-constant ERR_BAD_AMOUNT u108)

(define-constant FEE_DENOM u10000)

(define-data-var admin principal tx-sender)
(define-data-var is-initialized bool false)
(define-data-var reserve-x uint u0)
(define-data-var reserve-y uint u0)
(define-data-var swap-fee-bps uint u30)
(define-data-var total-shares uint u0)

(define-map pair-tokens
  { token_type: (string-ascii 1) }  ;; "x" or "y"
  { token_principal: principal })

(define-map lp-balances { who: principal } { shares: uint })

;; Events
(define-private (ev-initialize (x principal) (y principal))
  (print { event: "pair-initialized", token_x: x, token_y: y }))

(define-private (ev-add-liquidity (who principal) (amount-x uint) (amount-y uint) (shares uint))
  (print { event: "add-liquidity", who: who, amount_x: amount-x, amount_y: amount-y, minted_shares: shares }))

(define-private (ev-remove-liquidity (who principal) (amount-x uint) (amount-y uint) (shares uint))
  (print { event: "remove-liquidity", who: who, amount_x: amount-x, amount_y: amount-y, burned_shares: shares }))

(define-private (ev-swap (who principal) (token-in principal) (amount-in uint) (token-out principal) (amount-out uint) (fee uint))
  (print { event: "swap", who: who, token_in: token-in, amount_in: amount-in, token_out: token-out, amount_out: amount-out, fee: fee }))

;; Helpers
(define-private (lp-balance-of (who principal))
  (default-to u0 (get shares (map-get? lp-balances { who: who }))))

(define-private (set-lp-balance (who principal) (amt uint))
  (map-set lp-balances { who: who } { shares: amt }))

(define-private (min (a uint) (b uint))
  (if (< a b) a b))

(define-private (sqrt (x uint))
  (if (is-eq x u0)
      u0
      (let ((guess (/ (+ x u1) u2)))
        (if (> (* guess guess) x)
            (/ x guess)  
            guess))))

;; Helper to get token principal
(define-private (get-token-principal (token-type (string-ascii 1)))
  (match (map-get? pair-tokens { token_type: token-type })
    token-entry (get token_principal token-entry)
    tx-sender))

;; Initialize pair
(define-public (initialize-pair (tokenX <token-trait>) (tokenY <token-trait>) (fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (not (var-get is-initialized)) (err ERR_ALREADY_INITIALIZED))
    (asserts! (> fee-bps u0) (err ERR_BAD_AMOUNT))

    ;; First check that both contracts are valid
    (match (some (contract-of tokenX))
      token-x (match (some (contract-of tokenY))
                token-y (begin 
                         (map-set pair-tokens { token_type: "x" } { token_principal: token-x })
                         (map-set pair-tokens { token_type: "y" } { token_principal: token-y })
                         (var-set swap-fee-bps fee-bps)
                         (var-set is-initialized true)
                         (ev-initialize token-x token-y)
                         (ok true))
                (err ERR_NOT_INITIALIZED))
      (err ERR_NOT_INITIALIZED))))

;; Add liquidity
(define-public (add-liquidity (token-x-contract <token-trait>) (token-y-contract <token-trait>) (amount-x uint) (amount-y uint))
  (begin
    (asserts! (var-get is-initialized) (err ERR_NOT_INITIALIZED))
    (asserts! (> amount-x u0) (err ERR_ZERO_AMOUNT))
    (asserts! (> amount-y u0) (err ERR_ZERO_AMOUNT))
    (asserts! (and (is-eq (contract-of token-x-contract) (get-token-principal "x"))
                   (is-eq (contract-of token-y-contract) (get-token-principal "y")))
              (err ERR_BAD_AMOUNT))
    
    (let ((rx (var-get reserve-x))
          (ry (var-get reserve-y))
          (total (var-get total-shares)))
      
      ;; pull tokens
      (try! (contract-call? token-x-contract transfer? amount-x tx-sender (as-contract tx-sender) none))
      (try! (contract-call? token-y-contract transfer? amount-y tx-sender (as-contract tx-sender) none))
      
      (let ((shares (if (is-eq total u0)
                     (sqrt (* amount-x amount-y))
                     (min (/ (* amount-x total) rx) (/ (* amount-y total) ry)))))
        (begin
          (asserts! (> shares u0) (err ERR_INVALID_RATIO))
          (set-lp-balance tx-sender (+ (lp-balance-of tx-sender) shares))
          (var-set total-shares (+ total shares))
          (var-set reserve-x (+ rx amount-x))
          (var-set reserve-y (+ ry amount-y))
          (ev-add-liquidity tx-sender amount-x amount-y shares)
          (ok shares))))))

;; Remove liquidity
(define-public (remove-liquidity (token-x-contract <token-trait>) (token-y-contract <token-trait>) (shares uint))
  (let ((rx (var-get reserve-x))
        (ry (var-get reserve-y))
        (total (var-get total-shares))
        (user-shares (lp-balance-of tx-sender)))
    (begin
      (asserts! (> shares u0) (err ERR_ZERO_AMOUNT))
      (asserts! (var-get is-initialized) (err ERR_NOT_INITIALIZED))
      (asserts! (>= total shares) (err ERR_INSUFFICIENT_LIQUIDITY))
      (asserts! (>= user-shares shares) (err ERR_INSUFFICIENT_BALANCE))
      (asserts! (and (is-eq (contract-of token-x-contract) (get-token-principal "x"))
                     (is-eq (contract-of token-y-contract) (get-token-principal "y")))
                (err ERR_BAD_AMOUNT))
      
      (let ((amount-x (/ (* shares rx) total))
            (amount-y (/ (* shares ry) total)))
        (begin
          (asserts! (> amount-x u0) (err ERR_INVALID_RATIO))
          (asserts! (> amount-y u0) (err ERR_INVALID_RATIO))
          
          (set-lp-balance tx-sender (- user-shares shares))
          (var-set total-shares (- total shares))
          (var-set reserve-x (- rx amount-x))
          (var-set reserve-y (- ry amount-y))
          
          (try! (contract-call? token-x-contract transfer? amount-x (as-contract tx-sender) tx-sender none))
          (try! (contract-call? token-y-contract transfer? amount-y (as-contract tx-sender) tx-sender none))
          
          (ev-remove-liquidity tx-sender amount-x amount-y shares)
          (ok { amount_x: amount-x, amount_y: amount-y }))))))

;; Get amount out
(define-read-only (get-amount-out (amount-in uint) (reserve-in uint) (reserve-out uint))
  (begin
    (asserts! (> amount-in u0) (err ERR_BAD_AMOUNT))
    (asserts! (>= reserve-in u0) (err ERR_BAD_AMOUNT))
    (asserts! (>= reserve-out u0) (err ERR_BAD_AMOUNT))
    
    (let 
      ((fee (var-get swap-fee-bps))
       (amount-in-with-fee (/ (* amount-in (- FEE_DENOM fee)) FEE_DENOM)))
      (if (<= amount-in-with-fee u0)
          (ok u0)
          (ok (/ (* amount-in-with-fee reserve-out) (+ reserve-in amount-in-with-fee)))))))

;; Swap exact input
(define-public (swap-exact-in (token-in <token-trait>) (token-out <token-trait>) (amount-in uint) (to principal))
  (let ((x (get-token-principal "x"))
        (y (get-token-principal "y")))
    (begin
      (asserts! (var-get is-initialized) (err ERR_NOT_INITIALIZED))
      (asserts! (> amount-in u0) (err ERR_ZERO_AMOUNT))
      (asserts! (or (and (is-eq (contract-of token-in) x) 
                        (is-eq (contract-of token-out) y))
                    (and (is-eq (contract-of token-in) y) 
                        (is-eq (contract-of token-out) x)))
                (err ERR_BAD_AMOUNT))
      
      (let ((rx (var-get reserve-x))
            (ry (var-get reserve-y))
            (isX (is-eq (contract-of token-in) x))
            (reserve-in (if isX rx ry))
            (reserve-out (if isX ry rx)))
        
        (try! (contract-call? token-in transfer? amount-in tx-sender (as-contract tx-sender) none))
        
        (match (get-amount-out amount-in reserve-in reserve-out)
          amount-out
            (begin
              (asserts! (> amount-out u0) (err ERR_INSUFFICIENT_LIQUIDITY))
              (if isX
                  (begin
                    (var-set reserve-x (+ rx amount-in))
                    (var-set reserve-y (- ry amount-out))
                    (try! (contract-call? token-out transfer? amount-out (as-contract tx-sender) to none))
                    (ev-swap tx-sender (contract-of token-in) amount-in (contract-of token-out) amount-out (/ (* amount-in (var-get swap-fee-bps)) FEE_DENOM))
                    (ok { amount_out: amount-out }))
                  (begin
                    (var-set reserve-y (+ ry amount-in))
                    (var-set reserve-x (- rx amount-out))
                    (try! (contract-call? token-out transfer? amount-out (as-contract tx-sender) to none))
                    (ev-swap tx-sender (contract-of token-in) amount-in (contract-of token-out) amount-out (/ (* amount-in (var-get swap-fee-bps)) FEE_DENOM))
                    (ok { amount_out: amount-out }))))
          error 
            (err ERR_BAD_AMOUNT))))))

;; Admin: set fee
(define-public (set-fee (bps uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err ERR_NOT_ADMIN))
    (asserts! (< bps FEE_DENOM) (err ERR_BAD_AMOUNT))
    (var-set swap-fee-bps bps)
    (ok bps)))

;; Read-only getters
(define-read-only (get-reserves)
  (ok { reserve_x: (var-get reserve-x), reserve_y: (var-get reserve-y) }))

(define-read-only (get-pair-tokens)
  (ok { token_x: (get-token-principal "x"), token_y: (get-token-principal "y") }))

(define-read-only (get-total-shares)
  (ok (var-get total-shares)))

(define-read-only (get-lp-balance (who principal))
  (ok (lp-balance-of who)))

(define-read-only (get-fee-bps)
  (ok (var-get swap-fee-bps)))