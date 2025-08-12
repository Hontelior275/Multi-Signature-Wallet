;; Multi-Signature Wallet in Clarity
;; Only two functions: propose-transaction and approve-transaction

;; --------------------------
;; Data structures
;; --------------------------
(define-data-var owners (list 3 principal) (list))  ;; list of wallet owners (max 3)
(define-data-var required-approvals uint u2)        ;; number of approvals required
(define-data-var tx-counter uint u0)                ;; transaction counter

(define-map transactions uint
  {
    to: principal,
    amount: uint,
    approvals: (list 3 principal),
    executed: bool
  }
)

;; --------------------------
;; Initialization function (call once to set owners)
;; --------------------------
(define-public (initialize (owner-list (list 3 principal)))
  (begin
    (asserts! (is-eq (len (var-get owners)) u0) (err u999)) ;; Only initialize once
    (var-set owners owner-list)
    (ok true)
  )
)

;; --------------------------
;; Errors
;; --------------------------
(define-constant err-not-owner (err u100))
(define-constant err-tx-not-found (err u101))
(define-constant err-tx-executed (err u102))
(define-constant err-already-approved (err u103))

;; --------------------------
;; Helper functions
;; --------------------------
(define-private (list-contains (lst (list 3 principal)) (item principal))
  (is-some (index-of lst item))
)

;; --------------------------
;; Public functions
;; --------------------------

;; Propose a transaction
(define-public (propose-transaction (to principal) (amount uint))
  (begin
    (asserts! (list-contains (var-get owners) tx-sender) err-not-owner)
    (var-set tx-counter (+ (var-get tx-counter) u1))
    (map-set transactions (var-get tx-counter)
      {
        to: to,
        amount: amount,
        approvals: (list tx-sender),
        executed: false
      }
    )
    (ok (var-get tx-counter))
  )
)

;; Approve a transaction and execute if enough approvals
(define-public (approve-transaction (tx-id uint))
  (let ((transaction (map-get? transactions tx-id)))
    (match transaction
      tx
      (begin
        (asserts! (list-contains (var-get owners) tx-sender) err-not-owner)
        (asserts! (not (get executed tx)) err-tx-executed)
        (asserts! (not (list-contains (get approvals tx) tx-sender)) err-already-approved)

        ;; Add approval - use unwrap-panic since we know the list isn't full
        (let ((new-approvals (unwrap-panic (as-max-len? (append (get approvals tx) tx-sender) u3))))
          (map-set transactions tx-id
            (merge tx { approvals: new-approvals })
          )

          ;; Check approvals after adding
          (let ((updated-tx (unwrap-panic (map-get? transactions tx-id))))
            (if (>= (len (get approvals updated-tx)) (var-get required-approvals))
                (begin
                  (try! (stx-transfer? (get amount updated-tx) (as-contract tx-sender) (get to updated-tx)))
                  (map-set transactions tx-id (merge updated-tx { executed: true }))
                  (ok true)
                )
                (ok false)
            )
          )
        )
      )
      err-tx-not-found
    )
  )
)

;; --------------------------
;; Read-only functions for querying state
;; --------------------------

(define-read-only (get-owners)
  (var-get owners)
)

(define-read-only (get-required-approvals)
  (var-get required-approvals)
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

(define-read-only (get-tx-counter)
  (var-get tx-counter)
)