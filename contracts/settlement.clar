;; Settlement Contract
;; Handles automated inter-carrier payments

(define-data-var admin principal tx-sender)

;; Structure for settlement records
(define-map settlement-records
  { settlement-id: uint }
  { payer: principal,
    payee: principal,
    amount: uint,
    description: (string-ascii 100),
    usage-record-id: (optional uint),
    timestamp: uint,
    status: (string-ascii 10) })  ;; "pending", "paid", "disputed"

;; Counter for settlement IDs
(define-data-var settlement-id-counter uint u1)

;; Error codes
(define-constant err-not-admin (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-settlement-not-found (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin)))

;; Create a new settlement record
(define-public (create-settlement
  (payee principal)
  (amount uint)
  (description (string-ascii 100))
  (usage-record-id (optional uint)))
  (let ((settlement-id (var-get settlement-id-counter)))
    (var-set settlement-id-counter (+ settlement-id u1))
    (map-set settlement-records
      { settlement-id: settlement-id }
      { payer: tx-sender,
        payee: payee,
        amount: amount,
        description: description,
        usage-record-id: usage-record-id,
        timestamp: block-height,
        status: "pending" })
    (ok settlement-id)))

;; Process payment for a settlement
;; Note: In a real implementation, this would interact with a token contract
(define-public (process-payment (settlement-id uint))
  (let ((settlement-data (map-get? settlement-records { settlement-id: settlement-id })))
    (asserts! (is-some settlement-data) err-settlement-not-found)
    (let ((settlement (unwrap-panic settlement-data)))
      (asserts! (is-eq tx-sender (get payer settlement)) err-unauthorized)
      (asserts! (is-eq (get status settlement) "pending") err-invalid-status)

      ;; In a real implementation, transfer tokens here
      ;; For now, just update the status
      (map-set settlement-records
        { settlement-id: settlement-id }
        (merge settlement { status: "paid" }))
      (ok true))))

;; Dispute a settlement
(define-public (dispute-settlement (settlement-id uint))
  (let ((settlement-data (map-get? settlement-records { settlement-id: settlement-id })))
    (asserts! (is-some settlement-data) err-settlement-not-found)
    (let ((settlement (unwrap-panic settlement-data)))
      (asserts! (or (is-eq tx-sender (get payer settlement))
                   (is-eq tx-sender (get payee settlement)))
               err-unauthorized)
      (asserts! (is-eq (get status settlement) "pending") err-invalid-status)

      (map-set settlement-records
        { settlement-id: settlement-id }
        (merge settlement { status: "disputed" }))
      (ok true))))

;; Resolve a disputed settlement (admin only)
(define-public (resolve-settlement (settlement-id uint) (new-status (string-ascii 10)))
  (let ((settlement-data (map-get? settlement-records { settlement-id: settlement-id })))
    (asserts! (is-admin) err-not-admin)
    (asserts! (is-some settlement-data) err-settlement-not-found)
    (asserts! (or (is-eq new-status "pending")
                 (is-eq new-status "paid")
                 (is-eq new-status "disputed"))
             err-invalid-status)

    (let ((settlement (unwrap-panic settlement-data)))
      (map-set settlement-records
        { settlement-id: settlement-id }
        (merge settlement { status: new-status }))
      (ok true))))

;; Get settlement details
(define-read-only (get-settlement-details (settlement-id uint))
  (map-get? settlement-records { settlement-id: settlement-id }))

;; Get all settlements for a payer
(define-read-only (get-payer-settlements (payer principal))
  (let ((settlements (list)))
    ;; Note: In actual implementation, this would require off-chain indexing
    ;; or a more complex on-chain mechanism to efficiently retrieve filtered records
    ;; This is a simplified placeholder
    settlements))

;; Get all settlements for a payee
(define-read-only (get-payee-settlements (payee principal))
  (let ((settlements (list)))
    ;; Note: In actual implementation, this would require off-chain indexing
    ;; or a more complex on-chain mechanism to efficiently retrieve filtered records
    ;; This is a simplified placeholder
    settlements))
