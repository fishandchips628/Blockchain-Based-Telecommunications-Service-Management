;; Usage Tracking Contract
;; Monitors service consumption across networks

(define-data-var admin principal tx-sender)

;; Structure for tracking usage records
(define-map usage-records
  { record-id: uint }
  { provider: principal,
    consumer: principal,
    service-type: (string-ascii 20),
    quantity: uint,
    unit-price: uint,
    timestamp: uint,
    verified: bool })

;; Counter for record IDs
(define-data-var record-id-counter uint u1)

;; Error codes
(define-constant err-not-admin (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-record-not-found (err u102))
(define-constant err-already-verified (err u103))

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin)))

;; Record new usage
(define-public (record-usage
  (consumer principal)
  (service-type (string-ascii 20))
  (quantity uint)
  (unit-price uint))
  (let ((record-id (var-get record-id-counter)))
    (var-set record-id-counter (+ record-id u1))
    (map-set usage-records
      { record-id: record-id }
      { provider: tx-sender,
        consumer: consumer,
        service-type: service-type,
        quantity: quantity,
        unit-price: unit-price,
        timestamp: block-height,
        verified: false })
    (ok record-id)))

;; Verify a usage record (by consumer or admin)
(define-public (verify-usage (record-id uint))
  (let ((record-data (map-get? usage-records { record-id: record-id })))
    (asserts! (is-some record-data) err-record-not-found)
    (let ((record (unwrap-panic record-data)))
      (asserts! (or (is-eq tx-sender (get consumer record)) (is-admin)) err-unauthorized)
      (asserts! (not (get verified record)) err-already-verified)

      (map-set usage-records
        { record-id: record-id }
        (merge record { verified: true }))
      (ok true))))

;; Get usage record details
(define-read-only (get-usage-record (record-id uint))
  (map-get? usage-records { record-id: record-id }))

;; Calculate total usage cost
(define-read-only (calculate-usage-cost (record-id uint))
  (let ((record-data (map-get? usage-records { record-id: record-id })))
    (if (is-some record-data)
      (let ((record (unwrap-panic record-data)))
        (some (* (get quantity record) (get unit-price record))))
      none)))

;; Get all records for a provider-consumer pair
(define-read-only (get-provider-consumer-records (provider principal) (consumer principal) (limit uint))
  (let ((records (list)))
    ;; Note: In actual implementation, this would require off-chain indexing
    ;; or a more complex on-chain mechanism to efficiently retrieve filtered records
    ;; This is a simplified placeholder
    records))
