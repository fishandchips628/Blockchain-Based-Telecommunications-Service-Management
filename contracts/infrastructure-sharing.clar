;; Infrastructure Sharing Contract
;; Manages joint use of network assets between carriers

(define-data-var admin principal tx-sender)

;; Structure for infrastructure assets
(define-map infrastructure-assets
  { asset-id: uint, owner: principal }
  { description: (string-ascii 100),
    location: (string-ascii 100),
    available: bool,
    price-per-day: uint })

;; Map to track asset sharing agreements
(define-map sharing-agreements
  { asset-id: uint, user: principal }
  { start-time: uint,
    end-time: uint,
    total-cost: uint,
    active: bool })

;; Counter for asset IDs
(define-data-var asset-id-counter uint u1)

;; Error codes
(define-constant err-not-admin (err u100))
(define-constant err-not-owner (err u101))
(define-constant err-asset-not-found (err u102))
(define-constant err-asset-not-available (err u103))
(define-constant err-agreement-exists (err u104))
(define-constant err-agreement-not-found (err u105))
(define-constant err-unauthorized (err u106))

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin)))

;; Register a new infrastructure asset
(define-public (register-asset (description (string-ascii 100)) (location (string-ascii 100)) (price-per-day uint))
  (let ((asset-id (var-get asset-id-counter)))
    (var-set asset-id-counter (+ asset-id u1))
    (map-set infrastructure-assets
      { asset-id: asset-id, owner: tx-sender }
      { description: description,
        location: location,
        available: true,
        price-per-day: price-per-day })
    (ok asset-id)))

;; Update asset availability
(define-public (set-asset-availability (asset-id uint) (available bool))
  (let ((asset-data (map-get? infrastructure-assets { asset-id: asset-id, owner: tx-sender })))
    (asserts! (is-some asset-data) err-not-owner)
    (map-set infrastructure-assets
      { asset-id: asset-id, owner: tx-sender }
      (merge (unwrap-panic asset-data) { available: available }))
    (ok true)))

;; Create a sharing agreement
(define-public (create-sharing-agreement (asset-id uint) (owner principal) (days uint))
  (let ((asset-data (map-get? infrastructure-assets { asset-id: asset-id, owner: owner }))
        (agreement-data (map-get? sharing-agreements { asset-id: asset-id, user: tx-sender })))
    (asserts! (is-some asset-data) err-asset-not-found)
    (asserts! (get available (unwrap-panic asset-data)) err-asset-not-available)
    (asserts! (is-none agreement-data) err-agreement-exists)

    (let ((price-per-day (get price-per-day (unwrap-panic asset-data)))
          (total-cost (* price-per-day days))
          (current-time block-height)
          (end-time (+ block-height (* days u144)))) ;; Assuming ~144 blocks per day

      (map-set sharing-agreements
        { asset-id: asset-id, user: tx-sender }
        { start-time: current-time,
          end-time: end-time,
          total-cost: total-cost,
          active: true })
      (ok total-cost))))

;; End a sharing agreement
(define-public (end-sharing-agreement (asset-id uint) (user principal))
  (let ((agreement-data (map-get? sharing-agreements { asset-id: asset-id, user: user }))
        (asset-data (map-get? infrastructure-assets { asset-id: asset-id, owner: tx-sender })))
    (asserts! (is-some asset-data) err-not-owner)
    (asserts! (is-some agreement-data) err-agreement-not-found)

    (map-set sharing-agreements
      { asset-id: asset-id, user: user }
      (merge (unwrap-panic agreement-data) { active: false }))
    (ok true)))

;; Get asset details
(define-read-only (get-asset-details (asset-id uint) (owner principal))
  (map-get? infrastructure-assets { asset-id: asset-id, owner: owner }))

;; Get agreement details
(define-read-only (get-agreement-details (asset-id uint) (user principal))
  (map-get? sharing-agreements { asset-id: asset-id, user: user }))
