;; Service Level Contract
;; Records and enforces performance guarantees between carriers

(define-data-var admin principal tx-sender)

;; Structure for service level agreements (SLAs)
(define-map service-level-agreements
  { sla-id: uint }
  { provider: principal,
    consumer: principal,
    uptime-requirement: uint,  ;; percentage * 100 (e.g., 99.5% = 9950)
    latency-max: uint,         ;; in milliseconds
    bandwidth-min: uint,       ;; in Mbps
    penalty-rate: uint,        ;; penalty per violation in tokens
    start-block: uint,
    end-block: uint,
    active: bool })

;; Map to track performance reports
(define-map performance-reports
  { sla-id: uint, report-id: uint }
  { reporter: principal,
    uptime: uint,
    latency: uint,
    bandwidth: uint,
    timestamp: uint,
    disputed: bool })

;; Counters
(define-data-var sla-id-counter uint u1)
(define-data-var report-id-counter uint u1)

;; Error codes
(define-constant err-not-admin (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-sla-not-found (err u102))
(define-constant err-sla-inactive (err u103))
(define-constant err-invalid-reporter (err u104))

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin)))

;; Create a new SLA
(define-public (create-sla
  (provider principal)
  (consumer principal)
  (uptime-requirement uint)
  (latency-max uint)
  (bandwidth-min uint)
  (penalty-rate uint)
  (duration uint))
  (let ((sla-id (var-get sla-id-counter))
        (current-block block-height)
        (end-block (+ block-height duration)))
    (asserts! (or (is-admin) (is-eq tx-sender provider) (is-eq tx-sender consumer)) err-unauthorized)

    (var-set sla-id-counter (+ sla-id u1))
    (map-set service-level-agreements
      { sla-id: sla-id }
      { provider: provider,
        consumer: consumer,
        uptime-requirement: uptime-requirement,
        latency-max: latency-max,
        bandwidth-min: bandwidth-min,
        penalty-rate: penalty-rate,
        start-block: current-block,
        end-block: end-block,
        active: true })
    (ok sla-id)))

;; Submit a performance report
(define-public (submit-report (sla-id uint) (uptime uint) (latency uint) (bandwidth uint))
  (let ((sla-data (map-get? service-level-agreements { sla-id: sla-id }))
        (report-id (var-get report-id-counter)))
    (asserts! (is-some sla-data) err-sla-not-found)
    (let ((sla (unwrap-panic sla-data)))
      (asserts! (get active sla) err-sla-inactive)
      (asserts! (or (is-eq tx-sender (get provider sla))
                   (is-eq tx-sender (get consumer sla))
                   (is-admin))
               err-invalid-reporter)

      (var-set report-id-counter (+ report-id u1))
      (map-set performance-reports
        { sla-id: sla-id, report-id: report-id }
        { reporter: tx-sender,
          uptime: uptime,
          latency: latency,
          bandwidth: bandwidth,
          timestamp: block-height,
          disputed: false })
      (ok report-id))))

;; Dispute a performance report
(define-public (dispute-report (sla-id uint) (report-id uint))
  (let ((sla-data (map-get? service-level-agreements { sla-id: sla-id }))
        (report-data (map-get? performance-reports { sla-id: sla-id, report-id: report-id })))
    (asserts! (is-some sla-data) err-sla-not-found)
    (asserts! (is-some report-data) (err u105))

    (let ((sla (unwrap-panic sla-data))
          (report (unwrap-panic report-data)))
      (asserts! (or (is-eq tx-sender (get provider sla))
                   (is-eq tx-sender (get consumer sla)))
               err-unauthorized)
      (asserts! (not (is-eq tx-sender (get reporter report))) err-unauthorized)

      (map-set performance-reports
        { sla-id: sla-id, report-id: report-id }
        (merge report { disputed: true }))
      (ok true))))

;; Get SLA details
(define-read-only (get-sla-details (sla-id uint))
  (map-get? service-level-agreements { sla-id: sla-id }))

;; Get report details
(define-read-only (get-report-details (sla-id uint) (report-id uint))
  (map-get? performance-reports { sla-id: sla-id, report-id: report-id }))

;; Check if SLA is violated based on a report
(define-read-only (check-sla-violation (sla-id uint) (report-id uint))
  (let ((sla-data (map-get? service-level-agreements { sla-id: sla-id }))
        (report-data (map-get? performance-reports { sla-id: sla-id, report-id: report-id })))
    (if (and (is-some sla-data) (is-some report-data))
      (let ((sla (unwrap-panic sla-data))
            (report (unwrap-panic report-data)))
        (if (or (< (get uptime report) (get uptime-requirement sla))
                (> (get latency report) (get latency-max sla))
                (< (get bandwidth report) (get bandwidth-min sla)))
          (some (get penalty-rate sla))
          none))
      none)))
