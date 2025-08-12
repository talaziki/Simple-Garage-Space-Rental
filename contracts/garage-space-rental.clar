;; garage-rental.clar
;; Simple garage space rental system with listings, scheduling, and deposits

(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-SPACE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-BOOKED (err u102))
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u103))
(define-constant ERR-BOOKING-NOT-FOUND (err u104))
(define-constant ERR-BOOKING-ACTIVE (err u105))

(define-data-var next-space-id uint u1)
(define-data-var next-booking-id uint u1)

(define-map garage-spaces uint {
  owner: principal,
  price-per-month: uint,
  deposit-required: uint,
  available: bool,
  description: (string-ascii 256)
})

(define-map space-bookings uint {
  space-id: uint,
  renter: principal,
  start-block: uint,
  end-block: uint,
  deposit-paid: uint,
  active: bool
})

(define-map user-deposits { user: principal, space-id: uint } uint)

(define-public (list-space (price-per-month uint) (deposit-required uint) (description (string-ascii 256)))
  (let ((space-id (var-get next-space-id)))
    (map-set garage-spaces space-id {
      owner: tx-sender,
      price-per-month: price-per-month,
      deposit-required: deposit-required,
      available: true,
      description: description
    })
    (var-set next-space-id (+ space-id u1))
    (ok space-id)))

(define-public (update-space-availability (space-id uint) (available bool))
  (let ((space (unwrap! (map-get? garage-spaces space-id) ERR-SPACE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner space)) ERR-UNAUTHORIZED)
    (map-set garage-spaces space-id (merge space { available: available }))
    (ok true)))

(define-public (book-space (space-id uint) (duration-blocks uint))
  (let (
    (space (unwrap! (map-get? garage-spaces space-id) ERR-SPACE-NOT-FOUND))
    (booking-id (var-get next-booking-id))
    (start-block burn-block-height)
    (end-block (+ start-block duration-blocks))
    (deposit-amount (get deposit-required space))
  )
    (asserts! (get available space) ERR-ALREADY-BOOKED)
    (asserts! (>= (stx-get-balance tx-sender) deposit-amount) ERR-INSUFFICIENT-DEPOSIT)

    (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))

    (map-set space-bookings booking-id {
      space-id: space-id,
      renter: tx-sender,
      start-block: start-block,
      end-block: end-block,
      deposit-paid: deposit-amount,
      active: true
    })

    (map-set user-deposits { user: tx-sender, space-id: space-id } deposit-amount)
    (map-set garage-spaces space-id (merge space { available: false }))
    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)))

(define-public (end-booking (booking-id uint))
  (let ((booking (unwrap! (map-get? space-bookings booking-id) ERR-BOOKING-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get renter booking)) ERR-UNAUTHORIZED)
    (asserts! (get active booking) ERR-BOOKING-ACTIVE)
    (asserts! (>= burn-block-height (get end-block booking)) ERR-BOOKING-ACTIVE)

    (let (
      (space-id (get space-id booking))
      (deposit-amount (get deposit-paid booking))
      (space (unwrap! (map-get? garage-spaces space-id) ERR-SPACE-NOT-FOUND))
    )
      (try! (as-contract (stx-transfer? deposit-amount tx-sender (get renter booking))))

      (map-set space-bookings booking-id (merge booking { active: false }))
      (map-delete user-deposits { user: tx-sender, space-id: space-id })
      (map-set garage-spaces space-id (merge space { available: true }))
      (ok true))))

(define-public (owner-end-booking (booking-id uint))
  (let ((booking (unwrap! (map-get? space-bookings booking-id) ERR-BOOKING-NOT-FOUND)))
    (let (
      (space-id (get space-id booking))
      (space (unwrap! (map-get? garage-spaces space-id) ERR-SPACE-NOT-FOUND))
    )
      (asserts! (is-eq tx-sender (get owner space)) ERR-UNAUTHORIZED)
      (asserts! (get active booking) ERR-BOOKING-ACTIVE)

      (map-set space-bookings booking-id (merge booking { active: false }))
      (map-delete user-deposits { user: (get renter booking), space-id: space-id })
      (map-set garage-spaces space-id (merge space { available: true }))
      (ok true))))

(define-read-only (get-space (space-id uint))
  (map-get? garage-spaces space-id))

(define-read-only (get-booking (booking-id uint))
  (map-get? space-bookings booking-id))

(define-read-only (get-user-deposit (user principal) (space-id uint))
  (map-get? user-deposits { user: user, space-id: space-id }))
