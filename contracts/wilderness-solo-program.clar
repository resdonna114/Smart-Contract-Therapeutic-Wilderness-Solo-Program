;; Wilderness Solo Program Coordinator
;; Manages solo experiences, land access, and integration support

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_PARAMS (err u400))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_DEPOSIT (err u402))
(define-constant ERR_PROGRAM_ENDED (err u410))
(define-constant ERR_NOT_ACTIVE (err u411))

;; Program status enumeration
(define-constant STATUS_PENDING u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_EMERGENCY u3)
(define-constant STATUS_CANCELLED u4)

;; Data structures
(define-map Programs
  { program-id: uint }
  {
    participant: principal,
    guide: principal,
    location-id: uint,
    start-block: uint,
    duration-blocks: uint,
    deposit-amount: uint,
    status: uint,
    safety-check-interval: uint,
    last-safety-check: uint,
    emergency-contacts: (list 3 principal),
    program-type: (string-ascii 50),
    created-at: uint
  }
)

(define-map Locations
  { location-id: uint }
  {
    name: (string-ascii 100),
    coordinates: (string-ascii 50),
    capacity: uint,
    current-occupancy: uint,
    land-owner: principal,
    access-fee: uint,
    terrain-type: (string-ascii 30),
    difficulty-level: uint,
    water-access: bool,
    shelter-available: bool,
    emergency-distance: uint,
    active: bool
  }
)

(define-map Guides
  { guide: principal }
  {
    name: (string-ascii 100),
    certification-level: uint,
    specializations: (list 5 (string-ascii 30)),
    active-programs: uint,
    max-programs: uint,
    rating: uint,
    emergency-contact: principal,
    certified-until: uint,
    active: bool
  }
)

(define-map UserProfiles
  { user: principal }
  {
    name: (string-ascii 100),
    experience-level: uint,
    medical-clearance: bool,
    emergency-contact: principal,
    total-programs: uint,
    completed-programs: uint,
    preferred-terrain: (string-ascii 30),
    created-at: uint
  }
)

;; Safety check logging
(define-map SafetyCheckLog
  { program-id: uint, check-sequence: uint }
  {
    participant: principal,
    check-time: uint,
    location-confirmed: bool,
    health-status: uint,
    notes: (string-ascii 200)
  }
)

(define-map ProgramCheckCounters
  { program-id: uint }
  { check-count: uint }
)

;; Emergency event logging
(define-map EmergencyLog
  { program-id: uint, emergency-sequence: uint }
  {
    triggered-by: principal,
    emergency-type: (string-ascii 50),
    trigger-time: uint,
    resolved: bool
  }
)

(define-map ProgramEmergencyCounters
  { program-id: uint }
  { emergency-count: uint }
)

;; Data variables
(define-data-var next-program-id uint u1)
(define-data-var next-location-id uint u1)
(define-data-var program-deposit-base uint u1000000) ;; 1 STX base deposit
(define-data-var emergency-response-time uint u144) ;; ~24 hours in blocks

;; Read-only functions
(define-read-only (get-program (program-id uint))
  (map-get? Programs { program-id: program-id })
)

(define-read-only (get-location (location-id uint))
  (map-get? Locations { location-id: location-id })
)

(define-read-only (get-guide (guide principal))
  (map-get? Guides { guide: guide })
)

(define-read-only (get-user-profile (user principal))
  (map-get? UserProfiles { user: user })
)

(define-read-only (get-next-program-id)
  (var-get next-program-id)
)

(define-read-only (get-safety-check-log (program-id uint) (check-sequence uint))
  (map-get? SafetyCheckLog { program-id: program-id, check-sequence: check-sequence })
)

(define-read-only (get-emergency-log (program-id uint) (emergency-sequence uint))
  (map-get? EmergencyLog { program-id: program-id, emergency-sequence: emergency-sequence })
)

(define-read-only (get-program-check-count (program-id uint))
  (default-to { check-count: u0 } (map-get? ProgramCheckCounters { program-id: program-id }))
)

(define-read-only (get-program-emergency-count (program-id uint))
  (default-to { emergency-count: u0 } (map-get? ProgramEmergencyCounters { program-id: program-id }))
)

(define-read-only (calculate-program-cost (duration-blocks uint) (location-id uint))
  (let (
    (location-data (unwrap! (get-location location-id) (err u404)))
    (base-cost (var-get program-deposit-base))
    (duration-multiplier (/ duration-blocks u144)) ;; Per day approximation
    (location-fee (get access-fee location-data))
  )
    (ok (+ base-cost (* duration-multiplier location-fee)))
  )
)

(define-read-only (check-program-status (program-id uint))
  (let (
    (program-data (unwrap! (get-program program-id) (err u404)))
    (current-block stacks-block-height)
    (start-block (get start-block program-data))
    (end-block (+ start-block (get duration-blocks program-data)))
    (last-check (get last-safety-check program-data))
    (check-interval (get safety-check-interval program-data))
  )
    (ok {
      program-id: program-id,
      status: (get status program-data),
      blocks-remaining: (if (> end-block current-block) (- end-block current-block) u0),
      overdue-check: (> (- current-block last-check) check-interval),
      progress: (if (> current-block start-block)
                   (/ (* (- current-block start-block) u100) (get duration-blocks program-data))
                   u0)
    })
  )
)

;; Registration functions
(define-public (register-user (name (string-ascii 100)) (experience-level uint) (medical-clearance bool) (emergency-contact principal) (preferred-terrain (string-ascii 30)))
  (let (
    (user tx-sender)
  )
    (asserts! (<= experience-level u5) ERR_INVALID_PARAMS)
    (asserts! (is-none (get-user-profile user)) ERR_ALREADY_EXISTS)

    (map-set UserProfiles
      { user: user }
      {
        name: name,
        experience-level: experience-level,
        medical-clearance: medical-clearance,
        emergency-contact: emergency-contact,
        total-programs: u0,
        completed-programs: u0,
        preferred-terrain: preferred-terrain,
        created-at: stacks-block-height
      }
    )
    (ok user)
  )
)

(define-public (register-guide (name (string-ascii 100)) (certification-level uint) (specializations (list 5 (string-ascii 30))) (max-programs uint) (emergency-contact principal) (certified-until uint))
  (let (
    (guide tx-sender)
  )
    (asserts! (<= certification-level u3) ERR_INVALID_PARAMS)
    (asserts! (> max-programs u0) ERR_INVALID_PARAMS)
    (asserts! (> certified-until stacks-block-height) ERR_INVALID_PARAMS)
    (asserts! (is-none (get-guide guide)) ERR_ALREADY_EXISTS)

    (map-set Guides
      { guide: guide }
      {
        name: name,
        certification-level: certification-level,
        specializations: specializations,
        active-programs: u0,
        max-programs: max-programs,
        rating: u5, ;; Default 5/10 rating
        emergency-contact: emergency-contact,
        certified-until: certified-until,
        active: true
      }
    )
    (ok guide)
  )
)

(define-public (add-location (name (string-ascii 100)) (coordinates (string-ascii 50)) (capacity uint) (access-fee uint) (terrain-type (string-ascii 30)) (difficulty-level uint) (water-access bool) (shelter-available bool) (emergency-distance uint))
  (let (
    (location-id (var-get next-location-id))
    (land-owner tx-sender)
  )
    (asserts! (> capacity u0) ERR_INVALID_PARAMS)
    (asserts! (<= difficulty-level u5) ERR_INVALID_PARAMS)

    (map-set Locations
      { location-id: location-id }
      {
        name: name,
        coordinates: coordinates,
        capacity: capacity,
        current-occupancy: u0,
        land-owner: land-owner,
        access-fee: access-fee,
        terrain-type: terrain-type,
        difficulty-level: difficulty-level,
        water-access: water-access,
        shelter-available: shelter-available,
        emergency-distance: emergency-distance,
        active: true
      }
    )

    (var-set next-location-id (+ location-id u1))
    (ok location-id)
  )
)

;; Program management
(define-public (create-program (guide principal) (location-id uint) (duration-blocks uint) (safety-check-interval uint) (emergency-contacts (list 3 principal)) (program-type (string-ascii 50)))
  (let (
    (program-id (var-get next-program-id))
    (participant tx-sender)
    (location-data (unwrap! (get-location location-id) ERR_NOT_FOUND))
    (guide-data (unwrap! (get-guide guide) ERR_NOT_FOUND))
    (user-data (unwrap! (get-user-profile participant) ERR_NOT_FOUND))
    (program-cost (unwrap! (calculate-program-cost duration-blocks location-id) ERR_INVALID_PARAMS))
  )
    ;; Validations
    (asserts! (get medical-clearance user-data) ERR_UNAUTHORIZED)
    (asserts! (get active guide-data) ERR_NOT_ACTIVE)
    (asserts! (get active location-data) ERR_NOT_ACTIVE)
    (asserts! (< (get active-programs guide-data) (get max-programs guide-data)) ERR_UNAUTHORIZED)
    (asserts! (< (get current-occupancy location-data) (get capacity location-data)) ERR_UNAUTHORIZED)
    (asserts! (> duration-blocks u144) ERR_INVALID_PARAMS) ;; Minimum 1 day
    (asserts! (< duration-blocks u4320) ERR_INVALID_PARAMS) ;; Maximum 30 days
    (asserts! (> safety-check-interval u0) ERR_INVALID_PARAMS)

    ;; Transfer deposit
    (try! (stx-transfer? program-cost participant (as-contract tx-sender)))

    ;; Create program
    (map-set Programs
      { program-id: program-id }
      {
        participant: participant,
        guide: guide,
        location-id: location-id,
        start-block: u0, ;; Will be set when program starts
        duration-blocks: duration-blocks,
        deposit-amount: program-cost,
        status: STATUS_PENDING,
        safety-check-interval: safety-check-interval,
        last-safety-check: u0,
        emergency-contacts: emergency-contacts,
        program-type: program-type,
        created-at: stacks-block-height
      }
    )

    ;; Update counters
    (var-set next-program-id (+ program-id u1))
    (map-set UserProfiles
      { user: participant }
      (merge user-data { total-programs: (+ (get total-programs user-data) u1) })
    )
    (map-set Guides
      { guide: guide }
      (merge guide-data { active-programs: (+ (get active-programs guide-data) u1) })
    )
    (map-set Locations
      { location-id: location-id }
      (merge location-data { current-occupancy: (+ (get current-occupancy location-data) u1) })
    )

    (ok program-id)
  )
)

(define-public (start-program (program-id uint))
  (let (
    (program-data (unwrap! (get-program program-id) ERR_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (or (is-eq caller (get participant program-data)) (is-eq caller (get guide program-data))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status program-data) STATUS_PENDING) ERR_INVALID_PARAMS)

    (map-set Programs
      { program-id: program-id }
      (merge program-data {
        start-block: stacks-block-height,
        status: STATUS_ACTIVE,
        last-safety-check: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (safety-check-in (program-id uint) (location-confirmed bool) (health-status uint) (notes (string-ascii 200)))
  (let (
    (program-data (unwrap! (get-program program-id) ERR_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (is-eq caller (get participant program-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status program-data) STATUS_ACTIVE) ERR_NOT_ACTIVE)
    (asserts! (<= health-status u5) ERR_INVALID_PARAMS) ;; 1-5 health scale

    (map-set Programs
      { program-id: program-id }
      (merge program-data { last-safety-check: stacks-block-height })
    )

    ;; Log the safety check internally
    (let (
      (check-counter (default-to { check-count: u0 } (map-get? ProgramCheckCounters { program-id: program-id })))
      (next-check-seq (+ (get check-count check-counter) u1))
    )
      (map-set SafetyCheckLog
        { program-id: program-id, check-sequence: next-check-seq }
        {
          participant: (get participant program-data),
          check-time: stacks-block-height,
          location-confirmed: location-confirmed,
          health-status: health-status,
          notes: notes
        }
      )
      (map-set ProgramCheckCounters
        { program-id: program-id }
        { check-count: next-check-seq }
      )
    )

    (ok true)
  )
)

(define-public (complete-program (program-id uint))
  (let (
    (program-data (unwrap! (get-program program-id) ERR_NOT_FOUND))
    (caller tx-sender)
    (location-data (unwrap! (get-location (get location-id program-data)) ERR_NOT_FOUND))
    (guide-data (unwrap! (get-guide (get guide program-data)) ERR_NOT_FOUND))
    (user-data (unwrap! (get-user-profile (get participant program-data)) ERR_NOT_FOUND))
  )
    (asserts! (or (is-eq caller (get participant program-data)) (is-eq caller (get guide program-data))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status program-data) STATUS_ACTIVE) ERR_NOT_ACTIVE)

    ;; Update program status
    (map-set Programs
      { program-id: program-id }
      (merge program-data { status: STATUS_COMPLETED })
    )

    ;; Release deposit back to participant
    (try! (as-contract (stx-transfer? (get deposit-amount program-data) tx-sender (get participant program-data))))

    ;; Update counters
    (map-set UserProfiles
      { user: (get participant program-data) }
      (merge user-data { completed-programs: (+ (get completed-programs user-data) u1) })
    )
    (map-set Guides
      { guide: (get guide program-data) }
      (merge guide-data { active-programs: (- (get active-programs guide-data) u1) })
    )
    (map-set Locations
      { location-id: (get location-id program-data) }
      (merge location-data { current-occupancy: (- (get current-occupancy location-data) u1) })
    )

    (ok true)
  )
)

(define-public (trigger-emergency (program-id uint) (emergency-type (string-ascii 50)))
  (let (
    (program-data (unwrap! (get-program program-id) ERR_NOT_FOUND))
    (caller tx-sender)
  )
    (asserts! (or
      (is-eq caller (get participant program-data))
      (is-eq caller (get guide program-data))
      (is-some (index-of (get emergency-contacts program-data) caller))
    ) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status program-data) STATUS_ACTIVE) ERR_NOT_ACTIVE)

    (map-set Programs
      { program-id: program-id }
      (merge program-data { status: STATUS_EMERGENCY })
    )

    ;; Log the emergency internally
    (let (
      (emergency-counter (default-to { emergency-count: u0 } (map-get? ProgramEmergencyCounters { program-id: program-id })))
      (next-emergency-seq (+ (get emergency-count emergency-counter) u1))
    )
      (map-set EmergencyLog
        { program-id: program-id, emergency-sequence: next-emergency-seq }
        {
          triggered-by: caller,
          emergency-type: emergency-type,
          trigger-time: stacks-block-height,
          resolved: false
        }
      )
      (map-set ProgramEmergencyCounters
        { program-id: program-id }
        { emergency-count: next-emergency-seq }
      )
    )

    (ok true)
  )
)

;; Administrative functions
(define-public (update-program-deposit-base (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set program-deposit-base new-amount)
    (ok true)
  )
)

(define-public (deactivate-location (location-id uint))
  (let (
    (location-data (unwrap! (get-location location-id) ERR_NOT_FOUND))
  )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get land-owner location-data))) ERR_UNAUTHORIZED)
    (map-set Locations
      { location-id: location-id }
      (merge location-data { active: false })
    )
    (ok true)
  )
)
