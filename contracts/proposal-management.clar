;; Proposal Management Contract
;; Management system for governance proposals and execution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-unauthorized (err u201))
(define-constant err-proposal-not-found (err u202))
(define-constant err-proposal-expired (err u203))
(define-constant err-proposal-executed (err u204))
(define-constant err-insufficient-support (err u205))
(define-constant err-invalid-status (err u206))
(define-constant err-execution-failed (err u207))

;; Data variables
(define-data-var governance-admin principal tx-sender)
(define-data-var next-proposal-id uint u1)
(define-data-var min-proposal-threshold uint u1000) ;; Minimum tokens to create proposal
(define-data-var voting-period uint u1440) ;; Default voting period in blocks (~10 days)

;; Proposal data structures
(define-map proposals uint {
  title: (string-ascii 128),
  description: (string-ascii 1024),
  proposer: principal,
  proposal-type: (string-ascii 32), ;; parameter-change, funding, upgrade, etc.
  target-contract: (optional principal),
  execution-parameters: (string-ascii 512),
  creation-timestamp: uint,
  voting-start: uint,
  voting-end: uint,
  status: (string-ascii 16), ;; draft, active, passed, rejected, executed, expired
  votes-for: uint,
  votes-against: uint,
  votes-abstain: uint,
  total-voters: uint,
  execution-timestamp: uint,
  quorum-threshold: uint
})

;; Proposal voting records
(define-map proposal-votes { proposal-id: uint, voter: principal } {
  vote-type: (string-ascii 16), ;; for, against, abstain
  voting-power: uint,
  vote-timestamp: uint,
  vote-reason: (optional (string-ascii 256))
})

;; Governance token holders
(define-map governance-members principal {
  token-balance: uint,
  voting-power: uint,
  delegation-target: (optional principal),
  total-proposals-created: uint,
  total-votes-cast: uint,
  reputation-score: uint
})

;; Proposal execution queue
(define-map execution-queue uint {
  proposal-id: uint,
  execution-delay: uint,
  earliest-execution: uint,
  execution-attempts: uint,
  is-executed: bool
})

;; Proposal discussions and comments
(define-map proposal-discussions { proposal-id: uint, comment-id: uint } {
  commenter: principal,
  comment: (string-ascii 512),
  timestamp: uint,
  support-level: (string-ascii 16) ;; strong-support, support, neutral, oppose, strong-oppose
})

(define-data-var next-comment-id uint u1)

;; Delegation system
(define-map delegations { delegator: principal, delegate: principal } {
  delegated-power: uint,
  delegation-timestamp: uint,
  is-active: bool
})

;; Proposal categories and templates
(define-map proposal-categories (string-ascii 32) {
  category-name: (string-ascii 64),
  required-quorum: uint,
  required-majority: uint,
  execution-delay: uint,
  is-active: bool
})

;; Governance parameters
(define-map governance-parameters (string-ascii 32) {
  parameter-value: uint,
  last-updated: uint,
  updated-by: principal
})

;; Administrative functions
(define-public (set-governance-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set governance-admin new-admin))
  )
)

(define-public (update-governance-parameter (param-name (string-ascii 32)) (new-value uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (var-get governance-admin))) err-unauthorized)
    (map-set governance-parameters param-name {
      parameter-value: new-value,
      last-updated: block-height,
      updated-by: tx-sender
    })
    (ok true)
  )
)

(define-public (register-governance-member (member principal) (initial-balance uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (var-get governance-admin))) err-unauthorized)
    (ok (map-set governance-members member {
      token-balance: initial-balance,
      voting-power: initial-balance,
      delegation-target: none,
      total-proposals-created: u0,
      total-votes-cast: u0,
      reputation-score: u100
    }))
  )
)

;; Proposal creation and management
(define-public (create-proposal
    (title (string-ascii 128))
    (description (string-ascii 1024))
    (proposal-type (string-ascii 32))
    (target-contract (optional principal))
    (execution-parameters (string-ascii 512))
    (voting-duration uint)
  )
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (member-data (unwrap! (map-get? governance-members tx-sender) err-unauthorized))
      (category-data (default-to {
        category-name: "general",
        required-quorum: u5000, ;; 50%
        required-majority: u5000, ;; 50%
        execution-delay: u144, ;; ~24 hours
        is-active: true
      } (map-get? proposal-categories proposal-type)))
    )
    (asserts! (>= (get token-balance member-data) (var-get min-proposal-threshold)) err-insufficient-support)
    (asserts! (> voting-duration u0) err-unauthorized)
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      proposal-type: proposal-type,
      target-contract: target-contract,
      execution-parameters: execution-parameters,
      creation-timestamp: block-height,
      voting-start: (+ block-height u144), ;; Delay before voting starts
      voting-end: (+ block-height (+ u144 voting-duration)),
      status: "draft",
      votes-for: u0,
      votes-against: u0,
      votes-abstain: u0,
      total-voters: u0,
      execution-timestamp: u0,
      quorum-threshold: (get required-quorum category-data)
    })
    
    ;; Update member statistics
    (map-set governance-members tx-sender 
      (merge member-data { total-proposals-created: (+ (get total-proposals-created member-data) u1) })
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Voting on proposals
(define-public (vote-on-proposal (proposal-id uint) (vote-type (string-ascii 16)) (vote-reason (optional (string-ascii 256))))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
      (member-data (unwrap! (map-get? governance-members tx-sender) err-unauthorized))
      (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
      (voting-power (calculate-voting-power tx-sender))
    )
    (asserts! (is-eq (get status proposal-data) "active") err-invalid-status)
    (asserts! (>= block-height (get voting-start proposal-data)) err-unauthorized)
    (asserts! (< block-height (get voting-end proposal-data)) err-proposal-expired)
    (asserts! (is-none existing-vote) err-unauthorized) ;; One vote per proposal
    (asserts! (> voting-power u0) err-insufficient-support)
    
    ;; Record vote
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } {
      vote-type: vote-type,
      voting-power: voting-power,
      vote-timestamp: block-height,
      vote-reason: vote-reason
    })
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id 
      (merge proposal-data {
        votes-for: (if (is-eq vote-type "for") (+ (get votes-for proposal-data) voting-power) (get votes-for proposal-data)),
        votes-against: (if (is-eq vote-type "against") (+ (get votes-against proposal-data) voting-power) (get votes-against proposal-data)),
        votes-abstain: (if (is-eq vote-type "abstain") (+ (get votes-abstain proposal-data) voting-power) (get votes-abstain proposal-data)),
        total-voters: (+ (get total-voters proposal-data) u1)
      })
    )
    
    ;; Update member voting statistics
    (map-set governance-members tx-sender 
      (merge member-data { total-votes-cast: (+ (get total-votes-cast member-data) u1) })
    )
    
    (ok true)
  )
)

;; Calculate effective voting power (including delegations)
(define-private (calculate-voting-power (voter principal))
  (let
    (
      (member-data (default-to { token-balance: u0, voting-power: u0, delegation-target: none, total-proposals-created: u0, total-votes-cast: u0, reputation-score: u0 } 
        (map-get? governance-members voter)))
    )
    ;; Simplified calculation - in practice would include delegated power
    (get voting-power member-data)
  )
)

;; Finalize proposal voting
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
      (total-votes (+ (get votes-for proposal-data) (+ (get votes-against proposal-data) (get votes-abstain proposal-data))))
      (total-supply u10000) ;; Simplified - would get actual token supply
      (quorum-met (>= total-votes (/ (* total-supply (get quorum-threshold proposal-data)) u10000)))
      (majority-reached (> (get votes-for proposal-data) (get votes-against proposal-data)))
    )
    (asserts! (>= block-height (get voting-end proposal-data)) err-unauthorized)
    (asserts! (is-eq (get status proposal-data) "active") err-invalid-status)
    
    ;; Determine outcome
    (let
      (
        (new-status (if (and quorum-met majority-reached) "passed" "rejected"))
      )
      (map-set proposals proposal-id (merge proposal-data { status: new-status }))
      
      ;; If passed, add to execution queue
      (if (is-eq new-status "passed")
        (let
          (
            (category-data (default-to {
              category-name: "general",
              required-quorum: u5000,
              required-majority: u5000,
              execution-delay: u144,
              is-active: true
            } (map-get? proposal-categories (get proposal-type proposal-data))))
          )
          (map-set execution-queue proposal-id {
            proposal-id: proposal-id,
            execution-delay: (get execution-delay category-data),
            earliest-execution: (+ block-height (get execution-delay category-data)),
            execution-attempts: u0,
            is-executed: false
          })
        )
        true
      )
    )
    
    (ok true)
  )
)

;; Execute approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
      (execution-data (unwrap! (map-get? execution-queue proposal-id) err-unauthorized))
    )
    (asserts! (is-eq (get status proposal-data) "passed") err-invalid-status)
    (asserts! (>= block-height (get earliest-execution execution-data)) err-unauthorized)
    (asserts! (not (get is-executed execution-data)) err-proposal-executed)
    
    ;; Mark as executed
    (map-set proposals proposal-id (merge proposal-data { 
      status: "executed",
      execution-timestamp: block-height
    }))
    
    (map-set execution-queue proposal-id (merge execution-data {
      is-executed: true,
      execution-attempts: (+ (get execution-attempts execution-data) u1)
    }))
    
    ;; In practice, would execute the actual proposal logic here
    ;; This could involve parameter changes, fund transfers, etc.
    
    (ok true)
  )
)

;; Delegation functions
(define-public (delegate-voting-power (delegate principal) (amount uint))
  (let
    (
      (delegator-data (unwrap! (map-get? governance-members tx-sender) err-unauthorized))
      (delegate-data (unwrap! (map-get? governance-members delegate) err-unauthorized))
    )
    (asserts! (<= amount (get voting-power delegator-data)) err-insufficient-support)
    (asserts! (not (is-eq tx-sender delegate)) err-unauthorized)
    
    ;; Record delegation
    (map-set delegations { delegator: tx-sender, delegate: delegate } {
      delegated-power: amount,
      delegation-timestamp: block-height,
      is-active: true
    })
    
    ;; Update voting powers
    (map-set governance-members tx-sender 
      (merge delegator-data { voting-power: (- (get voting-power delegator-data) amount) })
    )
    
    (map-set governance-members delegate 
      (merge delegate-data { voting-power: (+ (get voting-power delegate-data) amount) })
    )
    
    (ok true)
  )
)

;; Add comment to proposal discussion
(define-public (add-proposal-comment (proposal-id uint) (comment (string-ascii 512)) (support-level (string-ascii 16)))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
      (comment-id (var-get next-comment-id))
      (member-data (map-get? governance-members tx-sender))
    )
    (asserts! (is-some member-data) err-unauthorized)
    
    (map-set proposal-discussions { proposal-id: proposal-id, comment-id: comment-id } {
      commenter: tx-sender,
      comment: comment,
      timestamp: block-height,
      support-level: support-level
    })
    
    (var-set next-comment-id (+ comment-id u1))
    (ok comment-id)
  )
)

;; Update proposal status (admin function)
(define-public (update-proposal-status (proposal-id uint) (new-status (string-ascii 16)))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (get proposer proposal-data))
      (is-eq tx-sender (var-get governance-admin))
    ) err-unauthorized)
    
    (map-set proposals proposal-id (merge proposal-data { status: new-status }))
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-governance-member (member principal))
  (map-get? governance-members member)
)

(define-read-only (get-execution-queue-item (proposal-id uint))
  (map-get? execution-queue proposal-id)
)

(define-read-only (get-proposal-comment (proposal-id uint) (comment-id uint))
  (map-get? proposal-discussions { proposal-id: proposal-id, comment-id: comment-id })
)

(define-read-only (get-delegation (delegator principal) (delegate principal))
  (map-get? delegations { delegator: delegator, delegate: delegate })
)

(define-read-only (get-governance-parameter (param-name (string-ascii 32)))
  (map-get? governance-parameters param-name)
)

(define-read-only (calculate-proposal-outcome (proposal-id uint))
  (let
    (
      (proposal-data (map-get? proposals proposal-id))
    )
    (match proposal-data
      data {
        total-votes: (+ (get votes-for data) (+ (get votes-against data) (get votes-abstain data))),
        majority: (if (> (get votes-for data) (get votes-against data)) "for" "against"),
        participation: (/ (* (+ (get votes-for data) (+ (get votes-against data) (get votes-abstain data))) u10000) u10000)
      }
      { total-votes: u0, majority: "none", participation: u0 }
    )
  )
)

;; Initialize contract
(begin
  (map-set governance-members contract-owner {
    token-balance: u10000,
    voting-power: u10000,
    delegation-target: none,
    total-proposals-created: u0,
    total-votes-cast: u0,
    reputation-score: u100
  })
  
  (map-set proposal-categories "parameter-change" {
    category-name: "Parameter Change",
    required-quorum: u3000, ;; 30%
    required-majority: u5000, ;; 50%
    execution-delay: u1440, ;; ~10 days
    is-active: true
  })
  
  (map-set proposal-categories "funding" {
    category-name: "Funding Proposal",
    required-quorum: u4000, ;; 40%
    required-majority: u6000, ;; 60%
    execution-delay: u2880, ;; ~20 days
    is-active: true
  })
)


;; title: proposal-management
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

