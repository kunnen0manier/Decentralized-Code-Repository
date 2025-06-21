;; =============================================================================
;; DECENTRALIZED CODE REPOSITORY CONTRACT
;; =============================================================================
;; A decentralized version control system with built-in governance,
;; bounties, and automated testing integration.

;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_STATUS (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_VOTING_ACTIVE (err u107))
(define-constant ERR_ALREADY_VOTED (err u108))

;; Repository status constants
(define-constant REPO_ACTIVE u1)
(define-constant REPO_ARCHIVED u2)
(define-constant REPO_SUSPENDED u3)

;; Merge request status constants
(define-constant MR_PENDING u1)
(define-constant MR_APPROVED u2)
(define-constant MR_REJECTED u3)
(define-constant MR_MERGED u4)

;; Test status constants
(define-constant TEST_PENDING u1)
(define-constant TEST_PASSED u2)
(define-constant TEST_FAILED u3)

;; Voting constants
(define-constant VOTING_PERIOD u144) ;; ~24 hours in blocks
(define-constant MIN_APPROVAL_THRESHOLD u60) ;; 60% approval needed

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; Repository data structure
(define-map repositories
  { repo-id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    description: (string-utf8 256),
    status: uint,
    contributors: (list 50 principal),
    total-commits: uint,
    total-bounties: uint,
    created-at: uint,
    last-updated: uint
  }
)

;; Merge request data structure
(define-map merge-requests
  { repo-id: uint, mr-id: uint }
  {
    author: principal,
    title: (string-ascii 128),
    description: (string-utf8 512),
    source-branch: (string-ascii 64),
    target-branch: (string-ascii 64),
    status: uint,
    votes-for: uint,
    votes-against: uint,
    voting-ends: uint,
    test-status: uint,
    test-results: (string-utf8 256),
    bounty-amount: uint,
    created-at: uint,
    updated-at: uint
  }
)

;; Commit data structure
(define-map commits
  { repo-id: uint, commit-hash: (string-ascii 64) }
  {
    author: principal,
    message: (string-utf8 256),
    parent-hash: (optional (string-ascii 64)),
    files-changed: uint,
    lines-added: uint,
    lines-removed: uint,
    timestamp: uint,
    verified: bool
  }
)

;; Contributor data structure
(define-map contributors
  { repo-id: uint, contributor: principal }
  {
    commits: uint,
    lines-contributed: uint,
    bounties-earned: uint,
    reputation-score: uint,
    first-contribution: uint,
    last-contribution: uint
  }
)

;; Voting records
(define-map votes
  { repo-id: uint, mr-id: uint, voter: principal }
  {
    vote: bool, ;; true = approve, false = reject
    weight: uint,
    timestamp: uint
  }
)

;; Bounty tracking
(define-map bounties
  { repo-id: uint, bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 128),
    description: (string-utf8 512),
    amount: uint,
    claimed-by: (optional principal),
    mr-id: (optional uint),
    status: uint, ;; 1=open, 2=claimed, 3=completed, 4=cancelled
    created-at: uint,
    expires-at: uint
  }
)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var next-repo-id uint u1)
(define-data-var next-mr-id uint u1)
(define-data-var next-bounty-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points
(define-data-var min-voting-weight uint u100) ;; Minimum STX to have voting weight

;; =============================================================================
;; REPOSITORY MANAGEMENT FUNCTIONS
;; =============================================================================

;; Create a new repository
(define-public (create-repository (name (string-ascii 64)) (description (string-utf8 256)))
  (let (
    (repo-id (var-get next-repo-id))
    (current-height stacks-block-height)
  )
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? repositories { repo-id: repo-id })) ERR_ALREADY_EXISTS)

    (map-set repositories
      { repo-id: repo-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        status: REPO_ACTIVE,
        contributors: (list tx-sender),
        total-commits: u0,
        total-bounties: u0,
        created-at: current-height,
        last-updated: current-height
      }
    )

    ;; Initialize contributor record
    (map-set contributors
      { repo-id: repo-id, contributor: tx-sender }
      {
        commits: u0,
        lines-contributed: u0,
        bounties-earned: u0,
        reputation-score: u100, ;; Starting reputation
        first-contribution: current-height,
        last-contribution: current-height
      }
    )

    (var-set next-repo-id (+ repo-id u1))
    (ok repo-id)
  )
)

;; Add a commit to a repository
(define-public (add-commit
  (repo-id uint)
  (commit-hash (string-ascii 64))
  (message (string-utf8 256))
  (parent-hash (optional (string-ascii 64)))
  (files-changed uint)
  (lines-added uint)
  (lines-removed uint)
)
  (let (
    (repo (unwrap! (map-get? repositories { repo-id: repo-id }) ERR_NOT_FOUND))
    (current-height stacks-block-height)
    (contributor-record (default-to
      { commits: u0, lines-contributed: u0, bounties-earned: u0,
        reputation-score: u50, first-contribution: current-height,
        last-contribution: current-height }
      (map-get? contributors { repo-id: repo-id, contributor: tx-sender })
    ))
  )
    (asserts! (is-eq (get status repo) REPO_ACTIVE) ERR_INVALID_STATUS)
    (asserts! (> (len commit-hash) u0) ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? commits { repo-id: repo-id, commit-hash: commit-hash })) ERR_ALREADY_EXISTS)

    ;; Add commit record
    (map-set commits
      { repo-id: repo-id, commit-hash: commit-hash }
      {
        author: tx-sender,
        message: message,
        parent-hash: parent-hash,
        files-changed: files-changed,
        lines-added: lines-added,
        lines-removed: lines-removed,
        timestamp: current-height,
        verified: false
      }
    )

    ;; Update contributor stats
    (map-set contributors
      { repo-id: repo-id, contributor: tx-sender }
      (merge contributor-record {
        commits: (+ (get commits contributor-record) u1),
        lines-contributed: (+ (get lines-contributed contributor-record) lines-added),
        last-contribution: current-height,
        reputation-score: (+ (get reputation-score contributor-record) u1)
      })
    )

    ;; Update repository stats
    (map-set repositories
      { repo-id: repo-id }
      (merge repo {
        total-commits: (+ (get total-commits repo) u1),
        last-updated: current-height
      })
    )

    (ok true)
  )
)

;; =============================================================================
;; MERGE REQUEST & GOVERNANCE FUNCTIONS
;; =============================================================================

;; Create a merge request
(define-public (create-merge-request
  (repo-id uint)
  (title (string-ascii 128))
  (description (string-utf8 512))
  (source-branch (string-ascii 64))
  (target-branch (string-ascii 64))
  (bounty-amount uint)
)
  (let (
    (repo (unwrap! (map-get? repositories { repo-id: repo-id }) ERR_NOT_FOUND))
    (mr-id (var-get next-mr-id))
    (current-height stacks-block-height)
  )
    (asserts! (is-eq (get status repo) REPO_ACTIVE) ERR_INVALID_STATUS)
    (asserts! (> (len title) u0) ERR_INVALID_INPUT)

    ;; Transfer bounty if specified
    (if (> bounty-amount u0)
      (unwrap! (stx-transfer? bounty-amount tx-sender (as-contract tx-sender)) ERR_INSUFFICIENT_FUNDS)
      true
    )

    (map-set merge-requests
      { repo-id: repo-id, mr-id: mr-id }
      {
        author: tx-sender,
        title: title,
        description: description,
        source-branch: source-branch,
        target-branch: target-branch,
        status: MR_PENDING,
        votes-for: u0,
        votes-against: u0,
        voting-ends: (+ current-height VOTING_PERIOD),
        test-status: TEST_PENDING,
        test-results: u"",
        bounty-amount: bounty-amount,
        created-at: current-height,
        updated-at: current-height
      }
    )

    (var-set next-mr-id (+ mr-id u1))
    (ok mr-id)
  )
)

;; Vote on a merge request
(define-public (vote-on-merge-request (repo-id uint) (mr-id uint) (approve bool))
  (let (
    (mr (unwrap! (map-get? merge-requests { repo-id: repo-id, mr-id: mr-id }) ERR_NOT_FOUND))
    (current-height stacks-block-height)
    (voter-balance (stx-get-balance tx-sender))
    (voting-weight (if (>= voter-balance (var-get min-voting-weight)) u1 u0))
  )
    (asserts! (< current-height (get voting-ends mr)) ERR_VOTING_ENDED)
    (asserts! (is-eq (get status mr) MR_PENDING) ERR_INVALID_STATUS)
    (asserts! (is-none (map-get? votes { repo-id: repo-id, mr-id: mr-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    (asserts! (> voting-weight u0) ERR_INSUFFICIENT_FUNDS)

    ;; Record vote
    (map-set votes
      { repo-id: repo-id, mr-id: mr-id, voter: tx-sender }
      {
        vote: approve,
        weight: voting-weight,
        timestamp: current-height
      }
    )

    ;; Update vote counts
    (map-set merge-requests
      { repo-id: repo-id, mr-id: mr-id }
      (merge mr {
        votes-for: (if approve
          (+ (get votes-for mr) voting-weight)
          (get votes-for mr)),
        votes-against: (if approve
          (get votes-against mr)
          (+ (get votes-against mr) voting-weight)),
        updated-at: current-height
      })
    )

    (ok true)
  )
)

;; Finalize merge request voting
(define-public (finalize-merge-request (repo-id uint) (mr-id uint))
  (let (
    (mr (unwrap! (map-get? merge-requests { repo-id: repo-id, mr-id: mr-id }) ERR_NOT_FOUND))
    (current-height stacks-block-height)
    (total-votes (+ (get votes-for mr) (get votes-against mr)))
    (approval-rate (if (> total-votes u0)
      (/ (* (get votes-for mr) u100) total-votes)
      u0))
  )
    (asserts! (>= current-height (get voting-ends mr)) ERR_VOTING_ACTIVE)
    (asserts! (is-eq (get status mr) MR_PENDING) ERR_INVALID_STATUS)

    (let (
      (new-status (if (>= approval-rate MIN_APPROVAL_THRESHOLD) MR_APPROVED MR_REJECTED))
    )
      ;; Update MR status
      (map-set merge-requests
        { repo-id: repo-id, mr-id: mr-id }
        (merge mr {
          status: new-status,
          updated-at: current-height
        })
      )

      ;; If approved and has bounty, transfer to author
      (if (and (is-eq new-status MR_APPROVED) (> (get bounty-amount mr) u0))
        (begin
          (unwrap! (as-contract (stx-transfer? (get bounty-amount mr) tx-sender (get author mr))) ERR_INSUFFICIENT_FUNDS)

          ;; Update contributor bounty earnings
          (let (
            (contributor-record (default-to
              { commits: u0, lines-contributed: u0, bounties-earned: u0,
                reputation-score: u50, first-contribution: current-height,
                last-contribution: current-height }
              (map-get? contributors { repo-id: repo-id, contributor: (get author mr) })
            ))
          )
            (map-set contributors
              { repo-id: repo-id, contributor: (get author mr) }
              (merge contributor-record {
                bounties-earned: (+ (get bounties-earned contributor-record) (get bounty-amount mr)),
                reputation-score: (+ (get reputation-score contributor-record) u10)
              })
            )
          )
        )
        true
      )

      (ok new-status)
    )
  )
)

;; =============================================================================
;; AUTOMATED TESTING INTEGRATION
;; =============================================================================

;; Update test results for a merge request (called by automated testing system)
(define-public (update-test-results
  (repo-id uint)
  (mr-id uint)
  (test-status uint)
  (test-results (string-utf8 256))
)
  (let (
    (repo (unwrap! (map-get? repositories { repo-id: repo-id }) ERR_NOT_FOUND))
    (mr (unwrap! (map-get? merge-requests { repo-id: repo-id, mr-id: mr-id }) ERR_NOT_FOUND))
  )
    ;; Only repo owner or contract owner can update test results
    (asserts! (or (is-eq tx-sender (get owner repo)) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (<= test-status TEST_FAILED) ERR_INVALID_INPUT)

    (map-set merge-requests
      { repo-id: repo-id, mr-id: mr-id }
      (merge mr {
        test-status: test-status,
        test-results: test-results,
        updated-at: stacks-block-height
      })
    )

    (ok true)
  )
)

;; =============================================================================
;; BOUNTY MANAGEMENT FUNCTIONS
;; =============================================================================

;; Create a bounty
(define-public (create-bounty
  (repo-id uint)
  (title (string-ascii 128))
  (description (string-utf8 512))
  (amount uint)
  (expires-in-blocks uint)
)
  (let (
    (repo (unwrap! (map-get? repositories { repo-id: repo-id }) ERR_NOT_FOUND))
    (bounty-id (var-get next-bounty-id))
    (current-height stacks-block-height)
  )
    (asserts! (is-eq (get status repo) REPO_ACTIVE) ERR_INVALID_STATUS)
    (asserts! (> amount u0) ERR_INVALID_INPUT)
    (asserts! (> expires-in-blocks u0) ERR_INVALID_INPUT)

    ;; Transfer bounty amount to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR_INSUFFICIENT_FUNDS)

    (map-set bounties
      { repo-id: repo-id, bounty-id: bounty-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        amount: amount,
        claimed-by: none,
        mr-id: none,
        status: u1, ;; open
        created-at: current-height,
        expires-at: (+ current-height expires-in-blocks)
      }
    )

    ;; Update repository bounty count
    (map-set repositories
      { repo-id: repo-id }
      (merge repo {
        total-bounties: (+ (get total-bounties repo) u1)
      })
    )

    (var-set next-bounty-id (+ bounty-id u1))
    (ok bounty-id)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get repository details
(define-read-only (get-repository (repo-id uint))
  (map-get? repositories { repo-id: repo-id })
)

;; Get merge request details
(define-read-only (get-merge-request (repo-id uint) (mr-id uint))
  (map-get? merge-requests { repo-id: repo-id, mr-id: mr-id })
)

;; Get commit details
(define-read-only (get-commit (repo-id uint) (commit-hash (string-ascii 64)))
  (map-get? commits { repo-id: repo-id, commit-hash: commit-hash })
)

;; Get contributor stats
(define-read-only (get-contributor-stats (repo-id uint) (contributor principal))
  (map-get? contributors { repo-id: repo-id, contributor: contributor })
)

;; Get bounty details
(define-read-only (get-bounty (repo-id uint) (bounty-id uint))
  (map-get? bounties { repo-id: repo-id, bounty-id: bounty-id })
)

;; Get vote details
(define-read-only (get-vote (repo-id uint) (mr-id uint) (voter principal))
  (map-get? votes { repo-id: repo-id, mr-id: mr-id, voter: voter })
)

;; Calculate reputation score for a contributor
(define-read-only (calculate-reputation (repo-id uint) (contributor principal))
  (match (map-get? contributors { repo-id: repo-id, contributor: contributor })
    contributor-data
    (let (
      (base-score (get reputation-score contributor-data))
      (commit-bonus (* (get commits contributor-data) u2))
      (bounty-bonus (/ (get bounties-earned contributor-data) u1000000)) ;; 1 point per 1 STX earned
    )
      (+ base-score commit-bonus bounty-bonus)
    )
    u0
  )
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

;; Update platform fee rate (only contract owner)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_INPUT) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Archive a repository (only repo owner)
(define-public (archive-repository (repo-id uint))
  (let (
    (repo (unwrap! (map-get? repositories { repo-id: repo-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner repo)) ERR_UNAUTHORIZED)
    (map-set repositories
      { repo-id: repo-id }
      (merge repo { status: REPO_ARCHIVED })
    )
    (ok true)
  )
)
