;; Renewable Energy DAO - A decentralized investment and governance platform for renewable energy assets
;; This contract enables users to invest in renewable energy projects through fractional ownership,
;; distribute revenue, and participate in governance decisions using a voting system.

;; Constants and Configuration
(define-constant contract-owner tx-sender)
(define-constant minimum-investment u1000000) ;; 1 STX minimum
(define-constant share-precision-factor u1000000) ;; 6 decimal places
(define-constant proposal-approval-threshold u75) ;; 75% threshold for proposals
(define-constant maintenance-block-window u144) ;; ~24 hours in blocks
(define-constant max-asset-id u1000000) ;; Maximum asset ID for validation

;; Error Codes
(define-constant ERR-UNAUTHORIZED-ACCESS (err u401))
(define-constant ERR-ASSET-ALREADY-EXISTS (err u402))
(define-constant ERR-INVALID-INVESTMENT-AMOUNT (err u403))
(define-constant ERR-ASSET-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-SHARE-BALANCE (err u405))
(define-constant ERR-TRANSFER-OPERATION-FAILED (err u406))
(define-constant ERR-RECORD-NOT-FOUND (err u407))
(define-constant ERR-INVALID-ASSET-NAME (err u408))
(define-constant ERR-UNSUPPORTED-ASSET-TYPE (err u409))
(define-constant ERR-INVALID-RECIPIENT-ADDRESS (err u410))
(define-constant ERR-SELF-TRANSFER-PROHIBITED (err u411))
(define-constant ERR-ZERO-SHARE-QUANTITY (err u412))
(define-constant ERR-PROPOSAL-STILL-ACTIVE (err u413))
(define-constant ERR-PROPOSAL-VOTING-EXPIRED (err u414))
(define-constant ERR-ALREADY-CAST-VOTE (err u415))
(define-constant ERR-INVALID-ASSET-STATUS (err u416))
(define-constant ERR-QUORUM-NOT-REACHED (err u417))
(define-constant ERR-INVALID-ASSET-ID (err u418))
(define-constant ERR-INVALID-PROPOSAL-CATEGORY (err u419))
(define-constant ERR-INVALID-PROPOSAL-DETAILS (err u420))
(define-constant ERR-INVALID-REQUESTED-FUNDS (err u421))

;; Data Types
(define-trait asset-ownership-trait
  (
    (transfer-ownership (uint principal uint) (response bool uint))
    (claim-revenue (uint) (response uint uint))
  )
)

;; Asset Registry
(define-map renewable-assets
    { asset-id: uint }
    {
        asset-name: (string-ascii 50),
        energy-type: (string-ascii 20),
        total-share-count: uint,
        remaining-shares: uint,
        share-price-in-stx: uint,
        cumulative-revenue: uint,
        revenue-per-share-unit: uint,
        operational-status: (string-ascii 10),
        creation-block-height: uint,
        last-update-block-height: uint,
        geographic-location: {
            latitude-coordinate: int,
            longitude-coordinate: int
        }
    }
)

;; Shareholder Registry
(define-map shareholder-registry
    { asset-id: uint, shareholder: principal }
    {
        share-balance: uint,
        claimed-revenue-amount: uint,
        last-claim-block-height: uint,
        governance-voting-power: uint
    }
)

;; Performance Metrics
(define-map performance-metrics
    { asset-id: uint }
    {
        lifetime-energy-output: uint,
        total-operational-hours: uint,
        accumulated-maintenance-costs: uint,
        current-efficiency-rating: uint,
        total-carbon-offset: uint,
        maximum-power-output: uint,
        calculated-lifetime-roi: uint
    }
)

;; Governance Parameters
(define-map governance-rules
    { asset-id: uint }
    {
        minimum-quorum-percentage: uint,
        voting-period-duration: uint,
        minimum-approval-percentage: uint,
        execution-cooldown-blocks: uint
    }
)

;; Governance Proposals
(define-map asset-proposals
    { asset-id: uint, proposal-id: uint }
    {
        proposal-creator: principal,
        proposal-category: (string-ascii 20),
        proposal-details: (string-ascii 500),
        requested-funds: uint,
        approval-vote-count: uint,
        rejection-vote-count: uint,
        proposal-status: (string-ascii 10),
        submission-block-height: uint,
        voting-end-block-height: uint,
        implementation-delay-blocks: uint,
        quorum-requirement-met: bool
    }
)

;; State Variables
(define-data-var next-asset-id uint u0)
(define-data-var next-proposal-id uint u0)
(define-data-var maintenance-schedule-counter uint u0)

;; Private Helper Functions
(define-private (validate-asset-name (name (string-ascii 50)))
    (let ((name-character-count (len name)))
        (and 
            (> name-character-count u0)
            (<= name-character-count u50)
            (not (is-eq name "")))))

(define-private (validate-energy-type (energy-type (string-ascii 20)))
    (or 
        (is-eq energy-type "solar")
        (is-eq energy-type "wind")
        (is-eq energy-type "hydro")
        (is-eq energy-type "biomass")))

(define-private (validate-recipient-address (recipient principal))
    (and
        (not (is-eq recipient tx-sender))
        (not (is-eq recipient (as-contract tx-sender)))))

(define-private (calculate-shareholder-voting-power (share-count uint) (total-shares uint))
    (/ (* share-count share-precision-factor) total-shares))

(define-private (validate-asset-id (asset-id uint))
    (and
        (> asset-id u0)
        (<= asset-id (var-get next-asset-id))
        (<= asset-id max-asset-id)
        (is-some (map-get? renewable-assets { asset-id: asset-id }))))

(define-private (validate-proposal-category (category (string-ascii 20)))
    (let ((category-len (len category)))
        (and 
            (> category-len u0)
            (<= category-len u20))))

(define-private (validate-proposal-details (details (string-ascii 500)))
    (let ((details-len (len details)))
        (and 
            (> details-len u0)
            (<= details-len u500))))

;; Public Functions - Asset Management

(define-public (register-renewable-asset 
    (asset-name (string-ascii 50))
    (energy-type (string-ascii 20))
    (total-share-count uint)
    (share-price-in-stx uint)
    (latitude-coordinate int)
    (longitude-coordinate int))
    (let ((new-asset-id (+ (var-get next-asset-id) u1)))
        (asserts! (validate-asset-name asset-name) ERR-INVALID-ASSET-NAME)
        (asserts! (validate-energy-type energy-type) ERR-UNSUPPORTED-ASSET-TYPE)
        (asserts! (> total-share-count u0) ERR-INVALID-INVESTMENT-AMOUNT)
        (asserts! (> share-price-in-stx u0) ERR-INVALID-INVESTMENT-AMOUNT)
        (asserts! (>= (* share-price-in-stx total-share-count) minimum-investment) ERR-INVALID-INVESTMENT-AMOUNT)

        (begin
            (map-set renewable-assets
                { asset-id: new-asset-id }
                {
                    asset-name: asset-name,
                    energy-type: energy-type,
                    total-share-count: total-share-count,
                    remaining-shares: total-share-count,
                    share-price-in-stx: share-price-in-stx,
                    cumulative-revenue: u0,
                    revenue-per-share-unit: u0,
                    operational-status: "proposed",
                    creation-block-height: block-height,
                    last-update-block-height: block-height,
                    geographic-location: {
                        latitude-coordinate: latitude-coordinate,
                        longitude-coordinate: longitude-coordinate
                    }
                })
            (map-set performance-metrics
                { asset-id: new-asset-id }
                {
                    lifetime-energy-output: u0,
                    total-operational-hours: u0,
                    accumulated-maintenance-costs: u0,
                    current-efficiency-rating: u100,
                    total-carbon-offset: u0,
                    maximum-power-output: u0,
                    calculated-lifetime-roi: u0
                })
            (map-set governance-rules
                { asset-id: new-asset-id }
                {
                    minimum-quorum-percentage: u50,          ;; 50% quorum
                    voting-period-duration: u144,            ;; ~24 hours
                    minimum-approval-percentage: u75,        ;; 75% threshold
                    execution-cooldown-blocks: u72           ;; ~12 hours
                })
            (var-set next-asset-id new-asset-id)
            (ok new-asset-id))))

;; Enhanced Share Purchase
(define-public (purchase-asset-shares (asset-id uint) (requested-share-count uint))
    (begin
        ;; Validate asset ID first
        (asserts! (validate-asset-id asset-id) ERR-INVALID-ASSET-ID)
        
        (let ((asset-record (unwrap! (map-get? renewable-assets { asset-id: asset-id })
                          ERR-ASSET-NOT-FOUND))
              (investor-record (default-to 
                                {
                                    share-balance: u0, 
                                    claimed-revenue-amount: u0,
                                    last-claim-block-height: block-height,
                                    governance-voting-power: u0
                                }
                                (map-get? shareholder-registry 
                                    { asset-id: asset-id, shareholder: tx-sender }))))
            (asserts! (> requested-share-count u0) ERR-ZERO-SHARE-QUANTITY)
            (asserts! (<= requested-share-count (get remaining-shares asset-record)) 
                     ERR-INSUFFICIENT-SHARE-BALANCE)
            (asserts! (is-eq (get operational-status asset-record) "active") ERR-INVALID-ASSET-STATUS)

            (let ((total-purchase-cost (* requested-share-count (get share-price-in-stx asset-record)))
                  (updated-voting-power (calculate-shareholder-voting-power 
                                    (+ requested-share-count (get share-balance investor-record))
                                    (get total-share-count asset-record))))
                (begin
                    (try! (stx-transfer? total-purchase-cost tx-sender (as-contract tx-sender)))
                    (map-set renewable-assets
                        { asset-id: asset-id }
                        (merge asset-record {
                            remaining-shares: (- (get remaining-shares asset-record) 
                                              requested-share-count),
                            last-update-block-height: block-height
                        }))
                    (map-set shareholder-registry
                        { asset-id: asset-id, shareholder: tx-sender }
                        {
                            share-balance: (+ (get share-balance investor-record) requested-share-count),
                            claimed-revenue-amount: (get claimed-revenue-amount investor-record),
                            last-claim-block-height: (get last-claim-block-height investor-record),
                            governance-voting-power: updated-voting-power
                        })
                    (ok true))))))

;; Revenue Management
(define-public (distribute-asset-revenue (asset-id uint) (revenue-amount uint))
    (begin
        ;; Validate asset ID first
        (asserts! (validate-asset-id asset-id) ERR-INVALID-ASSET-ID)
        
        (let ((asset-record (unwrap! (map-get? renewable-assets { asset-id: asset-id })
                          ERR-ASSET-NOT-FOUND)))
            (begin
                (asserts! (is-eq tx-sender contract-owner) ERR-UNAUTHORIZED-ACCESS)
                (asserts! (> revenue-amount u0) ERR-INVALID-INVESTMENT-AMOUNT)
                (try! (stx-transfer? revenue-amount tx-sender (as-contract tx-sender)))

                (let ((updated-total-revenue (+ (get cumulative-revenue asset-record) revenue-amount))
                      (updated-per-share-revenue (/ updated-total-revenue 
                                              (get total-share-count asset-record))))
                    (map-set renewable-assets
                        { asset-id: asset-id }
                        (merge asset-record {
                            cumulative-revenue: updated-total-revenue,
                            revenue-per-share-unit: updated-per-share-revenue,
                            last-update-block-height: block-height
                        }))
                    (ok true))))))

;; Governance Functions
(define-public (create-governance-proposal 
    (asset-id uint)
    (proposal-category (string-ascii 20))
    (proposal-details (string-ascii 500))
    (requested-funds uint))
    (begin
        ;; Validate all inputs first
        (asserts! (validate-asset-id asset-id) ERR-INVALID-ASSET-ID)
        (asserts! (validate-proposal-category proposal-category) ERR-INVALID-PROPOSAL-CATEGORY)
        (asserts! (validate-proposal-details proposal-details) ERR-INVALID-PROPOSAL-DETAILS)
        (asserts! (or (is-eq requested-funds u0) (> requested-funds u0)) ERR-INVALID-REQUESTED-FUNDS)
        
        (let ((new-proposal-id (+ (var-get next-proposal-id) u1))
              (governance-settings (unwrap! (map-get? governance-rules { asset-id: asset-id })
                                ERR-ASSET-NOT-FOUND))
              (shareholder-record (unwrap! (map-get? shareholder-registry 
                                        { asset-id: asset-id, shareholder: tx-sender })
                                      ERR-UNAUTHORIZED-ACCESS)))
            (begin
                (asserts! (>= (get governance-voting-power shareholder-record) 
                             (/ share-precision-factor u20)) ;; 5% minimum
                         ERR-INSUFFICIENT-SHARE-BALANCE)

                (map-set asset-proposals
                    { asset-id: asset-id, proposal-id: new-proposal-id }
                    {
                        proposal-creator: tx-sender,
                        proposal-category: proposal-category,
                        proposal-details: proposal-details,
                        requested-funds: requested-funds,
                        approval-vote-count: u0,
                        rejection-vote-count: u0,
                        proposal-status: "active",
                        submission-block-height: block-height,
                        voting-end-block-height: (+ block-height (get voting-period-duration governance-settings)),
                        implementation-delay-blocks: (get execution-cooldown-blocks governance-settings),
                        quorum-requirement-met: false
                    })
                (var-set next-proposal-id new-proposal-id)
                (ok new-proposal-id)))))

;; Read-only Functions
(define-read-only (get-asset-details (asset-id uint))
    (if (validate-asset-id asset-id)
        (map-get? renewable-assets { asset-id: asset-id })
        none))

(define-read-only (get-shareholder-details (asset-id uint) (shareholder principal))
    (if (validate-asset-id asset-id)
        (map-get? shareholder-registry { asset-id: asset-id, shareholder: shareholder })
        none))

(define-read-only (get-asset-performance-metrics (asset-id uint))
    (if (validate-asset-id asset-id)
        (map-get? performance-metrics { asset-id: asset-id })
        none))

(define-read-only (get-asset-governance-rules (asset-id uint))
    (if (validate-asset-id asset-id)
        (map-get? governance-rules { asset-id: asset-id })
        none))

(define-read-only (get-proposal-details 
    (asset-id uint)
    (proposal-id uint))
    (if (validate-asset-id asset-id)
        (map-get? asset-proposals { asset-id: asset-id, proposal-id: proposal-id })
        none))