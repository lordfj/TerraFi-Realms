;; Tokenized Real Estate Protocol 

;; Constants
(define-constant ERR-NOT-MANAGER (err u1))
(define-constant ERR-PROPERTY-UNLISTED (err u2))
(define-constant ERR-INVALID-OFFERING (err u3))
(define-constant ERR-OFFERING-SETTLED (err u4))
(define-constant ERR-INVALID-PARAMETER (err u5))
(define-constant ERR-INSUFFICIENT-SHARES (err u6))
(define-constant ERR-OFFERING-EXISTS (err u7))
(define-constant ERR-ALREADY-ALLOCATED (err u8))
(define-constant ERR-NOT-AUTHORIZED (err u9))
(define-constant MAX-PROPERTY-ID u1000) ;; Maximum allowed property ID

;; Data Variables
(define-data-var portfolio-manager principal tx-sender)
(define-data-var portfolio-active bool false)
(define-data-var investment-period uint u0)
(define-data-var minimum-investment uint u1000000) ;; 1 token minimum investment
(define-data-var capital-reserves uint u0)
(define-data-var approval-percentage uint u50) ;; 50% shareholder approval required

;; Property Offering Structure
(define-map property-offerings
    uint
    {
        title: (string-utf8 128),
        location: (string-utf8 512),
        property-deed: (buff 32),
        renovation-budget: uint,
        shares-allocated: uint,
        shares-available: uint,
        total-shares: uint,
        settled: bool,
        profitable: bool
    }
)

;; Investor Profiles
(define-map investor-profiles
    principal
    {
        share-balance: uint,
        properties-owned: (list 20 uint),
        voting-rights: uint
    }
)

;; Investment Records
(define-map investment-records
    {property-id: uint, investor: principal}
    {
        approved-renovations: bool,
        shares-owned: uint
    }
)

;; Authorization
(define-private (is-manager)
    (is-eq tx-sender (var-get portfolio-manager)))

;; Portfolio Management Functions
(define-public (activate-portfolio)
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set portfolio-active true)
        (var-set investment-period u0)
        (var-set capital-reserves u0)
        (ok true)))

(define-public (list-property
    (property-id uint)
    (title (string-utf8 128))
    (location (string-utf8 512))
    (property-deed (buff 32))
    (renovation-budget uint))
    (let (
        (total-share-supply u10000000) ;; 10M total shares
        )
        
        ;; Check portfolio status
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        
        ;; Validate property-id is within acceptable range
        (asserts! (<= property-id MAX-PROPERTY-ID) ERR-INVALID-PARAMETER)
        
        ;; Check if property already exists
        (asserts! (is-none (map-get? property-offerings property-id)) ERR-OFFERING-EXISTS)
        
        ;; Validate title and location are not empty
        (asserts! (> (len title) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len location) u0) ERR-INVALID-PARAMETER)
        
        ;; Set the property data
        (map-set property-offerings property-id
            {
                title: title,
                location: location,
                property-deed: property-deed,
                renovation-budget: renovation-budget,
                shares-allocated: u0,
                shares-available: total-share-supply,
                total-shares: total-share-supply,
                settled: false,
                profitable: false
            })
        
        (ok true)))

;; Investor Registration Functions
(define-public (register-investor (share-amount uint))
    (begin
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        ;; Require minimum share amount
        (asserts! (>= share-amount (var-get minimum-investment)) ERR-INSUFFICIENT-SHARES)
        
        ;; Transfer tokens to portfolio capital reserves
        (try! (stx-transfer? share-amount tx-sender (var-get portfolio-manager)))
        
        ;; Initialize investor profile
        (map-set investor-profiles tx-sender
            {
                share-balance: share-amount,
                properties-owned: (list),
                voting-rights: share-amount
            })
            
        ;; Update capital reserves
        (var-set capital-reserves (+ (var-get capital-reserves) share-amount))
        
        (ok true)))

;; Investment Functions
(define-public (invest-in-property
    (property-id uint)
    (approve-renovations bool))
    (let (
        (property (unwrap! (map-get? property-offerings property-id) ERR-INVALID-OFFERING))
        (investor (unwrap! (map-get? investor-profiles tx-sender) ERR-INSUFFICIENT-SHARES))
        (voting-rights (get voting-rights investor))
        )
        
        ;; Check portfolio status
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        
        ;; Check property hasn't been settled
        (asserts! (not (get settled property)) ERR-OFFERING-SETTLED)
        
        ;; Check investor hasn't already invested in this property
        (asserts! (is-none (map-get? investment-records {property-id: property-id, investor: tx-sender})) ERR-ALREADY-ALLOCATED)
        
        ;; Record investment
        (map-set investment-records 
            {property-id: property-id, investor: tx-sender}
            {
                approved-renovations: approve-renovations,
                shares-owned: voting-rights
            })
        
        ;; Update share allocation counts
        (if approve-renovations
            (map-set property-offerings property-id
                (merge property {shares-allocated: (+ (get shares-allocated property) voting-rights)}))
            (map-set property-offerings property-id
                (merge property {shares-available: (- (get shares-available property) voting-rights)}))
        )
        
        ;; Update investor properties owned
        (map-set investor-profiles tx-sender
            (merge investor {
                properties-owned: (unwrap! (as-max-len? 
                    (append (get properties-owned investor) property-id) u20)
                    ERR-INVALID-PARAMETER)
            }))
        
        (ok true)))

;; Property Settlement
(define-public (settle-property (property-id uint))
    (let (
        (property (unwrap! (map-get? property-offerings property-id) ERR-INVALID-OFFERING))
        )
        
        ;; Check portfolio status
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        
        ;; Only manager can settle properties
        (asserts! (is-manager) ERR-NOT-AUTHORIZED)
        
        ;; Check property hasn't been settled
        (asserts! (not (get settled property)) ERR-OFFERING-SETTLED)
        
        ;; Calculate if approval threshold was reached
        (let (
            (approval-threshold (/ (* (get total-shares property) (var-get approval-percentage)) u100))
            (property-profitable (>= (get shares-allocated property) approval-threshold))
            )
            
            ;; Update property status
            (map-set property-offerings property-id
                (merge property {
                    settled: true,
                    profitable: property-profitable
                }))
            
            ;; If property profitable and needs renovation budget, allocate funds
            (if (and property-profitable (> (get renovation-budget property) u0))
                (begin
                    ;; Ensure capital reserves has enough balance
                    (asserts! (>= (var-get capital-reserves) (get renovation-budget property)) ERR-INSUFFICIENT-SHARES)
                    
                    ;; Update capital reserves
                    (var-set capital-reserves (- (var-get capital-reserves) (get renovation-budget property)))
                    
                    (ok true))
                (ok false)))))

;; Read-only functions
(define-read-only (get-property-details (property-id uint))
    (map-get? property-offerings property-id))

(define-read-only (get-investor-profile (investor principal))
    (map-get? investor-profiles investor))

(define-read-only (get-investment-record (property-id uint) (investor principal))
    (map-get? investment-records {property-id: property-id, investor: investor}))

(define-read-only (get-portfolio-metrics)
    {
        active: (var-get portfolio-active),
        investment-period: (var-get investment-period),
        capital-reserves: (var-get capital-reserves),
        minimum-investment: (var-get minimum-investment),
        approval-percentage: (var-get approval-percentage)
    })

(define-public (update-minimum-investment (new-minimum uint))
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set minimum-investment new-minimum)
        (ok true)))

(define-public (update-approval-percentage (new-percentage uint))
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        ;; Validate percentage is between 1 and 100
        (asserts! (and (> new-percentage u0) (<= new-percentage u100)) ERR-INVALID-PARAMETER)
        (var-set approval-percentage new-percentage)
        (ok true)))

(define-public (freeze-portfolio)
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set portfolio-active false)
        (ok true)))

(define-public (advance-investment-period)
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        (var-set investment-period (+ (var-get investment-period) u1))
        (ok true)))

(define-public (transfer-manager-role (new-manager principal))
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set portfolio-manager new-manager)
        (ok true)))