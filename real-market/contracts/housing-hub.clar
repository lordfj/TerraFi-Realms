;; Tokenized Real Estate Protocol - v1
;; A basic implementation of a real estate tokenization system on Stacks blockchain

;; Constants
(define-constant ERR-NOT-MANAGER (err u1))
(define-constant ERR-PROPERTY-UNLISTED (err u2))
(define-constant ERR-INVALID-OFFERING (err u3))
(define-constant ERR-INVALID-PARAMETER (err u5))
(define-constant ERR-INSUFFICIENT-SHARES (err u6))
(define-constant ERR-NOT-AUTHORIZED (err u9))

;; Data Variables
(define-data-var portfolio-manager principal tx-sender)
(define-data-var portfolio-active bool false)
(define-data-var minimum-investment uint u1000000) ;; 1 million minimum investment
(define-data-var capital-reserves uint u0)

;; Property Offering Structure
(define-map property-offerings
    uint
    {
        title: (string-utf8 128),
        location: (string-utf8 512),
        property-deed: (buff 32),
        shares-available: uint,
        total-shares: uint,
        settled: bool
    }
)

;; Investor Profiles
(define-map investor-profiles
    principal
    {
        share-balance: uint,
        properties-owned: (list 10 uint)
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
        (var-set capital-reserves u0)
        (ok true)))

(define-public (list-property
    (property-id uint)
    (title (string-utf8 128))
    (location (string-utf8 512))
    (property-deed (buff 32)))
    (begin
        ;; Check portfolio status
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        
        ;; Validate title and location are not empty
        (asserts! (> (len title) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len location) u0) ERR-INVALID-PARAMETER)
        
        ;; Set the property data
        (map-set property-offerings property-id
            {
                title: title,
                location: location,
                property-deed: property-deed,
                shares-available: u10000000, ;; 10M total shares
                total-shares: u10000000,
                settled: false
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
                properties-owned: (list)
            })
            
        ;; Update capital reserves
        (var-set capital-reserves (+ (var-get capital-reserves) share-amount))
        
        (ok true)))

;; Investment Functions
(define-public (invest-in-property
    (property-id uint)
    (share-amount uint))
    (let (
        (property (unwrap! (map-get? property-offerings property-id) ERR-INVALID-OFFERING))
        (investor (unwrap! (map-get? investor-profiles tx-sender) ERR-INSUFFICIENT-SHARES))
        )
        
        ;; Check portfolio status
        (asserts! (var-get portfolio-active) ERR-PROPERTY-UNLISTED)
        
        ;; Check property hasn't been settled
        (asserts! (not (get settled property)) ERR-INVALID-OFFERING)
        
        ;; Check share amount is valid
        (asserts! (<= share-amount (get shares-available property)) ERR-INSUFFICIENT-SHARES)
        (asserts! (> share-amount u0) ERR-INVALID-PARAMETER)
        
        ;; Update investor profile
        (map-set investor-profiles tx-sender
            (merge investor {
                share-balance: (+ (get share-balance investor) share-amount),
                properties-owned: (unwrap! (as-max-len? 
                    (append (get properties-owned investor) property-id) u10)
                    ERR-INVALID-PARAMETER)
            }))
        
        ;; Update property
        (map-set property-offerings property-id
            (merge property {
                shares-available: (- (get shares-available property) share-amount)
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
        (asserts! (not (get settled property)) ERR-INVALID-OFFERING)
        
        ;; Update property status
        (map-set property-offerings property-id
            (merge property {
                settled: true
            }))
            
        (ok true)))

;; Read-only functions
(define-read-only (get-property-details (property-id uint))
    (map-get? property-offerings property-id))

(define-read-only (get-investor-profile (investor principal))
    (map-get? investor-profiles investor))

(define-read-only (get-portfolio-metrics)
    {
        active: (var-get portfolio-active),
        capital-reserves: (var-get capital-reserves),
        minimum-investment: (var-get minimum-investment)
    })

(define-public (update-minimum-investment (new-minimum uint))
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set minimum-investment new-minimum)
        (ok true)))

(define-public (freeze-portfolio)
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set portfolio-active false)
        (ok true)))

(define-public (transfer-manager-role (new-manager principal))
    (begin
        (asserts! (is-manager) ERR-NOT-MANAGER)
        (var-set portfolio-manager new-manager)
        (ok true)))