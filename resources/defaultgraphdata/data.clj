(add-order-with-rating "1"
                       (add-food "Pizza-Brötchen"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1464192331000 5 "")

(add-order-with-rating "1"
                       (add-food "Mam Burger"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1464192331000 3 "")

(add-order-with-rating (find-or-create-person "Heiko" {})
                       (add-food "Pizza-Brötchen"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1464192331000 4 "")


(add-order-with-rating (find-or-create-person "Heiko" {})
                       (add-food "Pizza"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1464192331000 4 "")

(add-order-with-rating "1"
                       (add-food "Ente süß-sauer"
                                 (find-or-create-deliverer "China Boy" {}) {})
                       1
                       1464283100347 2 "")

(add-order-with-rating "1"
                       (add-food "Pizza-Brötchen"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1458492283000 4 "")

(add-order-with-rating "1"
                       (add-food "Pizza"
                                 (find-or-create-deliverer "Napoli" {}) {})
                       1
                       1458492283000 3 "")
