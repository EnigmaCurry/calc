(ns calc.download-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]
            [calc.eval :as ev]
            [calc.test-helpers :as th]))

;; ==========================================================================
;; Parser tests
;; ==========================================================================

(deftest parses-download-explicit-units
  (testing "download with explicit data and rate units"
    (is (= :download (:op (parser/parse-request "download 10MB 1000Mbps"))))
    (is (= :download (:op (parser/parse-request "download 10GB 1Gbps"))))
    (is (= :download (:op (parser/parse-request "upload 100MB 10Mbps"))))))

(deftest parses-download-lazy-units
  (testing "bare prefix letters resolve correctly"
    (let [r (parser/parse-request "download 1g 1g")]
      (is (= :download (:op r)))
      (is (= :GB (:size-unit r)))
      (is (= {:Gb 1 :s -1} (:rate-unit r))))
    (let [r (parser/parse-request "download 10gb 1G")]
      (is (= :download (:op r)))
      (is (= :GB (:size-unit r)))
      (is (= {:Gb 1 :s -1} (:rate-unit r))))))

(deftest parses-upload-alias
  (testing "upload works the same as download"
    (let [r (parser/parse-request "upload 5GB 100Mbps")]
      (is (= :download (:op r)))
      (is (== 5 (:size-value r)))
      (is (= :GB (:size-unit r))))))

;; ==========================================================================
;; Eval tests
;; ==========================================================================

(deftest evaluates-download-time
  (testing "1 GB at 1 Gbps = 8 seconds"
    (let [r (ev/convert-request {:op :download
                                 :size-value 1 :size-unit :GB
                                 :rate-value 1 :rate-unit {:Gb 1 :s -1}
                                 :size-label "GB" :rate-label "Gbps"})]
      (is (:ok? r))
      (is (== 8 (:time-seconds r)))))

  (testing "10 MB at 1000 Mbps = 0.08 seconds"
    (let [r (ev/convert-request {:op :download
                                 :size-value 10 :size-unit :MB
                                 :rate-value 1000 :rate-unit {:Mb 1 :s -1}
                                 :size-label "MB" :rate-label "Mbps"})]
      (is (:ok? r))
      (is (th/approx== 0.08 (double (:time-seconds r))))))

  (testing "10 GB at 1 Gbps = 80 seconds"
    (let [r (ev/convert-request {:op :download
                                 :size-value 10 :size-unit :GB
                                 :rate-value 1 :rate-unit {:Gb 1 :s -1}
                                 :size-label "GB" :rate-label "Gbps"})]
      (is (:ok? r))
      (is (== 80 (:time-seconds r))))))

;; ==========================================================================
;; End-to-end tests
;; ==========================================================================

(deftest end-to-end-download
  (testing "download 1g 1g"
    (let [{:keys [result]} (th/evaluate "download 1g 1g" nil)]
      (is (= "1 GB at 1 Gbps → 8 s" result))))

  (testing "download 10GB 1Gbps"
    (let [{:keys [result]} (th/evaluate "download 10GB 1Gbps" nil)]
      (is (= "10 GB at 1 Gbps → 1 min 20 s" result))))

  (testing "upload works same as download"
    (let [{:keys [result]} (th/evaluate "upload 1g 1g" nil)]
      (is (= "1 GB at 1 Gbps → 8 s" result)))))
