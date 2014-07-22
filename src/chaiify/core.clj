(ns chaiify.core
  (:require [net.cgrand.enlive-html :as enlive]
            [ring.util.response :refer (response resource-response)]))



(defn- process-pred [pred]
  (let [sections (-> (name pred)
                     (.split "-")
                     js->clj)
        last-sect (last sections)
        sections (map #(str "-" %) (butlast sections))]
    (map symbol (concat sections (list last-sect)))))

(defmacro expect [subj pred & params]
  (let [pred-list (process-pred pred)
        between-preds (butlast pred-list)
        last-pred (last pred-list)]
    `(.. (js/expect ~subj) ~@between-preds (~last-pred ~@params))))



;; Ring helper stuff

(defn- head-str [base]
  (enlive/html [:script {:src (str base "/chai.js")}]))

(defn apply-chaiify [html base]
  (enlive/sniptest html
                   [:head] (enlive/append (head-str base))))

(defn wrap [app base]
  (fn [req]
    (let [resp (app req)]
      (condp = (:uri req)
        base (response (apply-chaiify (apply str (:body resp)) base))
        (str base "/chai.js") (resource-response "chai.js")
        resp))))
