(ns toolshed.style)

(defn map-class
  "Adds the class keyword to children elements' class keywords, in hiccup shorthand.
   Expects the class-kw to be a keyword like, :.flex.flex-row.p-4"
  [class-kw & children]
  (->> children
       (map #(if (keyword? (first %))
               (vec (concat [(keyword (str (name (first %)) (name class-kw)))]
                            (rest %)))
               %))
       (cons :<>)
       vec))

(comment
  (= [:<>
      [:div.flex-1.p-4.rounded-lg.text-center "hello"]
      [:div.bg-green-200.flex-1.p-4.rounded-lg.text-center "the"]
      [:div.bg-blue-200.flex-1.p-4.rounded-lg.text-center "world"]]
     (map-class
      :.flex-1.p-4.rounded-lg.text-center
      [:div "hello"]
      [:div.bg-green-200 "the"]
      [:div.bg-blue-200 "world"])))
