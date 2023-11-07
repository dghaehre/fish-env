
# TODO(make it awesome): somehow parse comments as valid fish as well


(defn get-file [filename]
  (let [[success v] (protect (slurp filename))]
    (when (not success)
      (do
        (print (string "Could not read file: " filename))
        (os/exit)))
    v))

(defn parse-line [line]
  (let [l (string/trim line)]
    (cond

      # Match a comment
      (string/has-prefix? "#" l)
      {:type :comment
       :value l}
      
      # Match a variable
      (string/has-prefix? "export " l)
      (let [[key value] (->> (string/slice l 7)
                             (string/trim)
                             (string/split "="))] 
       {:type :variable
        :key key
        :value (string/trim value "\"")}))))
      

(defn parse-env [content]
  (as-> (string/split "\n" content) _
        (filter |(not (empty? $)) _)
        (map parse-line _)
        (filter |(not (nil? $)) _)))

(defn format-value [v]
  (let [number (scan-number (string v))]
    (cond
      (not (nil? number)) number

      (or (= v "true") (= v "false")) v

      (let [comment-start (string/find "#" v)
            w (if (nil? comment-start) v
                 (string/slice v 0 comment-start))]
        (as-> (string/trim w) _
              (string/trim _ "\"")
              (string "'" _ "'"))))))

# TODO: add comment string back in
(defn format [parsed]
  (let [lines (catseq [x :in parsed]
                (cond
                  (= (x :type) :comment) (x :value)
                  (= (x :type) :variable) (string "setenv " (x :key) " " (format-value (x :value)))))]
    (string/join lines "\n")))

(defn main [_ filename &opt output]
  (default output "./output.fish")
  (->> (get-file filename)
       (parse-env)
       (format)
       (spit output)))

(comment

  (def v "something")
  (format-value "true")
  (format-value 3)
  (format-value "something # teste")
  (format-value "\"something\" # teste")

  (string "\"dev\"")
  (def c (slurp "/home/dghaehre/projects/personal/fish-env/test.env"))
  (-> (parse-env c)
      (format)))
