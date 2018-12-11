(ns advent-of-code-2018.day-seven)

(def input ["Step X must be finished before step M can begin.","Step A must be finished before step R can begin.","Step C must be finished before step K can begin.","Step H must be finished before step G can begin.","Step R must be finished before step Z can begin.","Step S must be finished before step K can begin.","Step K must be finished before step G can begin.","Step O must be finished before step Z can begin.","Step Q must be finished before step G can begin.","Step E must be finished before step Y can begin.","Step U must be finished before step I can begin.","Step G must be finished before step N can begin.","Step M must be finished before step P can begin.","Step Y must be finished before step I can begin.","Step I must be finished before step V can begin.","Step Z must be finished before step B can begin.","Step W must be finished before step V can begin.","Step D must be finished before step P can begin.","Step L must be finished before step J can begin.","Step N must be finished before step T can begin.","Step T must be finished before step P can begin.","Step B must be finished before step F can begin.","Step F must be finished before step P can begin.","Step J must be finished before step V can begin.","Step V must be finished before step P can begin.","Step Z must be finished before step F can begin.","Step B must be finished before step J can begin.","Step B must be finished before step P can begin.","Step X must be finished before step F can begin.","Step Y must be finished before step N can begin.","Step W must be finished before step D can begin.","Step G must be finished before step B can begin.","Step L must be finished before step V can begin.","Step K must be finished before step L can begin.","Step W must be finished before step P can begin.","Step E must be finished before step F can begin.","Step Y must be finished before step J can begin.","Step J must be finished before step P can begin.","Step A must be finished before step O can begin.","Step O must be finished before step E can begin.","Step T must be finished before step V can begin.","Step S must be finished before step E can begin.","Step I must be finished before step L can begin.","Step E must be finished before step B can begin.","Step G must be finished before step J can begin.","Step Z must be finished before step J can begin.","Step K must be finished before step T can begin.","Step L must be finished before step F can begin.","Step X must be finished before step S can begin.","Step U must be finished before step G can begin.","Step K must be finished before step N can begin.","Step Q must be finished before step W can begin.","Step H must be finished before step F can begin.","Step O must be finished before step P can begin.","Step M must be finished before step D can begin.","Step T must be finished before step J can begin.","Step G must be finished before step T can begin.","Step N must be finished before step P can begin.","Step O must be finished before step V can begin.","Step Q must be finished before step I can begin.","Step Z must be finished before step T can begin.","Step C must be finished before step J can begin.","Step D must be finished before step J can begin.","Step G must be finished before step W can begin.","Step U must be finished before step L can begin.","Step R must be finished before step B can begin.","Step H must be finished before step K can begin.","Step X must be finished before step I can begin.","Step X must be finished before step B can begin.","Step I must be finished before step P can begin.","Step L must be finished before step N can begin.","Step O must be finished before step Y can begin.","Step F must be finished before step J can begin.","Step E must be finished before step I can begin.","Step G must be finished before step M can begin.","Step Q must be finished before step E can begin.","Step D must be finished before step F can begin.","Step A must be finished before step Z can begin.","Step I must be finished before step D can begin.","Step B must be finished before step V can begin.","Step U must be finished before step J can begin.","Step Y must be finished before step T can begin.","Step O must be finished before step M can begin.","Step M must be finished before step B can begin.","Step M must be finished before step L can begin.","Step N must be finished before step B can begin.","Step X must be finished before step U can begin.","Step E must be finished before step Z can begin.","Step Z must be finished before step L can begin.","Step R must be finished before step E can begin.","Step M must be finished before step I can begin.","Step H must be finished before step N can begin.","Step X must be finished before step J can begin.","Step C must be finished before step S can begin.","Step R must be finished before step I can begin.","Step E must be finished before step D can begin.","Step Y must be finished before step L can begin.","Step S must be finished before step D can begin.","Step U must be finished before step Z can begin.","Step A must be finished before step C can begin.","Step Y must be finished before step W can begin."])
(def easy-input ["Step C must be finished before step A can begin.", "Step C must be finished before step F can begin.", "Step A must be finished before step B can begin.", "Step A must be finished before step D can begin.", "Step B must be finished before step E can begin.", "Step D must be finished before step E can begin.", "Step F must be finished before step E can begin."])

(def line-regex #"Step (.) must be finished before step (.) can begin.")

(defn parse-line
  [line]
  (let [match (->> line
                   (re-matcher line-regex)
                   (re-find))]
    [(nth match 1) (nth match 2)]))

(defn identify-nodes
  [parsed-dependencies]
  (let [distinct-nodes (distinct (flatten parsed-dependencies))]
    (zipmap
      distinct-nodes
      (repeat []))))

(defn add-dependency
  [dependency-map dependency-relationship]
  (let [antecedent (first dependency-relationship)
        dependant (last dependency-relationship)
        existing-dependencies (get dependency-map dependant)]
    (assoc dependency-map dependant (vec (sort (conj existing-dependencies antecedent))))))

(defn construct-dependency-map
  [parsed-dependencies]
  (let [distinct-nodes (identify-nodes parsed-dependencies)]
    (reduce add-dependency distinct-nodes parsed-dependencies)))

(defn no-more-dependencies?
  [item-to-dependencies]
  (let [item (first item-to-dependencies)
        dependencies (last item-to-dependencies)]
    ;(print item "->" dependencies "\n")
    (empty? dependencies)))

(defn find-next-step
  [dependency-map]
  (let [no-dependency-nodes (filter no-more-dependencies? (seq dependency-map))]
    (first (sort (map first no-dependency-nodes)))))

(defn complete-step
  [dependencies completed-step]
  (print "Completing step " completed-step  "\n")
  (let [is-completed? (fn ([item] (= item completed-step)))
        values (map #(vec (remove is-completed? %)) (vals dependencies))]
    (dissoc (zipmap
      (keys dependencies)
      values) completed-step)))

(defn determine-step-order
  [step-array]
  (let [parsed-steps (map parse-line step-array)
        dependency-map (construct-dependency-map parsed-steps)]
    (loop [step-order ""
           next-step (find-next-step dependency-map)
           dependencies dependency-map]
      (if (nil? next-step)
        step-order
        (let [updated-dependencies (complete-step dependencies next-step)]
          (recur (str step-order next-step) (find-next-step updated-dependencies) updated-dependencies))))))